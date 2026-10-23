import { join } from 'node:path'
import { app, BrowserWindow, ipcMain, shell } from 'electron'
import Database from 'better-sqlite3'
import { Engine } from '../engine/engine'
import { startMcp } from '../engine/mcp'
import { WorkerPool } from '../engine/workers'

const LOOP_MS = 60_000
const WORKERS = 4

async function main(): Promise<void> {
  await app.whenReady()

  const db = new Database(join(app.getPath('userData'), 'workflows.db'))
  db.pragma('journal_mode = WAL')
  const engine = new Engine(db)
  const mcp = await startMcp(engine)
  const pool = new WorkerPool(engine, mcp.url, WORKERS)
  engine.on('sourceError', (message: string) => console.error(message))

  // The central loop: every wait that is due, every finished dependency, every source.
  const tick = () => engine.tick().catch((e) => console.error('loop:', e))
  void tick()
  setInterval(tick, LOOP_MS)
  pool.fill()

  const handle = (name: string, fn: (...args: never[]) => unknown) =>
    ipcMain.handle(name, (_e, ...args) => fn(...(args as never[])))
  handle('overview', () => engine.overview(pool.status()))
  handle('detail', (id: string) => engine.detail(id))
  handle('next', (skip: string[]) => engine.next(skip))
  handle('create', (title: string, instruction: string) => engine.create(title, { instruction: instruction || undefined }))
  handle('dispatch', (id: string, instruction: string) => engine.dispatch(id, instruction))
  handle('execute', (id: string, inputs: Record<string, string>) => engine.execute(id, inputs))
  handle('close', (id: string, note: string) => engine.close(id, note))
  handle('handledOutside', (id: string, note: string) => engine.handledOutside(id, note))
  handle('setState', (id: string, state: string) => engine.setState(id, state))
  handle('takeOver', (id: string) => engine.takeOver(id))
  handle('pollSources', () => engine.pollSources(true))

  const win = new BrowserWindow({
    width: 1200,
    height: 800,
    title: 'Workflows',
    webPreferences: { preload: join(__dirname, '../preload/index.js'), sandbox: false },
  })
  win.webContents.setWindowOpenHandler(({ url }) => {
    if (/^https?:/.test(url)) void shell.openExternal(url)
    return { action: 'deny' }
  })
  engine.on('change', () => win.isDestroyed() || win.webContents.send('changed'))
  if (process.env.ELECTRON_RENDERER_URL) void win.loadURL(process.env.ELECTRON_RENDERER_URL)
  else void win.loadFile(join(__dirname, '../renderer/index.html'))

  app.on('window-all-closed', () => app.quit())
}

void main()
