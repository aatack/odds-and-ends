import { join } from 'node:path'
import { app, BrowserWindow, ipcMain, safeStorage, shell } from 'electron'
import Database from 'better-sqlite3'
import type { HandoffSpec } from '../shared/types'
import { Admin } from '../engine/admin'
import { Engine } from '../engine/engine'
import { catalog, startMcp } from '../engine/mcp'
import { plainSecrets, type Secrets } from '../engine/settings'
import { WorkerPool } from '../engine/workers'

const LOOP_MS = 60_000
const WORKERS = 4

/** Tokens are encrypted with the OS keyring where there is one. */
function secrets(): Secrets {
  if (!safeStorage.isEncryptionAvailable()) return plainSecrets
  return {
    encrypt: (s) => `safe:${safeStorage.encryptString(s).toString('base64')}`,
    decrypt: (s) =>
      s.startsWith('safe:') ? safeStorage.decryptString(Buffer.from(s.slice(5), 'base64')) : plainSecrets.decrypt(s),
  }
}

async function main(): Promise<void> {
  await app.whenReady()

  // v2: items as queues of handoffs. The first version's file is left as it was.
  const db = new Database(join(app.getPath('userData'), 'workflows-v2.db'))
  db.pragma('journal_mode = WAL')
  const engine = new Engine({ db, secrets: secrets() })
  const mcp = await startMcp(engine)
  const pool = new WorkerPool(engine, mcp.runUrl, WORKERS)
  const admin = new Admin(engine, mcp, WORKERS)
  engine.on('sourceError', (message: string) => console.error(message))

  // The central loop: every wait that is due, then every source.
  const tick = () => engine.tick().catch((e) => console.error('loop:', e))
  void tick()
  setInterval(tick, LOOP_MS)
  pool.fill()

  const handle = (name: string, fn: (...args: never[]) => unknown) =>
    ipcMain.handle(name, (_e, ...args) => fn(...(args as never[])))
  handle('overview', () => engine.overview(pool.status()))
  handle('detail', (id: string) => engine.detail(id))
  handle('catalog', () => catalog(engine))
  handle('next', (skip: string[]) => engine.next(skip))
  handle('viewed', (id: string) => engine.viewed(id))
  handle('skipped', (id: string) => engine.skipped(id))
  handle('create', (title: string, next: HandoffSpec[]) => engine.create(title, { next }))
  handle('handOff', (id: string, next: HandoffSpec[]) => engine.handOff(id, { next }))
  handle('approve', (id: string, inputs: Record<string, unknown>) => engine.approve(id, inputs))
  handle('close', (id: string) => engine.close(id))
  handle('handledOutside', (id: string) => engine.handledOutside(id))
  handle('takeOver', (id: string) => engine.takeOver(id))
  handle('pollSources', () => engine.pollSources(true))
  handle('integrations', () => admin.integrations())
  handle('addSlack', (token: string) => admin.addSlack(token))
  handle('removeSlack', (id: string) => admin.removeSlack(id))
  handle('mcp', () => admin.mcpView())
  handle('regenerateMcpToken', () => admin.regenerateMcpToken())
  handle('setMcpPort', (port: number) => admin.setMcpPort(port))

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
