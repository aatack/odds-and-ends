import { join } from 'node:path'
import { app, BrowserWindow, ipcMain, safeStorage, shell } from 'electron'
import Database from 'better-sqlite3'
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

  // v4: items are events too, in one tree. The earlier versions' files are left as they were.
  const db = new Database(join(app.getPath('userData'), 'workflows-v4.db'))
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
  // Checks come due by the second (scripts can run that often); this only looks for them.
  setInterval(() => void engine.checkDue().catch((e) => console.error('checks:', e)), 1000)
  pool.fill()

  const handle = (name: string, fn: (...args: never[]) => unknown) =>
    ipcMain.handle(name, (_e, ...args) => fn(...(args as never[])))
  handle('overview', () => engine.overview(pool.status()))
  handle('detail', (id: string) => engine.detail(id))
  handle('eventDetail', (id: string) => engine.eventDetail(id))
  handle('catalog', () => catalog(engine))
  handle('next', (skip: string[]) => engine.next(skip))
  handle('viewed', (id: string) => engine.viewed(id))
  handle('skipped', (id: string) => engine.skipped(id))
  handle('create', (title: string, parentId: string | null) => engine.create(title, { parentId }))
  handle('send', (id: string, preset: string, text: string) => engine.send(id, preset, text))
  handle('approve', (eventId: string, inputs: Record<string, unknown>) => engine.approve(eventId, inputs))
  handle('cancel', (eventId: string) => engine.cancel(eventId))
  handle('close', (id: string) => engine.close(id))
  handle('attach', (eventId: string, targetId: string) => engine.attach(eventId, targetId))
  handle('directives', () => engine.directives())
  handle('addDirective', (text: string) => engine.addDirective(text))
  handle('setPriority', (id: string, on: boolean) => engine.setPriority(id, on))
  handle('snooze', (id: string, until: number) => engine.snooze(id, until))
  handle('unsnooze', (id: string) => engine.unsnooze(id))
  handle('summarise', (id: string) => engine.summarise(id))
  handle('customActions', () => engine.customActions.list())
  handle('saveCustomAction', (a: Parameters<typeof engine.customActions.save>[0]) => engine.customActions.save(a))
  handle('removeCustomAction', (id: string) => engine.customActions.remove(id))
  handle('runCustomAction', async (id: string, eventId: string) => void (await engine.customActions.run(id, eventId)))
  handle('generateCustomAction', (a: Parameters<typeof engine.customActions.generate>[0]) => engine.customActions.generate(a))
  handle('handledOutside', (id: string) => engine.handledOutside(id))
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
  // Ctrl+W (rather than closing the window, as the default menu would) and Alt+Left take the top view off the stack;
  // Alt+Right puts the last one taken off back on.
  win.webContents.on('before-input-event', (event, input) => {
    if (input.type !== 'keyDown' || input.shift || input.meta) return
    const ctrlW = input.control && !input.alt && input.key.toLowerCase() === 'w'
    const altLeft = input.alt && !input.control && input.key === 'ArrowLeft'
    const altRight = input.alt && !input.control && input.key === 'ArrowRight'
    if (!ctrlW && !altLeft && !altRight) return
    event.preventDefault()
    win.webContents.send(altRight ? 'forward' : 'back')
  })
  engine.on('change', () => win.isDestroyed() || win.webContents.send('changed'))
  if (process.env.ELECTRON_RENDERER_URL) void win.loadURL(process.env.ELECTRON_RENDERER_URL)
  else void win.loadFile(join(__dirname, '../renderer/index.html'))

  app.on('window-all-closed', () => app.quit())
}

void main()
