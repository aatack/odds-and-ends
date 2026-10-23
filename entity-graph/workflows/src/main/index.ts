import { join } from 'node:path'
import { app, BrowserWindow, ipcMain, safeStorage, shell } from 'electron'
import Database from 'better-sqlite3'
import { Admin } from '../engine/admin'
import { Engine } from '../engine/engine'
import { startMcp } from '../engine/mcp'
import { startPhoneServer } from '../engine/server'
import { plainSecrets, type Secrets } from '../engine/settings'
import { WorkerPool } from '../engine/workers'
import { makeHandlers } from './handlers'

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

  const handlers = makeHandlers(engine, admin, pool)
  for (const [name, fn] of Object.entries(handlers))
    ipcMain.handle(name, (_e, ...args) => fn(...(args as never[])))

  // The phone server serves the built renderer as the PWA shell; it listens only when enabled.
  // admin.setPhoneEnabled / setPhonePort call phone.restart(), so a toggle or port change takes effect.
  const phone = await startPhoneServer(engine, handlers, { staticDir: join(__dirname, '../renderer') })
  admin.attachPhone(phone)
  app.on('will-quit', () => phone.close())

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
  // Ctrl+W takes the top view off the stack, rather than closing the window as the default menu would.
  win.webContents.on('before-input-event', (event, input) => {
    if (input.type !== 'keyDown' || !input.control || input.alt || input.shift || input.meta || input.key.toLowerCase() !== 'w') return
    event.preventDefault()
    win.webContents.send('back')
  })
  engine.on('change', () => win.isDestroyed() || win.webContents.send('changed'))
  if (process.env.ELECTRON_RENDERER_URL) void win.loadURL(process.env.ELECTRON_RENDERER_URL)
  else void win.loadFile(join(__dirname, '../renderer/index.html'))

  app.on('window-all-closed', () => app.quit())
}

void main()
