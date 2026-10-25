import { join } from 'node:path'
import { app, BrowserWindow, ipcMain, safeStorage, shell } from 'electron'
import { backend } from './backend'

/**
 * The app is a window onto a backend: one already running on its data folder,
 * or one it starts (and stops) itself. See backend.ts and src/backend.
 */
async function main(): Promise<void> {
  await app.whenReady()
  const server = await backend(app.getPath('userData'))
  console.log(`workflows: backend at ${server.info.url} (${server.owned ? 'started by the app' : 'already running'})`)

  // Slack tokens an older app saved with safeStorage: only the app can read them, so it hands them back.
  try {
    const legacy = (await server.call('legacySecrets', [])) as string[]
    if (legacy.length && safeStorage.isEncryptionAvailable())
      await server.call('replaceLegacySecrets', [
        Object.fromEntries(legacy.map((s) => [s, safeStorage.decryptString(Buffer.from(s.slice('safe:'.length), 'base64'))])),
      ])
  } catch (e) {
    console.error('workflows: could not hand back the old secrets:', (e as Error).message)
  }

  ipcMain.handle('rpc', (_e, method: string, args: unknown[]) => server.call(method, args))

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
  server.onChange(() => win.isDestroyed() || win.webContents.send('changed'))
  if (process.env.ELECTRON_RENDERER_URL) void win.loadURL(process.env.ELECTRON_RENDERER_URL)
  else void win.loadFile(join(__dirname, '../renderer/index.html'))

  // A backend the app started stops with it; one it found running goes on.
  let quitting = false
  app.on('before-quit', (e) => {
    if (quitting || !server.owned) return
    e.preventDefault()
    quitting = true
    void server.stop().finally(() => app.quit())
  })
  app.on('window-all-closed', () => app.quit())
}

void main().catch((e) => {
  console.error('workflows:', (e as Error).message)
  app.exit(1)
})
