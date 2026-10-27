import { join } from 'node:path'
import { app, BrowserWindow, ipcMain, shell } from 'electron'
import { sourceFile } from '../backend/info'
import { backend, type Backend } from './backend'

// A profile of its own (local storage, window state), apart from the older app's, so the two run side by side.
app.setPath('userData', join(app.getPath('appData'), 'workflows-egdb'))

/**
 * The app is a window onto a backend on one source (a file), which the person
 * chooses as they start: one already running on it, or one the app starts
 * itself and stops when it is done with the source. See backend.ts and src/backend.
 */
async function main(): Promise<void> {
  await app.whenReady()
  const win = new BrowserWindow({
    width: 1200,
    height: 800,
    title: 'YAWN',
    webPreferences: { preload: join(__dirname, '../preload/index.js'), sandbox: false },
  })

  let current: Backend | null = null
  /** Opens a source (made if it is not there), letting go of the one open before; answers its file as an absolute path. */
  ipcMain.handle('openSource', async (_e, typed: string) => {
    const file = sourceFile(typed)
    if (current?.file === file) return file
    await current?.close()
    current = null
    const opened = await backend(file)
    opened.onChange(() => current === opened && !win.isDestroyed() && win.webContents.send('changed'))
    current = opened
    console.log(`workflows: ${file}, backend at ${opened.info.url} (${opened.owned ? 'started by the app' : 'already running'})`)
    return file
  })
  ipcMain.handle('closeSource', async () => {
    await current?.close()
    current = null
  })
  ipcMain.handle('rpc', (_e, method: string, args: unknown[], user: string | null) => {
    if (!current) throw new Error('Open a source first.')
    return current.call(method, args, user)
  })

  win.webContents.setWindowOpenHandler(({ url }) => {
    if (/^https?:/.test(url)) void shell.openExternal(url)
    return { action: 'deny' }
  })
  // Ctrl+W (rather than closing the window, as the default menu would), Alt+Left and Ctrl+Shift+Tab take the top view
  // off the stack; Alt+Right and Ctrl+Tab put the last one taken off back on.
  win.webContents.on('before-input-event', (event, input) => {
    if (input.type !== 'keyDown' || input.meta) return
    const ctrlTab = input.control && !input.alt && input.key === 'Tab'
    const ctrlW = input.control && !input.alt && !input.shift && input.key.toLowerCase() === 'w'
    const altLeft = input.alt && !input.control && !input.shift && input.key === 'ArrowLeft'
    const altRight = input.alt && !input.control && !input.shift && input.key === 'ArrowRight'
    if (!ctrlTab && !ctrlW && !altLeft && !altRight) return
    event.preventDefault()
    win.webContents.send(altRight || (ctrlTab && !input.shift) ? 'forward' : 'back')
  })
  if (process.env.ELECTRON_RENDERER_URL) void win.loadURL(process.env.ELECTRON_RENDERER_URL)
  else void win.loadFile(join(__dirname, '../renderer/index.html'))

  // A backend the app started stops with it; one it found running goes on.
  let quitting = false
  app.on('before-quit', (e) => {
    if (quitting || !current?.owned) return
    e.preventDefault()
    quitting = true
    void current.close().finally(() => app.quit())
  })
  app.on('window-all-closed', () => app.quit())
}

void main().catch((e) => {
  console.error('workflows:', (e as Error).message)
  app.exit(1)
})
