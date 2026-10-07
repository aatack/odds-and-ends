import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { app, BrowserWindow, ipcMain, Menu, nativeTheme, protocol, shell, type MenuItemConstructorOptions } from 'electron'
import { Core } from '../core/core.ts'

let core: Core | null = null

/** `slack-image://<size>/<message id>/<file id>`: Slack images, through the core's cache. */
protocol.registerSchemesAsPrivileged([{ scheme: 'slack-image', privileges: { standard: false, secure: true } }])

function createWindow(): BrowserWindow {
  const window = new BrowserWindow({
    width: 1440,
    height: 960,
    minWidth: 640,
    minHeight: 420,
    show: false,
    backgroundColor: '#fafafa',
    title: 'detail-views',
    autoHideMenuBar: true,
    webPreferences: {
      preload: fileURLToPath(new URL('../preload/index.mjs', import.meta.url)),
      sandbox: false,
      contextIsolation: true,
      // Scales text and spacing together.
      zoomFactor: 1.25,
      // Link previews. Every guest is locked down in will-attach-webview.
      webviewTag: true,
    },
  })
  window.webContents.on('will-attach-webview', (event, preferences, params) => {
    delete preferences.preload
    preferences.nodeIntegration = false
    preferences.contextIsolation = true
    preferences.sandbox = true
    if (!/^https?:/.test(params.src) || params.partition !== 'persist:preview') event.preventDefault()
  })
  window.on('ready-to-show', () => window.show())
  window.webContents.setWindowOpenHandler((details) => {
    void shell.openExternal(details.url)
    return { action: 'deny' }
  })
  const devServer = process.env.ELECTRON_RENDERER_URL
  if (devServer) void window.loadURL(devServer)
  else void window.loadFile(fileURLToPath(new URL('../renderer/index.html', import.meta.url)))
  return window
}

/**
 * The menu, without the default Edit menu's undo and redo: those accelerators
 * would take Ctrl+Z and Ctrl+Y before the page sees them, and the app's own
 * undo is in its tool registry. A focused text field still undoes its typing
 * by itself. macOS needs clipboard roles in a menu for the shortcuts to work.
 */
function installMenu(): void {
  const isMac = process.platform === 'darwin'
  const template: MenuItemConstructorOptions[] = [
    ...(isMac ? [{ role: 'appMenu' } as MenuItemConstructorOptions] : []),
    { role: 'fileMenu' },
    ...(isMac
      ? [{ label: 'Edit', submenu: [{ role: 'cut' }, { role: 'copy' }, { role: 'paste' }, { role: 'selectAll' }] } as MenuItemConstructorOptions]
      : []),
    { role: 'viewMenu' },
    { label: 'Window', submenu: [{ role: 'minimize' }, ...(isMac ? [{ role: 'zoom' as const }] : [])] },
  ]
  Menu.setApplicationMenu(Menu.buildFromTemplate(template))
}

void app.whenReady().then(() => {
  installMenu()
  nativeTheme.themeSource = 'light'
  // What I make, and what was loaded from elsewhere (safe to delete). The
  // single file before them is only read, once, to bring what I owned over.
  const dir = process.env.DETAIL_VIEWS_DIR ?? app.getPath('userData')
  core = new Core({
    owned: join(dir, 'detail-views.owned.sqlite'),
    cache: join(dir, 'detail-views.cache.sqlite'),
    legacy: join(dir, 'detail-views.sqlite'),
  })
  core.start()
  const actions = core.actions as Record<string, (args: unknown) => unknown>

  protocol.handle('slack-image', async (request) => {
    try {
      const ref = decodeURIComponent(request.url.slice('slack-image://'.length))
      const { mime, data } = await core!.slack.image(ref)
      return new Response(Buffer.from(data), { headers: { 'content-type': mime } })
    } catch (error) {
      return new Response(error instanceof Error ? error.message : String(error), { status: 404 })
    }
  })

  ipcMain.handle('openExternal', (_event, url: string) => {
    if (/^(https?|mailto):/.test(url)) void shell.openExternal(url)
  })

  ipcMain.handle('invoke', (_event, name: string, args: unknown) => {
    const action = actions[name]
    if (!action) throw new Error(`no action ${name}`)
    return action(args)
  })
  core.onChange((changed) => {
    for (const window of BrowserWindow.getAllWindows()) window.webContents.send('changed', changed)
  })

  createWindow()
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

app.on('window-all-closed', () => {
  core?.stop()
  if (process.platform !== 'darwin') app.quit()
})
