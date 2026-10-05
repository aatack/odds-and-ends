import { join } from 'node:path'
import { fileURLToPath } from 'node:url'
import { app, BrowserWindow, ipcMain, nativeTheme, shell } from 'electron'
import { Core } from '../core/core.ts'

let core: Core | null = null

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
    },
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

void app.whenReady().then(() => {
  nativeTheme.themeSource = 'light'
  const path = process.env.DETAIL_VIEWS_DB ?? join(app.getPath('userData'), 'detail-views.sqlite')
  core = new Core({ path })
  core.start()
  const actions = core.actions as Record<string, (args: unknown) => unknown>

  ipcMain.handle('invoke', (_event, name: string, args: unknown) => {
    const action = actions[name]
    if (!action) throw new Error(`no action ${name}`)
    return action(args)
  })
  core.onChange(() => {
    for (const window of BrowserWindow.getAllWindows()) window.webContents.send('changed')
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
