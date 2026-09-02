import { app, BrowserWindow, ipcMain, shell } from 'electron'
import { existsSync } from 'fs'
import { writeFile } from 'fs/promises'
import { extname, join } from 'path'
import type { WriteOp } from '../core/api'
import { Store } from './db'

/** The one store, opened next to the app's own settings. */
let store: Store | null = null

function createWindow(): void {
  const window = new BrowserWindow({
    width: 1480,
    height: 940,
    minWidth: 1000,
    minHeight: 640,
    show: false,
    backgroundColor: '#f7f7f8',
    title: '3D modelling',
    webPreferences: {
      preload: join(__dirname, '../preload/index.js'),
      sandbox: false,
      contextIsolation: true,
    },
  })

  window.once('ready-to-show', () => window.show())

  const devServer = process.env.ELECTRON_RENDERER_URL
  if (devServer) window.loadURL(devServer)
  else window.loadFile(join(__dirname, '../renderer/index.html'))
}

/** A path in the downloads folder that nothing is using yet. */
function freePath(name: string): string {
  const directory = app.getPath('downloads')
  const extension = extname(name) || '.glb'
  const stem = name.slice(0, name.length - extension.length) || 'model'
  let candidate = join(directory, `${stem}${extension}`)
  let n = 2
  while (existsSync(candidate)) candidate = join(directory, `${stem} ${n++}${extension}`)
  return candidate
}

app.whenReady().then(async () => {
  store = await Store.open(join(app.getPath('userData'), 'models.sqlite'))

  ipcMain.handle('models:load', () => store!.load())
  ipcMain.handle('models:write', (_event, ops: WriteOp[]) => store!.apply(ops))

  ipcMain.handle('models:save', async (_event, name: string, bytes: Uint8Array) => {
    const path = freePath(name)
    await writeFile(path, Buffer.from(bytes))
    return path
  })

  ipcMain.handle('file:reveal', (_event, path: string) => shell.showItemInFolder(path))
  ipcMain.handle('file:open', (_event, path: string) => shell.openPath(path))

  createWindow()
  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow()
  })
})

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit()
})

// Whatever is still only in memory goes to the file before the process ends.
app.on('before-quit', () => {
  store?.close()
  store = null
})
