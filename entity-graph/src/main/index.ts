import { app, BrowserWindow, ipcMain } from 'electron'
import { join } from 'path'
import Store from 'electron-store'
import { v4 as uuidv4 } from 'uuid'
import { SqliteInterface } from '../core/interface/sqlite'
import { HttpInterface } from '../core/interface/http'
import { ComboInterface } from '../core/interface/combo'
import { EntityWrapper } from '../core/wrapper'
import type { AppEvent } from '../core/events'
import type { EntityInterface } from '../core/interface/index'

// ---------------------------------------------------------------------------
// Persistence
// ---------------------------------------------------------------------------

export interface SourceConfig {
  id: string
  label: string
  type: 'sqlite' | 'http'
  path?: string
  url?: string
}

interface AppConfig {
  sources: SourceConfig[]
  user: string
}

const store = new Store<AppConfig>({
  defaults: { sources: [], user: 'anonymous' },
})

// ---------------------------------------------------------------------------
// Runtime source registry
// ---------------------------------------------------------------------------

const openDbs = new Map<string, SqliteInterface>()

function buildInterface(cfg: SourceConfig): EntityInterface {
  if (cfg.type === 'sqlite') {
    if (!cfg.path) throw new Error('SQLite source missing path')
    if (!openDbs.has(cfg.id)) openDbs.set(cfg.id, new SqliteInterface(cfg.path))
    return openDbs.get(cfg.id)!
  }
  if (cfg.type === 'http') {
    if (!cfg.url) throw new Error('HTTP source missing url')
    return new HttpInterface(cfg.url)
  }
  throw new Error(`Unknown source type`)
}

function buildIface(sourceIds?: string[]): EntityInterface {
  const configs = store.get('sources')
  const active  = sourceIds ? configs.filter((c) => sourceIds.includes(c.id)) : configs
  if (active.length === 0) throw new Error('No sources configured')
  if (active.length === 1) return buildInterface(active[0])
  return new ComboInterface(active.map(buildInterface))
}

function buildWrapper(sourceIds?: string[]): EntityWrapper {
  return new EntityWrapper(buildIface(sourceIds), () => store.get('user'))
}

// ---------------------------------------------------------------------------
// IPC handlers
// ---------------------------------------------------------------------------

ipcMain.handle('config:getUser',    () => store.get('user'))
ipcMain.handle('config:setUser',    (_e, name: string) => store.set('user', name))
ipcMain.handle('config:getSources', () => store.get('sources'))

ipcMain.handle('config:addSource', (_e, cfg: Omit<SourceConfig, 'id'>) => {
  const id = uuidv4()
  const sources = store.get('sources')
  sources.push({ ...cfg, id })
  store.set('sources', sources)
  return id
})

ipcMain.handle('config:removeSource', (_e, id: string) => {
  const db = openDbs.get(id)
  if (db) { db.close(); openDbs.delete(id) }
  store.set('sources', store.get('sources').filter((s) => s.id !== id))
})

ipcMain.handle('data:readEvents', async (_e, entityIds: string[], sourceIds?: string[]) => {
  const map = await buildIface(sourceIds).readEvents(entityIds)
  return Object.fromEntries([...map.entries()])
})

ipcMain.handle('data:writeEvents', async (_e, events: AppEvent[], sourceIds?: string[]) => {
  await buildIface(sourceIds).writeEvents(events)
})

ipcMain.handle('data:readEntities', async (_e, ids: string[], sourceIds?: string[]) => {
  const map = await buildWrapper(sourceIds).readEntities(ids)
  return Object.fromEntries([...map.entries()])
})

ipcMain.handle(
  'data:resolveQuery',
  (_e, rootId: string, options: { maxDepth?: number; collapsed?: string[]; limit?: number }, sourceIds?: string[]) =>
    buildWrapper(sourceIds).resolveQuery(rootId, options),
)

ipcMain.handle(
  'data:createEntity',
  (_e, values: Record<string, unknown>, parentId?: string, sourceIds?: string[]) =>
    buildWrapper(sourceIds).createEntity(values, parentId),
)

ipcMain.handle(
  'data:moveEntity',
  (_e, entityId: string, fromParent: string, toParent: string, sourceIds?: string[]) =>
    buildWrapper(sourceIds).moveEntity(entityId, fromParent, toParent),
)

// ---------------------------------------------------------------------------
// Window
// ---------------------------------------------------------------------------

function createWindow(): void {
  const win = new BrowserWindow({
    width: 1280,
    height: 800,
    webPreferences: {
      preload: join(__dirname, '../preload/index.js'),
      sandbox: false,
    },
  })

  if (process.env['ELECTRON_RENDERER_URL']) {
    win.loadURL(process.env['ELECTRON_RENDERER_URL'])
  } else {
    win.loadFile(join(__dirname, '../renderer/index.html'))
  }
}

app.whenReady().then(createWindow)
app.on('window-all-closed', () => { if (process.platform !== 'darwin') app.quit() })
app.on('activate', () => { if (BrowserWindow.getAllWindows().length === 0) createWindow() })
