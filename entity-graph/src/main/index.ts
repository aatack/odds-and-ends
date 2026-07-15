import { app, BrowserWindow, ipcMain } from 'electron'
import { join } from 'path'
import Store from 'electron-store'
import { v4 as uuidv4 } from 'uuid'
import type { Connection, NewConnection } from '../core/client'

// ---------------------------------------------------------------------------
// Persistence
// ---------------------------------------------------------------------------

interface AppConfig {
  connections: Connection[]
  activeId: string | null
  user: string
}

const store = new Store<AppConfig>({
  defaults: { connections: [], activeId: null, user: 'anonymous' },
})

// ---------------------------------------------------------------------------
// HTTP proxy — the app has no local backend; every data operation is forwarded
// to the remote server. Running the fetch here (main process) rather than in
// the renderer avoids CORS, since the server sets no CORS headers.
// ---------------------------------------------------------------------------

class HttpError extends Error {}

function requireConnection(connId: string): Connection {
  const conn = store.get('connections').find((c) => c.id === connId)
  if (!conn) throw new HttpError(`no connection with id "${connId}"`)
  return conn
}

/** Perform an authenticated request against a connection's server, parsing JSON. */
async function request(
  conn: Connection,
  method: string,
  path: string,
  body?: unknown,
): Promise<unknown> {
  const res = await fetch(`${conn.baseUrl}${path}`, {
    method,
    headers: {
      Authorization: `Bearer ${conn.token}`,
      ...(body !== undefined ? { 'Content-Type': 'application/json' } : {}),
    },
    body: body !== undefined ? JSON.stringify(body) : undefined,
  })
  const text = await res.text()
  const data = text ? JSON.parse(text) : undefined
  if (!res.ok) {
    const msg = data && typeof data === 'object' && 'error' in data ? (data as { error: string }).error : text
    throw new HttpError(`HTTP ${res.status}: ${msg || res.statusText}`)
  }
  return data
}

/** Call a tool on the connection's source and unwrap `{ status, result }`. */
async function sourceCall(connId: string, tool: string, args: unknown): Promise<unknown> {
  const conn = requireConnection(connId)
  if (conn.kind !== 'source' || !conn.sourceId) throw new HttpError('not a source connection')
  const out = (await request(conn, 'POST', `/${conn.sourceId}/call`, { tool, args })) as
    | { status: 'success'; result: unknown }
    | { status: 'error'; message: string }
  if (out.status === 'error') throw new HttpError(out.message)
  return out.result
}

async function sourceTools(connId: string): Promise<unknown> {
  const conn = requireConnection(connId)
  if (conn.kind !== 'source' || !conn.sourceId) throw new HttpError('not a source connection')
  return request(conn, 'GET', `/${conn.sourceId}/tools`)
}

/** Admin request against an `admin` connection. */
async function adminRequest(
  connId: string,
  method: string,
  path: string,
  body?: unknown,
): Promise<unknown> {
  const conn = requireConnection(connId)
  if (conn.kind !== 'admin') throw new HttpError('not an admin connection')
  return request(conn, method, path, body)
}

// ---------------------------------------------------------------------------
// IPC — connections & user config
// ---------------------------------------------------------------------------

ipcMain.handle('config:getUser', () => store.get('user'))
ipcMain.handle('config:setUser', (_e, name: string) => store.set('user', name))

ipcMain.handle('conn:list', () => store.get('connections'))
ipcMain.handle('conn:getActive', () => {
  const id = store.get('activeId')
  return id ? (store.get('connections').find((c) => c.id === id) ?? null) : null
})
ipcMain.handle('conn:setActive', (_e, id: string | null) => store.set('activeId', id))

ipcMain.handle('conn:add', (_e, cfg: NewConnection) => {
  const id = uuidv4()
  store.set('connections', [...store.get('connections'), { ...cfg, id }])
  return id
})

ipcMain.handle('conn:update', (_e, id: string, patch: Partial<NewConnection>) => {
  store.set(
    'connections',
    store.get('connections').map((c) => (c.id === id ? { ...c, ...patch, id } : c)),
  )
})

ipcMain.handle('conn:remove', (_e, id: string) => {
  store.set(
    'connections',
    store.get('connections').filter((c) => c.id !== id),
  )
  if (store.get('activeId') === id) store.set('activeId', null)
})

// ---------------------------------------------------------------------------
// IPC — source (tools / call)
// ---------------------------------------------------------------------------

ipcMain.handle('source:tools', (_e, connId: string) => sourceTools(connId))
ipcMain.handle('source:call', (_e, connId: string, tool: string, args: unknown) =>
  sourceCall(connId, tool, args),
)

// ---------------------------------------------------------------------------
// IPC — admin (source CRUD + tokens)
// ---------------------------------------------------------------------------

ipcMain.handle('admin:listSources', (_e, connId: string) =>
  adminRequest(connId, 'GET', '/admin/sources'),
)
ipcMain.handle('admin:getSource', (_e, connId: string, id: string) =>
  adminRequest(connId, 'GET', `/admin/sources/${id}`),
)
ipcMain.handle('admin:createSource', (_e, connId: string, body: unknown) =>
  adminRequest(connId, 'POST', '/admin/sources', body),
)
ipcMain.handle('admin:updateSource', (_e, connId: string, id: string, body: unknown) =>
  adminRequest(connId, 'PUT', `/admin/sources/${id}`, body),
)
ipcMain.handle('admin:deleteSource', (_e, connId: string, id: string) =>
  adminRequest(connId, 'DELETE', `/admin/sources/${id}`),
)
ipcMain.handle('admin:listTokens', (_e, connId: string, id: string) =>
  adminRequest(connId, 'GET', `/admin/sources/${id}/tokens`),
)
ipcMain.handle('admin:issueToken', (_e, connId: string, id: string, label?: string) =>
  adminRequest(connId, 'POST', `/admin/sources/${id}/tokens`, { label: label ?? '' }),
)
ipcMain.handle('admin:revokeToken', (_e, connId: string, token: string) =>
  adminRequest(connId, 'DELETE', `/admin/tokens/${token}`),
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
