import { app, BrowserWindow, ipcMain } from 'electron'
import { join } from 'path'
import { randomBytes } from 'crypto'
import { v4 as uuidv4 } from 'uuid'
import type { ActiveSource, CurrentSource, NewServer, NewSourceConnection, Server, TokenRow } from '../core/client'
import { store } from './store'
import { ServerManager } from './servers'

// nanoid's default url-safe alphabet (64 chars ⇒ `byte & 63` selects uniformly).
const NANOID_ALPHABET = 'useandom-26T198340PX75pxJACKVERYMINDBUSHWOLF_GQZbfghjklqvwyzrict'

/** A short, URL-safe id in the style of `nanoid`. */
function nanoid(size = 21): string {
  const bytes = randomBytes(size)
  let id = ''
  for (let i = 0; i < size; i++) id += NANOID_ALPHABET[bytes[i] & 63]
  return id
}

/** Label carried by the token the app issues for itself when opening a source. */
const APP_TOKEN_LABEL = 'app'

// ---------------------------------------------------------------------------
// Local server processes
// ---------------------------------------------------------------------------

const servers = new ServerManager()

// ---------------------------------------------------------------------------
// HTTP proxy — the app has no local backend; every data operation is forwarded
// to the remote server. Running the fetch here (main process) rather than in
// the renderer avoids CORS, since the server sets no CORS headers.
// ---------------------------------------------------------------------------

class HttpError extends Error {}

/** Perform an authenticated request against a base URL, parsing JSON. */
async function request(
  baseUrl: string,
  token: string,
  method: string,
  path: string,
  body?: unknown,
): Promise<unknown> {
  const res = await fetch(`${baseUrl}${path}`, {
    method,
    headers: {
      Authorization: `Bearer ${token}`,
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

function requireServer(serverId: string): Server {
  const server = servers.get(serverId)
  if (!server) throw new HttpError(`no server with id "${serverId}"`)
  return server
}

/** Admin request against a server that has admin access. */
function adminRequest(serverId: string, method: string, path: string, body?: unknown): Promise<unknown> {
  const server = requireServer(serverId)
  if (!server.adminToken) throw new HttpError('server has no admin access')
  return request(server.baseUrl, server.adminToken, method, path, body)
}

// ---------------------------------------------------------------------------
// Active source — ephemeral, in-memory only. Opening a source resolves a bearer
// token (issued fresh for admin servers, stored for source connections) and
// keeps it here, keyed by an id the renderer passes back on every data call.
// ---------------------------------------------------------------------------

const activeSources = new Map<string, { baseUrl: string; token: string; sourceId: string }>()

function requireActive(id: string): { baseUrl: string; token: string; sourceId: string } {
  const active = activeSources.get(id)
  if (!active) throw new HttpError(`no open source "${id}"`)
  return active
}

/** Call a tool on the open source and unwrap `{ status, result }`. */
async function sourceCall(id: string, tool: string, args: unknown): Promise<unknown> {
  const { baseUrl, token, sourceId } = requireActive(id)
  const out = (await request(baseUrl, token, 'POST', `/${sourceId}/call`, { tool, args })) as
    | { status: 'success'; result: unknown }
    | { status: 'error'; message: string }
  if (out.status === 'error') throw new HttpError(out.message)
  return out.result
}

async function sourceTools(id: string): Promise<unknown> {
  const { baseUrl, token, sourceId } = requireActive(id)
  return request(baseUrl, token, 'GET', `/${sourceId}/tools`)
}

// ---------------------------------------------------------------------------
// IPC — user config
// ---------------------------------------------------------------------------

ipcMain.handle('config:getUser', () => store.get('user'))
ipcMain.handle('config:setUser', (_e, name: string) => store.set('user', name))
ipcMain.handle('config:getCurrentSource', () => store.get('currentSource'))
ipcMain.handle('config:setCurrentSource', (_e, source: CurrentSource | null) =>
  store.set('currentSource', source),
)

// ---------------------------------------------------------------------------
// IPC — servers
// ---------------------------------------------------------------------------

ipcMain.handle('server:list', () => servers.list())
ipcMain.handle('server:add', (_e, cfg: NewServer) => servers.addExternal(cfg))
ipcMain.handle('server:update', (_e, id: string, patch: Partial<NewServer>) => servers.update(id, patch))
ipcMain.handle('server:remove', (_e, id: string) => servers.remove(id))
ipcMain.handle('server:createLocal', (_e, label: string) => servers.createLocal(label))
ipcMain.handle('server:start', (_e, id: string) => servers.start(id))
ipcMain.handle('server:stop', (_e, id: string) => servers.stop(id))

// ---------------------------------------------------------------------------
// IPC — source connections (saved credentials for non-admin servers)
// ---------------------------------------------------------------------------

ipcMain.handle('sourceConn:list', () => store.get('sourceConnections'))
ipcMain.handle('sourceConn:add', (_e, cfg: NewSourceConnection) => {
  const id = uuidv4()
  store.set('sourceConnections', [...store.get('sourceConnections'), { ...cfg, id }])
  return id
})
ipcMain.handle('sourceConn:update', (_e, id: string, patch: Partial<NewSourceConnection>) => {
  store.set(
    'sourceConnections',
    store.get('sourceConnections').map((c) => {
      if (c.id !== id) return c
      // A blank/absent token means "keep the stored one" — don't clear it.
      const token = patch.token?.trim() ? patch.token : c.token
      return { ...c, ...patch, id, token }
    }),
  )
})
ipcMain.handle('sourceConn:remove', (_e, id: string) => {
  store.set(
    'sourceConnections',
    store.get('sourceConnections').filter((c) => c.id !== id),
  )
})

// ---------------------------------------------------------------------------
// IPC — open / close a source, and its data operations
// ---------------------------------------------------------------------------

ipcMain.handle('source:open', async (_e, serverId: string, sourceId: string, label: string): Promise<ActiveSource> => {
  const server = requireServer(serverId)
  let token: string
  if (server.adminToken) {
    // Reuse the app's own live token for this source if one exists, so repeated
    // opens don't pile up throwaway tokens; only mint a fresh one when there's none.
    const existing = (await adminRequest(
      serverId,
      'GET',
      `/admin/sources/${sourceId}/tokens`,
    )) as TokenRow[]
    const reusable = existing.find((t) => !t.revoked && t.label === APP_TOKEN_LABEL)
    if (reusable) {
      token = reusable.token
    } else {
      const issued = (await adminRequest(serverId, 'POST', `/admin/sources/${sourceId}/tokens`, {
        label: APP_TOKEN_LABEL,
      })) as { token: string }
      token = issued.token
    }
  } else {
    const conn = store
      .get('sourceConnections')
      .find((c) => c.serverId === serverId && c.sourceId === sourceId)
    if (!conn) throw new HttpError(`no saved credentials for source "${sourceId}"`)
    token = conn.token
  }
  const id = uuidv4()
  activeSources.set(id, { baseUrl: server.baseUrl, token, sourceId })
  return { id, label, serverId, sourceId }
})

ipcMain.handle('source:close', (_e, id: string) => {
  activeSources.delete(id)
})

ipcMain.handle('source:tools', (_e, id: string) => sourceTools(id))
ipcMain.handle('source:call', (_e, id: string, tool: string, args: unknown) => sourceCall(id, tool, args))

// ---------------------------------------------------------------------------
// IPC — admin (source CRUD + tokens), keyed by server id
// ---------------------------------------------------------------------------

ipcMain.handle('admin:listSources', (_e, serverId: string) =>
  adminRequest(serverId, 'GET', '/admin/sources'),
)
ipcMain.handle('admin:getSource', (_e, serverId: string, id: string) =>
  adminRequest(serverId, 'GET', `/admin/sources/${id}`),
)
ipcMain.handle('admin:createSource', (_e, serverId: string, body: { id?: string; label?: string; config: unknown }) =>
  // Source ids are opaque; auto-assign a nanoid so the user never has to pick one.
  adminRequest(serverId, 'POST', '/admin/sources', { ...body, id: body.id?.trim() || nanoid() }),
)
ipcMain.handle('admin:updateSource', (_e, serverId: string, id: string, body: unknown) =>
  adminRequest(serverId, 'PUT', `/admin/sources/${id}`, body),
)
ipcMain.handle('admin:deleteSource', (_e, serverId: string, id: string) =>
  adminRequest(serverId, 'DELETE', `/admin/sources/${id}`),
)
ipcMain.handle('admin:listTokens', (_e, serverId: string, id: string) =>
  adminRequest(serverId, 'GET', `/admin/sources/${id}/tokens`),
)
ipcMain.handle('admin:issueToken', (_e, serverId: string, id: string, label?: string) =>
  adminRequest(serverId, 'POST', `/admin/sources/${id}/tokens`, { label: label ?? '' }),
)
ipcMain.handle('admin:revokeToken', (_e, serverId: string, token: string) =>
  adminRequest(serverId, 'DELETE', `/admin/tokens/${token}`),
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

app.whenReady().then(() => {
  servers.startAll()
  createWindow()
})
app.on('window-all-closed', () => { if (process.platform !== 'darwin') app.quit() })
app.on('activate', () => { if (BrowserWindow.getAllWindows().length === 0) createWindow() })
app.on('will-quit', () => servers.stopAll())
