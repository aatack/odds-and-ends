import { app, BrowserWindow, clipboard, ipcMain, Menu, type MenuItemConstructorOptions } from 'electron'
import { basename, extname, join } from 'path'
import { existsSync } from 'fs'
import { mkdir, writeFile } from 'fs/promises'
import { pathToFileURL } from 'url'
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

/**
 * A file name that isn't taken in `dir`. Saving a resource is a "give me this"
 * gesture with no dialog behind it, so it must not be able to overwrite: a name
 * already in use gets the current time appended rather than replacing anything.
 */
function freeName(dir: string, name: string): string {
  const base = basename(name).trim()
  // `basename` strips directories, but not the two names that are directories.
  const safe = base === '' || base === '.' || base === '..' ? 'download' : base
  if (!existsSync(join(dir, safe))) return safe
  const ext = extname(safe)
  return `${basename(safe, ext)}-${Date.now()}${ext}`
}

ipcMain.handle('file:save', async (_e, name: string, data: string): Promise<string> => {
  const dir = app.getPath('downloads')
  const path = join(dir, freeName(dir, name))
  await writeFile(path, Buffer.from(data, 'base64'))
  return path
})

/**
 * The clipboard format that means "here is a file", per platform, and what goes
 * in it. Only one custom format can be written at a time, so it has to be the
 * one that carries the file itself rather than its name: Chromium and the Linux
 * desktops read `text/uri-list`, macOS `public.file-url`, and Windows the legacy
 * `FileNameW`.
 */
function fileOnClipboard(path: string): { format: string; buffer: Buffer } {
  const url = pathToFileURL(path).href
  if (process.platform === 'win32') {
    return { format: 'FileNameW', buffer: Buffer.from(`${path}\0`, 'utf16le') }
  }
  if (process.platform === 'darwin') return { format: 'public.file-url', buffer: Buffer.from(url) }
  return { format: 'text/uri-list', buffer: Buffer.from(`${url}\r\n`) }
}

/**
 * Put a file on the clipboard. The clipboard can only point at bytes that are
 * already somewhere, so they are written under the temp directory first — each
 * copy in a directory of its own, so the file keeps the name it had and two
 * copies of the same name can't tread on each other.
 */
ipcMain.handle('file:copy', async (_e, name: string, data: string): Promise<string> => {
  const dir = join(app.getPath('temp'), 'entity-graph-clipboard', uuidv4())
  await mkdir(dir, { recursive: true })
  const path = join(dir, freeName(dir, name))
  await writeFile(path, Buffer.from(data, 'base64'))
  const { format, buffer } = fileOnClipboard(path)
  clipboard.writeBuffer(format, buffer)
  return path
})

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
// IPC — integrations
//
// The server's reach into GitHub, Slack and Claude. Server-scoped rather than
// source-scoped, and behind the admin token: `runTool` is the server's only door
// onto them, and this is the app's only door onto it.
// ---------------------------------------------------------------------------

ipcMain.handle('integrations:tools', (_e, serverId: string) =>
  adminRequest(serverId, 'GET', '/tools'),
)
ipcMain.handle('integrations:run', async (_e, serverId: string, tool: string, args: unknown) => {
  const out = (await adminRequest(serverId, 'POST', '/runTool', { tool, args })) as
    | { status: 'success'; result: unknown }
    | { status: 'error'; message: string }
  if (out.status === 'error') throw new HttpError(out.message)
  return out.result
})

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

/**
 * The application menu. Built by hand rather than left to Electron's default for
 * one reason: a menu accelerator is consumed by the menu and never reaches the
 * page, and two of the default's accelerators are keys the app itself binds —
 * Window → Close takes ⌘/Ctrl+W, which closes a *tab* here, and Edit → Undo
 * takes ⌘/Ctrl+Z, which is the app's own undo tool. So neither item is here.
 *
 * Nothing is lost by dropping the Edit menu on Windows and Linux: Chromium
 * handles cut/copy/paste in a focused field itself. macOS does not — the roles
 * have to exist in a menu for the system shortcuts to work at all — so there it
 * keeps a clipboard-only Edit menu.
 */
function installMenu(): void {
  const isMac = process.platform === 'darwin'
  const template: MenuItemConstructorOptions[] = [
    ...(isMac ? [{ role: 'appMenu' } as MenuItemConstructorOptions] : []),
    { role: 'fileMenu' },
    ...(isMac
      ? [
          {
            label: 'Edit',
            submenu: [
              { role: 'cut' },
              { role: 'copy' },
              { role: 'paste' },
              { role: 'selectAll' },
            ],
          } as MenuItemConstructorOptions,
        ]
      : []),
    { role: 'viewMenu' },
    {
      label: 'Window',
      submenu: [{ role: 'minimize' }, ...(isMac ? [{ role: 'zoom' as const }] : [])],
    },
  ]
  Menu.setApplicationMenu(Menu.buildFromTemplate(template))
}

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
  installMenu()
  servers.startAll()
  createWindow()
})
app.on('window-all-closed', () => { if (process.platform !== 'darwin') app.quit() })
app.on('activate', () => { if (BrowserWindow.getAllWindows().length === 0) createWindow() })
app.on('will-quit', () => servers.stopAll())
