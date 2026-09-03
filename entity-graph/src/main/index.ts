import {
  app,
  BrowserWindow,
  clipboard,
  ipcMain,
  Menu,
  shell,
  type MenuItemConstructorOptions,
} from 'electron'
import { basename, extname, join, resolve } from 'path'
import { existsSync } from 'fs'
import { mkdir, writeFile } from 'fs/promises'
import { homedir } from 'os'
import { pathToFileURL } from 'url'
import { v4 as uuidv4 } from 'uuid'
import type {
  CurrentPensive,
  NodeKind,
  NodePatch,
  SourceEdge,
  SourceGraph,
  SourceNode,
  SourceToken,
} from '../core/client'
import { APP_MOUNT, nodeKind, phoneAppUrl, phoneBaseUrl, sourceMount } from '../core/client'
import { PausedPensive, type Pensive, type ToolMeta } from '../core/pensive/index'
import { toolMeta } from '../core/pensive/tool'
import { store } from './store'
import { GraphDb } from './pensive/graph'
import { PensiveRegistry, wouldCycle } from './pensive/registry'
import { PensiveServers, findFreePort } from './pensive/servers'
import { setDocsRoot } from './pensive/mcpServer'
import { INTEGRATION_TOOLS, runIntegrationTool } from './integrations/index'
import { loadEnvFile } from './integrations/env'
import { phoneAppDist, setServed, tailscaleView } from './tailscale'

// The app is the whole of it now. There is no server to start, no admin token to
// pass, and no source to open: the user draws a graph of pensives on the sources
// page, one node of it is this window, and everything below is the plumbing that
// keeps that drawing and the running processes in step.

/** Label carried by the token the app issues for a phone. */
const PHONE_TOKEN_NAME = 'phone'

class AppError extends Error {}

// ---------------------------------------------------------------------------
// The graph, the pensives built from it, and the servers publishing them
// ---------------------------------------------------------------------------

let graph: GraphDb
let registry: PensiveRegistry
let servers: PensiveServers

function setUpPensives(): void {
  const root = app.getAppPath()
  graph = new GraphDb(join(app.getPath('userData'), 'pensive', 'graph.db'))
  registry = new PensiveRegistry(graph, {
    // Where a path written down as a bare file name lands, so "notes.db" is a
    // real place rather than wherever the app happened to be started from.
    storeRoot: join(app.getPath('userData'), 'stores'),
    author: () => store.get('user'),
  })
  servers = new PensiveServers(graph, registry)
  setDocsRoot(root)
  loadEnvFile(root)
}

/**
 * Everything a change to the graph implies: the pensives built from it are
 * stale, the servers may need starting or stopping, and the window is very
 * likely looking at a different store than it was a moment ago.
 */
async function graphChanged(): Promise<void> {
  registry.invalidate()
  await servers.sync()
  for (const win of BrowserWindow.getAllWindows()) win.webContents.send('pensive:changed')
}

/** The page's whole answer: the drawing, plus how each node is getting on. */
async function readGraph(): Promise<SourceGraph> {
  const nodes = graph.nodes()
  const edges = graph.edges()
  const status: SourceGraph['status'] = {}
  for (const node of nodes) {
    // Building it is how a problem is found, so every node is asked about —
    // cheap, since the answers are cached until the graph changes.
    const built = await registry.tryGet(node.id)
    const server = servers.status(node.id)
    status[node.id] = {
      url: server.url,
      localUrl: server.localUrl,
      problem: 'problem' in built ? built.problem : server.problem,
    }
  }
  return { nodes, edges, status }
}

/**
 * What the outliner has to work with: a pensive, or the sentence saying why not.
 *
 * A paused store counts as the latter rather than as something to show. It would
 * otherwise open as an outline that fails on every read, and "somebody switched
 * this off" is worth saying once instead of a screenful of times.
 */
async function desktopState(): Promise<{ pensive: Pensive } | { problem: string }> {
  const built = await registry.desktop()
  if ('problem' in built) return built
  if (built.pensive instanceof PausedPensive) {
    return { problem: `"${built.pensive.label}" is paused` }
  }
  return built
}

/** The pensive the outliner is showing, or nothing if the desktop node is bare. */
async function currentPensive(): Promise<CurrentPensive | null> {
  const built = await desktopState()
  if ('problem' in built) return null
  return { id: built.pensive.id, label: built.pensive.label }
}

async function requireDesktop(): Promise<Pensive> {
  const built = await desktopState()
  if ('problem' in built) throw new AppError(built.problem)
  return built.pensive
}

// ---------------------------------------------------------------------------
// IPC — the open pensive
// ---------------------------------------------------------------------------

ipcMain.handle('pensive:current', () => currentPensive())
ipcMain.handle('pensive:problem', async () => {
  const built = await desktopState()
  return 'problem' in built ? built.problem : null
})
ipcMain.handle('pensive:tools', async (): Promise<ToolMeta[]> => (await requireDesktop()).listTools())
ipcMain.handle('pensive:call', async (_e, tool: string, args: unknown) =>
  (await requireDesktop()).callTool(tool, args),
)

// ---------------------------------------------------------------------------
// IPC — the graph of pensives
// ---------------------------------------------------------------------------

ipcMain.handle('graph:read', () => readGraph())

ipcMain.handle('graph:addNode', async (_e, kind: NodeKind, x: number, y: number): Promise<SourceNode> => {
  const info = nodeKind(kind)
  if (!info.addable) throw new AppError(`there is only ever one "${info.label}"`)
  const config = { ...info.config }
  // A published node needs a port before it can be drawn, and picking one is
  // not something to ask about: the app knows which are free and the user only
  // ever wanted a URL to copy.
  if ((config.kind === 'broadcast' || config.kind === 'mcp') && !config.port) {
    config.port = await findFreePort()
  }
  const node = graph.addNode({ label: info.label, x, y, config })
  await graphChanged()
  return node
})

ipcMain.handle('graph:updateNode', async (_e, id: string, patch: NodePatch): Promise<SourceNode> => {
  const node = graph.updateNode(id, patch)
  // Moving a node changes nothing that is running, and dragging one writes on
  // every frame, so only the parts that mean something rebuild.
  const cosmetic = Object.keys(patch).every((k) => k === 'x' || k === 'y')
  if (!cosmetic) await graphChanged()
  return node
})

ipcMain.handle('graph:removeNode', async (_e, id: string) => {
  graph.removeNode(id)
  await graphChanged()
})

ipcMain.handle('graph:connect', async (_e, from: string, to: string): Promise<SourceEdge> => {
  const source = graph.node(from)
  const target = graph.node(to)
  if (!source || !target) throw new AppError('one of those nodes is gone')
  if (!nodeKind(source.config.kind).output) {
    throw new AppError(`"${source.label}" has nothing to read`)
  }
  const accepts = nodeKind(target.config.kind).inputs
  if (accepts === 0) throw new AppError(`"${target.label}" takes no input`)
  if (wouldCycle(graph.edges(), from, to)) {
    throw new AppError(`that would put "${source.label}" downstream of itself`)
  }
  const existing = graph.edges().filter((e) => e.to === to)
  if (existing.some((e) => e.from === from)) return existing.find((e) => e.from === from)!
  // A one-input node has its plug *replaced* rather than refused: dragging a new
  // store into the app is the gesture, and being told to unplug the old one
  // first would be a step with no decision in it.
  if (accepts === 1) graph.clearInputs(to)
  const edge = graph.addEdge(from, to)
  await graphChanged()
  return edge
})

ipcMain.handle('graph:disconnect', async (_e, edgeId: string) => {
  graph.removeEdge(edgeId)
  await graphChanged()
})

// ---------------------------------------------------------------------------
// IPC — tokens on a broadcast or MCP node
// ---------------------------------------------------------------------------

ipcMain.handle('graph:tokens', (_e, nodeId: string): SourceToken[] => graph.tokens(nodeId))
ipcMain.handle('graph:issueToken', (_e, nodeId: string, name: string): SourceToken =>
  graph.issueToken(nodeId, name.trim() || 'anonymous'),
)
ipcMain.handle('graph:pauseToken', (_e, token: string, paused: boolean) =>
  graph.pauseToken(token, paused),
)
ipcMain.handle('graph:revokeToken', (_e, token: string) => graph.revokeToken(token))

// ---------------------------------------------------------------------------
// IPC — the integrations: the app's own reach outside itself
// ---------------------------------------------------------------------------

ipcMain.handle('integrations:tools', (): ToolMeta[] => INTEGRATION_TOOLS.map(toolMeta))
// Waits as long as the tool takes: `claude.runPrompt` holds a session open for
// minutes and can hold one for tens of them. Nothing here gives up first — the
// call shows as running in the activity log until it answers.
ipcMain.handle('integrations:run', (_e, tool: string, args: unknown) =>
  runIntegrationTool(tool, args),
)

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

/**
 * The schemes a link in a note could reasonably mean. `shell.openExternal` will
 * open a `file:` URL in whatever the desktop uses for one, and hand a custom
 * scheme to whatever registered it — and the strings that reach here come out of
 * the store rather than out of this app, so the list is named rather than
 * assumed.
 */
const OPENABLE = new Set(['http:', 'https:', 'mailto:'])

/**
 * Hand a URL to the desktop's own browser, which is the point: a page opened in
 * an Electron window of ours would be a browser signed in as nobody, without the
 * extensions, the history or the session the real one has.
 */
ipcMain.handle('shell:openExternal', async (_e, url: string): Promise<void> => {
  let parsed: URL
  try {
    parsed = new URL(url)
  } catch {
    throw new Error(`"${url}" isn't a URL`)
  }
  if (!OPENABLE.has(parsed.protocol)) {
    throw new Error(`Won't open a ${parsed.protocol} link — http, https and mailto only`)
  }
  await shell.openExternal(parsed.href)
})

/**
 * What is on the clipboard, as text. Read here rather than through
 * `navigator.clipboard`, which a browser gates behind a permission prompt there is
 * nobody to answer: this is the user's own clipboard on the user's own machine,
 * and a tool that reads it was asked to.
 */
ipcMain.handle('clipboard:readText', (): string => clipboard.readText())

/**
 * Show a file or directory where it lives. `showItemInFolder` opens the parent
 * with the item *selected*, which is the gesture — "here it is", rather than
 * "here is what is inside it" — and it does nothing at all for a path that isn't
 * there, so the check is what turns a typo into something that says so.
 */
ipcMain.handle('shell:revealPath', async (_e, path: string): Promise<string> => {
  const trimmed = String(path ?? '').trim()
  if (!trimmed) throw new Error('Which path?')
  // `~` is how a path is written down in a note, and nothing below expands it.
  const expanded =
    trimmed === '~' || trimmed.startsWith('~/') ? join(homedir(), trimmed.slice(1)) : trimmed
  const absolute = resolve(expanded)
  if (!existsSync(absolute)) throw new Error(`${absolute} isn't on this machine`)
  shell.showItemInFolder(absolute)
  return absolute
})

ipcMain.handle('config:getUser', () => store.get('user'))
ipcMain.handle('config:setUser', async (_e, name: string) => {
  store.set('user', name)
  // The author a store stamps on a write that names none is fixed when the
  // pensive is built, so changing who you are rebuilds them.
  await graphChanged()
})

// ---------------------------------------------------------------------------
// IPC — Tailscale, which is how the phone reaches any of this
//
// Machine-scoped rather than node-scoped: one tailnet name, one serve config,
// and the app and every broadcast it publishes share it. See `./tailscale.ts`.
// ---------------------------------------------------------------------------

ipcMain.handle('tailscale:status', () => tailscaleView(app.getAppPath()))

ipcMain.handle('tailscale:serveApp', (_e, on: boolean) => {
  const root = app.getAppPath()
  return setServed(root, { mount: APP_MOUNT, kind: 'path', target: phoneAppDist(root).path }, on)
})

ipcMain.handle('tailscale:serveNode', (_e, nodeId: string, on: boolean) => {
  const target = servers.localUrl(nodeId)
  if (!target) throw new AppError('that broadcast is not listening')
  return setServed(
    app.getAppPath(),
    { mount: sourceMount(nodeId), kind: 'proxy', target },
    on,
  )
})

/**
 * A link that connects a phone to one broadcast in a single tap: the whole
 * connection, base64'd into the URL fragment. Thumb-typing a 48-character bearer
 * token is miserable enough to be worth a code path, and a fragment is the one
 * part of a URL that never reaches a server or a log on the way in.
 */
ipcMain.handle(
  'tailscale:phoneLink',
  async (_e, nodeId: string, author: string): Promise<string> => {
    const view = await tailscaleView(app.getAppPath())
    if (!view.domain) throw new AppError(view.problem ?? 'Tailscale isn’t ready.')
    // Kept apart from the tokens issued by hand, so revoking the phone leaves
    // everything else signed in.
    const token =
      graph.tokenFor(nodeId, PHONE_TOKEN_NAME) ?? graph.issueToken(nodeId, PHONE_TOKEN_NAME)
    const connection = {
      baseUrl: phoneBaseUrl(view.domain),
      sourceId: nodeId,
      token: token.token,
      author,
    }
    const hash = Buffer.from(JSON.stringify(connection)).toString('base64').replace(/=+$/, '')
    return `${phoneAppUrl(view.domain)}#connect=${hash}`
  },
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

// Chromium only hands out SharedArrayBuffer to a cross-origin-isolated page, and
// the renderer is served over plain localhost in development. The code runner's
// tool calls are synchronous, which is to say the worker blocks on a shared word
// until the main thread writes the answer beside it — so without this switch there
// is no `tool` in a script at all. Must be set before the app is ready.
app.commandLine.appendSwitch('enable-features', 'SharedArrayBuffer')

app.whenReady().then(async () => {
  installMenu()
  setUpPensives()
  // Every broadcast and MCP node comes back up with the app: they are part of
  // the drawing rather than something started by hand.
  await servers.sync()
  createWindow()
})
app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit()
})
app.on('activate', () => {
  if (BrowserWindow.getAllWindows().length === 0) createWindow()
})
app.on('will-quit', () => {
  void servers.stopAll()
  registry?.invalidate()
  graph?.close()
})
