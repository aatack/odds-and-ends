import { contextBridge, ipcRenderer } from 'electron'
import type {
  CurrentPensive,
  NodeKind,
  NodePatch,
  SourceEdge,
  SourceGraph,
  SourceNode,
  SourceToken,
  TailscaleView,
  ToolMeta,
} from '../core/client'

/**
 * The renderer's entire capability surface. Thin forwarders to the main process,
 * which is where the pensives are: there is no server to reach and no source to
 * open, so a data call names a tool and nothing else — the pensive it lands on
 * is whatever is plugged into the desktop node.
 */
export interface EntityGraphAPI {
  // User
  getUser: () => Promise<string>
  setUser: (name: string) => Promise<void>

  /**
   * Write base64 bytes into the downloads folder under a name that isn't taken,
   * returning the path. No dialog: the renderer can't overwrite anything, and
   * can't choose where it goes.
   */
  saveFile: (name: string, data: string) => Promise<string>

  /**
   * Put base64 bytes on the system clipboard as a file, returning the temporary
   * path they were written to. The web clipboard only carries text and bitmaps,
   * so anything else has to be a real file for the clipboard to name.
   */
  copyFile: (name: string, data: string) => Promise<string>

  /**
   * Open a URL in the desktop's own browser — `http`, `https` or `mailto`, and
   * nothing else. Not another window of this app: a page opened in one would be
   * a browser signed in as nobody.
   */
  openExternal: (url: string) => Promise<void>

  /**
   * What is on the clipboard, as text. Through the main process because the web
   * clipboard's read side is behind a permission prompt there is nobody to
   * answer — the write side isn't, which is why only this half is here.
   */
  readClipboardText: () => Promise<string>

  /**
   * Show a file or directory where it lives, in whatever the desktop uses for
   * one, with the item selected. `~` is expanded; the absolute path comes back.
   */
  revealPath: (path: string) => Promise<string>

  // --- The open pensive ---------------------------------------------------

  /** What the outliner is showing, or null when nothing is plugged in. */
  currentPensive: () => Promise<CurrentPensive | null>
  /** Why there is nothing to show, when there isn't. */
  pensiveProblem: () => Promise<string | null>
  pensiveTools: () => Promise<ToolMeta[]>
  pensiveCall: (tool: string, args: unknown) => Promise<unknown>
  /**
   * Fires when the graph changes, so the shell can find out it is looking at a
   * different store. Returns the teardown.
   */
  onPensiveChanged: (listener: () => void) => () => void

  // --- The graph of pensives ----------------------------------------------

  readGraph: () => Promise<SourceGraph>
  addNode: (kind: NodeKind, x: number, y: number) => Promise<SourceNode>
  updateNode: (id: string, patch: NodePatch) => Promise<SourceNode>
  removeNode: (id: string) => Promise<void>
  /** Plug one node's output into another's input. Refuses a loop. */
  connectNodes: (from: string, to: string) => Promise<SourceEdge>
  disconnectNodes: (edgeId: string) => Promise<void>

  // --- Tokens on a broadcast or MCP node ----------------------------------

  listTokens: (nodeId: string) => Promise<SourceToken[]>
  /** `name` is who writes made with the token are attributed to. */
  issueToken: (nodeId: string, name: string) => Promise<SourceToken>
  pauseToken: (token: string, paused: boolean) => Promise<void>
  revokeToken: (token: string) => Promise<void>

  /**
   * The app's integrations — GitHub, Slack, Claude, git, a terminal. The app's
   * own hands rather than anything a pensive holds, which is why they are not
   * addressed by node.
   */
  integrationTools: () => Promise<ToolMeta[]>
  runIntegrationTool: (tool: string, args: unknown) => Promise<unknown>

  /**
   * Phone access, over Tailscale. Machine-scoped: there is one tailnet name and
   * one serve config, and the phone app and every broadcast share them.
   */
  tailscaleStatus: () => Promise<TailscaleView>
  /** Publish or unpublish the phone app's build at the root of the tailnet name. */
  tailscaleServeApp: (on: boolean) => Promise<void>
  /** Publish or unpublish one broadcast node at `/api/<nodeId>`. */
  tailscaleServeNode: (nodeId: string, on: boolean) => Promise<void>
  /** A one-tap link that hands a phone the whole connection, token included. */
  tailscalePhoneLink: (nodeId: string, author: string) => Promise<string>
}

const api: EntityGraphAPI = {
  getUser: () => ipcRenderer.invoke('config:getUser'),
  setUser: (name) => ipcRenderer.invoke('config:setUser', name),

  saveFile: (name, data) => ipcRenderer.invoke('file:save', name, data),
  copyFile: (name, data) => ipcRenderer.invoke('file:copy', name, data),
  openExternal: (url) => ipcRenderer.invoke('shell:openExternal', url),
  readClipboardText: () => ipcRenderer.invoke('clipboard:readText'),
  revealPath: (path) => ipcRenderer.invoke('shell:revealPath', path),

  currentPensive: () => ipcRenderer.invoke('pensive:current'),
  pensiveProblem: () => ipcRenderer.invoke('pensive:problem'),
  pensiveTools: () => ipcRenderer.invoke('pensive:tools'),
  pensiveCall: (tool, args) => ipcRenderer.invoke('pensive:call', tool, args),
  onPensiveChanged: (listener) => {
    const handler = (): void => listener()
    ipcRenderer.on('pensive:changed', handler)
    return () => ipcRenderer.off('pensive:changed', handler)
  },

  readGraph: () => ipcRenderer.invoke('graph:read'),
  addNode: (kind, x, y) => ipcRenderer.invoke('graph:addNode', kind, x, y),
  updateNode: (id, patch) => ipcRenderer.invoke('graph:updateNode', id, patch),
  removeNode: (id) => ipcRenderer.invoke('graph:removeNode', id),
  connectNodes: (from, to) => ipcRenderer.invoke('graph:connect', from, to),
  disconnectNodes: (edgeId) => ipcRenderer.invoke('graph:disconnect', edgeId),

  listTokens: (nodeId) => ipcRenderer.invoke('graph:tokens', nodeId),
  issueToken: (nodeId, name) => ipcRenderer.invoke('graph:issueToken', nodeId, name),
  pauseToken: (token, paused) => ipcRenderer.invoke('graph:pauseToken', token, paused),
  revokeToken: (token) => ipcRenderer.invoke('graph:revokeToken', token),

  integrationTools: () => ipcRenderer.invoke('integrations:tools'),
  runIntegrationTool: (tool, args) => ipcRenderer.invoke('integrations:run', tool, args),

  tailscaleStatus: () => ipcRenderer.invoke('tailscale:status'),
  tailscaleServeApp: (on) => ipcRenderer.invoke('tailscale:serveApp', on),
  tailscaleServeNode: (nodeId, on) => ipcRenderer.invoke('tailscale:serveNode', nodeId, on),
  tailscalePhoneLink: (nodeId, author) =>
    ipcRenderer.invoke('tailscale:phoneLink', nodeId, author),
}

contextBridge.exposeInMainWorld('entityGraph', api)

declare global {
  interface Window {
    entityGraph: EntityGraphAPI
  }
}
