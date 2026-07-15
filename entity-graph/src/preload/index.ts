import { contextBridge, ipcRenderer } from 'electron'
import type {
  ActiveSource,
  NewServer,
  NewSourceConnection,
  ServerView,
  SourceConnection,
  SourceRow,
  ToolMeta,
  TokenRow,
} from '../core/client'

/**
 * The renderer's entire capability surface. The app has no local backend: these
 * are thin forwarders to the main process, which proxies HTTP to the server.
 */
export interface EntityGraphAPI {
  // User
  getUser: () => Promise<string>
  setUser: (name: string) => Promise<void>

  // Servers
  listServers: () => Promise<ServerView[]>
  addServer: (cfg: NewServer) => Promise<string>
  updateServer: (id: string, patch: Partial<NewServer>) => Promise<void>
  removeServer: (id: string) => Promise<void>
  createLocalServer: (label: string) => Promise<{ id: string }>
  startServer: (id: string) => Promise<void>
  stopServer: (id: string) => Promise<void>

  // Source connections (saved credentials for non-admin servers)
  listSourceConnections: () => Promise<SourceConnection[]>
  addSourceConnection: (cfg: NewSourceConnection) => Promise<string>
  updateSourceConnection: (id: string, patch: Partial<NewSourceConnection>) => Promise<void>
  removeSourceConnection: (id: string) => Promise<void>

  // Open / close a source and operate on it
  openSource: (serverId: string, sourceId: string, label: string) => Promise<ActiveSource>
  closeSource: (id: string) => Promise<void>
  sourceTools: (id: string) => Promise<ToolMeta[]>
  sourceCall: (id: string, tool: string, args: unknown) => Promise<unknown>

  // Admin (servers with admin access), keyed by server id
  adminListSources: (serverId: string) => Promise<SourceRow[]>
  adminGetSource: (serverId: string, id: string) => Promise<SourceRow>
  adminCreateSource: (
    serverId: string,
    body: { id: string; label?: string; config: SourceRow['config'] },
  ) => Promise<SourceRow>
  adminUpdateSource: (
    serverId: string,
    id: string,
    body: { label?: string; config?: SourceRow['config'] },
  ) => Promise<SourceRow>
  adminDeleteSource: (serverId: string, id: string) => Promise<{ ok: true }>
  adminListTokens: (serverId: string, id: string) => Promise<TokenRow[]>
  adminIssueToken: (serverId: string, id: string, label?: string) => Promise<{ token: string; sourceId: string }>
  adminRevokeToken: (serverId: string, token: string) => Promise<{ ok: true }>
}

const api: EntityGraphAPI = {
  getUser: () => ipcRenderer.invoke('config:getUser'),
  setUser: (name) => ipcRenderer.invoke('config:setUser', name),

  listServers: () => ipcRenderer.invoke('server:list'),
  addServer: (cfg) => ipcRenderer.invoke('server:add', cfg),
  updateServer: (id, patch) => ipcRenderer.invoke('server:update', id, patch),
  removeServer: (id) => ipcRenderer.invoke('server:remove', id),
  createLocalServer: (label) => ipcRenderer.invoke('server:createLocal', label),
  startServer: (id) => ipcRenderer.invoke('server:start', id),
  stopServer: (id) => ipcRenderer.invoke('server:stop', id),

  listSourceConnections: () => ipcRenderer.invoke('sourceConn:list'),
  addSourceConnection: (cfg) => ipcRenderer.invoke('sourceConn:add', cfg),
  updateSourceConnection: (id, patch) => ipcRenderer.invoke('sourceConn:update', id, patch),
  removeSourceConnection: (id) => ipcRenderer.invoke('sourceConn:remove', id),

  openSource: (serverId, sourceId, label) => ipcRenderer.invoke('source:open', serverId, sourceId, label),
  closeSource: (id) => ipcRenderer.invoke('source:close', id),
  sourceTools: (id) => ipcRenderer.invoke('source:tools', id),
  sourceCall: (id, tool, args) => ipcRenderer.invoke('source:call', id, tool, args),

  adminListSources: (serverId) => ipcRenderer.invoke('admin:listSources', serverId),
  adminGetSource: (serverId, id) => ipcRenderer.invoke('admin:getSource', serverId, id),
  adminCreateSource: (serverId, body) => ipcRenderer.invoke('admin:createSource', serverId, body),
  adminUpdateSource: (serverId, id, body) => ipcRenderer.invoke('admin:updateSource', serverId, id, body),
  adminDeleteSource: (serverId, id) => ipcRenderer.invoke('admin:deleteSource', serverId, id),
  adminListTokens: (serverId, id) => ipcRenderer.invoke('admin:listTokens', serverId, id),
  adminIssueToken: (serverId, id, label) => ipcRenderer.invoke('admin:issueToken', serverId, id, label),
  adminRevokeToken: (serverId, token) => ipcRenderer.invoke('admin:revokeToken', serverId, token),
}

contextBridge.exposeInMainWorld('entityGraph', api)

declare global {
  interface Window {
    entityGraph: EntityGraphAPI
  }
}
