import { contextBridge, ipcRenderer } from 'electron'
import type { Connection, NewConnection, SourceRow, TokenRow, ToolMeta } from '../core/client'

/**
 * The renderer's entire capability surface. The app has no local backend: these
 * are thin forwarders to the main process, which proxies HTTP to the server.
 * Data operations take a `connId` naming the connection they act on.
 */
export interface EntityGraphAPI {
  // User
  getUser: () => Promise<string>
  setUser: (name: string) => Promise<void>

  // Connections
  listConnections: () => Promise<Connection[]>
  getActiveConnection: () => Promise<Connection | null>
  setActiveConnection: (id: string | null) => Promise<void>
  addConnection: (cfg: NewConnection) => Promise<string>
  updateConnection: (id: string, patch: Partial<NewConnection>) => Promise<void>
  removeConnection: (id: string) => Promise<void>

  // Source (kind='source' connections)
  sourceTools: (connId: string) => Promise<ToolMeta[]>
  sourceCall: (connId: string, tool: string, args: unknown) => Promise<unknown>

  // Admin (kind='admin' connections)
  adminListSources: (connId: string) => Promise<SourceRow[]>
  adminGetSource: (connId: string, id: string) => Promise<SourceRow>
  adminCreateSource: (
    connId: string,
    body: { id: string; label?: string; config: SourceRow['config'] },
  ) => Promise<SourceRow>
  adminUpdateSource: (
    connId: string,
    id: string,
    body: { label?: string; config?: SourceRow['config'] },
  ) => Promise<SourceRow>
  adminDeleteSource: (connId: string, id: string) => Promise<{ ok: true }>
  adminListTokens: (connId: string, id: string) => Promise<TokenRow[]>
  adminIssueToken: (connId: string, id: string, label?: string) => Promise<{ token: string; sourceId: string }>
  adminRevokeToken: (connId: string, token: string) => Promise<{ ok: true }>
}

const api: EntityGraphAPI = {
  getUser: () => ipcRenderer.invoke('config:getUser'),
  setUser: (name) => ipcRenderer.invoke('config:setUser', name),

  listConnections: () => ipcRenderer.invoke('conn:list'),
  getActiveConnection: () => ipcRenderer.invoke('conn:getActive'),
  setActiveConnection: (id) => ipcRenderer.invoke('conn:setActive', id),
  addConnection: (cfg) => ipcRenderer.invoke('conn:add', cfg),
  updateConnection: (id, patch) => ipcRenderer.invoke('conn:update', id, patch),
  removeConnection: (id) => ipcRenderer.invoke('conn:remove', id),

  sourceTools: (connId) => ipcRenderer.invoke('source:tools', connId),
  sourceCall: (connId, tool, args) => ipcRenderer.invoke('source:call', connId, tool, args),

  adminListSources: (connId) => ipcRenderer.invoke('admin:listSources', connId),
  adminGetSource: (connId, id) => ipcRenderer.invoke('admin:getSource', connId, id),
  adminCreateSource: (connId, body) => ipcRenderer.invoke('admin:createSource', connId, body),
  adminUpdateSource: (connId, id, body) => ipcRenderer.invoke('admin:updateSource', connId, id, body),
  adminDeleteSource: (connId, id) => ipcRenderer.invoke('admin:deleteSource', connId, id),
  adminListTokens: (connId, id) => ipcRenderer.invoke('admin:listTokens', connId, id),
  adminIssueToken: (connId, id, label) => ipcRenderer.invoke('admin:issueToken', connId, id, label),
  adminRevokeToken: (connId, token) => ipcRenderer.invoke('admin:revokeToken', connId, token),
}

contextBridge.exposeInMainWorld('entityGraph', api)

declare global {
  interface Window {
    entityGraph: EntityGraphAPI
  }
}
