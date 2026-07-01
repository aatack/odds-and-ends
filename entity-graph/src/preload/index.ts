import { contextBridge, ipcRenderer } from 'electron'
import type { AppEvent } from '../core/events'
import type { SourceConfig } from '../main/index'
import type { Entity, QueryPage } from '../core/wrapper'

export interface EntityGraphAPI {
  // Config
  getUser: () => Promise<string>
  setUser: (name: string) => Promise<void>
  getSources: () => Promise<SourceConfig[]>
  addSource: (cfg: Omit<SourceConfig, 'id'>) => Promise<string>
  removeSource: (id: string) => Promise<void>

  // Raw events
  readEvents: (entityIds: string[], sourceIds?: string[]) => Promise<Record<string, AppEvent[]>>
  writeEvents: (events: AppEvent[], sourceIds?: string[]) => Promise<void>

  // Wrapper
  readEntities: (ids: string[], sourceIds?: string[]) => Promise<Record<string, Entity>>
  resolveQuery: (
    rootId: string,
    options: { maxDepth?: number; collapsed?: string[]; limit?: number },
    sourceIds?: string[],
  ) => Promise<QueryPage>
  createEntity: (values: Record<string, unknown>, parentId?: string, sourceIds?: string[]) => Promise<string>
  moveEntity: (entityId: string, fromParent: string, toParent: string, sourceIds?: string[]) => Promise<void>
}

const api: EntityGraphAPI = {
  getUser:       ()             => ipcRenderer.invoke('config:getUser'),
  setUser:       (name)         => ipcRenderer.invoke('config:setUser', name),
  getSources:    ()             => ipcRenderer.invoke('config:getSources'),
  addSource:     (cfg)          => ipcRenderer.invoke('config:addSource', cfg),
  removeSource:  (id)           => ipcRenderer.invoke('config:removeSource', id),

  readEvents:    (ids, sids)          => ipcRenderer.invoke('data:readEvents', ids, sids),
  writeEvents:   (events, sids)       => ipcRenderer.invoke('data:writeEvents', events, sids),
  readEntities:  (ids, sids)          => ipcRenderer.invoke('data:readEntities', ids, sids),
  resolveQuery:  (root, opts, sids)   => ipcRenderer.invoke('data:resolveQuery', root, opts, sids),
  createEntity:  (vals, pid, sids)    => ipcRenderer.invoke('data:createEntity', vals, pid, sids),
  moveEntity:    (eid, from, to, sids) => ipcRenderer.invoke('data:moveEntity', eid, from, to, sids),
}

contextBridge.exposeInMainWorld('entityGraph', api)

declare global {
  interface Window { entityGraph: EntityGraphAPI }
}
