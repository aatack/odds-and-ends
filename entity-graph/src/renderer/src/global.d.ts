import type { AppEvent } from '../../core/events'
import type { Entity, QueryPage } from '../../core/wrapper'

interface SourceConfig {
  id: string
  label: string
  type: 'sqlite' | 'http'
  path?: string
  url?: string
}

interface EntityGraphAPI {
  getUser: () => Promise<string>
  setUser: (name: string) => Promise<void>
  getSources: () => Promise<SourceConfig[]>
  addSource: (cfg: Omit<SourceConfig, 'id'>) => Promise<string>
  removeSource: (id: string) => Promise<void>

  readEvents: (entityIds: string[], sourceIds?: string[]) => Promise<Record<string, AppEvent[]>>
  writeEvents: (events: AppEvent[], sourceIds?: string[]) => Promise<void>

  readEntities: (ids: string[], sourceIds?: string[]) => Promise<Record<string, Entity>>
  resolveQuery: (
    rootId: string,
    options: { maxDepth?: number; collapsed?: string[]; limit?: number },
    sourceIds?: string[],
  ) => Promise<QueryPage>
  createEntity: (values: Record<string, unknown>, parentId?: string, sourceIds?: string[]) => Promise<string>
  moveEntity: (entityId: string, fromParent: string, toParent: string, sourceIds?: string[]) => Promise<void>
}

declare global {
  interface Window {
    entityGraph: EntityGraphAPI
  }
}
