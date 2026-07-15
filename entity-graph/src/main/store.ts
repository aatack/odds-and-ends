import Store from 'electron-store'
import type { Connection } from '../core/client'

/** Persisted definition of a local server the app manages (includes its secret). */
export interface LocalServerDef {
  id: string
  label: string
  port: number
  adminToken: string
}

export interface AppConfig {
  connections: Connection[]
  activeId: string | null
  user: string
  localServers: LocalServerDef[]
}

export const store = new Store<AppConfig>({
  defaults: { connections: [], activeId: null, user: 'anonymous', localServers: [] },
})
