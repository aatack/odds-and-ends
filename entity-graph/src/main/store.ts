import Store from 'electron-store'
import type { CurrentSource, Server, SourceConnection } from '../core/client'

export interface AppConfig {
  /** Every server the app knows about (external + local). Includes secrets. */
  servers: Server[]
  /** Saved source credentials for non-admin servers. */
  sourceConnections: SourceConnection[]
  user: string
  /** The source the editor opens by default; null until the user picks one. */
  currentSource: CurrentSource | null
}

export const store = new Store<AppConfig>({
  defaults: { servers: [], sourceConnections: [], user: 'anonymous', currentSource: null },
})
