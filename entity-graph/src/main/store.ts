import Store from 'electron-store'
import type { Server, SourceConnection } from '../core/client'

export interface AppConfig {
  /** Every server the app knows about (external + local). Includes secrets. */
  servers: Server[]
  /** Saved source credentials for non-admin servers. */
  sourceConnections: SourceConnection[]
  user: string
}

export const store = new Store<AppConfig>({
  defaults: { servers: [], sourceConnections: [], user: 'anonymous' },
})
