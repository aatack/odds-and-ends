// The app's configuration, kept in the store beside the items.

import { randomBytes } from 'node:crypto'
import type { Store } from './store'

/** Not the older app's (47390), so that the two can run side by side. */
export const DEFAULT_MCP_PORT = 47391

export class Settings {
  constructor(private store: Store) {}

  get<T>(key: string, fallback: T): T {
    return this.store.settings.has(key) ? (this.store.settings.get(key) as T) : fallback
  }

  set(key: string, value: unknown): void {
    this.store.setSetting(key, value)
  }

  // ---- the operator's MCP endpoint ----

  mcpToken(): string {
    const token = this.get<string | null>('mcp.token', null)
    return token ?? this.regenerateMcpToken()
  }

  regenerateMcpToken(): string {
    const token = randomBytes(24).toString('base64url')
    this.set('mcp.token', token)
    return token
  }

  mcpPort(): number {
    return this.get('mcp.port', DEFAULT_MCP_PORT)
  }
}
