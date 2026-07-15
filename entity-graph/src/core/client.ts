import type { Safety } from './source/types'

// ---------------------------------------------------------------------------
// Connections
// ---------------------------------------------------------------------------

/**
 * A saved way to reach the server. Either an `admin` connection (base URL +
 * admin token, for source/token management) or a `source` connection (base URL
 * + a specific source id + that source's token, for reading/editing its graph).
 * Persisted by the main process in `electron-store`; the renderer only ever
 * refers to a connection by `id`.
 */
export interface Connection {
  id: string
  label: string
  /** e.g. `http://127.0.0.1:4000` — no trailing slash. */
  baseUrl: string
  kind: 'admin' | 'source'
  /** Admin token (kind='admin') or the source's bearer token (kind='source'). */
  token: string
  /** Required when `kind === 'source'`. */
  sourceId?: string
}

/** A new connection before the main process assigns it an id. */
export type NewConnection = Omit<Connection, 'id'>

// ---------------------------------------------------------------------------
// Admin shapes — client-side mirror of `server/src/config.ts`, kept here so the
// renderer stays decoupled from the server workspace.
// ---------------------------------------------------------------------------

export type SourceConfig =
  | { type: 'sqlite'; path: string; defaultAuthor?: string }
  | { type: 'combined'; children: string[] }
  | { type: 'frozen'; child: string; beforeTs: number }
  | { type: 'filter'; child: string; allow?: string[]; deny?: string[]; maxSafety?: Safety }
  | { type: 'remote'; url: string; token?: string }

export type SourceType = SourceConfig['type']

export interface SourceRow {
  id: string
  label: string
  type: SourceType
  config: SourceConfig
  createdAt: number
}

export interface TokenRow {
  token: string
  sourceId: string
  label: string
  revoked: boolean
}

// Re-export the tool metadata shape the `/tools` endpoint returns.
export type { ToolMeta } from './source/schema'
