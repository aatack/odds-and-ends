import type { Safety } from './source/types'

// ---------------------------------------------------------------------------
// Servers & sources
// ---------------------------------------------------------------------------

/**
 * A server the app knows about. Either *external* (a base URL the user pasted
 * in) or *local* (a child process the app runs — marked by `localPort`). A
 * server has admin access iff it carries an `adminToken`, which lets the UI
 * create/edit/delete that server's sources; without one the user can only
 * connect to existing sources by their id + token.
 *
 * Persisted by the main process in `electron-store`. The secret `adminToken`
 * never crosses to the renderer — see {@link ServerView}.
 */
export interface Server {
  id: string
  label: string
  /** e.g. `http://127.0.0.1:4000` — no trailing slash. Derived for local servers. */
  baseUrl: string
  /** Present ⇔ admin access is configured (always set for local servers). */
  adminToken?: string
  /** Present ⇔ this is a managed local child process. */
  localPort?: number
}

/** Fields needed to create a server (id/baseUrl assigned by the main process). */
export interface NewServer {
  label: string
  /** Required for external servers; ignored for local. */
  baseUrl?: string
  adminToken?: string
}

/**
 * A saved way to reach one source on a *non-admin* server: the source's id plus
 * its bearer token. (Admin servers enumerate their sources live instead.)
 */
export interface SourceConnection {
  id: string
  serverId: string
  sourceId: string
  label: string
  token: string
}

/** A new source connection before the main process assigns it an id. */
export type NewSourceConnection = Omit<SourceConnection, 'id'>

/** Renderer-facing view of a server: computed flags, secret token stripped. */
export interface ServerView {
  id: string
  label: string
  baseUrl: string
  kind: 'local' | 'external'
  /** `adminToken` is present. */
  admin: boolean
  /** Local servers only: whether the child process is currently running. */
  running: boolean
}

/**
 * The source currently open in the viewer. Ephemeral: the main process holds
 * its bearer token in memory and resolves data calls by `id`; nothing extra is
 * persisted per open.
 */
export interface ActiveSource {
  id: string
  label: string
  serverId: string
  sourceId: string
}

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
