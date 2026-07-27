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

/**
 * The user's chosen "current" source — the one the editor opens by default.
 * Persisted as a durable reference (which server + source), independent of any
 * ephemeral {@link ActiveSource} handle, so it survives restarts.
 */
export interface CurrentSource {
  serverId: string
  sourceId: string
  label: string
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

// ---------------------------------------------------------------------------
// Tailscale — putting the phone app and one source on the tailnet
// ---------------------------------------------------------------------------

/**
 * One thing `tailscale serve` is publishing on this machine's HTTPS name: a
 * directory read off disk (`path`), a reverse proxy (`proxy`), or a literal
 * string (`text`).
 */
export interface TailscaleHandler {
  /** The URL path it answers on, e.g. `/` or `/api/flow`. */
  mount: string
  kind: 'path' | 'proxy' | 'text'
  /** The directory, upstream URL, or literal, depending on `kind`. */
  target: string
}

/** What the app knows about Tailscale on this machine, refreshed on demand. */
export interface TailscaleView {
  /** The `tailscale` command exists and its daemon is up. */
  running: boolean
  /** Why phone access isn't available — an actionable sentence, or null. */
  problem: string | null
  /** The HTTPS name serve answers on, e.g. `laptop.tail1234.ts.net`. */
  domain: string | null
  /** Everything currently served, as tailscale reports it. */
  handlers: TailscaleHandler[]
  /**
   * False when the serve config holds something the app can't put back after
   * the `reset` that removing a mount requires — Funnel, a service, a
   * foreground serve. Adding is still safe; removing is refused.
   */
  editable: boolean
  /** What made it uneditable. */
  locked: string | null
  /** The phone app's build directory, and whether anything has been built into it. */
  app: { path: string; built: boolean }
}

/**
 * Where each thing sits on the tailnet name. These are shared rather than
 * written out at each end because they are the contract between what the main
 * process serves and what the renderer reads back as "on".
 */

/** The phone app itself, at the root of the name. */
export const APP_MOUNT = '/'

/**
 * One source, mounted under its own id rather than at a plain `/api`. Plain
 * `/api` would put the whole server on the tailnet, admin endpoints included;
 * the phone only ever needs the one source.
 */
export const sourceMount = (sourceId: string): string => `/api/${sourceId}`

/** What that mount proxies to — the source's own URL on the machine. */
export const sourceTarget = (baseUrl: string, sourceId: string): string => `${baseUrl}/${sourceId}`

/**
 * The base URL the phone app is given. `--set-path` strips its prefix before
 * proxying, so the app can carry an `/api` segment the server never sees, and
 * the app appends the source id itself.
 */
export const phoneBaseUrl = (domain: string): string => `https://${domain}/api`

/** Where the phone app is opened to install it. */
export const phoneAppUrl = (domain: string): string => `https://${domain}/`

// Re-export the tool metadata shape the `/tools` endpoint returns.
export type { ToolMeta } from './source/schema'
