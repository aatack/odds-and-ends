import { persistentAtom } from '../state/atom'
import type { ToolMeta } from '../core/types'

// The seam between the app and the one source it talks to.
//
// The desktop app reaches its source over IPC, through a main process that keeps
// servers, admin tokens and source configuration. There is none of that here: a
// phone points at exactly one source, by URL, and holds that source's bearer
// token itself. Everything the app knows about where its data comes from is the
// {@link Connection} below.

export interface Connection {
  /** e.g. `http://192.168.1.20:4000` — no trailing slash. */
  baseUrl: string
  sourceId: string
  token: string
  /** Recorded as the author of every write this client makes. */
  author: string
}

const isConnection = (v: unknown): boolean => {
  const c = v as Connection | null
  return !!c && typeof c.baseUrl === 'string' && typeof c.sourceId === 'string'
}

export const connectionAtom = persistentAtom<Connection | null>(
  'entity-graph-mobile.connection',
  null,
  (v) => v === null || isConnection(v),
)

/** The tool ids the open source actually exposes; null until they have loaded. */
export const capabilitiesAtom = persistentAtom<string[] | null>(
  'entity-graph-mobile.capabilities',
  null,
  (v) => v === null || Array.isArray(v),
)

export const getConnection = (): Connection | null => connectionAtom.get()

export const currentUser = (): string => connectionAtom.get()?.author || 'mobile'

export const currentSourceId = (): string | null => connectionAtom.get()?.sourceId ?? null

/** Whether the source exposes a tool — how undo and resources are detected. */
export const canCall = (toolId: string): boolean =>
  capabilitiesAtom.get()?.includes(toolId) ?? false

/** Trim a pasted base URL into the shape the fetches below assume. */
export const normaliseBaseUrl = (raw: string): string => raw.trim().replace(/\/+$/, '')

class SourceError extends Error {}

/**
 * What a rejected `fetch` means, since the browser will not say.
 *
 * Every failure to get a request out at all — no server, wrong port, a server bound to
 * loopback that this device cannot reach, a preflight the server didn't answer —
 * arrives as the same bare `TypeError: Failed to fetch`, with the real reason
 * deliberately withheld from the page. So the message names the three things it
 * actually is, in the order they are worth checking.
 */
const unreachable = (baseUrl: string): SourceError =>
  new SourceError(
    `Can't reach ${baseUrl}. Check the server is running; that it was started with ` +
      `HOST=0.0.0.0 (bound to 127.0.0.1 it only answers its own machine); and that it ` +
      `has been restarted since cross-origin calls were allowed.`,
  )

/**
 * Invoke one of the source's tools.
 *
 * Two layers of failure are flattened into one thrown error, because the caller
 * cannot act differently on them: the HTTP request itself can fail (phone off the
 * network, wrong address, revoked token), and the call can come back
 * `{ status: 'error' }` from a tool that ran and refused. Both surface as a toast.
 */
export async function callSource(toolId: string, args: unknown): Promise<unknown> {
  const c = connectionAtom.get()
  if (!c) throw new SourceError('Not connected to a source')

  let response: Response
  try {
    response = await fetch(`${c.baseUrl}/${encodeURIComponent(c.sourceId)}/call`, {
      method: 'POST',
      headers: { 'content-type': 'application/json', authorization: `Bearer ${c.token}` },
      body: JSON.stringify({ tool: toolId, args }),
    })
  } catch {
    throw unreachable(c.baseUrl)
  }

  if (response.status === 401) throw new SourceError('The source token was rejected')
  if (response.status === 404) throw new SourceError(`No source "${c.sourceId}" on that server`)
  if (!response.ok) throw new SourceError(`Server returned ${response.status}`)

  const body = (await response.json()) as
    | { status: 'success'; result: unknown }
    | { status: 'error'; message: string }
  if (body.status === 'error') throw new SourceError(body.message)
  return body.result
}

/**
 * Check a connection and return the tools it offers. Used by the setup screen to
 * say "this works" before anything is saved, and on every start to re-learn what
 * the source can do — whether undo is available, whether it can hold bytes.
 */
export async function fetchTools(c: Connection): Promise<ToolMeta[]> {
  let response: Response
  try {
    response = await fetch(`${c.baseUrl}/${encodeURIComponent(c.sourceId)}/tools`, {
      headers: { authorization: `Bearer ${c.token}` },
    })
  } catch {
    // The first request the app ever makes, and so the one most likely to fail for a
    // reason that has nothing to do with the details just typed in.
    throw unreachable(c.baseUrl)
  }
  if (response.status === 401) throw new Error('The source token was rejected')
  if (response.status === 404) throw new Error(`No source "${c.sourceId}" on that server`)
  if (!response.ok) throw new Error(`Server returned ${response.status}`)
  return (await response.json()) as ToolMeta[]
}

/** Save a connection and remember what it can do. */
export async function connect(c: Connection): Promise<void> {
  const tools = await fetchTools(c)
  connectionAtom.set(c)
  capabilitiesAtom.set(tools.map((t) => t.id))
}

/** Re-read the tool list of the open source, quietly. */
export async function refreshCapabilities(): Promise<void> {
  const c = connectionAtom.get()
  if (!c) return
  try {
    capabilitiesAtom.set((await fetchTools(c)).map((t) => t.id))
  } catch {
    // Offline, or the token has gone: leave the last known list in place rather
    // than blanking the UI's idea of what it can do.
  }
}

export function disconnect(): void {
  connectionAtom.set(null)
  capabilitiesAtom.set(null)
}

/**
 * A connection encoded for the URL fragment, so a laptop can hand one to a phone
 * as a link or a QR code. Typing a bearer token on a phone keyboard is miserable
 * enough to be worth a code path.
 *
 * The fragment, specifically: it never leaves the browser, so the token isn't in
 * a request line or a server log on the way in.
 */
export function encodeConnection(c: Connection): string {
  return `#connect=${btoa(JSON.stringify(c)).replace(/=+$/, '')}`
}

/** Read a connection out of `location.hash`, if one was handed in that way. */
export function connectionFromHash(hash: string): Connection | null {
  const match = /[#&]connect=([^&]+)/.exec(hash)
  if (!match) return null
  try {
    const parsed = JSON.parse(atob(match[1])) as Connection
    if (!isConnection(parsed)) return null
    return { ...parsed, baseUrl: normaliseBaseUrl(parsed.baseUrl), author: parsed.author || 'mobile' }
  } catch {
    return null
  }
}
