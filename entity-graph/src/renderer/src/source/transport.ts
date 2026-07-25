// The seam between the app and whatever source is open. Everything that talks to
// the entity store goes through here, so the state and tool layers never learn
// about IPC, HTTP, or `window.entityGraph` — and a headless driver can supply its
// own `call` instead.

export interface SourceTransport {
  /** Invoke one of the source's own tools. */
  call: (toolId: string, args: unknown) => Promise<unknown>
  /** The author recorded against writes. */
  user: string
  /**
   * Which source is open. Anything holding on to events — the undo stack — needs
   * this, so it can refuse to replay one store's events into another.
   */
  sourceId: string
}

let transport: SourceTransport | null = null

export const setSourceTransport = (next: SourceTransport | null): void => {
  transport = next
}

export const hasSource = (): boolean => transport != null

export const currentUser = (): string => transport?.user ?? 'anonymous'

export const currentSourceId = (): string | null => transport?.sourceId ?? null

export function callSource(toolId: string, args: unknown): Promise<unknown> {
  if (!transport) return Promise.reject(new Error('No source is open'))
  return transport.call(toolId, args)
}
