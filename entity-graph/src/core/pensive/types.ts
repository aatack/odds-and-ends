import type { AppEvent } from '../events'

// What a pensive is, as a piece of TypeScript. Everything else here — how one is
// stored, how one is exposed, what tools it wears — is arranged around this
// interface rather than the other way round: a SQLite file, several pensives
// joined together, and one reached over HTTP are the same thing to everything
// above them, so where the notes actually live is a decision the user makes on
// the sources page instead of one the app is built out of.

/**
 * How far back popping is allowed to reach, as standard. Undo takes events off
 * the store for real, so beyond this the history is settled: an event older than
 * five minutes is never deleted, whatever is asked for. It bounds the damage a
 * runaway client — or a user holding Ctrl+Z — can do to a store that has no other
 * copy.
 *
 * **The store decides this, not the caller.** A pensive is free to allow less —
 * an archive may allow none at all — and nothing a client sends can widen it.
 */
export const POP_AGE_LIMIT_MS = 5 * 60 * 1000

/**
 * How close to the last event another one has to be to count as part of the same
 * action, as standard. One gesture often writes several events at the same
 * instant — creating an entity writes its values and the link to its parent
 * together — and they come off as a unit or not at all.
 *
 * The store's too, for the same reason: a caller asking for "everything in the
 * last four minutes" is asking to undo somebody's afternoon.
 */
export const POP_GROUP_MS = 100

/**
 * A stored blob — an image pasted into the tree, a file dropped on it.
 *
 * `id` is an entity id, not an id of its own: a resource is the body of the
 * entity that describes it, so the entity carrying `type: 'file'` and the bytes
 * are looked up under the same key and there is no reference to keep in step.
 *
 * The bytes travel as base64 because JSON is all the transport carries; a store
 * is free to keep them decoded, and the SQLite one does.
 */
export interface ResourceRecord {
  id: string
  timestamp: number
  author: string
  mimeType: string
  /** The original file name, where there was one — the clipboard rarely gives one. */
  name: string | null
  /** Base64. */
  data: string
}

/** What one call to the `scanEvents` tool covers. */
export interface EventScan {
  /** Every entity whose events are complete in `events`. */
  entityIds: string[]
  events: AppEvent[]
}

/**
 * A store of notes, as the rest of the app needs it.
 *
 * Six calls, in two halves. The first five are the store itself: events in and
 * out, bytes in and out. The last two are its *tools* — the vocabulary a client,
 * a script or an agent works in (`query`, `createEntity`, whatever the user has
 * written under `@tools`) — which every implementation here builds over those
 * primitives, so a pensive gains them by being one rather than by declaring them
 * again.
 *
 * Nothing in it reaches outside the store. The integrations — GitHub, Slack,
 * Claude, a terminal — are the *app's* hands and are held in the app, so no
 * pensive can be handed them by being composed or published.
 */
export interface Pensive {
  /** Opaque, and the id of the node that defines it. */
  readonly id: string
  readonly label: string

  /** Events for the named entities, or every event when `entityIds` is omitted. */
  readEvents(entityIds?: string[]): Promise<AppEvent[]>
  /** Append events verbatim — the timestamps and authors they carry are kept. */
  writeEvents(events: AppEvent[]): Promise<void>
  /**
   * Remove the last action's events, returning them oldest first — what undo is.
   *
   * How much counts as one action ({@link POP_GROUP_MS}) and how far back this
   * reaches at all ({@link POP_AGE_LIMIT_MS}) are the *store's* to decide, so
   * there is nothing here to pass: a store that has been idle past its limit
   * answers with nothing rather than refusing.
   *
   * `author` narrows it to one person's own events, which is what makes undo
   * safe on a store more than one person is writing to. A pensive reached with a
   * bearer token has it forced to whoever the token belongs to, so a client
   * cannot undo somebody else's edit by asking to.
   */
  popEvents(author?: string): Promise<AppEvent[]>

  /** The bytes under an entity id, or null when there are none. */
  readResource(id: string): Promise<ResourceRecord | null>
  /** Store bytes under an entity id, replacing anything already there. */
  writeResource(resource: ResourceRecord): Promise<void>

  /** Everything callable on this pensive, described well enough to prompt for. */
  listTools(): Promise<ToolMeta[]>
  /** Validate the arguments against the named tool's schema, then run it. */
  callTool(toolId: string, args: unknown): Promise<unknown>
  /**
   * Re-read anything discovered rather than declared — the user's own tools,
   * which are notes in the store, or a remote registry. Called after a pensive
   * is built and whenever the graph changes; a pensive with nothing to discover
   * simply doesn't implement it.
   */
  refresh?(): Promise<void>
}

// --- What a tool looks like -------------------------------------------------

/**
 * How risky a tool is to call.
 * - `pure`: only reads; no effects.
 * - `safe-mutating`: only writes to the underlying event store, so its effects
 *   can be trivially undone.
 * - `dangerous`: interacts with the outside world in unpredictable ways.
 */
export type Safety = 'pure' | 'safe-mutating' | 'dangerous'

/** Ordering used where one safety has to be compared with another. Lower = safer. */
export const SAFETY_RANK: Record<Safety, number> = {
  pure: 0,
  'safe-mutating': 1,
  dangerous: 2,
}

/**
 * One tool as a caller sees it: what it is called, what it does, and the JSON
 * Schema of its arguments. This is what crosses a wire — the IPC bridge, the
 * broadcast API — so it carries no functions.
 */
export interface ToolMeta {
  id: string
  name: string
  description: string
  safety: string
  args: Record<string, unknown>
}

// --- The ways a call can fail that a caller can act on ----------------------

export class ToolNotFoundError extends Error {
  constructor(public toolId: string) {
    super(`No tool with id "${toolId}"`)
    this.name = 'ToolNotFoundError'
  }
}

/**
 * The node this call went through is switched off. Its own error rather than a
 * plain one because the answer is the same wherever it surfaces: nothing is
 * broken, somebody paused it, and the fix is to press play.
 */
export class PausedError extends Error {
  constructor(public label: string) {
    super(`"${label}" is paused`)
    this.name = 'PausedError'
  }
}

/**
 * A pensive that cannot do this at all — a combiner with no write source, a
 * store that only ever appends. Distinct from a tool that failed: retrying, or
 * asking a different way, will not help.
 */
export class NotSupportedError extends Error {
  constructor(message: string) {
    super(message)
    this.name = 'NotSupportedError'
  }
}
