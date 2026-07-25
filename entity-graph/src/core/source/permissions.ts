import type { AppEvent } from '../events'

/**
 * How far back popping is allowed to reach. Undo takes events off the store for
 * real, so beyond this the history is settled: an event older than five minutes
 * is never deleted, whatever is asked for. It bounds the damage a runaway client
 * — or a user holding Ctrl+Z — can do to a store that has no other copy.
 */
export const POP_AGE_LIMIT_MS = 5 * 60 * 1000

/**
 * The minimal storage surface the DB-backed tools need. `SqliteInterface`
 * satisfies this; Combined/Frozen provide their own implementations.
 */
export interface EventBacking {
  /** Read events for specific entities, deduplicated and flattened. */
  readEvents(entityIds: string[]): Promise<AppEvent[]>
  /** Read every event in the store, deduplicated. */
  readAllEvents(): Promise<AppEvent[]>
  writeEvents(events: AppEvent[]): Promise<void>
  /**
   * Optional: take the most recent events back off the store, returning them.
   * Append-only backings simply don't implement it. Implementations must honour
   * {@link POP_AGE_LIMIT_MS}.
   */
  popLatestEvents?(windowMs: number): Promise<AppEvent[]>
}

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

/**
 * The blob half of a store's surface. Optional, like popping: a backing that
 * can't hold bytes simply doesn't implement it, the tools are then absent, and a
 * client can tell resources aren't available by their absence.
 */
export interface ResourceBacking {
  writeResource(resource: ResourceRecord): Promise<void>
  readResource(id: string): Promise<ResourceRecord | null>
}

/** A single outbound HTTP request (see {@link Permissions.httpRequest}). */
export interface HttpRequest {
  method?: string
  url: string
  headers?: Record<string, string>
  body?: string
}

export interface HttpResponse {
  status: number
  headers: Record<string, string>
  body: string
}

/** A single shell command invocation (see {@link Permissions.runCommand}). */
export interface CommandRequest {
  command: string
  args?: string[]
  cwd?: string
}

export interface CommandResult {
  exitCode: number
  stdout: string
  stderr: string
}

/**
 * The fundamental capabilities a source can grant. Tools — both the default
 * ones and user-defined ones — are built on top of these primitives:
 *
 * - `readEvents` / `writeEvents`: read from / write to the event store.
 * - `httpRequest`: make an outbound HTTP request.
 * - `runCommand`: invoke a command line program.
 *
 * DB read/write are implemented today; HTTP and CLI are scaffolded as stubs
 * (see {@link stubbedIO}) and throw until real handlers land.
 */
export interface Permissions {
  /** Read events; omit `entityIds` (or pass undefined) to read every event. */
  readEvents(entityIds?: string[]): Promise<AppEvent[]>
  writeEvents(events: AppEvent[]): Promise<void>
  /**
   * Remove the most recent event, and any within `windowMs` of it, returning
   * them. Nothing older than {@link POP_AGE_LIMIT_MS} comes off. Optional — the
   * store may be append-only, in which case the `popEvents` tool is simply
   * absent and the client has no undo.
   */
  popLatestEvents?(windowMs: number): Promise<AppEvent[]>
  /**
   * Store bytes under an entity id, and read them back. Optional as a pair — a
   * store that can't hold blobs grants neither, and the resource tools are then
   * absent from the source.
   */
  writeResource?(resource: ResourceRecord): Promise<void>
  readResource?(id: string): Promise<ResourceRecord | null>
  httpRequest(req: HttpRequest): Promise<HttpResponse>
  runCommand(req: CommandRequest): Promise<CommandResult>
}

/** Thrown by capability handlers that have not been implemented yet. */
export class NotImplementedError extends Error {
  constructor(message: string) {
    super(message)
    this.name = 'NotImplementedError'
  }
}

/** HTTP + CLI handlers that throw. Composed into DB permissions until real. */
export function stubbedIO(): Pick<Permissions, 'httpRequest' | 'runCommand'> {
  return {
    httpRequest: async () => {
      throw new NotImplementedError('httpRequest permission is not implemented')
    },
    runCommand: async () => {
      throw new NotImplementedError('runCommand permission is not implemented')
    },
  }
}

/**
 * Build a `Permissions` from an event-store backing: real DB read/write, with
 * HTTP and CLI stubbed out. `readEvents(undefined)` dumps every event. Blob
 * storage is granted only when the backing offers both halves of it.
 */
export function dbPermissions(backing: EventBacking & Partial<ResourceBacking>): Permissions {
  const pop = backing.popLatestEvents?.bind(backing)
  const writeResource = backing.writeResource?.bind(backing)
  const readResource = backing.readResource?.bind(backing)
  return {
    readEvents: (entityIds) =>
      entityIds === undefined ? backing.readAllEvents() : backing.readEvents(entityIds),
    writeEvents: (events) => backing.writeEvents(events),
    // Granted only when the backing can actually take events off again.
    ...(pop ? { popLatestEvents: pop } : {}),
    ...(writeResource && readResource ? { writeResource, readResource } : {}),
    ...stubbedIO(),
  }
}
