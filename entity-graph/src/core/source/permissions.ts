import type { AppEvent } from '../events'

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
 * HTTP and CLI stubbed out. `readEvents(undefined)` dumps every event.
 */
export function dbPermissions(backing: EventBacking): Permissions {
  return {
    readEvents: (entityIds) =>
      entityIds === undefined ? backing.readAllEvents() : backing.readEvents(entityIds),
    writeEvents: (events) => backing.writeEvents(events),
    ...stubbedIO(),
  }
}
