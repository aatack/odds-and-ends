// A thin typed client over the egdb HTTP API: one method per endpoint, no
// caching and no state. The React layer builds on it; it is also usable on its
// own (in Node, a worker, or a script).

import type {
  BranchId,
  BranchInfo,
  EntityId,
  EventRef,
  ErrorKind,
  Filter,
  FindOptions,
  FindPage,
  Link,
  LinkChange,
  LinkGroup,
  LinkPair,
  LogFilter,
  ParentRef,
  Prove,
  ProveRequest,
  SeqRange,
  UndoOutcome,
  Value,
  ValueChange,
  Values,
  WalkPage,
  WalkRequest,
  WriteBatch,
  WriteResult,
} from './types.js'

export interface ClientOptions {
  /** The server's base URL, for example `http://127.0.0.1:7878` or `/api`. */
  url: string
  /** Sent as `Authorization: Bearer <token>` when set. */
  token?: string
  /** A `fetch` to use in place of the global one. */
  fetch?: typeof fetch
}

/** An error the server returned, or a failure to reach it (`kind: 'network'`). */
export class EgdbError extends Error {
  constructor(
    readonly kind: ErrorKind | 'network',
    message: string,
    readonly status?: number,
  ) {
    super(message)
    this.name = 'EgdbError'
  }
}

export class EgdbClient {
  readonly url: string
  private readonly token?: string
  private readonly fetcher: typeof fetch

  constructor(options: ClientOptions) {
    this.url = options.url.replace(/\/+$/, '')
    this.token = options.token
    this.fetcher = options.fetch ?? ((...args) => globalThis.fetch(...args))
  }

  private async call<T>(method: 'GET' | 'POST', path: string, body?: unknown): Promise<T> {
    const headers: Record<string, string> = {}
    if (body !== undefined) headers['content-type'] = 'application/json'
    if (this.token) headers.authorization = `Bearer ${this.token}`
    let response: Response
    try {
      response = await this.fetcher(`${this.url}${path}`, {
        method,
        headers,
        body: body === undefined ? undefined : JSON.stringify(body),
      })
    } catch (error) {
      throw new EgdbError('network', error instanceof Error ? error.message : String(error))
    }
    const text = await response.text()
    const json = text ? JSON.parse(text) : null
    if (!response.ok) {
      throw new EgdbError(json?.error ?? 'internal', json?.message ?? response.statusText, response.status)
    }
    return json as T
  }

  health(): Promise<{ ok: boolean }> {
    return this.call('GET', '/health')
  }

  // --- writes ---------------------------------------------------------------

  write(batch: WriteBatch): Promise<WriteResult> {
    return this.call('POST', '/write', batch)
  }

  archiveEntity(branch: BranchId, author: string, entity: EntityId): Promise<WriteResult> {
    return this.call('POST', '/archive_entity', { branch, author, entity })
  }

  restoreEntity(branch: BranchId, author: string, entity: EntityId, entityType: string): Promise<WriteResult> {
    return this.call('POST', '/restore_entity', { branch, author, entity, entity_type: entityType })
  }

  /** Undo the author's newest batch on the branch; `null` when there is none. */
  undo(branch: BranchId, author: string): Promise<UndoOutcome | null> {
    return this.call('POST', '/undo', { branch, author })
  }

  // --- reads ----------------------------------------------------------------

  values(branch: BranchId, entities: readonly EntityId[]): Promise<Record<EntityId, Values>> {
    return this.call('POST', '/values', { branch, entities })
  }

  /** The event that set each value of each entity (when, by whom), shaped as `values`. */
  valueEvents(branch: BranchId, entities: readonly EntityId[]): Promise<Record<EntityId, Record<string, EventRef>>> {
    return this.call('POST', '/value_events', { branch, entities })
  }

  /** Every value event the branch sees that the filter keeps, the newest first: the edit log of values. */
  valueLog(branch: BranchId, filter: LogFilter = {}): Promise<ValueChange[]> {
    return this.call('POST', '/value_log', { branch, ...filter })
  }

  /** Every link event the branch sees that the filter keeps (entities match either end), the newest first. */
  linkLog(branch: BranchId, filter: LogFilter = {}): Promise<LinkChange[]> {
    return this.call('POST', '/link_log', { branch, ...filter })
  }

  value(branch: BranchId, entity: EntityId, key: string): Promise<Value> {
    return this.call('POST', '/value', { branch, entity, key })
  }

  links(
    branch: BranchId,
    entities: readonly EntityId[],
    groups: readonly LinkPair[],
  ): Promise<Record<EntityId, LinkGroup[]>> {
    return this.call('POST', '/links', { branch, entities, groups })
  }

  linksIncludingArchived(
    branch: BranchId,
    entities: readonly EntityId[],
    groups: readonly LinkPair[],
  ): Promise<Record<EntityId, LinkGroup[]>> {
    return this.call('POST', '/links_including_archived', { branch, entities, groups })
  }

  children(branch: BranchId, entity: EntityId, linkType: string): Promise<Link[]> {
    return this.call('POST', '/children', { branch, entity, link_type: linkType })
  }

  parents(branch: BranchId, entity: EntityId, linkType: string): Promise<Link[]> {
    return this.call('POST', '/parents', { branch, entity, link_type: linkType })
  }

  exists(branch: BranchId, entities: readonly EntityId[]): Promise<Record<EntityId, boolean>> {
    return this.call('POST', '/exists', { branch, entities })
  }

  isArchived(branch: BranchId, entities: readonly EntityId[]): Promise<Record<EntityId, boolean>> {
    return this.call('POST', '/is_archived', { branch, entities })
  }

  find(branch: BranchId, filter: Filter, options?: FindOptions): Promise<FindPage> {
    return this.call('POST', '/find', { branch, filter, options })
  }

  walk(branch: BranchId, request: WalkRequest): Promise<WalkPage> {
    return this.call('POST', '/walk', { branch, ...request })
  }

  /** Whether the walk `request` describes would emit `request.item`, with the path. */
  prove(branch: BranchId, request: ProveRequest): Promise<Prove> {
    return this.call('POST', '/prove', { branch, ...request })
  }

  // --- branches -------------------------------------------------------------

  branches(): Promise<BranchInfo[]> {
    return this.call('GET', '/branches')
  }

  branch(branch: BranchId): Promise<BranchInfo> {
    return this.call('POST', '/branch', { branch })
  }

  async createBranch(name: string, parents: readonly ParentRef[] = []): Promise<BranchId> {
    return (await this.call<{ branch: BranchId }>('POST', '/create_branch', { name, parents })).branch
  }

  async fork(from: BranchId, name: string): Promise<BranchId> {
    return (await this.call<{ branch: BranchId }>('POST', '/fork', { from, name })).branch
  }

  async pull(into: BranchId, from: BranchId, range: SeqRange): Promise<void> {
    await this.call('POST', '/pull', { into, from, range })
  }

  async merge(into: BranchId, from: BranchId): Promise<void> {
    await this.call('POST', '/merge', { into, from })
  }

  async setParents(branch: BranchId, parents: readonly ParentRef[]): Promise<void> {
    await this.call('POST', '/set_parents', { branch, parents })
  }

  async archiveBranch(branch: BranchId, archived: boolean): Promise<void> {
    await this.call('POST', '/archive_branch', { branch, archived })
  }
}
