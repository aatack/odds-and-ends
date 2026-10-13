import { execFile } from 'node:child_process'
import { randomUUID } from 'node:crypto'
import { outlineEvents, splitText } from './outline.ts'
import { existsSync } from 'node:fs'
import { tmpdir } from 'node:os'
import type { Source } from './graph/cache.ts'
import { bucketEvents, rollupEntity, type GraphEntity } from './graph/entity.ts'
import { eventKey, link, value, values, type AppEvent, type Changed, type Scan } from './graph/events.ts'
import { cacheMigrations, openDatabase, ownedMigrations } from './db.ts'
import { importLegacy } from './legacy.ts'
import { Claude, author as claudeAuthor, type NewSession } from './modules/claude/claude.ts'
import { claudeIds } from './modules/claude/view.ts'
import { Recents } from './modules/recents/recents.ts'
import { GitHub } from './modules/github/github.ts'
import type { Module, ModuleContext } from './modules/module.ts'
import { Slack } from './modules/slack/slack.ts'
import { me, tasksModule } from './modules/tasks/tasks.ts'
import { Topics, type PastedFile } from './modules/topics/topics.ts'
import { topicsIds } from './modules/topics/view.ts'
import type { McpAddress } from './mcp.ts'
import { focusOf, lensOf, moduleInfos, toItem, typeOf, viewOf, type Folds, type Lens } from './present.ts'
import { Blobs, EventStore, Settings } from './store.ts'
import { loadedKey } from './types.ts'
import type { Entity, Focus, LoadPart, LoadRequest, LoadResult, ModuleInfo, NoteValues, Outcome, View } from './types.ts'

const changeEvery = 150
/** The author of a module's own data on its root (a token): not mine to undo. */
const moduleAuthor = 'module'
const clearEvery = 7 * 24 * 60 * 60_000
const checkEvery = 10 * 60_000
/** How far a scan reads past what it was asked for, in layers and in entities per layer. */
const scanDepth = 1
const scanOverscan = 64

export interface CoreOptions {
  /** The owned event log: a file path, or `:memory:`. */
  owned: string
  /** The cache store, likewise. Safe to delete at any time. */
  cache: string
  /** The database before these two, imported once if it is there. */
  legacy?: string
  fetch?: typeof fetch
  gh?: ModuleContext['gh']
  /** Runs git and claude; tests give a fake. */
  run?: ModuleContext['run']
  /** Where the app keeps its files (worktrees go under it). Defaults to a temporary directory. */
  dataDir?: string
  now?: () => number
}

/**
 * Runs a program and returns what it printed; a failure says everything it
 * can: which program, where, its exit code, and all it printed. Node reports a
 * missing working directory as the program being missing (`spawn x ENOENT`),
 * so the two are told apart here.
 */
export function runCommand(command: string, args: string[], cwd: string): Promise<string> {
  return new Promise((resolve, reject) => {
    execFile(command, args, { cwd, maxBuffer: 64 * 1024 * 1024 }, (error, stdout, stderr) => {
      if (!error) return resolve(stdout)
      const code = (error as NodeJS.ErrnoException).code
      let reason: string
      if (code === 'ENOENT') {
        reason = existsSync(cwd) ? `${command} isn't installed, or isn't on the PATH (${process.env.PATH ?? ''})` : `the directory doesn't exist: ${cwd}`
      } else {
        reason = typeof code === 'number' || typeof (error as { code?: unknown }).code === 'number' ? `exit code ${(error as { code?: unknown }).code}` : error.message
      }
      const printed = [stderr.trim(), stdout.trim()].filter(Boolean).join('\n')
      const failure = new Error([`${command} failed in ${cwd}: ${reason}`, printed].filter(Boolean).join('\n\n')) as Error & { stdout?: string }
      failure.stdout = stdout
      reject(failure)
    })
  })
}

function runGh(args: string[]): Promise<string> {
  return new Promise((resolve, reject) => {
    // Away from any checkout, so `--delete-branch` can only touch the remote.
    execFile('gh', args, { cwd: tmpdir(), maxBuffer: 32 * 1024 * 1024 }, (error, stdout, stderr) => {
      if (error) reject(new Error(stderr.trim() || error.message))
      else resolve(stdout)
    })
  })
}

const message = (error: unknown): string => (error instanceof Error ? error.message : String(error))

/**
 * The whole app without a screen. Everything it can do is in `actions`, which
 * is the only thing a UI reaches.
 *
 * It holds two stores: `owned`, the log of what I made, and `cache`, what was
 * loaded from elsewhere. Every read is of both together, so a later event in
 * either wins: what I write now overrides what Slack said, and a cached value
 * written at timestamp 0 never overrides anything of mine.
 */
export class Core {
  readonly owned: EventStore
  readonly cache: EventStore
  readonly settings: Settings
  readonly blobs: Blobs
  readonly modules: Module[]
  readonly slack: Slack
  readonly github: GitHub
  readonly claude: Claude
  readonly recents: Recents
  readonly topics: Topics

  private readonly listeners = new Set<(changed: Changed) => void>()
  private readonly errors = new Map<string, string>()
  private readonly inFlight = new Map<string, Promise<LoadResult>>()
  private readonly now: () => number
  private readonly cacheMeta: { get(): number | null; set(at: number): void }
  private pendingChange: Set<string> | null = null
  private everything = false
  private notifying = false
  private lastNotified = 0
  private clearer: ReturnType<typeof setInterval> | null = null
  private stopWatches: (() => void) | null = null

  constructor(options: CoreOptions) {
    this.now = options.now ?? Date.now
    const ownedDb = openDatabase(options.owned, ownedMigrations)
    const cacheDb = openDatabase(options.cache, cacheMigrations)
    this.owned = new EventStore(ownedDb, 'log', (ids) => this.changed(ids))
    this.cache = new EventStore(cacheDb, 'snapshot', (ids) => this.changed(ids))
    this.settings = new Settings(ownedDb)
    this.blobs = new Blobs(cacheDb)
    this.cacheMeta = {
      get: () => {
        const row = cacheDb.prepare(`SELECT value FROM meta WHERE key = 'created'`).get() as { value: string } | undefined
        return row ? Number(row.value) : null
      },
      set: (at) => {
        cacheDb.prepare(`INSERT INTO meta (key, value) VALUES ('created', ?) ON CONFLICT (key) DO UPDATE SET value = excluded.value`).run(String(at))
      },
    }
    if (this.cacheMeta.get() === null) this.cacheMeta.set(this.now())
    if (options.legacy) importLegacy(options.legacy, this.owned, this.settings)
    this.moveSettingsToModules()

    const context: ModuleContext = {
      owned: this.owned,
      cache: this.cache,
      data: {
        get: (root) => this.entity(root).values,
        set: (root, fields) => this.owned.write(values(root, fields, this.now(), moduleAuthor)),
      },
      blobs: this.blobs,
      lens: this.lens(),
      fetch: options.fetch ?? fetch,
      gh: options.gh ?? runGh,
      run: options.run ?? runCommand,
      dataDir: options.dataDir ?? tmpdir(),
      load: async (id, part, force) => {
        const { error } = await this.load({ id, part, force })
        if (error) throw new Error(error)
      },
      now: this.now,
      mcp: () => this.mcpAddress,
    }
    this.slack = new Slack(context)
    this.github = new GitHub(context)
    this.claude = new Claude(context)
    this.recents = new Recents(context)
    this.topics = new Topics(context, (first, second) => this.shareContext(first, second, claudeAuthor))
    this.modules = [this.slack, this.github, tasksModule(), this.claude, this.recents, this.topics]
  }

  /**
   * A module's data lives on its root entity, not in the settings table:
   * what an earlier version kept there (Slack's token and who I am) moves
   * over once, and leaves the table.
   */
  private moveSettingsToModules(): void {
    const moves: [string, string, string][] = [
      ['slack.token', 'slack', 'secret.token'],
      ['slack.self', 'slack', 'self'],
      ['slack.url', 'slack', 'url'],
    ]
    for (const [setting, root, key] of moves) {
      const held = this.settings.get(setting)
      if (held === null) continue
      this.owned.write([value(root, key, held, this.now(), moduleAuthor)])
      this.settings.set(setting, null)
    }
  }

  /**
   * Starts what runs while the app is open: the weekly clear-out of the cache,
   * and the watches that keep it current. Leave it off in tests and call
   * `clearCache` and `slack.poll` directly.
   */
  start(): void {
    this.clearCacheIfOld()
    this.clearer ??= setInterval(() => this.clearCacheIfOld(), checkEvery)
    this.stopWatches ??= this.slack.watch()
    void this.claude.refreshRepos()
    void this.recoverAnswers()
  }

  /**
   * Answers cut off when the app last stopped, in the Claude module's
   * sessions and in topics: waited for while their claude still runs, then
   * read from its transcript (`recover` in `claude.ts`). Run by `start`.
   */
  recoverAnswers(every?: number): Promise<void> {
    return this.claude.recover([claudeIds.root, topicsIds.root].flatMap((root) => this.lens().children(root)), every)
  }

  /**
   * Sends a message to Slack as me from a topic's box, to a conversation or
   * thread a Slack item in the topic offers (`sendTargets`), and puts it in
   * the chat. The only write to Slack, so it is deliberately not one of
   * `actions`: only the window's own IPC handler calls it, for a key I
   * pressed; the phone, the MCP server and Claude can't reach it.
   */
  topicSlackSend({ topic, on, text }: { topic: string; on: string; text: string }): Promise<Outcome> {
    return this.attempt(topic, async () => {
      const target = this.topics.sendTarget(topic, on)
      const id = await this.slack.send(target.channel, text, target.threadTs)
      return this.topics.sent(topic, id)
    })
  }

  stop(): void {
    if (this.clearer) clearInterval(this.clearer)
    this.clearer = null
    this.stopWatches?.()
    this.stopWatches = null
  }

  private clearCacheIfOld(): void {
    if (this.now() - (this.cacheMeta.get() ?? 0) > clearEvery) this.clearCache()
  }

  /** Where the app's MCP server listens, once the host has started it (`src/main/mcp.ts`). */
  private mcpAddress: McpAddress | null = null

  /** Says where the MCP server is, so every Claude session started from now on is given it. */
  setMcp(address: McpAddress | null): void {
    this.mcpAddress = address
  }

  /** Empties the cache store. Everything in it loads again as it is next looked at. */
  clearCache(): void {
    this.cache.clear()
    this.blobs.clear()
    this.cacheMeta.set(this.now())
    this.changed(null)
  }

  // --- Change notification -----------------------------------------------------

  /**
   * Hears which entities changed, at most once per `changeEvery` ms however
   * many writes there were. Null means anything may have.
   */
  onChange(listener: (changed: Changed) => void): () => void {
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  private changed(ids: string[] | null): void {
    if (ids === null) this.everything = true
    else for (const id of ids) (this.pendingChange ??= new Set()).add(id)
    if (this.notifying) return
    this.notifying = true
    const wait = Math.max(0, this.lastNotified + changeEvery - Date.now())
    setTimeout(() => {
      const changed = this.everything ? null : [...(this.pendingChange ?? [])]
      this.notifying = false
      this.everything = false
      this.pendingChange = null
      this.lastNotified = Date.now()
      for (const listener of this.listeners) listener(changed)
    }, wait)
  }

  // --- Reading -----------------------------------------------------------------

  /** Every event touching `ids`, from both stores. Cached first, so on a tie mine win. */
  read(ids: readonly string[]): AppEvent[] {
    return [...this.cache.read(ids), ...this.owned.read(ids)]
  }

  /** What leaves the core (to the window, to the phone): everything but secrets. */
  private readOut(ids: readonly string[]): AppEvent[] {
    return this.read(ids).filter((e) => !(e.type === 'value' && e.key.startsWith('secret.')))
  }

  entity(id: string): GraphEntity {
    return rollupEntity(id, this.read([id]))
  }

  /** The stores as a cache that already has everything: what a headless caller reads through. */
  source(): Source {
    return {
      get: (ids) => {
        const buckets = bucketEvents(ids, this.read(ids))
        return Object.fromEntries(ids.map((id) => [id, rollupEntity(id, buckets.get(id) ?? [])]))
      },
      pending: () => false,
      loading: (id) => [...this.inFlight.keys()].some((key) => key.startsWith(`${id} `)),
      error: (id) => this.errors.get(id) ?? null,
      expand: () => {},
    }
  }

  lens(): Lens {
    return lensOf(this.source())
  }

  /** A path for a topic: checked, made a worktree if asked, remembered for next time, then the item under the topic. */
  private async addPath(topic: string, cwd: string, worktree: boolean): Promise<AppEvent[]> {
    const requested = this.claude.directory(cwd)
    if (!requested) throw new Error('No directory given')
    const id = randomUUID()
    const place = await this.claude.place(requested, worktree, String(this.item(topic)?.data.text ?? 'topic'), id)
    const events = this.topics.addPath(topic, place, id)
    await this.claude.remember(requested)
    return events
  }

  item(id: string): Entity | null {
    return toItem(this.entity(id))
  }

  /** What the focus view of `id` shows, worked out exactly as the UI does. */
  focus(id: string): Focus {
    return focusOf(id, this.source())
  }

  /**
   * Complete events for `ids`, and for what they link out to a layer down: a
   * view almost always walks downwards, so reading ahead saves a round trip.
   * A layer clipped by the overscan isn't reported as covered.
   */
  scan(ids: readonly string[]): Scan {
    const covered = new Set<string>()
    const seen = new Set<string>()
    const events: AppEvent[] = []
    let frontier = [...new Set(ids)]
    for (const id of frontier) covered.add(id)
    for (let layer = 0; frontier.length; layer++) {
      const batch = this.readOut(frontier)
      for (const e of batch) {
        const key = eventKey(e)
        if (seen.has(key)) continue
        seen.add(key)
        events.push(e)
      }
      if (layer >= scanDepth) break
      const buckets = bucketEvents(frontier, batch)
      const next = new Set<string>()
      for (const id of frontier) {
        for (const child of rollupEntity(id, buckets.get(id) ?? []).outboundLinks) if (!covered.has(child)) next.add(child)
      }
      frontier = [...next].slice(0, scanOverscan)
      for (const id of frontier) covered.add(id)
    }
    return { entityIds: [...covered], events }
  }

  // --- Loading from other services ----------------------------------------------

  private moduleFor(id: string, values: Record<string, unknown> = {}): Module | null {
    const type = typeOf(id, values)
    return type ? (this.modules.find((module) => module.view.owns(type)) ?? null) : null
  }

  /**
   * Loads part of an entity from its service into the cache store, then marks
   * it loaded on the entity. Unless forced, a part still fresh is left alone,
   * and a load already running is joined rather than repeated.
   */
  load(request: LoadRequest): Promise<LoadResult> {
    const { id, part } = request
    const key = `${id} ${part}`
    const running = this.inFlight.get(key)
    if (running) return running
    const entity = this.entity(id)
    const type = typeOf(id, entity.values)
    const module = this.moduleFor(id, entity.values)
    const fresh = type && module?.view.foreign?.(id, type)?.[part]
    if (!type || !module?.load || fresh === undefined || fresh === null) return Promise.resolve({ error: null })
    const loadedAt = Number(entity.values[loadedKey(part)]) || 0
    if (!request.force && loadedAt && this.now() - loadedAt < fresh) return Promise.resolve({ error: null })

    const started = module
      .load(id, part, type)
      .then((): LoadResult => {
        this.cache.write([value(id, loadedKey(part), this.now(), 0, 'app')])
        this.errors.delete(id)
        return { error: null }
      })
      .catch((error: unknown): LoadResult => {
        this.errors.set(id, message(error))
        this.changed([id])
        return { error: message(error) }
      })
      .finally(() => this.inFlight.delete(key))
    this.inFlight.set(key, started)
    return started
  }

  /** Loads every part of an entity again, fresh or not. */
  async refresh(id: string): Promise<void> {
    const parts: LoadPart[] = ['self', 'children']
    await Promise.all(parts.map((part) => this.load({ id, part, force: true })))
  }

  // --- Doing things --------------------------------------------------------------

  private async attempt(id: string, body: () => Promise<AppEvent[]>): Promise<Outcome> {
    try {
      const events = await body()
      this.errors.delete(id)
      return { events, error: null }
    } catch (error) {
      this.errors.set(id, message(error))
      this.changed([id])
      return { events: [], error: message(error) }
    }
  }

  /** The composer under a focus view. */
  submit(id: string, text: string): Promise<Outcome> {
    const entity = this.item(id)
    const module = entity && this.moduleFor(id, { type: entity.type })
    if (!entity || !module?.submit || !text.trim()) return Promise.resolve({ events: [], error: null })
    return this.attempt(id, () => module.submit!(entity, text))
  }

  /** Does an action the focus offered, then loads the entity again to show what it did. */
  async perform(id: string, action: string, text: string): Promise<Outcome> {
    const lens = this.lens()
    const entity = this.item(id)
    const module = entity && this.moduleFor(id, { type: entity.type })
    if (!entity || !module?.perform) return { events: [], error: null }
    const offered = module.view.actions?.(entity, lens).find((candidate) => candidate.id === action)
    if (!offered) return this.attempt(id, () => Promise.reject(new Error(`${action} is not available here`)))
    if (offered.disabled) return this.attempt(id, () => Promise.reject(new Error(offered.disabled)))
    const outcome = await this.attempt(id, () => module.perform!(entity, action, text.trim()))
    await this.load({ id, part: 'children', force: true })
    return outcome
  }

  older(id: string): Promise<Outcome> {
    const entity = this.item(id)
    const module = entity && this.moduleFor(id, { type: entity.type })
    if (!entity || !module?.older || !module.view.older?.(entity)) return Promise.resolve({ events: [], error: null })
    return this.attempt(id, async () => {
      await module.older!(id)
      return []
    })
  }

  /**
   * Puts two items in one context (an item collecting related things, named
   * in each one's `context`). Neither has one: a new context, both under it.
   * One has: the other joins it. Both have: the first joins the second's, and
   * stays under its old one too. `author` is who did it: me (`c`), or Claude
   * through the MCP server.
   */
  shareContext(first: string, second: string, author: string): Outcome {
    if (first === second) return { events: [], error: 'An item cannot share a context with itself' }
    const contextOf = (id: string) => {
      const value = this.entity(id).values.context
      return typeof value === 'string' && value ? value : null
    }
    const [mine, theirs] = [contextOf(first), contextOf(second)]
    const now = this.now()
    const join = (id: string, context: string) => [value(id, 'context', context, now, author), link(context, id, now, author)]
    if (!mine && !theirs) {
      const context = randomUUID()
      return this.write([value(context, 'type', 'context', now, author), ...join(first, context), ...join(second, context)])
    }
    if (!mine) return this.write(join(first, theirs!))
    if (!theirs) return this.write(join(second, mine))
    if (mine === theirs) return { events: [], error: null }
    return this.write(join(first, theirs))
  }

  private write(events: AppEvent[]): Outcome {
    this.owned.write(events)
    return { events, error: null }
  }

  /** The single registry of what a UI may ask for. */
  readonly actions = {
    modules: (): ModuleInfo[] => moduleInfos,
    /** Complete events for entities: the only way the UI reads. */
    scan: ({ ids }: { ids: string[] }): Scan => this.scan(ids),
    load: (request: LoadRequest): Promise<LoadResult> => this.load(request),
    /** What a focus view shows, for a caller with no cache of its own. */
    focus: ({ id }: { id: string }): Focus => this.focus(id),
    perform: ({ id, action, text }: { id: string; action: string; text: string }): Promise<Outcome> =>
      this.perform(id, action, text),
    submit: ({ id, text }: { id: string; text: string }): Promise<Outcome> => this.submit(id, text),
    toggle: ({ id }: { id: string }): Outcome => {
      const entity = this.item(id)
      if (entity?.type !== 'task') return { events: [], error: null }
      return this.write([value(id, 'done', !entity.data.done, this.now(), me)])
    },
    /** Empties the cache store now (never the owned one): what I asked for after confirming twice. */
    clearCache: (): Outcome => {
      this.clearCache()
      return { events: [], error: null }
    },
    /** Pauses Slack's watch, or resumes it (catching up at once). */
    pauseSlack: async ({ paused }: { paused: boolean }): Promise<Outcome> => this.attempt('slack', async () => {
      await this.slack.setPaused(paused)
      return []
    }),
    /** `m` on a row, by its path from the view's root: everything on it is read up to the row's time. */
    markRead: ({ path }: { path: string[] }): Promise<Outcome> => this.attempt(path[path.length - 1] ?? '', () => this.slack.markRead(path)),
    /** Loads further back than anything loads on its own. */
    older: ({ id }: { id: string }): Promise<Outcome> => this.older(id),
    /** Bytes of an image a message presented, by the ref it gave. */
    slackImage: ({ ref }: { ref: string }) => this.slack.image(ref),
    /**
     * Takes my last action back off the owned log (within five minutes) and
     * returns its events, so they can come out of a cache and be redone.
     */
    undo: (): Outcome => ({ events: this.owned.pop(this.now(), me), error: null }),
    /** Writes undone events back verbatim, times and all. */
    redo: ({ events }: { events: AppEvent[] }): Outcome => this.write(events),
    /** A new Claude session, under the Claude root. Its directory may take a moment (a worktree). */
    claudeCreate: (input: NewSession): Promise<Outcome> => this.attempt('claude', () => this.claude.createSession(input)),
    /** A session's directory changed: to `cwd`, or a new worktree of its repo. Its conversation goes with it. */
    claudeSetDirectory: ({ session, cwd, worktree }: { session: string; cwd: string; worktree: boolean }): Promise<Outcome> =>
      this.attempt(session, () => this.claude.setDirectory(session, cwd, worktree)),
    /** A prompt to a Claude session: it and the response go under the session, the answer filling the response when it comes. */
    claudePrompt: ({ session, text }: { session: string; text: string }): Outcome =>
      text.trim() ? { events: this.claude.prompt(session, text), error: null } : { events: [], error: null },
    /** A new topic, named, in the one list of them. */
    /** A new topic, named; with a path, if one is given (`cwd`, or a new worktree of it). */
    topicCreate: ({ name, cwd, worktree, parent }: { name: string; cwd?: string; worktree?: boolean; parent?: string }): Promise<Outcome> =>
      this.attempt('topics', async () => {
        if (parent && this.item(parent)?.type !== 'topic') throw new Error('Not a topic')
        const made = this.topics.create(name)
        const listed = made.find((event) => event.type === 'link')
        if (listed?.type !== 'link') return made
        const id = listed.destinationId
        // Made from another topic (`?`): in its chat too, as a pill.
        const under = parent ? [link(parent, id, this.now(), me)] : []
        this.owned.write(under)
        return [...made, ...under, ...(cwd?.trim() ? await this.addPath(id, cwd, worktree ?? false) : [])]
      }),
    /** A path for a topic: `cwd` as it is, or a new worktree of its repo; a `path` item under the topic, where the next dispatch runs. */
    topicAddPath: ({ topic, cwd, worktree }: { topic: string; cwd: string; worktree: boolean }): Promise<Outcome> =>
      this.attempt(topic, () => this.addPath(topic, cwd, worktree)),
    /** Sends a topic's messages since its last dispatch to Claude as one prompt; the answer fills the response put after them. */
    topicDispatch: ({ topic }: { topic: string }): Promise<Outcome> => this.attempt(topic, async () => this.topics.dispatch(topic)),
    /** Text pasted into a topic: a PR, Slack message or branch link as that item, anything else as a message. */
    topicPaste: ({ topic, text }: { topic: string; text: string }): Promise<Outcome> => this.attempt(topic, async () => this.topics.paste(topic, text)),
    /** What would be lost resolving a topic (its worktree's uncommitted, unpushed or unmerged work), for me to confirm first. */
    topicResolveCheck: async ({ topic }: { topic: string }): Promise<{ warnings: string[]; error: string | null }> => {
      try {
        return { warnings: await this.topics.resolveWarnings(topic), error: null }
      } catch (error) {
        return { warnings: [], error: error instanceof Error ? error.message : String(error) }
      }
    },
    /** Resolves a topic: out of the list, its worktree (only one proved to be) removed. Refused with work to lose, unless `force`. */
    topicResolve: ({ topic, force }: { topic: string; force: boolean }): Promise<Outcome> => this.attempt(topic, () => this.topics.resolve(topic, force)),
    /** A topic's worktree's commit, checked out (detached) in its repo. */
    topicCheckOut: ({ topic }: { topic: string }): Promise<Outcome> => this.attempt(topic, () => this.topics.checkOut(topic)),
    /** The default branch, checked out in a topic's repo. */
    topicCheckOutDefault: ({ topic }: { topic: string }): Promise<Outcome> => this.attempt(topic, () => this.topics.checkOutDefault(topic)),
    /** A topic's branch pushed to GitHub, from its worktree or path. */
    topicPush: ({ topic }: { topic: string }): Promise<Outcome> => this.attempt(topic, () => this.topics.push(topic)),
    /** Puts a topic down until something comes into each of `on` (Slack conversations or threads), from now. */
    topicWait: ({ topic, on }: { topic: string; on: string[] }): Promise<Outcome> => this.attempt(topic, async () => this.topics.wait(topic, on)),
    /** Takes a topic's wait on `on` away. */
    topicUnwait: ({ topic, on }: { topic: string; on: string }): Promise<Outcome> => this.attempt(topic, async () => this.topics.unwait(topic, on)),
    /** Passes over a message that came into a topic's wait on `on`. */
    topicDismiss: ({ topic, on, message }: { topic: string; on: string; message: string }): Promise<Outcome> =>
      this.attempt(topic, async () => this.topics.dismiss(topic, on, message)),
    /** A file pasted into a topic, kept, as an item under it. */
    topicAttach: ({ topic, file }: { topic: string; file: PastedFile }): Promise<Outcome> => this.attempt(topic, async () => this.topics.attach(topic, file)),
    /** A view of `id` was pushed: it goes to the front of Recents (unless it is a plain note). */
    visit: ({ id }: { id: string }): void => this.recents.visit(id, this.entity(id).values),
    /** What a view shows, as a tree, for a caller with no cache of its own. */
    view: ({ id, folds }: { id: string; folds?: Folds }): View => viewOf(id, this.source(), { folds }),
    /** A note under `parent`: an owned entity with no type, just text. */
    create: ({ parent, text, values = {} }: { parent: string; text: string; values?: NoteValues }): Outcome => {
      if (!text.trim()) return { events: [], error: null }
      const id = randomUUID()
      const now = this.now()
      const extra = Object.entries(values).filter(([, v]) => v !== undefined).map(([key, v]) => value(id, key, v, now, me))
      return this.write([value(id, 'text', text, now, me), ...extra, link(parent, id, now, me)])
    },
    /** One of my values on an item (a note's checkbox, say). */
    setValue: ({ id, key, value: v }: { id: string; key: string; value: unknown }): Outcome =>
      this.write([value(id, key, v, this.now(), me)]),
    /**
     * An item's text, for me: what it is called or says everywhere it is
     * shown. Fetched text sits at its own time or at 0, so this, written now,
     * overrides it without changing it for anyone else.
     */
    setText: ({ id, text }: { id: string; text: string }): Outcome => this.write([value(id, 'text', text, this.now(), me)]),
    /**
     * Splits an item's text by its headings, bullets and blocks: the text
     * becomes the first part, the rest notes under it, all at one time so one
     * undo takes it back.
     */
    split: ({ id }: { id: string }): Outcome => {
      const text = this.item(id)?.data.text
      const split = typeof text === 'string' ? splitText(text) : null
      if (!split) return { events: [], error: 'Nothing to split' }
      const now = this.now()
      // The parts keep the item's own type (a Claude response's stay Claude's); a service's type, given by its id, isn't copied.
      const type = this.entity(id).values.type
      return this.write([value(id, 'text', split.lead, now, me), ...outlineEvents(id, split.items, now, me, typeof type === 'string' ? type : undefined)])
    },
    /** Moves `child` from under `from` to under `to`. */
    move: ({ child, from, to }: { child: string; from: string; to: string }): Outcome => {
      if (from === to) return { events: [], error: null }
      if (to === child) return { events: [], error: 'An item cannot be its own parent' }
      const now = this.now()
      return this.write([link(from, child, now, me, 1), link(to, child, now, me)])
    },
    /** `c`: puts two items in one context (`shareContext`), as mine. */
    context: ({ first, second }: { first: string; second: string }): Outcome => this.shareContext(first, second, me),
    link: ({ parent, child }: { parent: string; child: string }): Outcome =>
      parent === child ? { events: [], error: 'An item cannot be linked to itself' } : this.write([link(parent, child, this.now(), me)]),
    unlink: ({ parent, child }: { parent: string; child: string }): Outcome =>
      this.write([link(parent, child, this.now(), me, 1)]),
  }
}

export type Actions = Core['actions']
