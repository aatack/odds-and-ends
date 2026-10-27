import { Processes } from './processes'
import { CustomActions } from './customActions'
import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { mkdirSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { z } from 'zod'
import type { Agent, EventDetail, EventSpec, Item, ItemDetail, ItemEvent, LastRun, Overview, User } from '../shared/types'
import { itemActions } from './actions'
import { EVENT_TYPES, whenNext } from './eventTypes'
import { execCli, type Action, type ActionContext, type Author, type EventType, type Exec } from './integrations'
import { PRESETS, type Preset } from './presets'
import { plainSecrets, Settings, type Secrets } from './settings'
import { Store, type EventRow, type Inspection } from './store'

/** How long a blocker sleeps between checks, by default: at once, then a minute, then longer, up to the last. */
export const BACKOFF_MINUTES = [1, 2, 5, 10]
const MINUTE = 60_000
const DEFAULT_SCHEDULE = [0, ...BACKOFF_MINUTES.map((m) => m * 60)]
/** A bot's model, unless it says another. */
export const DEFAULT_MODEL = 'opus'
/** How many minutes a bot waits after the last thing that happened to it before it wakes, unless it says otherwise. */
export const DEFAULT_DEBOUNCE = 1
/** A bot that has tasks wakes this often, when something has happened in them since its last run. */
export const PERIODIC_MINUTES = 15
/** How many scripts may run at once, unless the `scripts.max_running` setting says otherwise. */
export const MAX_SCRIPTS = 8
/** Who the engine acts as when nobody is said and there is nobody yet (the tests, mostly). */
export const NOBODY = 'human'

/** What a bot says about one task (or its own chat), in its run's output. */
export interface BotUpdate {
  /** The task's id (or the bot's own, for its chat). */
  item: string
  /** To the people in it: what it did, or one direct question. */
  message?: string
  /** Events to add in it: waits, approvals, child tasks. */
  events?: EventSpec[]
  /** Who has the task from now on: people by username, bots by name or id. Left out: see Engine.settle. */
  assign?: string[]
  close?: boolean
}

/** What a bot's run returns, as its structured output. */
export interface BotOutput {
  updates?: BotUpdate[]
}

/** A point in the order of things: a moment (`Store.moment`; 0 for what was loaded), and a time, for what was loaded. */
interface Mark {
  n: number
  at: number
}
/** Whether something at (`n`, `at`) came after `mark`. */
const after = (n: number, at: number, mark: Mark) => n > mark.n || (n === mark.n && at > mark.at)

/** A run going now: in memory only. */
interface Run {
  id: string
  agentId: string
  itemIds: string[]
  startedAt: number
  /** The tasks whose assignees the run changed. */
  assigned: Set<string>
}

/** A person added it, or the system did; a bot, or a bot's script, is a bot. */
const authorOf = (createdBy: string): Author => (createdBy === 'bot' || createdBy === 'script:bot' || createdBy.startsWith('bot:') ? 'bot' : 'human')

/** `[id]()` in text: a reference to an item (a task, a bot, or a person by their username). */
export const refsIn = (text: string): string[] => [...text.matchAll(/\[([\w.-]+)\]\(\)/g)].map((m) => m[1])

const slug = (name: string) => name.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'bot'

export interface EngineOptions {
  store?: Store
  secrets?: Secrets
  exec?: Exec
  fetch?: typeof fetch
  now?: () => number
  /** Where bots get their own folders (`<dataDir>/bots/…`); a folder under the temp dir if left out. */
  dataDir?: string
}

/**
 * The whole system, short of the processes: one tree of items (tasks, bots and
 * people among them), who has what, the bots' wakes, and the loop that checks
 * the blockers. Every change goes through here, and emits `change`. The history
 * is egdb's: every write says who made it and when.
 */
export class Engine extends EventEmitter {
  readonly store: Store
  readonly settings: Settings
  readonly exec: Exec
  readonly fetch: typeof fetch
  readonly now: () => number
  readonly dataDir: string
  readonly actions: Action[] = [...itemActions]
  readonly eventTypes: EventType[] = EVENT_TYPES
  readonly presets: Preset[] = PRESETS
  /** People's own actions, per event type (the Types page). */
  readonly customActions: CustomActions
  /** What `process` events run, while the backend lasts. */
  readonly processes: Processes
  /** Stops a bot's run. Set by the runner. */
  onCancel: (agentId: string) => void = () => {}
  /** The run each bot has going, by the bot's id. In memory only: a crash leaves nothing behind. */
  private running = new Map<string, Run>()
  /** Where each bot was last prompted from: what came after is new to it. */
  private marks = new Map<string, Mark>()
  /** When each bot's settings last changed, which wakes it. */
  private configured = new Map<string, Mark>()
  /** When each bot was woken by hand: it runs then, whatever its debounce says. */
  private woken = new Map<string, number>()
  /** The events being checked right now: a check never starts while the last is still going. */
  private checking = new Set<string>()
  /** Who pressed play on each event about to run (`runNow`): its next check is theirs. */
  private pressed = new Map<string, string>()
  private queues = new Map<string, Promise<unknown>>()

  constructor(opts: EngineOptions = {}) {
    super()
    this.store = opts.store ?? new Store(null, opts.now)
    this.processes = new Processes(opts.now)
    this.settings = new Settings(this.store, opts.secrets ?? plainSecrets)
    this.exec = opts.exec ?? execCli
    this.fetch = opts.fetch ?? fetch
    this.now = opts.now ?? Date.now
    this.dataDir = opts.dataDir ?? join(tmpdir(), 'workflows')
    this.customActions = new CustomActions(this, this.store)
    for (const r of this.store.ofType('bot')) {
      const last = r.params.last_run as LastRun | undefined
      this.marks.set(r.id, last ? { n: 0, at: last.startedAt } : { n: r.added_n, at: r.created_at })
      // A run the app was stopped during is over; what it was on goes back to its owner, saying so.
      if (last && last.endedAt === null && !r.resolved) {
        this.store.setParams(r.id, { ...r.params, last_run: { ...last, endedAt: this.now(), error: 'The app stopped during the run.' } }, `bot:${r.id}`, this.now())
        for (const itemId of last.items)
          if (this.get(itemId) && !this.get(itemId)!.closed && this.assignees(itemId).includes(r.id)) {
            this.add(itemId, [{ type: 'note', params: { text: `@${this.agentName(r.id)} was stopped (the app stopped) before it finished.` } }], 'system')
            this.handBack(itemId, r.id, null)
          }
      }
    }
    // Waits are checked at once after a start; a script, when it asked to be (its wake_at), or at once if that has passed.
    for (const r of this.store.events.values()) {
      if (r.resolved || !this.typeOrNull(r.type)?.check) continue
      if (r.type === 'script' && typeof r.params.wake_at === 'number') r.next_check_at = Math.max(this.now(), r.params.wake_at)
      else if (this.scheduleOf(this.type(r.type), r.params)) r.next_check_at = this.now()
    }
  }

  type(name: string): EventType {
    const t = this.typeOrNull(name)
    if (!t) throw new Error(`No event type ${name}: one of ${this.eventTypes.map((t) => t.type).join(', ')}.`)
    return t
  }

  private typeOrNull(name: string): EventType | null {
    return this.eventTypes.find((t) => t.type === name) ?? null
  }

  /** Whether events of a type are blockers: for the loader. */
  blocking = (type: string): boolean => Boolean(this.typeOrNull(type)?.blocking)

  /** Runs `fn` after everything already queued for this item: one change to an item at a time. */
  private serial<T>(itemId: string, fn: () => Promise<T>): Promise<T> {
    const run = (this.queues.get(itemId) ?? Promise.resolve()).then(fn, fn)
    const tail = run.catch(() => {})
    this.queues.set(itemId, tail)
    void tail.then(() => this.queues.get(itemId) === tail && this.queues.delete(itemId))
    return run
  }

  /** Settles once nothing is in flight. For the tests. */
  async idle(): Promise<void> {
    while (this.queues.size) await Promise.all(this.queues.values())
  }

  // ---- reads ----

  /** A row as the shared event shape, its text filled in by its type. */
  private toEvent(r: EventRow): ItemEvent {
    const type = this.type(r.type)
    const params = r.params
    const data = r.data
    const summary = type.type === 'task' || type.type === 'bot' ? this.title(r.id, params) : type.describe(params)
    return {
      id: r.id,
      parentId: r.parent_id,
      type: type.type,
      params,
      createdBy: r.created_by,
      createdAt: r.created_at,
      resolved: r.resolved,
      resolvedAt: r.resolved_at,
      resolvedBy: r.resolved_by,
      cancelled: r.cancelled,
      data,
      actor: (type.blocking ? r.resolved_by : null) ?? r.created_by,
      summary,
      message: !r.resolved ? null : r.cancelled ? `Cancelled: ${summary}` : type.said ? type.said(params, data ?? {}) : summary,
      nextCheckAt: r.next_check_at,
      runs: r.runs,
      lastError: r.last_error,
      reactions: r.reactions,
    }
  }

  private row(id: string): EventRow | undefined {
    return this.store.events.get(id)
  }

  event(id: string): ItemEvent | null {
    const r = this.row(id)
    return r ? this.toEvent(r) : null
  }

  /** The events in an item, oldest first; only the open ones (its blockers) with `open`. */
  children(itemId: string, open = false): ItemEvent[] {
    return this.store
      .children(itemId)
      .filter((r) => !open || !r.resolved)
      .map((r) => this.toEvent(r))
  }

  /** A task's title (a bot's: its @name). */
  private title(id: string, params: Record<string, unknown>): string {
    return this.row(id)?.type === 'bot' ? `@${this.agentName(id)}` : String(params.title)
  }

  /** Who a task is assigned to: its `assigned_to` links, the earliest first. */
  assignees(itemId: string): string[] {
    return this.store.assignees(itemId)
  }

  private toItem(r: EventRow, viewer = this.me()): Item {
    const id = r.id
    const kids = this.store.children(id)
    // Neither a script nor a task sits in another as a dependency: a task waits on something by being assigned to it.
    const blockers = kids.filter((k) => !k.resolved && k.type !== 'script' && k.type !== 'task').map((k) => this.toEvent(k))
    const assignees = this.assignees(id)
    const touched = Math.max(
      r.created_at,
      ...kids.map((k) => Math.max(k.created_at, k.resolved_at ?? 0)),
      ...assignees.map((a) => this.store.assignment(id, a)!.at),
    )
    const closed = r.resolved
    const bot = [...this.running.values()].find((run) => run.itemIds.includes(id))?.agentId ?? null
    return {
      id,
      title: this.title(id, r.params),
      parentId: r.parent_id,
      closed,
      how: (r.data?.how as string) ?? null,
      blockers,
      assignees,
      withYou: !closed && assignees.includes(viewer) && blockers.every((b) => this.waitsOnHuman(b)),
      bot,
      since: touched,
      createdAt: r.created_at,
      closedAt: r.resolved_at,
    }
  }

  /** A blocker a person resolves (an approval): the task is still the person's turn. */
  private waitsOnHuman(b: ItemEvent): boolean {
    return Boolean(this.type(b.type).human)
  }

  /** The task with this id, or null; `withYou` for `viewer`. */
  get(id: string, viewer = this.me()): Item | null {
    const r = this.row(id)
    return r?.type === 'task' ? this.toItem(r, viewer) : null
  }

  private must(id: string, viewer = this.me()): Item {
    const item = this.get(id, viewer)
    if (!item) throw new Error(`No task ${id}.`)
    return item
  }

  /** Every open task, at any depth, and those closed in the last day; the bots and the people: as `viewer` sees them. */
  overview(viewer = this.me()): Overview {
    const since = this.now() - 24 * 60 * MINUTE
    const items = this.store
      .ofType('task')
      .filter((r) => !r.resolved || (r.resolved_at ?? 0) > since)
      .map((r) => this.toItem(r, viewer))
      .sort((a, b) => a.since - b.since)
    const scripts = this.store
      .ofType('script')
      .filter((r) => !r.resolved && this.assignedTo(r.id).length)
      .map((r) => ({ id: r.id, name: String(r.params.description ?? 'Script'), wakesAt: r.next_check_at, running: this.checking.has(r.id) }))
    return { me: this.user(viewer) ?? { id: viewer, name: viewer }, items, bots: this.agents(), users: this.users(), recentAssignees: this.recentAssignees(viewer), scripts }
  }

  /** Whoever `viewer` gave the tasks they have now, the most recently first. */
  private recentAssignees(viewer: string): string[] {
    const given = this.store
      .allAssignments()
      .filter((a) => a.given.by === viewer && a.to !== viewer)
      .sort((a, b) => b.given.at - a.given.at || b.given.n - a.given.n)
    return [...new Set(given.map((a) => a.to))]
  }

  /** A task in full; or a bot, its own chat being its events. */
  detail(id: string, viewer = this.me()): ItemDetail | null {
    const r = this.row(id)
    const item = r?.type === 'task' || r?.type === 'bot' ? this.toItem(r, viewer) : null
    if (!item) return null
    const parent = item.parentId ? this.get(item.parentId) : null
    return { item, parent: parent && { id: parent.id, title: parent.title }, events: this.children(id) }
  }

  /** Any item, with what its views need. */
  eventDetail(id: string, viewer = this.me()): EventDetail | null {
    const event = this.event(id)
    if (!event) return null
    const parent = event.parentId ? this.get(event.parentId) : null
    return {
      event,
      parent: parent && { id: parent.id, title: parent.title },
      item: event.type === 'task' || event.type === 'bot' ? this.detail(id, viewer) : null,
    }
  }

  /** An item as egdb holds it: its values and links, and who made it and when. */
  inspect(id: string): Promise<Inspection> {
    return this.store.inspect(id)
  }

  /**
   * What needs `viewer` next, of the open tasks assigned to them that nothing but approvals blocks: the one least
   * recently touched.
   */
  next(skip: string[] = [], viewer = this.me()): string | null {
    const open = this.store
      .assignedTo(viewer)
      .flatMap((id) => this.get(id, viewer) ?? [])
      .filter((i) => i.withYou && !skip.includes(i.id))
    return open.sort((a, b) => a.since - b.since)[0]?.id ?? null
  }

  /** Renames a task: its `title` value changes (egdb keeps the old one, and who changed it). */
  async rename(itemId: string, title: string, actor = this.me()): Promise<void> {
    const t = title.trim()
    if (!t || this.must(itemId).title === t) return
    this.store.setParams(itemId, { ...this.row(itemId)!.params, title: t }, actor, this.now())
    this.emit('change')
  }

  /** Turns `actor`'s reaction (a thumbs-up) to an event on or off. */
  async react(eventId: string, on: boolean, actor = this.me()): Promise<void> {
    const row = this.row(eventId)
    if (!row) throw new Error(`No event ${eventId}.`)
    if (row.reactions.includes(actor) === on) return
    this.store.setReactions(eventId, on ? [...row.reactions, actor] : row.reactions.filter((r) => r !== actor), actor, this.now())
    this.emit('change')
  }

  // ---- people, and who has what ----

  /** Every person, the first made first. */
  users(): User[] {
    return this.store
      .ofType('user')
      .filter((r) => !r.resolved)
      .sort((a, b) => a.created_at - b.created_at)
      .map((r) => this.user(r.id)!)
  }

  /** A person by their username; null if there is none. */
  user(id: string): User | null {
    const r = this.row(id)
    return r?.type === 'user' && !r.resolved ? { id, name: String(r.params.name || id) } : null
  }

  /** Whether an actor is a person. */
  isUser(id: string): boolean {
    return this.user(id) !== null
  }

  /** Who the engine acts as when nobody says: the first person, else NOBODY. The app always says. */
  me(): string {
    return this.store.ofType('user').find((r) => !r.resolved)?.id ?? NOBODY
  }

  /** Adds a person (or names one anew). Their username is their id, and stays. */
  async saveUser(username: string, name = '', actor = this.me()): Promise<User> {
    const id = username.trim().toLowerCase()
    if (this.user(id)) {
      if (name.trim() && name.trim() !== this.user(id)!.name) this.store.setParams(id, { ...this.row(id)!.params, name: name.trim() }, actor, this.now())
      this.emit('change')
      return this.user(id)!
    }
    await this.addEvents(null, [{ type: 'user', params: { username: id, ...(name.trim() ? { name: name.trim() } : {}) } }], this.isUser(actor) ? actor : id)
    return this.user(id)!
  }

  /** Signing in: the person with this username, made if there is nobody by it yet. */
  async signIn(username: string): Promise<User> {
    const id = username.trim().toLowerCase()
    return this.user(id) ?? this.saveUser(id, '', id)
  }

  /** A person or a bot, by a username, a bot's name, or an id; or an error saying who there is. */
  resolveAssignee(ref: string): string {
    const r = ref.trim().replace(/^@/, '')
    if (this.user(r.toLowerCase())) return r.toLowerCase()
    const bot = this.findAgent(r)
    if (bot) return bot.id
    if (this.isOpenScript(r)) return r
    throw new Error(`Nobody is called ${ref}, to assign. People: ${this.users().map((u) => u.id).join(', ') || 'none'}; bots: ${this.agents().map((a) => a.name).join(', ') || 'none'}.`)
  }

  /** Whether an id is a script that is still open: one that can have tasks. */
  isOpenScript(id: string): boolean {
    const r = this.row(id)
    return r?.type === 'script' && !r.resolved
  }

  /** Who a task goes back to when a bot lets go of it: the person who made it, else the owner of the task it sits in. */
  owner(itemId: string): string | null {
    const r = this.row(itemId)
    if (r && this.isUser(r.created_by)) return r.created_by
    if (r?.parent_id && this.row(r.parent_id)?.type === 'task') return this.owner(r.parent_id)
    return null
  }

  /**
   * Assigns a task to someone (an `assigned_to` link from it), or takes it off them. Assigning someone who has it
   * already writes the link again: for a bot, that is something happening to it, so it wakes.
   */
  setAssignee(itemId: string, to: string, on: boolean, actor: string, runId: string | null = null): void {
    if (this.row(itemId)?.type !== 'task') throw new Error(`No task ${itemId}.`)
    const bot = this.agent(to)
    if (on && !this.isUser(to) && !(bot && !bot.deleted) && !this.isOpenScript(to)) throw new Error(`Nobody is ${to}, to assign.`)
    if (!on && !this.assignees(itemId).includes(to)) return
    this.store.setAssigned(itemId, to, on, actor, this.now())
    for (const run of this.running.values()) if (run.id === runId) run.assigned.add(itemId)
    this.emit('change')
  }

  /** Assigns a task to a person or a bot (by username, name or id), or takes it off them. */
  async assign(itemId: string, to: string, on: boolean, actor = this.me(), runId: string | null = null): Promise<void> {
    if (this.must(itemId).closed && on) throw new Error('That task is closed.')
    this.setAssignee(itemId, on ? this.resolveAssignee(to) : (this.assignees(itemId).find((a) => a === to) ?? this.resolveAssignee(to)), on, actor, runId)
    // A script is run from inside a task's queue, so waiting for the queues to empty would wait for itself.
    if (actor !== 'script') await this.idle()
  }

  // ---- bots ----

  /** A bot's settings: its params (egdb keeps every value they had). */
  private agentParams(id: string): Record<string, unknown> {
    const r = this.row(id)
    return r?.type === 'bot' ? r.params : {}
  }

  private agentName(id: string): string {
    return String(this.agentParams(id).name ?? id)
  }

  /** Who did something, for reading: a bot's actor (`bot:<id>`) as its @name, a person by their name; anyone else as they are. */
  actorName(actor: string): string {
    if (actor.startsWith('bot:')) return `@${this.agentName(actor.slice(4))}`
    if (this.row(actor)?.type === 'bot') return `@${this.agentName(actor)}`
    return this.user(actor)?.name ?? actor
  }

  /** A bot's own folder: given in its settings, or one under the app's that lasts. */
  agentFolder(id: string): string {
    const p = this.agentParams(id)
    const dir = p.folder ? String(p.folder) : join(this.dataDir, 'bots', `${slug(String(p.name))}-${id.slice(0, 8)}`)
    mkdirSync(dir, { recursive: true })
    return dir
  }

  /** The open tasks assigned to someone: their `assigned_to` links, followed backwards. */
  assignedTo(assignee: string): string[] {
    return this.store.assignedTo(assignee).filter((id) => this.row(id)?.type === 'task' && !this.row(id)!.resolved)
  }

  agent(id: string): Agent | null {
    const r = this.row(id)
    if (r?.type !== 'bot') return null
    const p = this.agentParams(id)
    const name = String(p.name)
    return {
      id,
      name,
      instructions: String(p.instructions ?? ''),
      provider: String(p.provider ?? 'claude'),
      model: String(p.model || DEFAULT_MODEL),
      debounceMinutes: Number(p.debounce_minutes ?? DEFAULT_DEBOUNCE),
      folder: p.folder ? String(p.folder) : join(this.dataDir, 'bots', `${slug(name)}-${id.slice(0, 8)}`),
      sessionId: (p.session_id as string) ?? null,
      lastRun: (p.last_run as LastRun) ?? null,
      createdAt: r.created_at,
      deleted: r.resolved,
      runId: this.running.get(id)?.id ?? null,
      wakesAt: r.resolved ? null : (this.wake(id)?.at ?? null),
      assigned: this.assignedTo(id),
    }
  }

  /** Every bot that lasts, the oldest first. */
  agents(): Agent[] {
    return this.store
      .ofType('bot')
      .filter((r) => !r.resolved)
      .sort((a, b) => a.created_at - b.created_at)
      .map((r) => this.agent(r.id)!)
  }

  /** A bot by its id or its name (any case, with or without the @), among those that last. */
  findAgent(ref: string): Agent | null {
    const name = ref.replace(/^@/, '').toLowerCase()
    return this.agents().find((a) => a.id === ref || a.name.toLowerCase() === name) ?? null
  }

  /** Makes a bot (no id) or changes its settings (which wakes it). Answers its id. */
  async saveAgent(
    a: { id?: string; name: string; instructions: string; provider: string; model: string; debounceMinutes: number; folder?: string },
    actor = this.me(),
  ): Promise<string> {
    const name = a.name.trim().replace(/^@/, '')
    if (!/^[\w.-]+$/.test(name)) throw new Error('A bot needs a name of letters, digits, dots, dashes or underscores: it is @ed by it.')
    if (this.user(name.toLowerCase())) throw new Error(`${name} is a person's username.`)
    const clash = this.findAgent(name)
    if (clash && clash.id !== a.id) throw new Error(`There is a bot called @${clash.name} already.`)
    const params: Record<string, unknown> = {
      name,
      instructions: a.instructions,
      provider: a.provider || 'claude',
      model: a.model.trim() || DEFAULT_MODEL,
      debounce_minutes: Number.isFinite(Number(a.debounceMinutes)) && String(a.debounceMinutes) !== '' ? Math.max(0, Number(a.debounceMinutes)) : DEFAULT_DEBOUNCE,
      ...(a.folder?.trim() ? { folder: a.folder.trim() } : {}),
    }
    if (!a.id) {
      const [id] = await this.addEvents(null, [{ type: 'bot', params }], actor)
      return id
    }
    const now = this.agentParams(a.id)
    if (!this.agent(a.id) || this.agent(a.id)!.deleted) throw new Error(`No bot ${a.id}.`)
    const changed = Object.fromEntries(Object.entries(params).filter(([k, v]) => String(now[k] ?? '') !== String(v ?? '')))
    if (Object.keys(changed).length) {
      this.store.setParams(a.id, { ...this.row(a.id)!.params, ...changed }, actor, this.now())
      this.configured.set(a.id, { n: this.store.moment(), at: this.now() })
      this.emit('change')
    }
    return a.id
  }

  /** Deletes a bot: it is cancelled, it stops if it is running, and every task it had goes back to whoever owns it. */
  async deleteAgent(id: string, actor = this.me()): Promise<void> {
    const agent = this.agent(id)
    if (!agent || agent.deleted) throw new Error(`No bot ${id}.`)
    this.store.batch(() => {
      for (const itemId of agent.assigned) this.handBack(itemId, id, null, actor)
      this.resolve(id, { cancelled: true }, actor, true)
    })
    if (this.running.has(id)) this.onCancel(id)
    this.emit('change')
    await this.idle()
  }

  /** Wakes a bot now, whatever its debounce says. */
  wakeAgent(id: string): void {
    if (!this.agent(id)) throw new Error(`No bot ${id}.`)
    this.woken.set(id, this.now())
    this.emit('change')
  }

  /**
   * When a bot wakes next, and for what: its own chat (when someone wrote in it, or it is woken by hand), and every
   * open task it has, each with the events new in it since the bot was last prompted (less what the bot itself did,
   * but a child task closing is always news). It wakes only when something happens to it, its debounce after: a task
   * is assigned to it (again, even), its settings change, or someone writes in its chat; or when woken by hand. And
   * every PERIODIC_MINUTES while something has happened in its tasks since its last run. Null when nothing calls for it.
   */
  wake(agentId: string): { at: number; items: Map<string, Set<string>>; fresh: Set<string> } | null {
    const self = `bot:${agentId}`
    const mark = this.marks.get(agentId) ?? { n: 0, at: 0 }
    const lastStart = (this.agentParams(agentId).last_run as LastRun | undefined)?.startedAt ?? this.row(agentId)?.created_at ?? 0
    const woken = this.woken.get(agentId)
    const assigned = this.assignedTo(agentId)
    const seen = this.seen(agentId)
    const items = new Map(assigned.map((id) => [id, new Set<string>()]))
    const fresh = new Set<string>()
    let activity = 0
    let trigger = 0
    for (const itemId of assigned) {
      const given = this.store.assignment(itemId, agentId)!
      if (after(given.n, given.at, mark) && given.by !== self) trigger = Math.max(trigger, given.at)
      // A task its session has not seen yet (one it gave itself, say) is news to it.
      if (!seen.includes(itemId)) {
        fresh.add(itemId)
        activity = Math.max(activity, given.at)
      }
      for (const e of this.store.children(itemId)) {
        const added = after(e.added_n, e.created_at, mark) && e.created_by !== self
        const resolved = e.resolved && after(e.resolved_n, e.resolved_at ?? 0, mark) && (e.resolved_by !== self || e.type === 'task')
        if (!added && !resolved) continue
        items.get(itemId)!.add(e.id)
        fresh.add(itemId)
        activity = Math.max(activity, added ? e.created_at : (e.resolved_at ?? 0))
      }
    }
    // Its own chat: someone wrote in it.
    const chat = new Set(this.store.children(agentId).filter((e) => after(e.added_n, e.created_at, mark) && e.created_by !== self).map((e) => e.id))
    for (const id of chat) trigger = Math.max(trigger, this.row(id)!.created_at)
    if (chat.size || woken !== undefined) {
      items.set(agentId, chat)
      if (chat.size) fresh.add(agentId)
    }
    const configured = this.configured.get(agentId)
    if (configured && after(configured.n, configured.at, mark)) trigger = Math.max(trigger, configured.at)
    if (!items.size) {
      this.woken.delete(agentId)
      return null
    }
    const debounce = Number(this.agentParams(agentId).debounce_minutes ?? DEFAULT_DEBOUNCE) * MINUTE
    const due: number[] = []
    if (woken !== undefined) due.push(woken)
    if (trigger) due.push(trigger + debounce)
    if (activity) due.push(Math.max(activity + debounce, lastStart + PERIODIC_MINUTES * MINUTE))
    return due.length ? { at: Math.min(...due), items, fresh } : null
  }

  /**
   * Starts a bot's run, if it is due and not running: its chat and every task it has, each with its new events (or,
   * for a task its session has not seen, its whole history). Its session is its own: resumed if it has one, otherwise new.
   */
  claim(agentId: string): { agent: Agent; items: { item: Item; events: ItemEvent[]; first: boolean; own?: boolean }[]; resume: boolean } | null {
    const agent = this.agent(agentId)
    if (!agent || agent.deleted || this.running.has(agentId)) return null
    const wake = this.wake(agentId)
    if (!wake || wake.at > this.now()) return null
    const seen = this.seen(agentId)
    const items = [...wake.items].map(([itemId, ids]) => {
      if (itemId === agentId) return { item: this.toItem(this.row(agentId)!), events: this.children(agentId).filter((e) => ids.has(e.id)), first: false, own: true }
      const first = !seen.includes(itemId)
      const events = first ? this.children(itemId) : this.children(itemId).filter((e) => ids.has(e.id))
      return { item: this.must(itemId), events, first }
    })
    const resume = agent.sessionId !== null
    if (!resume) {
      this.setParam(agentId, 'session_id', randomUUID(), `bot:${agentId}`)
      this.setParam(agentId, 'seen', [], `bot:${agentId}`)
    }
    this.woken.delete(agentId)
    return { agent: this.agent(agentId)!, items, resume }
  }

  /** What a bot's session has seen: the tasks in its runs since the session began. */
  private seen(agentId: string): string[] {
    const p = this.agentParams(agentId)
    return p.session_id ? ((p.seen as string[] | undefined) ?? []) : []
  }

  /** A run begins: the bot's `last_run` says so, and what comes after now is new to it. */
  startRun(agentId: string, itemIds: string[]): string {
    const id = randomUUID()
    const at = this.now()
    this.running.set(agentId, { id, agentId, itemIds, startedAt: at, assigned: new Set() })
    this.marks.set(agentId, { n: this.store.moment(), at })
    const lastRun: LastRun = { id, startedAt: at, endedAt: null, items: itemIds, costUsd: null, turns: null, error: null, output: null }
    this.setParam(agentId, 'last_run', lastRun, `bot:${agentId}`)
    const seen = this.seen(agentId)
    const unseen = itemIds.filter((i) => i !== agentId && !seen.includes(i))
    if (unseen.length) this.setParam(agentId, 'seen', [...seen, ...unseen], `bot:${agentId}`)
    this.emit('change')
    return id
  }

  /** A run still going: its bot and its tasks; or null. */
  activeRun(runId: string): { agentId: string; itemIds: string[] } | null {
    const run = [...this.running.values()].find((r) => r.id === runId)
    return run ? { agentId: run.agentId, itemIds: run.itemIds } : null
  }

  /**
   * A run is over. The bot's `last_run` says how it went. Its output's updates are applied, task by task: a note, the
   * events to add, who has it, closing. Then each task it had that it said nothing of who has goes back to its owner
   * (`settle`). If the run failed, each task it has says so, and goes back to its owner.
   */
  async endRun(
    runId: string,
    end: { exitCode: number | null; structured?: unknown; output?: string; costUsd?: number; turns?: number; error?: string },
  ): Promise<void> {
    const run = [...this.running.values()].find((r) => r.id === runId)
    if (!run) return
    const agentId = run.agentId
    const actor = `bot:${agentId}`
    this.running.delete(agentId)
    const last = this.agentParams(agentId).last_run as LastRun | undefined
    this.setParam(
      agentId,
      'last_run',
      { ...last, endedAt: this.now(), costUsd: end.costUsd ?? null, turns: end.turns ?? null, error: end.error ?? null, output: end.output?.slice(0, 4000) ?? null },
      actor,
    )

    let error = end.error ?? null
    let updates: BotUpdate[] = []
    if (!error) {
      try {
        const out = z
          .object({
            updates: z
              .array(
                z.object({
                  item: z.string(),
                  message: z.string().optional(),
                  events: z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })).optional(),
                  assign: z.array(z.string()).optional(),
                  close: z.boolean().optional(),
                }),
              )
              .optional(),
          })
          .parse(end.structured ?? {})
        updates = (out.updates ?? []) as BotUpdate[]
        for (const u of updates) if (!this.get(u.item) && u.item !== agentId) throw new Error(`No task ${u.item}.`)
      } catch (e) {
        error = `The bot's output was not usable: ${(e as Error).message}`
      }
    }
    const closing: string[] = []
    // A bot deleted while it ran: what it returns doesn't land.
    if (this.row(agentId)?.resolved) {
      this.emit('change')
      return
    }
    this.store.batch(() => {
      // A failure is said in each task the bot has, which go back to their owners.
      const failed = (itemId: string, why: string) => {
        if (this.get(itemId)?.closed || !this.assignees(itemId).includes(agentId)) return
        this.add(itemId, [{ type: 'note', params: { text: `@${this.agentName(agentId)} failed: ${why}` } }], 'system')
        this.handBack(itemId, agentId, runId)
      }
      if (error) {
        for (const itemId of run.itemIds) failed(itemId, error)
        return
      }
      const broken = new Set<string>()
      // What a person said in a task while the run went on, the run did not see: a run's update can't hand that task
      // away from the bot. The bot keeps it (beside whoever the update gave it to), and wakes again for it.
      const mark = this.marks.get(agentId) ?? { n: 0, at: 0 }
      const heard = (itemId: string) =>
        run.assigned.has(itemId) || this.store.children(itemId).some((e) => after(e.added_n, e.created_at, mark) && this.isUser(e.created_by))
      let missed = false
      for (const u of updates) {
        if (this.get(u.item)?.closed) continue
        try {
          // Each update is checked on its own: one that is wrong fails only its task.
          const to = u.assign && u.item !== agentId ? new Set(u.assign.map((a) => this.resolveAssignee(a))) : null
          if (to && !to.has(agentId) && !u.close && heard(u.item)) {
            to.add(agentId)
            missed = true
          }
          const events: EventSpec[] = [...(u.message ? [{ type: 'note' as const, params: { text: u.message } }] : []), ...this.prepare(u.events ?? [], 'bot')]
          if (events.length) this.add(u.item, events, actor)
          if (to) {
            const now = this.assignees(u.item)
            for (const a of to) if (!now.includes(a)) this.setAssignee(u.item, a, true, actor, runId)
            for (const a of now) if (!to.has(a)) this.setAssignee(u.item, a, false, actor, runId)
          }
          if (u.close && u.item !== agentId) closing.push(u.item)
        } catch (e) {
          broken.add(u.item)
          failed(u.item, `its update could not be applied: ${(e as Error).message}`)
        }
      }
      // A task it said who has is settled; the rest are settled here.
      const said = new Set([...updates.filter((u) => u.assign).map((u) => u.item), ...broken])
      for (const itemId of run.itemIds) if (!closing.includes(itemId) && !said.has(itemId) && this.get(itemId) && !this.get(itemId)!.closed) this.settle(itemId, run)
      // Woken for what it missed, after its debounce (not the periodic wait that news in a task otherwise has).
      if (missed) this.woken.set(agentId, this.now() + Number(this.agentParams(agentId).debounce_minutes ?? DEFAULT_DEBOUNCE) * MINUTE)
    })
    for (const itemId of closing) await this.close(itemId, { actor })
    this.emit('change')
    await this.idle()
  }

  /**
   * After a run: a task the bot still has goes back to its owner, unless the run changed who has it, or it is waiting
   * on something the bot added that is not a person (it wakes again for that), or something new came in while it ran.
   */
  private settle(itemId: string, run: Run): void {
    const agentId = run.agentId
    if (!this.assignees(itemId).includes(agentId) || run.assigned.has(itemId)) return
    const { blockers } = this.must(itemId)
    if (blockers.some((b) => b.createdBy === `bot:${agentId}` && !this.waitsOnHuman(b))) return
    if (this.wake(agentId)?.fresh.has(itemId)) return
    this.handBack(itemId, agentId, run.id)
  }

  /** Takes a task off a bot; if that leaves it with nobody, it goes to its owner, if it has one. */
  private handBack(itemId: string, agentId: string, runId: string | null, actor = `bot:${agentId}`): void {
    this.setAssignee(itemId, agentId, false, actor, runId)
    const owner = this.owner(itemId)
    if (!this.assignees(itemId).length && owner && this.isUser(owner)) this.setAssignee(itemId, owner, true, actor, runId)
  }

  // ---- actions ----

  /** The one way into an action, from MCP, a script or the UI: checked, then run. */
  async callAction(name: string, args: Record<string, unknown>, ctx: ActionContext): Promise<unknown> {
    const action = this.actions.find((a) => a.name === name)
    if (!action) throw new Error(`No action ${name}.`)
    // The rule on unsafe actions (see Action in integrations.ts), checked here
    // too, whatever the caller was shown.
    if (action.unsafe && ctx.author !== 'human')
      throw new Error(`${name} is unsafe, so bots, and scripts bots wrote, may not run it. Ask a person: add an approval event for it.`)
    const parsed = z.object(action.input).safeParse(args ?? {})
    if (!parsed.success) throw new Error(`Bad arguments for ${name}: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
    return action.run(parsed.data, ctx)
  }

  // ---- events ----

  /** Checks events and expands them as their types say, without touching the store. */
  prepare(specs: EventSpec[], author: Author): EventSpec[] {
    return specs.flatMap((s) => {
      const type = this.type(s?.type)
      if (author === 'bot' && !type.bots) throw new Error(`A bot may not add a ${s.type} event.`)
      const parsed = z.object(type.params).safeParse(s.params ?? {})
      if (!parsed.success)
        throw new Error(`Bad params for a ${s.type} event: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
      const params = parsed.data as Record<string, unknown>
      if (s.type === 'approval' && !this.actions.find((a) => a.name === params.action)?.unsafe)
        throw new Error(`${params.action} is not an unsafe action, so it needs no approval.`)
      // Who a new task goes to, as ids.
      if (s.type === 'task' && params.assignees) params.assignees = (params.assignees as string[]).map((a) => this.resolveAssignee(a))
      if (s.type === 'user' && this.row(String(params.username))) throw new Error(`The username ${params.username} is taken.`)
      // Prepared now, so a bad one fails the whole list; added in the task as it is.
      if (s.type === 'task' && params.events) params.events = this.prepare(params.events as EventSpec[], author)
      const expanded = type.expand ? type.expand(params, author) : [{ type: s.type, params }]
      return expanded.map((e, i) => ({ type: e.type, params: e.params, ref: i === 0 ? (s.ref ?? e.ref) : e.ref }))
    })
  }

  /**
   * Adds events, already prepared, in a task or a bot's chat (or, for tasks, bots and people, in none). Plain ones
   * resolve at once; blockers wait. A task's first events (`events`) are added in it, and its `assignees` assigned;
   * neither is kept in its values. A task whose key is already taken is left out. Answers the ids added.
   */
  add(parentId: string | null, specs: EventSpec[], createdBy: string): string[] {
    return this.store.batch(() => this.addNow(parentId, specs, createdBy))
  }

  private addNow(parentId: string | null, specs: EventSpec[], createdBy: string): string[] {
    // A detached task sits in no task, wherever it is added.
    const detached = parentId ? specs.filter((s) => s.type === 'task' && s.params.detached) : []
    if (detached.length) return [...this.addNow(parentId, specs.filter((s) => !detached.includes(s)), createdBy), ...this.addNow(null, detached, createdBy)]
    const keyed = (s: EventSpec) => s.type === 'task' && s.params.key && this.store.ofType('task').some((r) => r.params.key === s.params.key)
    specs = specs.filter((s) => !keyed(s))
    if (!parentId && specs.some((s) => !['task', 'bot', 'user'].includes(s.type))) throw new Error('Only a task, a bot or a person can sit in no task.')
    if (parentId && this.row(parentId)?.type === 'bot' && specs.some((s) => s.type !== 'note')) throw new Error("A bot's own chat holds only notes.")
    const ids: string[] = []
    const seq = this.store.nextSeq(parentId)
    const at = this.now()
    specs.forEach((spec, i) => {
      const type = this.type(spec.type)
      // A person's id is their username.
      const id = spec.type === 'user' ? String(spec.params.username) : randomUUID()
      ids.push(id)
      const { assignees, events, detached: _d, owns, ...params } = spec.params
      // A script's first wake is kept as a time, whatever form it was given in.
      if (spec.type === 'script' && params.wake_at !== undefined) params.wake_at = whenNext(params.wake_at as number | string, at)
      const first = spec.type === 'script' ? ((params.wake_at as number | undefined) ?? null) : null
      this.store.insertEvent({
        id, parent_id: parentId, seq: seq + i, type: spec.type, params, created_by: createdBy, created_at: at,
        resolved: !type.blocking, resolved_at: type.blocking ? null : at, resolved_by: type.blocking ? null : createdBy, cancelled: false,
        data: null, memo: null, reactions: [], check_step: 0, next_check_at: first ?? (type.check && this.scheduleOf(type, params) ? at + this.scheduleOf(type, params)![0] * 1000 : null), last_error: null,
        runs: 0, added_n: 0, resolved_n: 0,
      })
      if (spec.type === 'task') {
        if (Array.isArray(events) && events.length) this.addNow(id, events as EventSpec[], createdBy)
        // A new task goes to those given; else, made by a person, to them; else (a bot made it) to nobody.
        const to = Array.isArray(assignees) && assignees.length ? (assignees as string[]) : this.isUser(createdBy) ? [createdBy] : []
        for (const a of to) this.setAssignee(id, a, true, createdBy)
      }
      // A script takes on the tasks it owns, as a bot is given them.
      if (spec.type === 'script' && Array.isArray(owns)) for (const t of owns as string[]) this.setAssignee(t, id, true, createdBy)
    })
    // New waits are checked at once.
    if (specs.some((s) => this.type(s.type).check)) queueMicrotask(() => void this.checkDue())
    this.emit('change')
    return ids
  }

  /** Resolves a blocker, as `by`. */
  private resolve(eventId: string, data: Record<string, unknown>, by: string, cancelled = false): void {
    const r = this.row(eventId)
    if (!r || r.resolved) return
    this.store.resolveEvent(eventId, data, by, this.now(), cancelled)
    // A process stops as its event ends (stopped, or its task closed).
    if (r.type === 'process') this.processes.stop(eventId)
    // A script that ends lets go of the tasks it still has: each goes back to its owner, if nobody else has it.
    if (r.type === 'script') for (const t of this.assignedTo(eventId)) if (!this.get(t)?.closed) this.handBack(t, eventId, null, 'script')
  }

  /**
   * An event's check schedule, in seconds: its type's, never under a second apart after the first, or the default.
   * Null when its type has schedules and it has none: then it runs only when someone runs it (`runNow`).
   */
  private scheduleOf(type: EventType, params: Record<string, unknown>): number[] | null {
    const s = type.schedule ? type.schedule(params) : DEFAULT_SCHEDULE
    if (!s?.length) return null
    return s.map((d, i) => (i === 0 ? Math.max(0, d) : Math.max(1, d)))
  }

  /** Runs an open check (a script with no schedule, say) now, once. */
  async runNow(eventId: string, actor = this.me()): Promise<void> {
    const r = this.row(eventId)
    if (!r || r.resolved || !this.type(r.type).check) throw new Error('There is nothing to run.')
    r.next_check_at = this.now()
    this.pressed.set(eventId, actor)
    await this.checkOne(r.parent_id!, eventId)
  }

  /** Writes one param of an item: for the app's own bookkeeping, such as a bot's session and last run. */
  private setParam(eventId: string, key: string, value: unknown, by: string): void {
    this.store.setParams(eventId, { ...this.row(eventId)!.params, [key]: value }, by, this.now())
  }

  // ---- people ----

  /** A new task, in `parentId` or in none, with events to start it off. */
  async create(
    title: string,
    opts: { parentId?: string | null; key?: string; events?: EventSpec[]; assignees?: string[]; actor?: string; author?: Author } = {},
  ): Promise<string | null> {
    const author = opts.author ?? 'human'
    const params = { title, ...(opts.key ? { key: opts.key } : {}), ...(opts.events ? { events: opts.events } : {}), ...(opts.assignees ? { assignees: opts.assignees } : {}) }
    const [spec] = this.prepare([{ type: 'task', params }], author)
    const [id] = this.add(opts.parentId ?? null, [spec], author === 'bot' ? 'bot' : (opts.actor ?? this.me()))
    await this.idle()
    return id ?? null
  }

  /**
   * The chat input: `text` through a preset, as `actor`. Whoever (a bot or a person) is `[id]()`-referred to in the
   * text is assigned the task, as is whoever an `assign:<id>` preset names; then the sender is taken off it. Assigning
   * a bot that has the task already assigns it again, which wakes it.
   */
  async send(itemId: string, presetId: string, text: string, actor = this.me()): Promise<void> {
    const assignTo = presetId.startsWith('assign:') ? presetId.slice('assign:'.length) : null
    const preset = this.presets.find((p) => p.id === (assignTo ? 'note' : presetId))
    if (!preset) throw new Error(`No preset ${presetId}.`)
    // A bot's own chat takes notes, and nothing is assigned there.
    if (this.row(itemId)?.type === 'bot') {
      const events = this.prepare(preset.build(text), 'human')
      if (!events.length) throw new Error('Nothing to send.')
      this.add(itemId, events, actor)
      return this.idle()
    }
    if (this.must(itemId).closed) throw new Error('This task is closed.')
    const live = (id: string) => this.isUser(id) || (this.agent(id) !== null && !this.agent(id)!.deleted)
    const to = [...new Set([...(assignTo ? [this.resolveAssignee(assignTo)] : []), ...refsIn(text).filter(live)])].filter((id) => id !== actor)
    const events = this.prepare(preset.build(text), 'human')
    if (!events.length && !to.length) throw new Error('Nothing to send.')
    this.store.batch(() => {
      if (events.length) this.add(itemId, events, actor)
      for (const a of to) this.setAssignee(itemId, a, true, actor)
      // Handing it on takes it off whoever sent it.
      if (to.length) this.setAssignee(itemId, actor, false, actor)
    })
    await this.idle()
  }

  /** Adds events in a task (or tasks in none): from a person, or from an agent through MCP (author 'bot', so its rules apply). */
  async addEvents(parentId: string | null, specs: EventSpec[], actor = this.me(), author: Author = 'human'): Promise<string[]> {
    if (parentId && this.row(parentId)?.type !== 'bot' && this.must(parentId).closed) throw new Error('This task is closed.')
    const ids = this.add(parentId, this.prepare(specs, author), actor)
    // A script is run from inside a task's queue, so waiting for the queues to empty would wait for itself.
    if (actor !== 'script') await this.idle()
    return ids
  }

  /** Runs an approval's action with a person's inputs, which resolves it. */
  async approve(eventId: string, inputs: Record<string, unknown>, actor = this.me()): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.type !== 'approval' || event.resolved) throw new Error('No approval waiting with that id.')
    const proposed = event.params.inputs as Record<string, unknown>
    const changed = Object.keys({ ...proposed, ...inputs }).filter((k) => String(inputs[k] ?? '') !== String(proposed[k] ?? ''))
    const name = String(event.params.action)
    const output = await this.callAction(name, inputs, { engine: this, author: 'human', actor, itemId: event.parentId, runId: null, approving: true })
    this.resolve(eventId, { ran: true, inputs, changed, output }, actor)
    this.emit('change')
    await this.idle()
  }

  /** Ends a blocker without it happening; a task is closed as cancelled, and a bot deleted. */
  async cancel(eventId: string, actor = this.me()): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.resolved) throw new Error('That event is not waiting.')
    if (event.type === 'task') return this.close(eventId, { actor, how: 'cancelled' })
    if (event.type === 'bot') return this.deleteAgent(eventId, actor)
    this.resolve(eventId, { cancelled: true }, actor, true)
    this.emit('change')
    if (actor !== 'script') await this.idle()
  }

  /**
   * Opens a closed task again (unresolving it), as it was before it closed: who has it is unchanged, and what its
   * closing cancelled stays cancelled. The chat says who reopened it.
   */
  async reopen(itemId: string, actor = this.me()): Promise<void> {
    const item = this.must(itemId)
    if (!item.closed) return
    this.store.batch(() => {
      this.store.reopenEvent(itemId, actor, this.now())
      this.add(itemId, [{ type: 'note', params: { text: `Reopened by ${this.actorName(actor)}.` } }], 'system')
    })
    this.emit('change')
    if (actor !== 'script') await this.idle()
  }

  /** Closes a task: its open blockers are cancelled, then it resolves. Tasks in it are left as they are: they don't depend on it. */
  async close(itemId: string, opts: { actor?: string; how?: 'closed' | 'cancelled' } = {}): Promise<void> {
    const actor = opts.actor ?? this.me()
    const shut = (id: string, how: string) => {
      for (const b of this.children(id, true)) if (b.type !== 'task') this.resolve(b.id, { cancelled: true, detail: 'Its task closed.' }, actor, true)
      this.resolve(id, { how }, actor, false)
    }
    const item = this.must(itemId)
    if (item.closed) return
    this.store.batch(() => shut(itemId, opts.how ?? 'closed'))
    this.emit('change')
    if (actor !== 'script') await this.idle() // a script waiting on its own queue would wait for itself
  }

  // ---- the loop ----

  /** One turn of the central loop: every blocker check that is due. */
  async tick(): Promise<void> {
    await this.checkDue()
  }

  async checkDue(): Promise<void> {
    const now = this.now()
    const due = [...this.store.events.values()]
      .filter((r) => !r.resolved && r.next_check_at !== null && r.next_check_at <= now && !this.checking.has(r.id))
      .sort((a, b) => a.next_check_at! - b.next_check_at!)
    // At most `scripts.max_running` scripts at once (MAX_SCRIPTS by default): those due longest go first; the rest stay
    // due, for the next turn of the loop.
    const max = Number(this.settings.get('scripts.max_running', MAX_SCRIPTS))
    let room = max - [...this.checking].filter((id) => this.row(id)?.type === 'script').length
    const allowed = due.filter((r) => r.type !== 'script' || room-- > 0)
    await Promise.all(allowed.map((r) => this.checkOne(r.parent_id!, r.id)))
  }

  private checkOne(itemId: string, eventId: string): Promise<void> {
    this.checking.add(eventId)
    return this.serial(itemId, async () => {
      const row = this.row(eventId)
      if (!row || row.resolved || (row.next_check_at ?? 0) > this.now()) return
      const event = this.toEvent(row)
      const type = this.type(event.type)
      let data: Record<string, unknown> | null = null
      let error: string | null = null
      const pressedBy = this.pressed.get(eventId) ?? null
      this.pressed.delete(eventId)
      // Undefined: as its type's schedule says. A number or null: what the check asked for.
      let next: number | null | undefined
      try {
        data = await type.check!(event.params, {
          trigger: pressedBy ? 'press' : 'schedule',
          pressedBy,
          setNext: (at) => (next = at),
          engine: this,
          item: this.must(itemId),
          event,
          author: authorOf(event.createdBy),
          memo: row.memo ?? null,
          runs: row.runs,
          setMemo: (value) => this.store.setState(eventId, value, this.now()),
        })
      } catch (e) {
        error = (e as Error).message
      }
      row.runs++
      const timeout = Number(event.params.timeout_minutes ?? 0)
      if (!data && timeout && this.now() >= event.createdAt + timeout * MINUTE)
        data = { outcome: 'timed_out', detail: `Nothing after ${timeout} minutes.` }
      if (data) {
        this.resolve(eventId, data, 'system')
        this.emit('change')
        return
      }
      if (next !== undefined) {
        Object.assign(row, { next_check_at: next, last_error: error })
        // A script's wake is kept, so a restart wakes it when it asked.
        if (row.type === 'script' && row.params.wake_at !== (next ?? undefined)) {
          const { wake_at: _w, ...rest } = row.params
          this.store.setParams(eventId, next === null ? rest : { ...rest, wake_at: next }, 'script', this.now())
        }
        this.emit('change')
        return
      }
      const schedule = this.scheduleOf(type, event.params)
      if (!schedule) Object.assign(row, { next_check_at: null, last_error: error })
      else {
        const step = Math.min(row.check_step + 1, schedule.length - 1)
        Object.assign(row, { check_step: step, next_check_at: this.now() + schedule[step] * 1000, last_error: error })
      }
      this.emit('change')
    }).finally(() => this.checking.delete(eventId))
  }
}
