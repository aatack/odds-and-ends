import { CustomActions } from './customActions'
import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { mkdirSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { z } from 'zod'
import type {
  Agent, BotRun, EventDetail, EventSpec, EventTimes, Item, ItemDetail, ItemEvent, Overview, TraceEntry, User,
} from '../shared/types'
import { itemActions } from './actions'
import { EVENT_TYPES } from './eventTypes'
import { execCli, type Action, type ActionContext, type Author, type EventType, type Exec } from './integrations'
import { PRESETS, type Preset } from './presets'
import { Settings } from './settings'
import { Store, type EventRow, type RunRow, type TraceRow } from './store'

/** How long a blocker sleeps between checks, by default: at once, then a minute, then longer, up to the last. */
export const BACKOFF_MINUTES = [1, 2, 5, 10]
const MINUTE = 60_000
const DEFAULT_SCHEDULE = [0, ...BACKOFF_MINUTES.map((m) => m * 60)]
/** A bot's model, unless it says another. */
export const DEFAULT_MODEL = 'opus'
/** How many minutes a bot waits after the last thing that happened to it before it wakes, unless it says otherwise. */
export const DEFAULT_DEBOUNCE = 1
/** A bot that has items wakes this often, when something has happened in them since its last run. */
export const PERIODIC_MINUTES = 15
/** Who the engine acts as when nobody is said and there is nobody yet (the tests, mostly). */
export const NOBODY = 'human'

/** What a bot says about one item, in its run's output. */
export interface BotUpdate {
  /** The item's id. */
  item: string
  /** To the people in the item: what it did, or one direct question. */
  message?: string
  /** Events to add in the item: waits, approvals, afters, child items. */
  events?: EventSpec[]
  /** Who has the item from now on: people by username, bots by name or id. Left out: see Engine.settle. */
  assign?: string[]
  close?: boolean
}

/** What a bot's run returns, as its structured output. */
export interface BotOutput {
  updates?: BotUpdate[]
}

const NO_TIMES: EventTimes = {
  added: null, firstChecked: null, lastChecked: null, checks: 0, firstRunStarted: null, firstRunEnded: null, runs: 0, resolved: null,
}

/** What happens in an item: something added, resolved or cancelled in it. */
const WAKING = new Set(['event_added', 'event_resolved', 'event_cancelled'])
/** Events that are no news for a bot: a summary is for people. */
const QUIET = new Set<string>()

const toTrace = (r: TraceRow): TraceEntry => ({
  id: r.id,
  at: r.at,
  itemId: r.item_id,
  eventId: r.event_id,
  runId: r.run_id,
  actor: r.actor,
  kind: r.kind,
  data: r.data,
})

const toRun = (r: RunRow): BotRun => ({
  id: r.id,
  agentId: r.agent_id,
  itemIds: r.item_ids,
  eventIds: r.event_ids,
  startedAt: r.started_at,
  endedAt: r.ended_at,
  costUsd: r.cost_usd,
  turns: r.turns,
  result: r.result,
})

/** A human added it, or the system or a source did; a bot, or a bot's script, is a bot. */
const authorOf = (createdBy: string): Author => (createdBy === 'bot' || createdBy === 'script:bot' || createdBy.startsWith('bot:') ? 'bot' : 'human')

/** The params that name another event, by id or by a ref given alongside. */
const REF_KEYS = new Set(['event', 'thread_of'])

/** Replaces refs with the ids they stand for, in the params that name events, at any depth (an after's `then` too). */
function resolveRefs(value: unknown, ids: Map<string, string>, key?: string): unknown {
  if (typeof value === 'string') return key && REF_KEYS.has(key) ? (ids.get(value) ?? value) : value
  if (Array.isArray(value)) return value.map((v) => resolveRefs(v, ids))
  if (value && typeof value === 'object')
    return Object.fromEntries(Object.entries(value).map(([k, v]) => [k, resolveRefs(v, ids, k)]))
  return value
}

/** `[id]()` in text: a reference to an event (an item, a bot, or a person by their username). */
export const refsIn = (text: string): string[] => [...text.matchAll(/\[([\w.-]+)\]\(\)/g)].map((m) => m[1])

const slug = (name: string) => name.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'bot'

export interface EngineOptions {
  store?: Store
  exec?: Exec
  fetch?: typeof fetch
  now?: () => number
  /** Where bots get their own folders (`<dataDir>/bots/…`); a folder under the temp dir if left out. */
  dataDir?: string
}

/**
 * The whole system, short of the processes: one tree of events (items and bots
 * among them), the trace, and the loop that checks the blockers. Every change
 * goes through here, and emits `change`.
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
  /** The human's own actions, per event type (the Types page). */
  readonly customActions: CustomActions
  /** Stops a bot's run. Set by the runner. */
  onCancel: (agentId: string) => void = () => {}
  /** The run each bot has going, by the bot's id. In memory only: a crash leaves nothing behind. */
  private running = new Map<string, string>()
  /** When each bot was woken by hand: it runs then, whatever its debounce says. */
  private woken = new Map<string, number>()
  /** The events being checked right now: a check never starts while the last is still going. */
  private checking = new Set<string>()
  private queues = new Map<string, Promise<unknown>>()

  constructor(opts: EngineOptions = {}) {
    super()
    this.store = opts.store ?? new Store()
    this.settings = new Settings(this.store)
    this.exec = opts.exec ?? execCli
    this.fetch = opts.fetch ?? fetch
    this.now = opts.now ?? Date.now
    this.dataDir = opts.dataDir ?? join(tmpdir(), 'workflows')
    this.customActions = new CustomActions(this, this.store)
    // A run the app was closed during is over; what it was on goes back to the human, saying so, since the bot
    // counts what woke it as seen.
    for (const r of [...this.store.runs.values()].filter((r) => r.ended_at === null)) {
      this.store.updateRun(r.id, { ended_at: this.now(), result: 'The app stopped during the run.' })
      if (!r.agent_id) continue
      for (const itemId of r.item_ids)
        if (this.get(itemId) && !this.get(itemId)!.closed && this.assignees(itemId).includes(r.agent_id)) {
          this.add(itemId, [{ type: 'note', params: { text: `@${this.agentName(r.agent_id)} was stopped (the app stopped) before it finished.` } }], 'system', r.id)
          this.handBack(itemId, r.agent_id, r.id)
        }
    }
    // Waits are checked at once after a start.
    for (const r of this.store.events.values()) if (!r.resolved && this.typeOrNull(r.type)?.check) r.next_check_at = this.now()
  }

  type(name: string): EventType {
    const t = this.typeOrNull(name)
    if (!t) throw new Error(`No event type ${name}: one of ${this.eventTypes.map((t) => t.type).join(', ')}.`)
    return t
  }

  private typeOrNull(name: string): EventType | null {
    return this.eventTypes.find((t) => t.type === name) ?? null
  }

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
    // An item reads by its latest title: its rename events', else the one it was made with. A bot reads by its name.
    const summary = type.type === 'item' ? this.title(r.id, params) : type.type === 'bot' ? `@${this.agentName(r.id)}` : type.describe(params)
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
      lastError: r.last_error,
      times: this.times(r.id),
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

  /** An event's timeline, read from the trace. */
  private times(id: string): EventTimes {
    const t = { ...NO_TIMES }
    for (const r of this.store.traceOfEvent(id)) {
      if (r.kind === 'event_added') t.added ??= r.at
      if (r.kind === 'check') {
        t.firstChecked ??= r.at
        t.lastChecked = r.at
        t.checks++
      }
      if (r.kind === 'run_started') {
        t.firstRunStarted ??= r.at
        t.runs++
      }
      if (r.kind === 'run_ended') t.firstRunEnded ??= r.at
      if (r.kind === 'event_resolved' || r.kind === 'event_cancelled') t.resolved ??= r.at
    }
    return t
  }

  /** An item's title (a bot's: its @name). */
  private title(id: string, params: Record<string, unknown>): string {
    return this.row(id)?.type === 'bot' ? `@${this.agentName(id)}` : String(params.title)
  }

  /** Who an item is assigned to: its `assigned_to` links, the earliest first. */
  assignees(itemId: string): string[] {
    return this.store.assignees(itemId)
  }

  private toItem(r: EventRow, viewer = this.me()): Item {
    const id = r.id
    const kids = this.store.children(id)
    const blockers = kids.filter((k) => !k.resolved).map((k) => this.toEvent(k))
    const last = kids.reduce((m, k) => Math.max(m, k.created_at, k.resolved_at ?? 0), r.created_at)
    const closed = r.resolved
    const assignees = this.assignees(id)
    const bot = [...this.running].find(([, runId]) => this.store.runs.get(runId)?.item_ids.includes(id))?.[0] ?? null
    return {
      id,
      title: this.title(id, r.params),
      parentId: r.parent_id,
      closed,
      how: (r.data?.how as string) ?? null,
      blockers,
      assignees,
      withYou: !closed && assignees.includes(viewer) && blockers.every((b) => this.waitsOnHuman(b, blockers)),
      bot,
      since: Math.max(last, this.lastAssigned(id)),
      createdAt: r.created_at,
      closedAt: r.resolved_at,
    }
  }

  /** When an item was last assigned to anyone, or taken off them: it counts as a touch. */
  private lastAssigned(itemId: string): number {
    return this.store.traceOfItem(itemId).findLast((t) => t.kind === 'assigned' || t.kind === 'unassigned')?.at ?? 0
  }

  /** A blocker a person resolves (an approval): the item is still the person's turn. */
  private waitsOnHuman(b: ItemEvent, _blockers?: ItemEvent[]): boolean {
    return Boolean(this.type(b.type).human)
  }

  /** The item event with this id, or null; `withYou` for `viewer`. */
  get(id: string, viewer = this.me()): Item | null {
    const r = this.row(id)
    return r?.type === 'item' ? this.toItem(r, viewer) : null
  }

  private must(id: string, viewer = this.me()): Item {
    const item = this.get(id, viewer)
    if (!item) throw new Error(`No item ${id}.`)
    return item
  }

  /** Every open item, at any depth, and those closed in the last day; the bots and the people: as `viewer` sees them. */
  overview(viewer = this.me()): Overview {
    const since = this.now() - 24 * 60 * MINUTE
    const items = this.store
      .ofType('item')
      .filter((r) => !r.resolved || (r.resolved_at ?? 0) > since)
      .map((r) => this.toItem(r, viewer))
      .sort((a, b) => a.since - b.since)
    return { me: this.user(viewer) ?? { id: viewer, name: viewer }, items, bots: this.agents(), users: this.users(), ...this.inHand(viewer) }
  }

  /**
   * The items `viewer` has had on screen in the last week, and whoever they assigned things to: the most recent
   * first, each once.
   */
  inHand(viewer = this.me()): Pick<Overview, 'viewed' | 'recentAssignees'> {
    const since = this.now() - 7 * 24 * 60 * MINUTE
    const viewed: string[] = []
    const recentAssignees: string[] = []
    for (let i = this.store.trace.length - 1; i >= 0; i--) {
      const t = this.store.trace[i]
      if (t.at <= since) break
      if (t.actor !== viewer) continue
      if (t.kind === 'viewed' && viewed.length < 50 && !viewed.includes(t.item_id)) viewed.push(t.item_id)
      const to = String(t.data?.to ?? '')
      if (t.kind === 'assigned' && to && to !== viewer && !recentAssignees.includes(to) && this.row(t.item_id)?.type === 'item') recentAssignees.push(to)
    }
    return { viewed, recentAssignees }
  }

  private runsOf(pred: (r: RunRow) => boolean): BotRun[] {
    return [...this.store.runs.values()].filter(pred).sort((a, b) => a.started_at - b.started_at).map(toRun)
  }

  /** An item in full; or a bot, its own chat being its events. */
  detail(id: string, viewer = this.me()): ItemDetail | null {
    const r = this.row(id)
    const item = r?.type === 'item' || r?.type === 'bot' ? this.toItem(r, viewer) : null
    if (!item) return null
    const parent = item.parentId ? this.get(item.parentId) : null
    return {
      item,
      parent: parent && { id: parent.id, title: parent.title },
      events: this.children(id),
      runs: this.runsOf((r) => r.item_ids.includes(id) || r.agent_id === id),
      trace: this.trace(id),
    }
  }

  /** Any event, item or not, with what its views need. */
  eventDetail(id: string, viewer = this.me()): EventDetail | null {
    const event = this.event(id)
    if (!event) return null
    const parent = event.parentId ? this.get(event.parentId) : null
    return {
      event,
      parent: parent && { id: parent.id, title: parent.title },
      runs: this.runsOf((r) => r.item_ids.includes(id) || r.event_ids.includes(id) || r.agent_id === id),
      trace: this.store.traceOfEvent(id).map(toTrace),
      item: event.type === 'item' || event.type === 'bot' ? this.detail(id, viewer) : null,
    }
  }

  /**
   * What needs `viewer` next, of the open items assigned to them that nothing but approvals blocks: the one least
   * recently touched.
   */
  next(skip: string[] = [], viewer = this.me()): string | null {
    const open = this.store
      .assignedTo(viewer)
      .flatMap((id) => this.get(id, viewer) ?? [])
      .filter((i) => i.withYou && !skip.includes(i.id))
    const first = open.sort((a, b) => a.since - b.since)[0]
    return first?.id ?? null
  }

  /** Renames an item: its title param changes (egdb keeps the old value), and the trace says who did it. */
  async rename(itemId: string, title: string, actor = this.me()): Promise<void> {
    const t = title.trim()
    if (!t || this.must(itemId).title === t) return
    this.store.updateEvent(itemId, { params: { ...this.row(itemId)!.params, title: t } })
    this.mark(itemId, 'renamed', { eventId: itemId, actor, data: { title: t } })
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
      if (name.trim() && name.trim() !== this.user(id)!.name) this.store.updateEvent(id, { params: { ...this.row(id)!.params, name: name.trim() } })
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
    throw new Error(`Nobody is called ${ref}, to assign. People: ${this.users().map((u) => u.id).join(', ') || 'none'}; bots: ${this.agents().map((a) => a.name).join(', ') || 'none'}.`)
  }

  /**
   * Who an item goes back to when a bot lets go of it: the person who last had it, else the person who made it, else
   * whoever has (or had) the item it sits in, else the default person.
   */
  owner(itemId: string): string {
    const lastPerson = this.store
      .traceOfItem(itemId)
      .findLast((t) => t.kind === 'assigned' && this.isUser(String(t.data?.to)))
    if (lastPerson) return String(lastPerson.data!.to)
    const r = this.row(itemId)
    if (r && this.isUser(r.created_by)) return r.created_by
    if (r?.parent_id && this.row(r.parent_id)?.type === 'item') return this.owner(r.parent_id)
    return this.me()
  }

  /**
   * Assigns an item to someone (an `assigned_to` link from it), or takes it off them. Assigning someone who has it
   * already writes the link again: for a bot, that is something happening to it, so it wakes. Traced in the item and
   * in the assignee.
   */
  setAssignee(itemId: string, to: string, on: boolean, actor: string, runId: string | null = null): void {
    if (this.row(itemId)?.type !== 'item') throw new Error(`No item ${itemId}.`)
    const bot = this.agent(to)
    if (on && !this.isUser(to) && !(bot && !bot.deleted)) throw new Error(`Nobody is ${to}, to assign.`)
    if (!on && !this.assignees(itemId).includes(to)) return
    this.store.batch(() => {
      this.store.setAssigned(itemId, to, on, this.now())
      const kind = on ? 'assigned' : 'unassigned'
      this.mark(itemId, kind, { actor, runId, data: { to } })
      this.mark(to, kind, { actor, runId, data: { item: itemId } })
    })
    this.emit('change')
  }

  /** Assigns an item to a person or a bot (by username, name or id), or takes it off them. */
  async assign(itemId: string, to: string, on: boolean, actor = this.me(), runId: string | null = null): Promise<void> {
    if (this.must(itemId).closed && on) throw new Error('That task is closed.')
    this.setAssignee(itemId, on ? this.resolveAssignee(to) : (this.assignees(itemId).find((a) => a === to) ?? this.resolveAssignee(to)), on, actor, runId)
    // A script is run from inside an item's queue, so waiting for the queues to empty would wait for itself.
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

  /** The open items assigned to someone: their `assigned_to` links, followed backwards. */
  assignedTo(assignee: string): string[] {
    return this.store.assignedTo(assignee).filter((id) => this.row(id)?.type === 'item' && !this.row(id)!.resolved)
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
      sessionId: (r.params.sessionId as string) ?? null,
      createdAt: r.created_at,
      deleted: r.resolved,
      runId: this.running.get(id) ?? null,
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

  /** Makes a bot (no id) or changes its settings (which wakes it; the trace says what changed). Answers its id. */
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
      this.store.updateEvent(a.id, { params: { ...this.row(a.id)!.params, ...changed } })
      this.mark(a.id, 'configured', { eventId: a.id, actor, data: { changed: Object.keys(changed) } })
      this.emit('change')
    }
    return a.id
  }

  /** Deletes a bot: it is cancelled, it stops if it is running, and every item it had goes back to whoever owns it. */
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

  /** The runs a bot has had, the newest first (by where they marked the trace: two may start in the same millisecond). */
  private agentRuns(id: string): RunRow[] {
    return [...this.store.runs.values()].filter((r) => r.agent_id === id).sort((a, b) => b.trace_mark - a.trace_mark || b.started_at - a.started_at)
  }

  /**
   * When a bot wakes next, and for what: every open item it has, each with the events new in it since the bot was
   * last prompted (less what its own runs did, but a child item closing is always news). It wakes only when
   * something happens to it, its debounce after: an item is assigned to it (again, even), or its settings change;
   * or when woken by hand. And every PERIODIC_MINUTES while something has happened in its items since its last run.
   * Null while it has no items, or nothing calls for it.
   */
  wake(agentId: string): { at: number; items: Map<string, Set<string>>; fresh: Set<string> } | null {
    const runs = this.agentRuns(agentId)
    const own = new Set(runs.map((r) => r.id))
    const madeAt = this.row(agentId)?.created_at ?? 0
    const since = runs[0]?.trace_mark ?? this.store.traceOfEvent(agentId)[0]?.id ?? 0
    const assigned = this.assignedTo(agentId)
    const woken = this.woken.get(agentId)
    const items = new Map(assigned.map((id) => [id, new Set<string>()]))
    const fresh = new Set<string>()
    let activity = 0
    for (const t of this.store.traceAfter(since)) {
      if (!WAKING.has(t.kind) || !t.event_id || !items.has(t.item_id)) continue
      const e = this.row(t.event_id)
      if (!e || QUIET.has(e.type) || e.id === t.item_id) continue
      if (t.run_id && own.has(t.run_id) && !(e.type === 'item' && t.kind !== 'event_added')) continue
      items.get(t.item_id)!.add(t.event_id)
      fresh.add(t.item_id)
      activity = Math.max(activity, t.at)
    }
    // What happened to the bot itself: an item assigned to it, its settings changed, or something written in its own chat.
    let trigger = 0
    const chat = new Set<string>()
    for (const t of this.store.traceOfItem(agentId)) {
      if (t.id <= since || (t.run_id && own.has(t.run_id))) continue
      const note = t.kind === 'event_added' && this.row(t.event_id ?? '')?.type === 'note'
      if (note) chat.add(t.event_id!)
      if (t.kind === 'assigned' || t.kind === 'configured' || note) trigger = Math.max(trigger, t.at)
    }
    if (chat.size) {
      items.set(agentId, chat)
      fresh.add(agentId)
    }
    if (!items.size) {
      this.woken.delete(agentId)
      return null
    }
    const debounce = Number(this.agentParams(agentId).debounce_minutes ?? DEFAULT_DEBOUNCE) * MINUTE
    const due: number[] = []
    if (woken !== undefined) due.push(woken)
    if (trigger) due.push(trigger + debounce)
    if (activity) due.push(Math.max(activity + debounce, (runs[0]?.started_at ?? madeAt) + PERIODIC_MINUTES * MINUTE))
    return due.length ? { at: Math.min(...due), items, fresh } : null
  }

  /**
   * Starts a bot's run, if it is due and not running: every item it has, each with its new events (or its whole
   * history, the first time the bot sees it). Its session is its own: resumed if it has one, otherwise new.
   */
  claim(agentId: string): { agent: Agent; items: { item: Item; events: ItemEvent[]; first: boolean; own?: boolean }[]; resume: boolean } | null {
    const agent = this.agent(agentId)
    if (!agent || agent.deleted || this.running.has(agentId)) return null
    const wake = this.wake(agentId)
    if (!wake || wake.at > this.now()) return null
    const seen = new Set(this.agentRuns(agentId).flatMap((r) => r.item_ids))
    const items = [...wake.items].map(([itemId, ids]) => {
      const first = !seen.has(itemId)
      const events = first ? this.children(itemId) : this.children(itemId).filter((e) => ids.has(e.id))
      // Its own chat comes first, as itself.
      if (itemId === agentId) return { item: this.toItem(this.row(agentId)!), events, first, own: true }
      return { item: this.must(itemId), events, first }
    })
    const resume = agent.sessionId !== null
    if (!resume) this.setParam(agentId, 'sessionId', randomUUID())
    this.woken.delete(agentId)
    return { agent: this.agent(agentId)!, items, resume }
  }

  startRun(agentId: string, itemIds: string[], prompt: string): string {
    const id = randomUUID()
    this.store.insertRun({
      id, agent_id: agentId, item_ids: itemIds, event_ids: [], started_at: this.now(), trace_mark: this.store.lastTraceId(),
      ended_at: null, cost_usd: null, turns: null, result: null, prompt,
    })
    this.running.set(agentId, id)
    this.mark(agentId, 'run_started', { eventId: agentId, runId: id, actor: `bot:${agentId}`, data: { items: itemIds } })
    for (const itemId of itemIds) this.mark(itemId, 'run_started', { runId: id, actor: `bot:${agentId}` })
    this.emit('change')
    return id
  }

  appendTranscript(runId: string, chunk: string): void {
    this.store.appendTranscript(runId, chunk)
  }

  /** A run still going: its bot and its items; or null. */
  activeRun(runId: string): { agentId: string; itemIds: string[] } | null {
    const r = this.store.runs.get(runId)
    return r && r.ended_at === null && r.agent_id ? { agentId: r.agent_id, itemIds: r.item_ids } : null
  }

  /**
   * A run is over. Its output's updates are applied, item by item: a message, the events to add, who has it,
   * closing. Then each item it had that it said nothing of who has goes back to its owner (`settle`). If the run
   * failed, each item it has says so, and goes back to its owner.
   */
  async endRun(
    runId: string,
    end: { exitCode: number | null; structured?: unknown; output?: string; costUsd?: number; turns?: number; error?: string },
  ): Promise<void> {
    const run = this.store.runs.get(runId)
    if (!run || run.ended_at !== null) return
    const agentId = run.agent_id!
    const actor = `bot:${agentId}`
    this.store.updateRun(runId, { ended_at: this.now(), cost_usd: end.costUsd ?? null, turns: end.turns ?? null, result: end.output ?? end.error ?? null })
    this.store.endTranscript(runId)
    this.mark(agentId, 'run_ended', {
      eventId: agentId, runId, actor, data: { exitCode: end.exitCode, costUsd: end.costUsd ?? null, turns: end.turns ?? null, error: end.error ?? null },
    })
    if (this.running.get(agentId) === runId) this.running.delete(agentId)

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
        for (const u of updates) if (!this.get(u.item) && u.item !== agentId) throw new Error(`No item ${u.item}.`)
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
      // A failure is said in each item the bot has, which go back to their owners.
      const failed = (itemId: string, why: string) => {
        if (this.get(itemId)?.closed || !this.assignees(itemId).includes(agentId)) return
        this.add(itemId, [{ type: 'note', params: { text: `@${this.agentName(agentId)} failed: ${why}` } }], 'system', runId)
        this.handBack(itemId, agentId, runId)
      }
      if (error) {
        for (const itemId of run.item_ids) failed(itemId, error)
        return
      }
      const broken = new Set<string>()
      for (const u of updates) {
        if (this.get(u.item)?.closed) continue
        try {
          // Each update is checked on its own: one that is wrong fails only its item.
          const to = u.assign ? new Set(u.assign.map((a) => this.resolveAssignee(a))) : null
          const events: EventSpec[] = [...(u.message ? [{ type: 'note' as const, params: { text: u.message } }] : []), ...this.prepare(u.events ?? [], 'bot')]
          if (events.length) this.add(u.item, events, actor, runId)
          if (to) {
            const now = this.assignees(u.item)
            for (const a of to) if (!now.includes(a)) this.setAssignee(u.item, a, true, actor, runId)
            for (const a of now) if (!to.has(a)) this.setAssignee(u.item, a, false, actor, runId)
          }
          if (u.close) closing.push(u.item)
        } catch (e) {
          broken.add(u.item)
          failed(u.item, `its update could not be applied: ${(e as Error).message}`)
        }
      }
      // An item it said who has is settled; the rest are settled here.
      const said = new Set([...updates.filter((u) => u.assign).map((u) => u.item), ...broken])
      for (const itemId of run.item_ids) if (!closing.includes(itemId) && !said.has(itemId) && !this.get(itemId)?.closed) this.settle(itemId, agentId, runId)
    })
    for (const itemId of closing) await this.close(itemId, { actor, runId })
    this.emit('change')
    await this.idle()
  }

  /**
   * After a run: an item the bot still has goes back to its owner, unless the run assigned it, or it is waiting on
   * something the bot added that is not a person (it wakes again for that), or something new came in while it ran.
   */
  private settle(itemId: string, agentId: string, runId: string): void {
    if (!this.assignees(itemId).includes(agentId)) return
    if (this.store.traceOfItem(itemId).some((t) => (t.kind === 'assigned' || t.kind === 'unassigned') && t.run_id === runId)) return
    const { blockers } = this.must(itemId)
    const own = new Set(this.agentRuns(agentId).map((r) => r.id))
    const mine = (id: string) => this.store.traceOfEvent(id).some((t) => t.kind === 'event_added' && t.run_id !== null && own.has(t.run_id))
    if (blockers.some((b) => mine(b.id) && !this.waitsOnHuman(b, blockers))) return
    if (this.wake(agentId)?.fresh.has(itemId)) return
    this.handBack(itemId, agentId, runId)
  }

  /** Takes an item off a bot; if that leaves it with nobody, it goes to its owner. */
  private handBack(itemId: string, agentId: string, runId: string | null, actor = `bot:${agentId}`): void {
    this.setAssignee(itemId, agentId, false, actor, runId)
    const owner = this.owner(itemId)
    if (!this.assignees(itemId).length && this.isUser(owner)) this.setAssignee(itemId, owner, true, actor, runId)
  }

  transcript(runId: string): Promise<string | null> {
    const r = this.store.runs.get(runId)
    if (!r) return Promise.resolve(null)
    return this.store.transcript(runId).then((t) => `PROMPT:\n${r.prompt}\n\nTRANSCRIPT (stream-json):\n${t ?? '(not kept)'}`)
  }

  // ---- the trace ----

  trace(itemId: string): TraceEntry[] {
    return this.store.traceOfItem(itemId).map(toTrace)
  }

  /** Records one thing that happened in an item. Every change, view, check and call comes through here. */
  mark(
    itemId: string,
    kind: string,
    opts: { eventId?: string | null; runId?: string | null; actor?: string; data?: Record<string, unknown> } = {},
  ): void {
    this.store.appendTrace({
      at: this.now(), item_id: itemId, event_id: opts.eventId ?? null, run_id: opts.runId ?? null, actor: opts.actor ?? 'system', kind, data: opts.data ?? null,
    })
  }

  viewed(itemId: string, actor = this.me()): void {
    this.mark(itemId, 'viewed', { actor })
  }

  skipped(itemId: string, actor = this.me()): void {
    this.mark(itemId, 'skipped', { actor })
  }

  /**
   * The items touched in the last `days`, each with how long it spent blocked by
   * each kind of event and what came of them. The start of any look back.
   */
  recent(days: number) {
    const since = this.now() - days * 24 * 60 * MINUTE
    const touched = new Set(this.store.traceAfter(0).filter((t) => t.at >= since).map((t) => t.item_id))
    const rows = this.store
      .ofType('item')
      .filter((r) => r.created_at >= since || (r.resolved_at ?? 0) >= since || touched.has(r.id))
      .sort((a, b) => a.created_at - b.created_at)
    return rows.map(({ id }) => {
      const item = this.must(id)
      const events = this.children(id)
      const blocked: Record<string, number> = {}
      for (const e of events.filter((e) => this.type(e.type).blocking))
        blocked[e.type] = (blocked[e.type] ?? 0) + ((e.resolvedAt ?? this.now()) - e.createdAt) / MINUTE
      const runs = [...this.store.runs.values()].filter((r) => r.item_ids.includes(id))
      const trace = this.trace(id)
      return {
        id,
        title: item.title,
        parentId: item.parentId,
        closed: item.closed,
        how: item.how,
        assignees: item.assignees,
        events: events.map((e) => e.type),
        minutesBlockedBy: Object.fromEntries(Object.entries(blocked).map(([k, v]) => [k, Math.round(v)])),
        botRuns: runs.length,
        // A run that covered several items is counted in full for each.
        costUsd: runs.reduce((s, r) => s + (r.cost_usd ?? 0), 0),
        humanMessages: events.filter((e) => e.type === 'note' && this.isUser(e.createdBy)).length,
        approvalsChanged: events.filter((e) => ((e.data?.changed as string[] | undefined) ?? []).length).length,
        cancelled: events.filter((e) => e.cancelled).length,
        timedOut: events.filter((e) => e.data?.outcome === 'timed_out').length,
        botFailures: events.filter((e) => e.type === 'note' && e.createdBy === 'system' && / failed: /.test(String(e.params.text))).length,
        views: trace.filter((t) => t.kind === 'viewed').length,
        skips: trace.filter((t) => t.kind === 'skipped').length,
      }
    })
  }

  // ---- actions ----

  /** The one way into an action, from MCP, a script or the UI: checked, run, and traced. */
  async callAction(name: string, args: Record<string, unknown>, ctx: ActionContext): Promise<unknown> {
    const action = this.actions.find((a) => a.name === name)
    if (!action) throw new Error(`No action ${name}.`)
    // The rule on unsafe actions (see Action in integrations.ts), checked here
    // too, whatever the caller was shown.
    if (action.unsafe && ctx.author !== 'human')
      throw new Error(`${name} is unsafe, so bots, and scripts bots wrote, may not run it. Ask a human: add an approval event for it.`)
    const parsed = z.object(action.input).safeParse(args ?? {})
    if (!parsed.success) throw new Error(`Bad arguments for ${name}: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
    const started = this.now()
    const traced = (ok: boolean, error?: string) =>
      ctx.itemId &&
      this.mark(ctx.itemId, 'action_called', {
        runId: ctx.runId,
        actor: ctx.actor,
        data: { action: name, args: parsed.data, ok, ...(error ? { error } : {}), ms: this.now() - started },
      })
    let out: unknown
    try {
      out = await action.run(parsed.data, ctx)
    } catch (e) {
      traced(false, (e as Error).message)
      throw e
    }
    traced(true)
    return out
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
      // Who a new item goes to, as ids.
      if (s.type === 'item' && params.assignees) params.assignees = (params.assignees as string[]).map((a) => this.resolveAssignee(a))
      if (s.type === 'user' && this.row(String(params.username))) throw new Error(`The username ${params.username} is taken.`)
      // Prepared now, so a bad one fails the whole list; added in the item as it starts.
      if (s.type === 'item' && params.events) params.events = this.prepare(params.events as EventSpec[], author)
      const expanded = type.expand ? type.expand(params, author) : [{ type: s.type, params }]
      return expanded.map((e, i) => ({ type: e.type, params: e.params, ref: i === 0 ? (s.ref ?? e.ref) : e.ref }))
    })
  }

  /**
   * Adds events, already prepared, in an item or a bot's chat (or, for items, bots and people, in none). Plain ones
   * resolve at once; blockers wait, and those whose type has a start hook start
   * right after. An item whose key is already taken is left out. Answers the ids added.
   */
  add(parentId: string | null, specs: EventSpec[], createdBy: string, runId: string | null = null): string[] {
    return this.store.batch(() => this.addNow(parentId, specs, createdBy, runId))
  }

  private addNow(parentId: string | null, specs: EventSpec[], createdBy: string, runId: string | null): string[] {
    // A detached item sits in no item, wherever it is added.
    const detached = parentId ? specs.filter((s) => s.type === 'item' && s.params.detached) : []
    if (detached.length)
      return [...this.addNow(parentId, specs.filter((s) => !detached.includes(s)), createdBy, runId), ...this.addNow(null, detached, createdBy, runId)]
    const keyed = (s: EventSpec) => s.type === 'item' && s.params.key && this.store.ofType('item').some((r) => r.params.key === s.params.key)
    specs = specs.filter((s) => !keyed(s))
    if (!parentId && specs.some((s) => !['item', 'bot', 'user'].includes(s.type))) throw new Error('Only an item, a bot or a person can sit in no item.')
    if (parentId && this.row(parentId)?.type === 'bot' && specs.some((s) => s.type !== 'note')) throw new Error("A bot's own chat holds only notes.")
    const ids = new Map<string, string>()
    const rows = specs.map((s) => {
      // A person's id is their username.
      const id = s.type === 'user' ? String(s.params.username) : randomUUID()
      if (s.ref) ids.set(s.ref, id)
      return { id, spec: s }
    })
    const seq = this.store.nextSeq(parentId)
    const at = this.now()
    rows.forEach(({ id, spec }, i) => {
      const type = this.type(spec.type)
      const { assignees, ...params } = resolveRefs(spec.params, ids) as Record<string, unknown>
      this.store.insertEvent({
        id, parent_id: parentId, seq: seq + i, type: spec.type, params, created_by: createdBy, created_at: at,
        resolved: !type.blocking, resolved_at: type.blocking ? null : at, resolved_by: type.blocking ? null : createdBy, cancelled: false,
        started: !type.start, data: null, memo: null, check_step: 0,
        next_check_at: type.check ? at + this.scheduleOf(type, params)[0] * 1000 : null, last_error: null,
      })
      const where = parentId ?? id
      this.mark(where, 'event_added', { eventId: id, runId, actor: createdBy, data: { type: spec.type } })
      if (!type.blocking) this.mark(where, 'event_resolved', { eventId: id, runId, actor: createdBy })
      // A new item goes to those given; else, made by a person, to them; else (a bot or a source made it) to nobody.
      if (spec.type === 'item') {
        const to = Array.isArray(assignees) && assignees.length ? (assignees as string[]) : this.isUser(createdBy) ? [createdBy] : []
        for (const a of to) this.setAssignee(id, a, true, createdBy, runId)
      }
    })
    for (const { id, spec } of rows) if (this.type(spec.type).start) void this.startPending(parentId ?? id)
    // New waits are checked at once; an after may already have what it waits for.
    if (rows.some(({ spec }) => this.type(spec.type).check)) queueMicrotask(() => void this.checkDue())
    this.emit('change')
    return rows.map((r) => r.id)
  }

  /** Runs the start hooks of the new blockers in an item (and of the item itself). */
  private startPending(itemId: string): Promise<void> {
    return this.serial(itemId, async () => {
      const pending = [this.row(itemId), ...this.store.children(itemId)].filter((r): r is EventRow => !!r && !r.started && !r.resolved).map((r) => this.toEvent(r))
      for (const event of pending) {
        this.store.updateEvent(event.id, { started: true })
        // Resolving as it starts is part of whatever added it: a bot's run does not wake its bot again for that.
        const runId = this.store.traceOfEvent(event.id).find((t) => t.kind === 'event_added')?.run_id ?? null
        try {
          const ctx = { engine: this, item: this.must(event.parentId ?? event.id), event, author: authorOf(event.createdBy), memo: null, setMemo: () => {} }
          const out = await this.type(event.type).start!(event.params, ctx)
          if (out?.params) this.store.updateEvent(event.id, { params: out.params })
          if (out?.resolve) this.resolve(event.id, out.resolve, 'system', false, runId)
        } catch (e) {
          this.resolve(event.id, { outcome: 'error', error: (e as Error).message, detail: `Could not start: ${(e as Error).message}` }, 'system', false, runId)
        }
      }
      this.emit('change')
    })
  }

  /** Resolves a blocker, and wakes whatever was waiting on it. */
  private resolve(eventId: string, data: Record<string, unknown>, by: string, cancelled = false, runId: string | null = null): void {
    const r = this.row(eventId)
    if (!r || r.resolved) return
    this.store.updateEvent(eventId, { resolved: true, resolved_at: this.now(), resolved_by: by, cancelled, data, next_check_at: null })
    this.mark(r.parent_id ?? eventId, cancelled ? 'event_cancelled' : 'event_resolved', { eventId, runId, actor: by })
    this.wakeAfters()
  }

  /** Afters are checked at once after anything resolves, rather than at their next turn. */
  private wakeAfters(): void {
    for (const r of this.store.ofType('after')) if (!r.resolved) r.next_check_at = this.now()
    queueMicrotask(() => void this.checkDue())
  }

  /** An event's check schedule, in seconds: its type's, never under a second apart after the first, or the default. */
  private scheduleOf(type: EventType, params: Record<string, unknown>): number[] {
    const s = type.schedule?.(params) ?? DEFAULT_SCHEDULE
    const delays = s.length ? s : DEFAULT_SCHEDULE
    return delays.map((d, i) => (i === 0 ? Math.max(0, d) : Math.max(1, d)))
  }

  /** Writes one param of an event: for the app's own bookkeeping, such as a bot's session. */
  private setParam(eventId: string, key: string, value: unknown): void {
    const row = this.row(eventId)!
    this.store.updateEvent(eventId, { params: { ...row.params, [key]: value } })
  }

  // ---- people ----

  /** A new item, in `parentId` or in none, with events to start it off. */
  async create(
    title: string,
    opts: { parentId?: string | null; key?: string; events?: EventSpec[]; assignees?: string[]; actor?: string; author?: Author } = {},
  ): Promise<string | null> {
    const author = opts.author ?? 'human'
    const params = { title, ...(opts.key ? { key: opts.key } : {}), ...(opts.events ? { events: opts.events } : {}), ...(opts.assignees ? { assignees: opts.assignees } : {}) }
    const [spec] = this.prepare([{ type: 'item', params }], author)
    const [id] = this.add(opts.parentId ?? null, [spec], author === 'bot' ? 'bot' : (opts.actor ?? this.me()))
    await this.idle()
    return id ?? null
  }

  /**
   * The chat input: `text` through a preset, as `actor`. Whoever (a bot or a person) is `[id]()`-referred to in the
   * text is assigned the item, as is whoever an `assign:<id>` preset names; then the sender is taken off it. Assigning
   * a bot that has the item already assigns it again, which wakes it.
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
    if (this.must(itemId).closed) throw new Error('This item is closed.')
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

  /** Adds events in an item (or items in none): from the human, or from an agent through MCP (author 'bot', so its rules apply). */
  async addEvents(parentId: string | null, specs: EventSpec[], actor = this.me(), author: Author = 'human', runId: string | null = null): Promise<string[]> {
    if (parentId && this.row(parentId)?.type !== 'bot' && this.must(parentId).closed) throw new Error('This item is closed.')
    const ids = this.add(parentId, this.prepare(specs, author), actor, runId)
    // A script is run from inside an item's queue, so waiting for the queues to empty would wait for itself.
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

  /** Ends a blocker without it happening; an item is closed as cancelled, and a bot deleted. */
  async cancel(eventId: string, actor = this.me(), runId: string | null = null): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.resolved) throw new Error('That event is not waiting.')
    if (event.type === 'item') return this.close(eventId, { actor, how: 'cancelled', runId: runId ?? undefined })
    if (event.type === 'bot') return this.deleteAgent(eventId, actor)
    this.resolve(eventId, { cancelled: true }, actor, true, runId)
    this.emit('change')
    if (actor !== 'script') await this.idle()
  }

  /** Closes an item: its open children are cancelled (child items closed, all the way down), then it resolves. */
  async close(itemId: string, opts: { actor?: string; how?: 'closed' | 'cancelled'; runId?: string } = {}): Promise<void> {
    const actor = opts.actor ?? this.me()
    const runId = opts.runId ?? null
    const shut = (id: string, how: string) => {
      for (const b of this.children(id, true))
        if (b.type === 'item') shut(b.id, 'cancelled')
        else this.resolve(b.id, { cancelled: true, detail: 'Its item closed.' }, actor, true, runId)
      this.resolve(id, { how }, actor, false, runId)
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
    const due = [...this.store.events.values()].filter((r) => !r.resolved && r.started && r.next_check_at !== null && r.next_check_at <= now)
    await Promise.all(due.filter((r) => !this.checking.has(r.id)).map((r) => this.checkOne(r.parent_id!, r.id)))
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
      try {
        data = await type.check!(event.params, {
          engine: this,
          item: this.must(itemId),
          event,
          author: authorOf(event.createdBy),
          memo: row.memo ?? null,
          setMemo: (value) => this.store.updateEvent(eventId, { memo: value ?? null }),
        })
      } catch (e) {
        error = (e as Error).message
      }
      const timeout = Number(event.params.timeout_minutes ?? 0)
      if (!data && timeout && this.now() >= event.createdAt + timeout * MINUTE)
        data = { outcome: 'timed_out', detail: `Nothing after ${timeout} minutes.` }
      this.mark(itemId, 'check', {
        eventId,
        data: data ? { result: 'resolved', outcome: data.outcome ?? null } : error ? { result: 'error', error } : { result: 'pending' },
      })
      if (data) return this.resolve(eventId, data, 'system')
      const schedule = this.scheduleOf(type, event.params)
      const step = Math.min(row.check_step + 1, schedule.length - 1)
      Object.assign(row, { check_step: step, next_check_at: this.now() + schedule[step] * 1000, last_error: error })
      this.emit('change')
    }).finally(() => this.checking.delete(eventId))
  }
}
