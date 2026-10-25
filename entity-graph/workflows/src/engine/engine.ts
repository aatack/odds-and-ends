import { CustomActions } from './customActions'
import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { mkdirSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { z } from 'zod'
import {
  HUMAN, type Agent, type Assignment, type BotRun, type EventDetail, type EventSpec, type EventTimes, type Item, type ItemDetail,
  type ItemEvent, type Overview, type TraceEntry,
} from '../shared/types'
import { itemActions } from './actions'
import { EVENT_TYPES } from './eventTypes'
import { githubActions, githubSources } from './github'
import { execCli, type Action, type ActionContext, type Author, type EventType, type Exec, type Source } from './integrations'
import { PRESETS, type Preset } from './presets'
import { plainSecrets, Settings, type Secrets } from './settings'
import { slackActions } from './slack'
import { Store, type EventRow, type RunRow, type TraceRow } from './store'

/** How long a blocker sleeps between checks, by default: at once, then a minute, then longer, up to the last. */
export const BACKOFF_MINUTES = [1, 2, 5, 10]
const MINUTE = 60_000
const DEFAULT_SCHEDULE = [0, ...BACKOFF_MINUTES.map((m) => m * 60)]

/** What a bot says about one item, in its run's output. */
export interface BotUpdate {
  /** The item's id. */
  item: string
  /** To the human, in the item: what it did, or one direct question. */
  message?: string
  /** Events to add in the item: waits, approvals, afters, child items, assigns. */
  events?: EventSpec[]
  /** Who the item is assigned to from now on: 'human', bots' names or ids. Left out: see Engine.endRun. */
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

/** What wakes a bot: something added, resolved or cancelled in an item it has. */
const WAKING = new Set(['event_added', 'event_resolved', 'event_cancelled'])
/** Events that never wake a bot: a summary is for the human, and an assign that takes a bot off wakes nobody. */
const QUIET = new Set(['summary'])

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

/** `[id]()` in text: a reference to an event (an item, or a bot). */
export const refsIn = (text: string): string[] => [...text.matchAll(/\[([\w-]{8,})\]\(\)/g)].map((m) => m[1])

const slug = (name: string) => name.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'bot'

export interface EngineOptions {
  store?: Store
  secrets?: Secrets
  exec?: Exec
  fetch?: typeof fetch
  now?: () => number
  sources?: Source[]
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
  readonly actions: Action[] = [...itemActions, ...githubActions, ...slackActions]
  readonly eventTypes: EventType[] = EVENT_TYPES
  readonly presets: Preset[] = PRESETS
  readonly sources: Source[]
  /** The human's own actions, per event type (the Types page). */
  readonly customActions: CustomActions
  /** Stops a bot's run. Set by the runner. */
  onCancel: (agentId: string) => void = () => {}
  /** The run each bot has going, by the bot's id. In memory only: a crash leaves nothing behind. */
  private running = new Map<string, string>()
  /** When each bot was woken by hand: it runs then, whatever its debounce says. */
  private woken = new Map<string, number>()
  private lastPoll = new Map<string, number>()
  /** The events being checked right now: a check never starts while the last is still going. */
  private checking = new Set<string>()
  private queues = new Map<string, Promise<unknown>>()

  constructor(opts: EngineOptions = {}) {
    super()
    this.store = opts.store ?? new Store()
    this.settings = new Settings(this.store, opts.secrets ?? plainSecrets)
    this.exec = opts.exec ?? execCli
    this.fetch = opts.fetch ?? fetch
    this.now = opts.now ?? Date.now
    this.sources = opts.sources ?? githubSources
    this.dataDir = opts.dataDir ?? join(tmpdir(), 'workflows')
    this.customActions = new CustomActions(this, this.store)
    // A run the app was closed during is over.
    for (const r of this.store.runs.values()) if (r.ended_at === null) this.store.updateRun(r.id, { ended_at: this.now(), result: 'The app stopped during the run.' })
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
    const summary = type.type === 'item' ? this.title(r.id, params) : type.type === 'agent' ? `@${this.agentName(r.id)}` : type.describe(params)
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

  /** The latest event of a type in an item, if any. */
  private latest(itemId: string, type: string): EventRow | undefined {
    return this.store.children(itemId).findLast((r) => r.type === type)
  }

  /** An item's title now: its latest rename event's, else the one it was made with. */
  private title(id: string, params: Record<string, unknown>): string {
    return String(this.latest(id, 'rename')?.params.title ?? params.title)
  }

  /** Who an item is assigned to: by the latest assign event for each, the earliest assigned first. */
  assignees(itemId: string): string[] {
    const on = new Map<string, boolean>()
    for (const r of this.store.children(itemId))
      if (r.type === 'assign') {
        const to = String(r.params.to)
        on.delete(to)
        on.set(to, r.params.on !== false)
      }
    return [...on].filter(([, v]) => v).map(([k]) => k)
  }

  /** The bots watching an item. */
  private watchers(itemId: string): string[] {
    const on = new Map<string, boolean>()
    for (const r of this.store.children(itemId)) if (r.type === 'watch') on.set(String(r.params.agent), r.params.on !== false)
    return [...on].filter(([, v]) => v).map(([k]) => k)
  }

  private toItem(r: EventRow): Item {
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
      withHuman: !closed && (!assignees.length || assignees.includes(HUMAN)) && blockers.every((b) => this.waitsOnHuman(b, blockers)),
      priority: Boolean(this.latest(id, 'priority')?.params.on),
      snoozedUntil: snoozedUntil(blockers),
      bot,
      sessionId: (r.params.sessionId as string) ?? null,
      since: last,
      createdAt: r.created_at,
      closedAt: r.resolved_at,
    }
  }

  /** A blocker the human resolves, or an after waiting on one: the item is still the human's turn. */
  private waitsOnHuman(b: ItemEvent, blockers: ItemEvent[], depth = 0): boolean {
    if (this.type(b.type).human) return true
    const waited = b.type === 'after' && depth < 20 && blockers.find((x) => x.id === b.params.event)
    return Boolean(waited && this.waitsOnHuman(waited, blockers, depth + 1))
  }

  /** The item event with this id, or null. */
  get(id: string): Item | null {
    const r = this.row(id)
    return r?.type === 'item' ? this.toItem(r) : null
  }

  private must(id: string): Item {
    const item = this.get(id)
    if (!item) throw new Error(`No item ${id}.`)
    return item
  }

  /** Every open item, at any depth, and those closed in the last day; and the bots. */
  overview(): Overview {
    const since = this.now() - 24 * 60 * MINUTE
    const items = this.store
      .ofType('item')
      .filter((r) => !r.resolved || (r.resolved_at ?? 0) > since)
      .map((r) => this.toItem(r))
      .sort((a, b) => a.since - b.since)
    return { items, bots: this.agents(), ...this.inHand() }
  }

  /**
   * The items the human has had on screen, and those they have linked (attached
   * something to, or attached), in the last week: the most recent first, each once.
   */
  inHand(): Pick<Overview, 'viewed' | 'linked' | 'attachments' | 'recentBots'> {
    const since = this.now() - 7 * 24 * 60 * MINUTE
    const viewed: string[] = []
    for (let i = this.store.trace.length - 1; i >= 0 && viewed.length < 50; i--) {
      const t = this.store.trace[i]
      if (t.at <= since) break
      if (t.kind === 'viewed' && !viewed.includes(t.item_id)) viewed.push(t.item_id)
    }
    const attachments = this.store
      .ofType('attachment')
      .filter((r) => r.created_by === HUMAN && r.created_at > since)
      .sort((a, b) => b.created_at - a.created_at)
      .slice(0, 100)
    const linked = attachments.flatMap((r) => (r.params.event === r.params.item ? [r.parent_id!, String(r.params.event)] : [r.parent_id!]))
    const attached = attachments.map((r) => ({
      event: String(r.params.event),
      title: String(r.params.title),
      item: String(r.params.item),
      target: r.parent_id!,
    }))
    const recentBots = [
      ...new Set(
        this.store
          .ofType('assign')
          .filter((r) => r.created_by === HUMAN && r.params.to !== HUMAN && r.params.on !== false)
          .sort((a, b) => b.created_at - a.created_at || b.seq - a.seq)
          .map((r) => String(r.params.to)),
      ),
    ]
    return { viewed, linked: [...new Set(linked)], attachments: attached, recentBots }
  }

  private runsOf(pred: (r: RunRow) => boolean): BotRun[] {
    return [...this.store.runs.values()].filter(pred).sort((a, b) => a.started_at - b.started_at).map(toRun)
  }

  detail(id: string): ItemDetail | null {
    const item = this.get(id)
    if (!item) return null
    const parent = item.parentId ? this.get(item.parentId) : null
    return {
      item,
      parent: parent && { id: parent.id, title: parent.title },
      events: this.children(id),
      runs: this.runsOf((r) => r.item_ids.includes(id)),
      trace: this.trace(id),
    }
  }

  /** Any event, item or not, with what its views need. */
  eventDetail(id: string): EventDetail | null {
    const event = this.event(id)
    if (!event) return null
    const parent = event.parentId ? this.get(event.parentId) : null
    return {
      event,
      parent: parent && { id: parent.id, title: parent.title },
      runs: this.runsOf((r) => r.item_ids.includes(id) || r.event_ids.includes(id) || r.agent_id === id),
      trace: this.store.traceOfEvent(id).map(toTrace),
      item: event.type === 'item' ? this.detail(id) : null,
    }
  }

  /** What needs the human next: their priority items, the most recently active first; then the rest, the least recently touched first. */
  next(skip: string[] = []): string | null {
    const open = this.store
      .ofType('item')
      .filter((r) => !r.resolved && !skip.includes(r.id))
      .map((r) => this.toItem(r))
      .filter((i) => i.withHuman)
    const first = open.filter((i) => i.priority).sort((a, b) => b.since - a.since)[0] ?? open.sort((a, b) => a.since - b.since)[0]
    return first?.id ?? null
  }

  /**
   * The open items the human might lose track of: those they made or had assigned to them at any time, untouched
   * (nothing added or resolved in them) for `hours`, the longest untouched first.
   */
  stale(hours: number): Item[] {
    const before = this.now() - hours * 60 * MINUTE
    return this.store
      .ofType('item')
      .filter((r) => !r.resolved)
      .filter((r) => authorOf(r.created_by) === 'human' || this.store.children(r.id).some((k) => k.type === 'assign' && k.params.to === HUMAN))
      .map((r) => this.toItem(r))
      .filter((i) => i.since < before)
      .sort((a, b) => a.since - b.since)
  }

  /** Assign events, the newest first: to one assignee (or anyone), in the last `days`. */
  assignments(assignee: string | null, days: number): Assignment[] {
    const since = this.now() - days * 24 * 60 * MINUTE
    return this.store
      .ofType('assign')
      .filter((r) => r.created_at > since && (!assignee || r.params.to === assignee) && r.parent_id)
      .sort((a, b) => b.created_at - a.created_at)
      .map((r) => ({
        itemId: r.parent_id!,
        title: this.title(r.parent_id!, this.row(r.parent_id!)?.params ?? {}),
        assignee: String(r.params.to),
        on: r.params.on !== false,
        by: r.created_by,
        at: r.created_at,
      }))
  }

  /** Asks for a summary of where an item stands: a summary event in it, which a cheap model answers (see eventTypes). */
  async summarise(itemId: string, actor = HUMAN): Promise<void> {
    await this.addEvents(itemId, [{ type: 'summary', params: {} }], actor)
  }

  /** The open events of one type, anywhere, the newest first. */
  openOfType(type: string): ItemEvent[] {
    return this.store
      .ofType(type)
      .filter((r) => !r.resolved)
      .sort((a, b) => b.created_at - a.created_at || b.seq - a.seq)
      .map((r) => this.toEvent(r))
  }

  /** The directives that hold (not yet deleted), the newest first. */
  directives(): ItemEvent[] {
    return this.openOfType('directive')
  }

  /** A new directive from the Directives page: a directive event, in no item. Deleting it is cancelling it. */
  async addDirective(text: string): Promise<string> {
    if (!text.trim()) throw new Error('A directive needs some text.')
    const [id] = await this.addEvents(null, [{ type: 'directive', params: { text } }])
    return id
  }

  /**
   * Edits a directive: a new one replaces it (events are never changed), so
   * its age starts again. The old one is kept, cancelled, pointing at the new.
   */
  async editDirective(id: string, text: string): Promise<string> {
    if (!text.trim()) throw new Error('A directive needs some text.')
    const [added] = await this.addEvents(null, [{ type: 'directive', params: { text, replaces: id } }])
    return added
  }

  /** Renames an item, with a rename event in it. */
  async rename(itemId: string, title: string, actor = HUMAN): Promise<void> {
    const t = title.trim()
    if (!t || this.must(itemId).title === t) return
    await this.addEvents(itemId, [{ type: 'rename', params: { title: t } }], actor)
  }

  /** Marks an item as a priority, or not, with a priority event in it. */
  async setPriority(itemId: string, on: boolean, actor = HUMAN): Promise<void> {
    if (this.must(itemId).priority === on) return
    await this.addEvents(itemId, [{ type: 'priority', params: { on } }], actor)
  }

  /** Puts an item away until `until` (ms): a snooze event in it, which blocks it until then. */
  async snooze(itemId: string, until: number, actor = HUMAN): Promise<void> {
    if (this.must(itemId).closed) throw new Error('That task is closed.')
    if (!(until > this.now())) throw new Error('Snooze until a time to come.')
    await this.unsnooze(itemId, actor)
    await this.addEvents(itemId, [{ type: 'snooze', params: { until: new Date(until).toISOString() } }], actor)
  }

  /** Wakes an item now: every snooze in it is cancelled. */
  async unsnooze(itemId: string, actor = HUMAN): Promise<void> {
    for (const b of this.must(itemId).blockers.filter((b) => b.type === 'snooze')) await this.cancel(b.id, actor)
  }

  /** Assigns an item to someone (on), or takes it off them: an assign event in it. */
  async assign(itemId: string, to: string, on: boolean, actor = HUMAN): Promise<void> {
    await this.addEvents(itemId, [{ type: 'assign', params: { to, on } }], actor)
  }

  // ---- bots ----

  /** A bot's settings now: its own params, then each configure event's on top. */
  private agentParams(id: string): Record<string, unknown> {
    const r = this.row(id)
    if (r?.type !== 'agent') return {}
    const p = { ...r.params }
    for (const k of this.store.children(id)) if (k.type === 'configure') Object.assign(p, k.params)
    return p
  }

  private agentName(id: string): string {
    return String(this.agentParams(id).name ?? id)
  }

  /** Who did something, for reading: a bot's actor (`bot:<id>`) as its @name; anyone else as they are. */
  actorName(actor: string): string {
    return actor.startsWith('bot:') ? `@${this.agentName(actor.slice(4))}` : actor
  }

  /** A bot's own folder: given in its settings, or one under the app's that lasts. */
  agentFolder(id: string): string {
    const p = this.agentParams(id)
    const dir = p.folder ? String(p.folder) : join(this.dataDir, 'bots', `${slug(String(p.name))}-${id.slice(0, 8)}`)
    mkdirSync(dir, { recursive: true })
    return dir
  }

  agent(id: string): Agent | null {
    const r = this.row(id)
    if (r?.type !== 'agent') return null
    const p = this.agentParams(id)
    const items = this.store.ofType('item').filter((i) => !i.resolved)
    const name = String(p.name)
    return {
      id,
      name,
      instructions: String(p.instructions ?? ''),
      provider: String(p.provider ?? 'claude'),
      model: String(p.model ?? ''),
      debounceMinutes: Number(p.debounce_minutes ?? 0),
      folder: p.folder ? String(p.folder) : join(this.dataDir, 'bots', `${slug(name)}-${id.slice(0, 8)}`),
      sessionId: (r.params.sessionId as string) ?? null,
      createdAt: r.created_at,
      deleted: r.resolved,
      runId: this.running.get(id) ?? null,
      wakesAt: r.resolved ? null : this.wake(id)?.at ?? null,
      assigned: items.filter((i) => this.assignees(i.id).includes(id)).map((i) => i.id),
      watching: items.filter((i) => this.watchers(i.id).includes(id)).map((i) => i.id),
    }
  }

  /** Every bot that lasts, the oldest first. */
  agents(): Agent[] {
    return this.store
      .ofType('agent')
      .filter((r) => !r.resolved)
      .sort((a, b) => a.created_at - b.created_at)
      .map((r) => this.agent(r.id)!)
  }

  /** A bot by its id or its name (any case, with or without the @), among those that last. */
  findAgent(ref: string): Agent | null {
    const name = ref.replace(/^@/, '').toLowerCase()
    return this.agents().find((a) => a.id === ref || a.name.toLowerCase() === name) ?? null
  }

  /** The bot an old `bot` event goes to: the `bots.default` setting's, else the first. */
  defaultAgent(): Agent | null {
    const chosen = this.settings.get<string | null>('bots.default', null)
    return (chosen && this.findAgent(chosen)) || this.agents()[0] || null
  }

  /** Makes a bot (no id) or changes one (a configure event with what differs). Answers its id. */
  async saveAgent(a: { id?: string; name: string; instructions: string; provider: string; model: string; debounceMinutes: number; folder?: string }): Promise<string> {
    const name = a.name.trim().replace(/^@/, '')
    if (!/^[\w.-]+$/.test(name)) throw new Error('A bot needs a name of letters, digits, dots, dashes or underscores: it is @ed by it.')
    if (name.toLowerCase() === HUMAN) throw new Error(`“${HUMAN}” stands for you.`)
    const clash = this.findAgent(name)
    if (clash && clash.id !== a.id) throw new Error(`There is a bot called @${clash.name} already.`)
    const params: Record<string, unknown> = {
      name,
      instructions: a.instructions,
      provider: a.provider || 'claude',
      model: a.model,
      debounce_minutes: Math.max(0, Number(a.debounceMinutes) || 0),
      ...(a.folder?.trim() ? { folder: a.folder.trim() } : {}),
    }
    if (!a.id) {
      const [id] = await this.addEvents(null, [{ type: 'agent', params }])
      return id
    }
    const now = this.agentParams(a.id)
    if (!this.agent(a.id) || this.agent(a.id)!.deleted) throw new Error(`No bot ${a.id}.`)
    const changed = Object.fromEntries(Object.entries(params).filter(([k, v]) => String(now[k] ?? '') !== String(v ?? '')))
    if (Object.keys(changed).length) this.add(a.id, this.prepare([{ type: 'configure', params: changed }], 'human'), HUMAN)
    return a.id
  }

  /** Deletes a bot: it is cancelled, it stops if it is running, and every item it had goes back to the human. */
  async deleteAgent(id: string, actor = HUMAN): Promise<void> {
    const agent = this.agent(id)
    if (!agent || agent.deleted) throw new Error(`No bot ${id}.`)
    this.store.batch(() => {
      for (const itemId of agent.assigned)
        this.add(itemId, [{ type: 'assign', params: { to: id, on: false, name: agent.name } }, { type: 'assign', params: { to: HUMAN, on: true } }], actor)
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
   * What is new for a bot: per item it is assigned or watches, the events added,
   * resolved or cancelled since it was last prompted (or since it was made), less
   * what its own runs did; and when it should wake for them (the last of them,
   * plus its debounce). Null when nothing is new.
   */
  wake(agentId: string): { at: number; items: Map<string, Set<string>> } | null {
    const runs = this.agentRuns(agentId)
    const own = new Set(runs.map((r) => r.id))
    const since = runs[0]?.trace_mark ?? this.store.traceOfEvent(agentId)[0]?.id ?? 0
    const items = new Map<string, Set<string>>()
    let last = 0
    const has = new Map<string, boolean>()
    const mine = (itemId: string) => {
      if (!has.has(itemId)) {
        const r = this.row(itemId)
        has.set(itemId, r?.type === 'item' && !r.resolved && (this.assignees(itemId).includes(agentId) || this.watchers(itemId).includes(agentId)))
      }
      return has.get(itemId)!
    }
    for (const t of this.store.traceAfter(since)) {
      if (!WAKING.has(t.kind) || !t.event_id || (t.run_id && own.has(t.run_id)) || !mine(t.item_id)) continue
      const e = this.row(t.event_id)
      if (!e || QUIET.has(e.type) || e.id === t.item_id) continue
      // Taking someone off an item is news only for whoever takes over.
      if (e.type === 'assign' && e.params.on === false) continue
      const set = items.get(t.item_id) ?? new Set()
      set.add(t.event_id)
      items.set(t.item_id, set)
      last = Math.max(last, t.at)
    }
    // Woken by hand: now, with every item it has, new or not.
    const woken = this.woken.get(agentId)
    if (woken !== undefined) {
      for (const r of this.store.ofType('item')) if (!items.has(r.id) && !r.resolved && this.assignees(r.id).includes(agentId)) items.set(r.id, new Set())
      return items.size ? { at: woken, items } : null
    }
    if (!items.size) return null
    const debounce = Number(this.agentParams(agentId).debounce_minutes ?? 0) * MINUTE
    return { at: last + debounce, items }
  }

  /**
   * Starts a bot's run, if it is due and not running: the items it is woken for,
   * each with its new events (or its whole history, the first time the bot sees
   * it). Its session is its own: resumed if it has one, otherwise new.
   */
  claim(agentId: string): { agent: Agent; items: { item: Item; events: ItemEvent[]; first: boolean }[]; resume: boolean } | null {
    const agent = this.agent(agentId)
    if (!agent || agent.deleted || this.running.has(agentId)) return null
    const wake = this.wake(agentId)
    if (!wake || wake.at > this.now()) return null
    const seen = new Set(this.agentRuns(agentId).flatMap((r) => r.item_ids))
    const items = [...wake.items].map(([itemId, ids]) => {
      const first = !seen.has(itemId)
      const events = first ? this.children(itemId) : this.children(itemId).filter((e) => ids.has(e.id))
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
   * A run is over. Its output's updates are applied, item by item: a message, the
   * events to add, the assignees, closing. Then each item it was woken for that is
   * still the bot's, and that it did not assign itself, goes back to the human,
   * unless the run left it waiting on something (a blocker it added). If the run
   * failed, each item says so, and goes back to the human.
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
        for (const u of updates) {
          if (!this.get(u.item)) throw new Error(`No item ${u.item}.`)
          u.events = this.prepare(u.events ?? [], 'bot')
          for (const to of u.assign ?? []) if (to !== HUMAN && !this.findAgent(to)) throw new Error(`No bot ${to}, to assign.`)
        }
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
      if (error) {
        for (const itemId of run.item_ids) {
          if (this.get(itemId)?.closed) continue
          this.add(itemId, [{ type: 'message', params: { text: `@${this.agentName(agentId)} failed: ${error}` } }], 'system', runId)
          this.handBack(itemId, agentId, runId)
        }
        return
      }
      for (const u of updates) {
        if (this.get(u.item)?.closed) continue
        const events: EventSpec[] = [...(u.message ? [{ type: 'message' as const, params: { text: u.message } }] : []), ...(u.events ?? [])]
        if (u.assign) {
          const to = new Set(u.assign.map((a) => (a === HUMAN ? HUMAN : this.findAgent(a)!.id)))
          const now = this.assignees(u.item)
          for (const a of now) if (!to.has(a)) events.push(...this.prepare([{ type: 'assign', params: { to: a, on: false } }], 'bot'))
          for (const a of to) if (!now.includes(a)) events.push(...this.prepare([{ type: 'assign', params: { to: a, on: true } }], 'bot'))
        }
        if (events.length) this.add(u.item, events, actor, runId)
        if (u.close) closing.push(u.item)
      }
      // An item it said who has is settled; the rest are settled here.
      const said = new Set(updates.filter((u) => u.assign).map((u) => u.item))
      for (const itemId of run.item_ids) if (!closing.includes(itemId) && !said.has(itemId) && !this.get(itemId)?.closed) this.settle(itemId, agentId, runId)
    })
    for (const itemId of closing) await this.close(itemId, { actor, runId })
    this.emit('change')
    await this.idle()
  }

  /**
   * After a run: an item the bot still has goes back to the human, unless the run
   * assigned it, or left it waiting on something other than the human (a wait it
   * added: it wakes when that resolves), or something new came in while it ran
   * (it wakes again for that).
   */
  private settle(itemId: string, agentId: string, runId: string): void {
    if (!this.assignees(itemId).includes(agentId)) return
    const added = new Set(
      this.store.children(itemId).filter((r) => this.store.traceOfEvent(r.id).some((t) => t.kind === 'event_added' && t.run_id === runId)).map((r) => r.id),
    )
    if (this.store.children(itemId).some((r) => added.has(r.id) && r.type === 'assign')) return
    const { blockers } = this.must(itemId)
    if (blockers.some((b) => added.has(b.id) && !this.waitsOnHuman(b, blockers))) return
    if (this.wake(agentId)?.items.has(itemId)) return
    this.handBack(itemId, agentId, runId)
  }

  /** Takes an item off a bot and gives it to the human. */
  private handBack(itemId: string, agentId: string, runId: string | null): void {
    const events: EventSpec[] = []
    if (this.assignees(itemId).includes(agentId)) events.push({ type: 'assign', params: { to: agentId, on: false, name: this.agentName(agentId) } })
    if (!this.assignees(itemId).includes(HUMAN)) events.push({ type: 'assign', params: { to: HUMAN, on: true } })
    if (events.length) this.add(itemId, events, `bot:${agentId}`, runId)
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

  viewed(itemId: string): void {
    this.mark(itemId, 'viewed', { actor: HUMAN })
  }

  skipped(itemId: string): void {
    this.mark(itemId, 'skipped', { actor: HUMAN })
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
        humanMessages: events.filter((e) => e.type === 'message' && e.createdBy === HUMAN).length,
        approvalsChanged: events.filter((e) => ((e.data?.changed as string[] | undefined) ?? []).length).length,
        cancelled: events.filter((e) => e.cancelled).length,
        timedOut: events.filter((e) => e.data?.outcome === 'timed_out').length,
        botFailures: events.filter((e) => e.type === 'message' && e.createdBy === 'system' && / failed: /.test(String(e.params.text))).length,
        handledOutside: item.how === 'handled_outside',
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
    // An unsafe action run straight away (not through an approval) is part of the item's history.
    if (action.unsafe && ctx.itemId && this.get(ctx.itemId) && !ctx.approving)
      this.add(ctx.itemId, [{ type: 'action', params: { name, inputs: parsed.data, output: out } }], ctx.actor)
    return out
  }

  // ---- events ----

  /** Checks events and expands them as their types say, without touching the store. */
  prepare(specs: EventSpec[], author: Author): EventSpec[] {
    return specs.flatMap((s) => {
      // From before bots were items: a bot event is an assign to the default bot, with its instructions said first.
      if (s?.type === 'bot') {
        const to = this.defaultAgent()
        if (!to) throw new Error('There is no bot to assign: make one on the Bots page.')
        const said = s.params?.instructions ? [{ type: 'message' as const, params: { text: String(s.params.instructions) } }] : []
        return this.prepare([...said, { type: 'assign', params: { to: to.id }, ref: s.ref }], author)
      }
      const type = this.type(s?.type)
      if (author === 'bot' && !type.bots) throw new Error(`A bot may not add a ${s.type} event.`)
      const parsed = z.object(type.params).safeParse(s.params ?? {})
      if (!parsed.success)
        throw new Error(`Bad params for a ${s.type} event: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
      const params = parsed.data as Record<string, unknown>
      if (s.type === 'approval' && !this.actions.find((a) => a.name === params.action)?.unsafe)
        throw new Error(`${params.action} is not an unsafe action, so it needs no approval.`)
      // A bot is named by its id from here on; its name is kept beside it, for reading.
      if (s.type === 'assign' && params.to !== HUMAN) {
        const to = this.findAgent(String(params.to))
        if (!to) throw new Error(`No bot ${params.to}, to assign. Assign 'human' or one of: ${this.agents().map((a) => a.name).join(', ') || 'none yet'}.`)
        Object.assign(params, { to: to.id, name: to.name })
      }
      if (s.type === 'watch') {
        const to = this.findAgent(String(params.agent))
        if (!to) throw new Error(`No bot ${params.agent}, to watch.`)
        Object.assign(params, { agent: to.id, name: to.name })
      }
      // Prepared now, so a bad one fails the whole list; added in the item as it starts.
      if (s.type === 'item' && params.events) params.events = this.prepare(params.events as EventSpec[], author)
      const expanded = type.expand ? type.expand(params, author) : [{ type: s.type, params }]
      return expanded.map((e, i) => ({ type: e.type, params: e.params, ref: i === 0 ? (s.ref ?? e.ref) : e.ref }))
    })
  }

  /**
   * Adds events, already prepared, in an item (or, for items, bots and directives, in none). Plain ones
   * resolve at once; blockers wait, and those whose type has a start hook start
   * right after. An item whose key is already taken is left out. Answers the ids added.
   */
  add(parentId: string | null, specs: EventSpec[], createdBy: string, runId: string | null = null): string[] {
    return this.store.batch(() => this.addNow(parentId, specs, createdBy, runId))
  }

  private addNow(parentId: string | null, specs: EventSpec[], createdBy: string, runId: string | null): string[] {
    // A detached item sits in no item, wherever it is added; so does a directive.
    const detached = parentId ? specs.filter((s) => (s.type === 'item' && s.params.detached) || s.type === 'directive') : []
    if (detached.length)
      return [...this.addNow(parentId, specs.filter((s) => !detached.includes(s)), createdBy, runId), ...this.addNow(null, detached, createdBy, runId)]
    const keyed = (s: EventSpec) => s.type === 'item' && s.params.key && this.store.ofType('item').some((r) => r.params.key === s.params.key)
    specs = specs.filter((s) => !keyed(s))
    if (!parentId && specs.some((s) => !['item', 'directive', 'agent'].includes(s.type))) throw new Error('Only an item, a bot or a directive can sit in no item.')
    if (parentId && this.row(parentId)?.type === 'agent' && specs.some((s) => s.type !== 'configure')) throw new Error('A bot holds only its configure events.')
    if (specs.some((s) => s.type === 'configure') && this.row(parentId ?? '')?.type !== 'agent') throw new Error('A configure event goes in a bot.')
    // An edit of a directive replaces one that still holds.
    for (const s of specs)
      if (s.type === 'directive' && s.params.replaces) {
        const old = this.event(String(s.params.replaces))
        if (old?.type !== 'directive' || old.resolved) throw new Error(`No directive ${s.params.replaces} holds, to replace.`)
      }
    const ids = new Map<string, string>()
    const rows = specs.map((s) => {
      const id = randomUUID()
      if (s.ref) ids.set(s.ref, id)
      return { id, spec: s }
    })
    const seq = this.store.nextSeq(parentId)
    const at = this.now()
    rows.forEach(({ id, spec }, i) => {
      const type = this.type(spec.type)
      const params = resolveRefs(spec.params, ids) as Record<string, unknown>
      this.store.insertEvent({
        id, parent_id: parentId, seq: seq + i, type: spec.type, params, created_by: createdBy, created_at: at,
        resolved: !type.blocking, resolved_at: type.blocking ? null : at, resolved_by: type.blocking ? null : createdBy, cancelled: false,
        started: !type.start, data: null, memo: null, check_step: 0,
        next_check_at: type.check ? at + this.scheduleOf(type, params)[0] * 1000 : null, last_error: null,
      })
      const where = parentId ?? id
      this.mark(where, 'event_added', { eventId: id, runId, actor: createdBy, data: { type: spec.type } })
      if (!type.blocking) this.mark(where, 'event_resolved', { eventId: id, runId, actor: createdBy })
      if (spec.type === 'directive' && params.replaces) this.resolve(String(params.replaces), { cancelled: true, replacedBy: id }, createdBy, true, runId)
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
        try {
          const ctx = { engine: this, item: this.must(event.parentId ?? event.id), event, author: authorOf(event.createdBy), memo: null, setMemo: () => {} }
          const out = await this.type(event.type).start!(event.params, ctx)
          if (out?.params) this.store.updateEvent(event.id, { params: out.params })
          if (out?.resolve) this.resolve(event.id, out.resolve, 'system')
        } catch (e) {
          this.resolve(event.id, { outcome: 'error', error: (e as Error).message, detail: `Could not start: ${(e as Error).message}` }, 'system')
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

  // ---- the human ----

  /** A new item, in `parentId` or in none, with events to start it off. */
  async create(
    title: string,
    opts: { parentId?: string | null; key?: string; events?: EventSpec[]; actor?: string; author?: Author } = {},
  ): Promise<string | null> {
    const author = opts.author ?? 'human'
    const [spec] = this.prepare([{ type: 'item', params: { title, ...(opts.key ? { key: opts.key } : {}), ...(opts.events ? { events: opts.events } : {}) } }], author)
    const [id] = this.add(opts.parentId ?? null, [spec], author === 'bot' ? 'bot' : (opts.actor ?? HUMAN))
    await this.idle()
    return id ?? null
  }

  /**
   * The chat input: `text` through a preset, as the human. Bots `[id]()`-referred
   * to in the text are assigned the item, as is the bot an `assign:<id>` preset
   * names; then the human is taken off it.
   */
  async send(itemId: string, presetId: string, text: string): Promise<void> {
    const assignTo = presetId.startsWith('assign:') ? presetId.slice('assign:'.length) : null
    const preset = this.presets.find((p) => p.id === (assignTo ? 'message' : presetId))
    if (!preset) throw new Error(`No preset ${presetId}.`)
    if (this.must(itemId).closed) throw new Error('This item is closed.')
    const bots = [...new Set([...(assignTo ? [assignTo] : []), ...refsIn(text).filter((id) => this.agent(id) && !this.agent(id)!.deleted)])]
    const events = this.prepare(preset.build(text), 'human')
    const now = this.assignees(itemId)
    const assigns: EventSpec[] = bots.filter((b) => !now.includes(b)).map((b) => ({ type: 'assign', params: { to: b, on: true } }))
    // Handing it to a bot takes it off the human.
    if (bots.length && now.includes(HUMAN)) assigns.push({ type: 'assign', params: { to: HUMAN, on: false } })
    if (!events.length && !assigns.length && !bots.length) throw new Error('Nothing to send.')
    this.add(itemId, [...events, ...this.prepare(assigns, 'human')], HUMAN)
    // Already the bot's, with nothing new to say: wake it anyway.
    if (!events.length && !assigns.some((a) => a.params.to !== HUMAN)) for (const b of bots) this.wakeAgent(b)
    await this.idle()
  }

  /** Adds events in an item (or items in none): from the human, or from an agent through MCP (author 'bot', so its rules apply). */
  async addEvents(parentId: string | null, specs: EventSpec[], actor = HUMAN, author: Author = 'human', runId: string | null = null): Promise<string[]> {
    if (parentId && this.row(parentId)?.type !== 'agent' && this.must(parentId).closed) throw new Error('This item is closed.')
    const ids = this.add(parentId, this.prepare(specs, author), actor, runId)
    // A script is run from inside an item's queue, so waiting for the queues to empty would wait for itself.
    if (actor !== 'script') await this.idle()
    return ids
  }

  /**
   * Attaches an event (any, items included) to an item, to gather it there: an
   * attachment event in the item points at it. The event stays where it was.
   */
  async attach(eventId: string, targetId: string, actor = HUMAN, author: Author = 'human', runId: string | null = null): Promise<string> {
    const e = this.event(eventId)
    if (!e) throw new Error(`No event ${eventId}.`)
    if (this.event(targetId)?.type !== 'item') throw new Error(`No item ${targetId}.`)
    if (eventId === targetId) throw new Error('An item can not be attached to itself.')
    const params = { event: e.id, title: e.summary, item: e.type === 'item' ? e.id : e.parentId }
    const [id] = await this.addEvents(targetId, [{ type: 'attachment', params }], actor, author, runId)
    return id
  }

  /** Runs an approval's action with the human's inputs, which resolves it. */
  async approve(eventId: string, inputs: Record<string, unknown>): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.type !== 'approval' || event.resolved) throw new Error('No approval waiting with that id.')
    const proposed = event.params.inputs as Record<string, unknown>
    const changed = Object.keys({ ...proposed, ...inputs }).filter((k) => String(inputs[k] ?? '') !== String(proposed[k] ?? ''))
    const name = String(event.params.action)
    const output = await this.callAction(name, inputs, { engine: this, author: 'human', actor: HUMAN, itemId: event.parentId, runId: null, approving: true })
    this.resolve(eventId, { ran: true, inputs, changed, output }, HUMAN)
    this.emit('change')
    await this.idle()
  }

  /** Ends a blocker without it happening; an item is closed as cancelled, and a bot deleted. */
  async cancel(eventId: string, actor = HUMAN): Promise<void> {
    const event = this.event(eventId)
    if (!event || event.resolved) throw new Error('That event is not waiting.')
    if (event.type === 'item') return this.close(eventId, { actor, how: 'cancelled' })
    if (event.type === 'agent') return this.deleteAgent(eventId, actor)
    this.resolve(eventId, { cancelled: true }, actor, true)
    this.emit('change')
    if (actor !== 'script') await this.idle()
  }

  /** Closes an item: its open children are cancelled (child items closed, all the way down), then it resolves. */
  async close(itemId: string, opts: { actor?: string; how?: 'closed' | 'handled_outside' | 'cancelled'; runId?: string } = {}): Promise<void> {
    const actor = opts.actor ?? HUMAN
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

  handledOutside(itemId: string): Promise<void> {
    return this.close(itemId, { how: 'handled_outside' })
  }

  // ---- the loop ----

  /** One turn of the central loop: every blocker check that is due, then every source that is. */
  async tick(): Promise<void> {
    await this.checkDue()
    await this.pollSources(false)
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

  async pollSources(force: boolean): Promise<void> {
    for (const source of this.sources) {
      const last = this.lastPoll.get(source.id) ?? 0
      if (!force && this.now() - last < source.everyMinutes * MINUTE) continue
      this.lastPoll.set(source.id, this.now())
      try {
        this.add(null, this.prepare(await source.poll(this), 'human'), source.id)
        await this.idle()
      } catch (e) {
        this.emit('sourceError', `${source.id}: ${(e as Error).message}`)
      }
    }
  }
}

/** When an item's snoozes wake it: the latest, or null when none blocks it. */
const snoozedUntil = (blockers: ItemEvent[]): number | null => {
  const times = blockers.filter((b) => b.type === 'snooze').map((b) => Date.parse(String(b.params.until)))
  return times.length ? Math.max(...times) : null
}
