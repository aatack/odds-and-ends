import { BUILTIN_SECTIONS, DEFAULT_SECTIONS, matches, type SearchRow, type SearchSection } from '../shared/search'
import { runScript } from './script'
import { github } from './github'
import { slackActions } from './slack'
import { parseSlackLink } from '../shared/slack'
import { dueAt } from './wakes'
import { CustomActions } from './customActions'
import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { mkdirSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { z } from 'zod'
import type { Agent, ItemInfoDetail, ItemSpec, Item, ItemDetail, ItemInfo, LastRun, Overview, User } from '../shared/types'
import { current, itemActions } from './actions'
import { ITEM_TYPES, whenNext } from './itemTypes'
import { execCli, type Action, type ActionContext, type Author, type ItemType, type Exec, type Integration } from './integrations'
import { PRESETS, type Preset } from './presets'
import { plainSecrets, Settings, type Secrets } from './settings'
import { STATE_KEYS, Store, type ItemRow, type Inspection } from './store'

/** What the app keeps on items for itself: never set by `setValue`. */
const OWN_KEYS = new Set(['session_id', 'seen', 'last_run', 'last_wake', 'handoff', 'key'])

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
  /** Items to add in it: waits, approvals, child tasks. */
  items?: ItemSpec[]
  /** Who has the task from now on: people by username, bots by name or id. Left out: see Engine.settle. */
  assign?: string[]
  close?: boolean
}

/** What a bot's run returns, as its structured output. */
export interface BotOutput {
  updates?: BotUpdate[]
  /** Done for now: a few lines for its next run, which starts a fresh session with them. */
  handoff?: string
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
  /** Where the bot stood before it: put back if the run never really happened (a usage limit). */
  before: { mark: Mark | undefined; seen: string[] }
}

/** A person added it, or the system did; a bot, or a bot's script, is a bot. */
const authorOf = (createdBy: string): Author => (createdBy === 'bot' || createdBy === 'script:bot' || createdBy.startsWith('bot:') ? 'bot' : 'human')

/** `[id]()` in text: a reference to an item (a task, a bot, or a person by their username). */
/** The item a URL is pulled in as: a Slack message, a Slack channel (its last day), or a link. */
function pulledIn(url: string, at: number): ItemSpec {
  if (parseSlackLink(url)) return { type: 'slack_message', params: { link: url } }
  const channel = /^https:\/\/[\w-]+(?:\.enterprise)?\.slack\.com\/archives\/([CGD][A-Z0-9]+)\/?(?:[?#].*)?$/.exec(url)
  if (channel) return { type: 'slack_channel', params: { channel: channel[1], oldest: new Date(at - 24 * 60 * MINUTE).toISOString() } }
  return { type: 'link', params: { url } }
}

export const refsIn = (text: string): string[] => [
  ...[...text.matchAll(/\[([\w.-]+)\]\(\)/g)].map((m) => m[1]),
  // `[label](id)`, as markdown writes a link, refers to the item `id` too.
  ...[...text.matchAll(/\[[^\]]*\]\(([\w.-]+)\)/g)].map((m) => m[1]),
]

/** Every URL a text mentions: bare, `[label](url)`, or `[url]()`; each once, in order. */
export const urlsIn = (text: string): string[] => [...new Set([...text.matchAll(/https?:\/\/[^\s<>()\[\]]+[^\s<>()\[\].,;:!?'"]/g)].map((m) => m[0]))]

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
  /** The integrations, each in the one shape (integrations.ts): their tools are among the actions. */
  readonly integrations: Integration[] = [github]
  readonly actions: Action[] = [...itemActions, ...slackActions, ...this.integrations.flatMap((i) => i.tools)]
  readonly itemTypes: ItemType[] = ITEM_TYPES
  readonly presets: Preset[] = PRESETS
  /** People's own actions, per item type (the Types page). */
  readonly customActions: CustomActions
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
  /** The items being checked right now: a check never starts while the last is still going. */
  private checking = new Set<string>()
  /** Who pressed play on each item about to run (`runNow`): its next check is theirs. */
  private pressed = new Map<string, string>()
  private queues = new Map<string, Promise<unknown>>()

  constructor(opts: EngineOptions = {}) {
    super()
    this.store = opts.store ?? new Store(null, opts.now)
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
    for (const r of this.store.items.values()) {
      if (r.resolved || !this.typeOrNull(r.type)?.check) continue
      if (r.type === 'script') continue
      if (this.scheduleOf(this.type(r.type), r.params)) r.next_check_at = this.now()
    }
  }

  type(name: string): ItemType {
    const t = this.typeOrNull(name)
    if (!t) throw new Error(`No item type ${name}: one of ${this.itemTypes.map((t) => t.type).join(', ')}.`)
    return t
  }

  private typeOrNull(name: string): ItemType | null {
    return this.itemTypes.find((t) => t.type === name) ?? null
  }

  /** Whether items of a type are blockers: for the loader. */
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

  /** A row as the shared item shape, its text filled in by its type. */
  private toInfo(r: ItemRow): ItemInfo {
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
      containers: this.store.containers(r.id),
    }
  }

  private row(id: string): ItemRow | undefined {
    return this.store.items.get(id)
  }

  info(id: string): ItemInfo | null {
    const r = this.row(id)
    return r ? this.toInfo(r) : null
  }

  /** The items in an item, oldest first; only the open ones (its blockers) with `open`. */
  children(itemId: string, open = false): ItemInfo[] {
    return this.store
      .children(itemId)
      .filter((r) => !open || !r.resolved)
      .map((r) => this.toInfo(r))
  }

  /** A task's title (a bot's: its @name). */
  private title(id: string, params: Record<string, unknown>): string {
    return this.row(id)?.type === 'bot' ? `@${this.agentName(id)}` : String(params.title)
  }

  /** Who a task is assigned to: its `assigned_to` links, the earliest first. */
  assignees(itemId: string): string[] {
    return this.store.assignees(itemId)
  }

  private toItem(r: ItemRow, viewer = this.me()): Item {
    const id = r.id
    const kids = this.store.children(id)
    // Neither a script nor a task sits in another as a dependency: a task waits on something by being assigned to it.
    const blockers = kids.filter((k) => !k.resolved && k.type !== 'script' && k.type !== 'task').map((k) => this.toInfo(k))
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
  private waitsOnHuman(b: ItemInfo): boolean {
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
      // The most recently touched first: what is moving stays on top.
      .sort((a, b) => b.since - a.since)
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

  /** A task in full; or a bot, its own chat being its items. */
  detail(id: string, viewer = this.me()): ItemDetail | null {
    const r = this.row(id)
    const item = r?.type === 'task' || r?.type === 'bot' ? this.toItem(r, viewer) : null
    if (!item) return null
    const parent = item.parentId ? this.get(item.parentId) : null
    // What it contains by a later link shows too, where it was linked; its own parent stays where it sits.
    const linked = this.store.linked(id).flatMap((l) => (this.row(l.id) ? [{ e: this.toInfo(this.row(l.id)!), at: l.at }] : []))
    const own = this.children(id).map((e) => ({ e, at: e.createdAt }))
    const children = [...own, ...linked].sort((a, b) => a.at - b.at).map((x) => x.e)
    return { item, parent: parent && { id: parent.id, title: parent.title }, children }
  }

  /** Any item, with what its views need. */
  itemInfoDetail(id: string, viewer = this.me()): ItemInfoDetail | null {
    const info = this.info(id)
    if (!info) return null
    const parent = info.parentId ? this.get(info.parentId) : null
    return {
      info,
      parent: parent && { id: parent.id, title: parent.title },
      item: info.type === 'task' || info.type === 'bot' ? this.detail(id, viewer) : null,
    }
  }

  /** An item as egdb holds it: its values and links, and who made it and when. */
  inspect(id: string): Promise<Inspection> {
    return this.store.inspect(id)
  }

  /**
   * What needs `viewer` next, of the open tasks assigned to them that nothing but approvals blocks: the one most
   * recently touched, so the few in hand get closed out before the rest come round.
   */
  next(skip: string[] = [], viewer = this.me()): string | null {
    const open = this.store
      .assignedTo(viewer)
      .flatMap((id) => this.get(id, viewer) ?? [])
      .filter((i) => i.withYou && !skip.includes(i.id))
    return open.sort((a, b) => b.since - a.since)[0]?.id ?? null
  }

  /** Renames a task: its `title` value changes (egdb keeps the old one, and who changed it). */
  async rename(itemId: string, title: string, actor = this.me()): Promise<void> {
    const t = title.trim()
    if (!t || this.must(itemId).title === t) return
    this.store.setParams(itemId, { ...this.row(itemId)!.params, title: t }, actor, this.now())
    this.emit('change')
  }

  /**
   * Runs a tool for a person, from a task's chat input (`/`): any action, through `callAction` as theirs (so unsafe ones
   * too: they pressed it). What it answered, or why it failed, is a note in the task.
   */
  async runTool(itemId: string, name: string, args: Record<string, unknown>, actor = this.me()): Promise<unknown> {
    let out: unknown
    try {
      out = await this.callAction(name, args, { engine: this, author: this.isUser(actor) ? 'human' : 'bot', actor, itemId, runId: null })
    } catch (e) {
      this.add(itemId, [{ type: 'note', params: { text: `Ran \`${name}\`: it failed: ${(e as Error).message}` } }], actor)
      throw e
    }
    const shown = typeof out === 'string' ? out : JSON.stringify(out, null, 2)
    const text = shown && shown !== 'null' ? `Ran \`${name}\`:\n\n\`\`\`${typeof out === 'string' ? '' : 'json'}\n${shown.length > 3000 ? `${shown.slice(0, 3000)}\n…` : shown}\n\`\`\`` : `Ran \`${name}\`.`
    this.add(itemId, [{ type: 'note', params: { text } }], actor)
    await this.idle()
    return out
  }

  /** The search page's sections, as kept on the `search` item; the defaults when there is none. */
  searchSections(): SearchSection[] {
    const r = this.store.ofType('search')[0]
    return r && Array.isArray(r.params.sections) ? (r.params.sections as SearchSection[]) : DEFAULT_SECTIONS
  }

  /** Replaces the search page's sections (null: the defaults again), checking each; the `search` item is made the first time. */
  async setSearchSections(sections: SearchSection[] | null, actor = this.me()): Promise<void> {
    const next = sections ?? DEFAULT_SECTIONS
    for (const s of next) {
      if (s.kind === 'builtin' && !(BUILTIN_SECTIONS as readonly string[]).includes(s.id)) throw new Error(`No built-in section ${s.id}: one of ${BUILTIN_SECTIONS.join(', ')}.`)
      else if (s.kind === 'query') matches({}, s.filter)
      else if (s.kind === 'function' && !String(s.source ?? '').trim()) throw new Error(`The function section ${s.label} has no source.`)
      else if (!['builtin', 'query', 'function'].includes(s.kind)) throw new Error(`A section's kind is builtin, query or function, not ${(s as { kind: string }).kind}.`)
    }
    const r = this.store.ofType('search')[0]
    if (r) this.store.setParams(r.id, { ...r.params, sections: next }, actor, this.now())
    else this.add(null, [{ type: 'search', params: { sections: next } }], actor)
    this.emit('change')
  }

  /** A query's or a function's rows, for what is typed (a function is told it; a query's rows are filtered by the finder). */
  async runSearchSection(section: SearchSection, text: string): Promise<SearchRow[]> {
    if (section.kind === 'query') {
      const rows = [...this.store.items.values()]
        .filter((r) => r.type !== 'search' && matches(this.valuesOf(r), section.filter))
        .sort((a, b) => b.created_at - a.created_at)
        .slice(0, section.limit ?? 50)
      return rows.map((r) => {
        const e = this.toInfo(r)
        return { id: r.id, type: r.type, left: e.summary, right: r.resolved && this.type(r.type).blocking ? (r.cancelled ? 'cancelled' : 'closed') : '' }
      })
    }
    if (section.kind === 'function') {
      const ran = await runScript(section.source, { text, now: new Date(this.now()).toISOString() }, (name, args) =>
        this.callAction(name, args, { engine: this, author: 'bot', actor: 'script', itemId: null, runId: null }),
      )
      if (!Array.isArray(ran.value)) throw new Error(`${section.label}: it returned ${JSON.stringify(ran.value)?.slice(0, 100)}, not a list of rows.`)
      return (ran.value as SearchRow[]).filter((x) => x && typeof x.id === 'string').map((x) => ({ id: x.id, type: String(x.type ?? 'task'), left: String(x.left ?? x.id), right: x.right ? String(x.right) : '' }))
    }
    return []
  }

  /** An item's values, as a query reads them: its type and params, how it stands, and keys made from its links. */
  private valuesOf(r: ItemRow): Record<string, unknown> {
    return {
      ...r.params,
      type: r.type,
      resolved: r.resolved,
      cancelled: r.cancelled,
      parent: r.parent_id,
      assignees: r.type === 'task' ? this.assignees(r.id) : [],
      created_by: r.created_by,
      created_at: r.created_at,
    }
  }

  /** Changes a widget's state: a value on its event (egdb keeps every one before). */
  async setWidgetState(itemId: string, state: unknown, actor = this.me()): Promise<void> {
    if (this.row(itemId)?.type !== 'widget') throw new Error(`No widget ${itemId}.`)
    return this.setValue(itemId, 'widget_state', state, actor)
  }

  /**
   * Sets one value on an item (null clears it): any of its type's params (a task's title, a widget's state, a bot's
   * instructions, the search sections), checked against them. Not how it stands (`type`, `resolved` and the like: those
   * have their own ways), nor what the app keeps for itself on a bot. egdb keeps every value it had. Whatever shows it
   * is told at once (every change is).
   */
  async setValue(id: string, key: string, value: unknown, actor = this.me()): Promise<void> {
    const row = this.row(id)
    if (!row) throw new Error(`No item ${id}.`)
    if (STATE_KEYS.has(key) || OWN_KEYS.has(key)) throw new Error(`${key} can't be set this way.`)
    const type = this.type(row.type)
    if (!(key in type.params)) throw new Error(`A ${row.type} has no ${key}: it has ${Object.keys(type.params).join(', ')}.`)
    const next = { ...row.params, [key]: value ?? undefined }
    const parsed = z.object(type.params).safeParse(next)
    if (!parsed.success) throw new Error(`Bad ${key}: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
    if (JSON.stringify(row.params[key] ?? null) === JSON.stringify(value ?? null)) return
    this.store.setParams(id, Object.fromEntries(Object.entries(next).filter(([, v]) => v !== undefined)), actor, this.now())
    // A bot's settings changing is something happening to it.
    if (row.type === 'bot') this.configured.set(id, { n: this.store.moment(), at: this.now() })
    this.emit('change')
  }

  /** Turns `actor`'s reaction (a thumbs-up) to an item on or off. */
  async react(itemId: string, on: boolean, actor = this.me()): Promise<void> {
    const row = this.row(itemId)
    if (!row) throw new Error(`No item ${itemId}.`)
    if (row.reactions.includes(actor) === on) return
    this.store.setReactions(itemId, on ? [...row.reactions, actor] : row.reactions.filter((r) => r !== actor), actor, this.now())
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
    await this.addItems(null, [{ type: 'user', params: { username: id, ...(name.trim() ? { name: name.trim() } : {}) } }], this.isUser(actor) ? actor : id)
    return this.user(id)!
  }

  /** Signing in: the person with this username, made if there is nobody by it yet. */
  async signIn(username: string): Promise<User> {
    const id = username.trim().toLowerCase()
    return this.user(id) ?? this.saveUser(id, '', id)
  }

  /** A person or a bot, by a username, a bot's name, or an id; or an error saying who there is. */
  /**
   * Who can hold a task, by any name for it: the id of an open item that can (a person by username, a bot by name, with or
   * without the @, or id; an open script by id); null if there is none. Which of them it is doesn't matter to holding.
   */
  holder(ref: string): string | null {
    const r = ref.trim().replace(/^@/, '')
    if (this.user(r.toLowerCase())) return r.toLowerCase()
    return this.findAgent(r)?.id ?? (this.isOpenScript(r) ? r : null)
  }

  resolveAssignee(ref: string): string {
    const id = this.holder(ref)
    if (id) return id
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
    if (on && this.holder(to) !== to) throw new Error(`Nobody is ${to}, to assign.`)
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
      cycleMinutes: Number(p.cycle_minutes ?? PERIODIC_MINUTES),
      folder: p.folder ? String(p.folder) : join(this.dataDir, 'bots', `${slug(name)}-${id.slice(0, 8)}`),
      sessionId: (p.session_id as string) ?? null,
      lastRun: (p.last_run as LastRun) ?? null,
      createdAt: r.created_at,
      deleted: r.resolved,
      runId: this.running.get(id)?.id ?? null,
      wakesAt: r.resolved ? null : (this.wake(id)?.at ?? null),
      pausedUntil: this.pausedUntil(String(p.provider ?? 'claude')),
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
    a: { id?: string; name: string; instructions: string; provider: string; model: string; debounceMinutes: number; cycleMinutes?: number; folder?: string },
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
      ...(a.cycleMinutes !== undefined && String(a.cycleMinutes) !== '' && Number.isFinite(Number(a.cycleMinutes)) ? { cycle_minutes: Math.max(0, Number(a.cycleMinutes)) } : {}),
    }
    if (!a.id) {
      const [id] = await this.addItems(null, [{ type: 'bot', params }], actor)
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
   * open task it has, each with the items new in it since the bot was last prompted (less what the bot itself did,
   * but a child task closing is always news). The rule is `dueAt` (wakes.ts), the same as a script's: once anything
   * affected it since it last woke (a task assigned to it, anything new in its tasks or its chat, its settings), its
   * debounce after; or its cycle (`cycle_minutes`, PERIODIC_MINUTES by default) after it last woke, while it has
   * tasks. Woken by hand, at once. Null when nothing calls for it.
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
      // It is a reason to wake only if it was just given (after a fresh start, its old tasks are news when they move).
      if (!seen.includes(itemId)) {
        fresh.add(itemId)
        if (after(given.n, given.at, mark)) activity = Math.max(activity, given.at)
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
    const p = this.agentParams(agentId)
    const edited = Math.max(trigger, activity)
    const at = dueAt({
      lastWake: lastStart,
      edited: edited || null,
      debounceMs: Number(p.debounce_minutes ?? DEFAULT_DEBOUNCE) * MINUTE,
      cycleMs: Number(p.cycle_minutes ?? PERIODIC_MINUTES) * MINUTE || null,
      hasWork: assigned.length > 0,
    })
    const due = [...(woken !== undefined ? [woken] : []), ...(at !== null ? [at] : [])]
    return due.length ? { at: Math.min(...due), items, fresh } : null
  }

  /**
   * Starts a bot's run, if it is due and not running: its chat and every task it has, each with its new items (or,
   * for a task its session has not seen, its whole history). Its session is its own: resumed if it has one, otherwise new.
   */
  claim(agentId: string): { agent: Agent; items: { item: Item; children: ItemInfo[]; first: boolean; own?: boolean }[]; resume: boolean; handoff: string | null } | null {
    const agent = this.agent(agentId)
    if (!agent || agent.deleted || this.running.has(agentId)) return null
    const wake = this.wake(agentId)
    if (!wake || wake.at > this.now()) return null
    const seen = this.seen(agentId)
    const items = [...wake.items].map(([itemId, ids]) => {
      if (itemId === agentId) return { item: this.toItem(this.row(agentId)!), children: this.children(agentId).filter((e) => ids.has(e.id)), first: false, own: true }
      const first = !seen.includes(itemId)
      const children = first ? this.children(itemId) : this.children(itemId).filter((e) => ids.has(e.id))
      return { item: this.must(itemId), children, first }
    })
    const resume = agent.sessionId !== null
    // A fresh session hears the handoff its last one left, once.
    const handoff = !resume && typeof this.agentParams(agentId).handoff === 'string' ? String(this.agentParams(agentId).handoff) : null
    if (!resume) {
      this.setParam(agentId, 'session_id', randomUUID(), `bot:${agentId}`)
      this.setParam(agentId, 'seen', [], `bot:${agentId}`)
      if (handoff !== null) this.setParam(agentId, 'handoff', null, `bot:${agentId}`)
    }
    this.woken.delete(agentId)
    return { agent: this.agent(agentId)!, items, resume, handoff }
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
    this.running.set(agentId, { id, agentId, itemIds, startedAt: at, assigned: new Set(), before: { mark: this.marks.get(agentId), seen: this.seen(agentId) } })
    this.marks.set(agentId, { n: this.store.moment(), at })
    const lastRun: LastRun = { id, startedAt: at, endedAt: null, items: itemIds, costUsd: null, turns: null, error: null, output: null }
    this.setParam(agentId, 'last_run', lastRun, `bot:${agentId}`)
    const seen = this.seen(agentId)
    const unseen = itemIds.filter((i) => i !== agentId && !seen.includes(i))
    if (unseen.length) this.setParam(agentId, 'seen', [...seen, ...unseen], `bot:${agentId}`)
    this.emit('change')
    return id
  }

  /** Until when each provider's bots wait out a usage limit: the runner runs none of them till then. In memory only. */
  private paused = new Map<string, number>()

  pause(provider: string, until: number): void {
    this.paused.set(provider, Math.max(until, this.paused.get(provider) ?? 0))
    this.emit('change')
  }

  pausedUntil(provider: string): number | null {
    const until = this.paused.get(provider)
    return until && until > this.now() ? until : null
  }

  /** A run still going: its bot and its tasks; or null. */
  activeRun(runId: string): { agentId: string; itemIds: string[] } | null {
    const run = [...this.running.values()].find((r) => r.id === runId)
    return run ? { agentId: run.agentId, itemIds: run.itemIds } : null
  }

  /**
   * A run is over. The bot's `last_run` says how it went. Its output's updates are applied, task by task: a note, the
   * items to add, who has it, closing. Then each task it had that it said nothing of who has goes back to its owner
   * (`settle`). If the run failed, each task it has says so, and goes back to its owner.
   */
  async endRun(
    runId: string,
    end: { exitCode: number | null; structured?: unknown; output?: string; costUsd?: number; turns?: number; error?: string; limited?: boolean },
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
    let handoff: string | null = null
    if (!error) {
      try {
        const out = z
          .object({
            updates: z
              .array(
                z.object({
                  item: z.string(),
                  message: z.string().optional(),
                  items: z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })).optional(),
                  // The old name (from when items were called items).
                  events: z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional() })).optional(),
                  assign: z.array(z.string()).optional(),
                  close: z.boolean().optional(),
                }),
              )
              .optional(),
            handoff: z.string().optional(),
          })
          .parse(end.structured ?? {})
        updates = (out.updates ?? []).map(({ events, ...u }) => ({ ...u, items: [...(u.items ?? []), ...(events ?? [])] })) as BotUpdate[]
        handoff = out.handoff?.trim() || null
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
      // Stopped by a usage limit, the run never really happened: the bot keeps its tasks, and what was new stays new, for
      // when the runner lets it run again.
      if (error && end.limited) {
        if (run.before.mark) this.marks.set(agentId, run.before.mark)
        else this.marks.delete(agentId)
        this.setParam(agentId, 'seen', run.before.seen, actor)
        return
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
      for (const u of updates) {
        if (this.get(u.item)?.closed) continue
        try {
          // Each update is checked on its own: one that is wrong fails only its task.
          const to = u.assign && u.item !== agentId ? new Set(u.assign.map((a) => this.resolveAssignee(a))) : null
          if (to && !to.has(agentId) && !u.close && heard(u.item)) {
            to.add(agentId)
          }
          const specs: ItemSpec[] = [...(u.message ? [{ type: 'note' as const, params: { text: u.message } }] : []), ...this.prepare(u.items ?? [], 'bot')]
          if (specs.length) this.add(u.item, specs, actor)
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
    })
    // Done for now: its next run starts a fresh session (with the handoff said once), rather than carrying this one on.
    if (handoff && !error && !this.row(agentId)?.resolved) {
      this.setParam(agentId, 'handoff', handoff, actor)
      this.setParam(agentId, 'session_id', null, actor)
      this.setParam(agentId, 'seen', [], actor)
    }
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
  async callAction(called: string, given: Record<string, unknown>, ctx: ActionContext): Promise<unknown> {
    // Old names and parameters (from when items were called items) are taken as the new.
    const { name, args } = current(called, given ?? {})
    const action = this.actions.find((a) => a.name === name)
    if (!action) throw new Error(`No action ${name}.`)
    // The rule on unsafe actions (see Action in integrations.ts), checked here
    // too, whatever the caller was shown.
    if (action.unsafe && ctx.author !== 'human')
      throw new Error(`${name} is unsafe, so bots, and scripts bots wrote, may not run it. Ask a person: add an approval item for it.`)
    const parsed = z.object(action.input).safeParse(args ?? {})
    if (!parsed.success) throw new Error(`Bad arguments for ${name}: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
    return action.run(parsed.data, ctx)
  }

  // ---- items ----

  /** Checks items and expands them as their types say, without touching the store. */
  prepare(specs: ItemSpec[], author: Author): ItemSpec[] {
    return specs.flatMap((s) => {
      const type = this.type(s?.type)
      if (s.type === 'process') throw new Error('Processes are switched off for now.')
      if (author === 'bot' && !type.bots) throw new Error(`A bot may not add a ${s.type} item.`)
      // A task's first items, by their old name (from when items were called items).
      const given = s.type === 'task' && s.params && 'events' in s.params && !('items' in s.params) ? { ...s.params, items: s.params.events } : s.params
      const parsed = z.object(type.params).safeParse(given ?? {})
      if (!parsed.success)
        throw new Error(`Bad params for a ${s.type} item: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
      const params = parsed.data as Record<string, unknown>
      if (s.type === 'approval' && !this.actions.find((a) => a.name === params.action)?.unsafe)
        throw new Error(`${params.action} is not an unsafe action, so it needs no approval.`)
      // Who a new task goes to, as ids.
      if (s.type === 'task' && params.assignees) params.assignees = (params.assignees as string[]).map((a) => this.resolveAssignee(a))
      if (s.type === 'user' && this.row(String(params.username))) throw new Error(`The username ${params.username} is taken.`)
      // Prepared now, so a bad one fails the whole list; added in the task as it is.
      if (s.type === 'task' && params.items) params.items = this.prepare(params.items as ItemSpec[], author)
      const expanded = type.expand ? type.expand(params, author) : [{ type: s.type, params }]
      return expanded.map((e, i) => ({ type: e.type, params: e.params, ref: i === 0 ? (s.ref ?? e.ref) : e.ref }))
    })
  }

  /**
   * Adds items, already prepared, in a task or a bot's chat (or, for tasks, bots and people, in none). Plain ones
   * resolve at once; blockers wait. A task's first items (`items`) are added in it, and its `assignees` assigned;
   * neither is kept in its values. A task whose key is already taken is left out. Answers the ids added.
   */
  add(parentId: string | null, specs: ItemSpec[], createdBy: string): string[] {
    return this.store.batch(() => this.addNow(parentId, specs, createdBy))
  }

  /** Adds one plain item with an id of its own choosing (a URL), in a task. */
  private addAs(parentId: string, id: string, spec: ItemSpec, createdBy: string): void {
    const at = this.now()
    this.store.insertItem({
      id, parent_id: parentId, seq: this.store.nextSeq(parentId), type: spec.type, params: this.prepare([spec], 'human')[0].params, created_by: createdBy, created_at: at,
      resolved: true, resolved_at: at, resolved_by: createdBy, cancelled: false, data: null, memo: null, reactions: [], check_step: 0, next_check_at: null,
      last_error: null, runs: 0, added_n: 0, resolved_n: 0,
    })
  }

  private addNow(parentId: string | null, specs: ItemSpec[], createdBy: string): string[] {
    // A detached task sits in no task, wherever it is added.
    const detached = parentId ? specs.filter((s) => s.type === 'task' && s.params.detached) : []
    if (detached.length) return [...this.addNow(parentId, specs.filter((s) => !detached.includes(s)), createdBy), ...this.addNow(null, detached, createdBy)]
    const keyed = (s: ItemSpec) => s.type === 'task' && s.params.key && this.store.ofType('task').some((r) => r.params.key === s.params.key)
    specs = specs.filter((s) => !keyed(s))
    if (!parentId && specs.some((s) => !['task', 'bot', 'user', 'search'].includes(s.type))) throw new Error('Only a task, a bot, a person or the search item can sit in no task.')
    if (parentId && specs.some((s) => s.type === 'search')) throw new Error('The search item sits in no task.')
    if (parentId && this.row(parentId)?.type === 'bot' && specs.some((s) => s.type !== 'note')) throw new Error("A bot's own chat holds only notes.")
    const ids: string[] = []
    const seq = this.store.nextSeq(parentId)
    const at = this.now()
    specs.forEach((spec, i) => {
      const type = this.type(spec.type)
      // A person's id is their username.
      const id = spec.type === 'user' ? String(spec.params.username) : randomUUID()
      ids.push(id)
      const { assignees, items: first, detached: _d, owns, ...params } = spec.params
      // A script's first wake is kept as a time, whatever form it was given in.
      if (spec.type === 'script' && params.wake_at !== undefined) params.wake_at = whenNext(params.wake_at as number | string, at)
      this.store.insertItem({
        id, parent_id: parentId, seq: seq + i, type: spec.type, params, created_by: createdBy, created_at: at,
        resolved: !type.blocking, resolved_at: type.blocking ? null : at, resolved_by: type.blocking ? null : createdBy, cancelled: false,
        data: null, memo: null, reactions: [], check_step: 0, next_check_at: spec.type === 'script' ? null : type.check && this.scheduleOf(type, params) ? at + this.scheduleOf(type, params)![0] * 1000 : null, last_error: null,
        runs: 0, added_n: 0, resolved_n: 0,
      })
      if (spec.type === 'task') {
        if (Array.isArray(first) && first.length) this.addNow(id, first as ItemSpec[], createdBy)
        // A new task goes to those given; else, made by a person, to them; else (a bot made it) to nobody.
        const to = Array.isArray(assignees) && assignees.length ? (assignees as string[]) : this.isUser(createdBy) ? [createdBy] : []
        for (const a of to) this.setAssignee(id, a, true, createdBy)
      }
      // A script takes on the tasks it owns, as a bot is given them.
      if (spec.type === 'script' && Array.isArray(owns)) for (const t of owns as string[]) this.setAssignee(t, id, true, createdBy)
    })
    // Whatever a note links to is pulled in, as an item whose id is its URL (a Slack message or channel, or a link): made
    // the first time, and put in this task as well after that. So `[url]()` (or the URL itself) opens it.
    if (parentId && this.row(parentId)?.type === 'task')
      for (const spec of specs.filter((s) => s.type === 'note'))
        for (const url of urlsIn(String(spec.params.text ?? ''))) {
          if (this.row(url)) this.store.linkItem(parentId, url, createdBy, at)
          else this.addAs(parentId, url, pulledIn(url, at), createdBy)
        }
    // New waits are checked at once.
    if (specs.some((s) => this.type(s.type).check)) queueMicrotask(() => void this.checkDue())
    this.emit('change')
    return ids
  }

  /** Resolves a blocker, as `by`. */
  private resolve(itemId: string, data: Record<string, unknown>, by: string, cancelled = false): void {
    const r = this.row(itemId)
    if (!r || r.resolved) return
    this.store.resolveItem(itemId, data, by, this.now(), cancelled)
    // A script that ends lets go of the tasks it still has: each goes back to its owner, if nobody else has it.
    if (r.type === 'script') for (const t of this.assignedTo(itemId)) if (!this.get(t)?.closed) this.handBack(t, itemId, null, 'script')
  }

  /**
   * An item's check schedule, in seconds: its type's, never under a second apart after the first, or the default.
   * Null when its type has schedules and it has none: then it runs only when someone runs it (`runNow`).
   */
  private scheduleOf(type: ItemType, params: Record<string, unknown>): number[] | null {
    const s = type.schedule ? type.schedule(params) : DEFAULT_SCHEDULE
    if (!s?.length) return null
    return s.map((d, i) => (i === 0 ? Math.max(0, d) : Math.max(1, d)))
  }

  /**
   * When a script is due, by the rule bots wake by (`dueAt`, wakes.ts): once anything (but itself) affected the tasks it
   * owns since it last woke (a task assigned to it, something new in one), its debounce after (`debounce_minutes`, 1);
   * or its cycle (`cycle_minutes`, 15) after it last woke, while it owns tasks. A script that owns nothing runs only
   * when pressed. An older script waits for the `wake_at` it asked for, and an older `schedule` is its cycle.
   */
  private scriptDue(r: ItemRow): number | null {
    const owned = this.assignedTo(r.id)
    // Never woken: as if it woke as it was made (its cycle counts from then; what made it is not news).
    const lastWake = Number(r.params.last_wake ?? 0) || r.created_at - 1
    const mine = (by: string | null) => !!by && (by === 'script' || by === r.id)
    let edited = 0
    for (const t of owned) {
      const given = this.store.assignment(t, r.id)
      if (given && given.at > lastWake) edited = Math.max(edited, given.at)
      for (const e of this.store.children(t)) {
        if (e.id === r.id) continue
        if (e.created_at > lastWake && !mine(e.created_by)) edited = Math.max(edited, e.created_at)
        if (e.resolved && (e.resolved_at ?? 0) > lastWake && !mine(e.resolved_by) && e.resolved_n !== e.added_n) edited = Math.max(edited, e.resolved_at ?? 0)
      }
    }
    const schedule = Array.isArray(r.params.schedule) ? (r.params.schedule as number[]) : null
    const cycle = r.params.cycle_minutes !== undefined ? Number(r.params.cycle_minutes) : schedule?.length ? schedule[schedule.length - 1] / 60 : PERIODIC_MINUTES
    let at = dueAt({
      lastWake,
      edited: edited || null,
      debounceMs: Number(r.params.debounce_minutes ?? DEFAULT_DEBOUNCE) * MINUTE,
      cycleMs: cycle * MINUTE || null,
      // A timer: given a cycle of its own, it runs every cycle whether or not it owns anything.
      hasWork: owned.length > 0 || r.params.cycle_minutes !== undefined,
    })
    // An older schedule with nothing owned: it polls on its cycle, from when it was made.
    const woke = Number(r.params.last_wake ?? 0)
    if (schedule?.length && !owned.length) at = woke ? woke + Math.max(1000, cycle * MINUTE) : r.created_at + schedule[0] * 1000
    if (at !== null && typeof r.params.wake_at === 'number') at = Math.max(at, r.params.wake_at)
    return at
  }

  /** Runs an open check (a script with no schedule, say) now, once. */
  async runNow(itemId: string, actor = this.me()): Promise<void> {
    const r = this.row(itemId)
    if (!r || r.resolved || !this.type(r.type).check) throw new Error('There is nothing to run.')
    r.next_check_at = this.now()
    this.pressed.set(itemId, actor)
    await this.checkOne(r.parent_id!, itemId)
  }

  /** Writes one param of an item: for the app's own bookkeeping, such as a bot's session and last run. */
  private setParam(itemId: string, key: string, value: unknown, by: string): void {
    this.store.setParams(itemId, { ...this.row(itemId)!.params, [key]: value }, by, this.now())
  }

  // ---- people ----

  /** A new task, in `parentId` or in none, with items to start it off. */
  async create(
    title: string,
    opts: { parentId?: string | null; key?: string; specs?: ItemSpec[]; assignees?: string[]; actor?: string; author?: Author } = {},
  ): Promise<string | null> {
    const author = opts.author ?? 'human'
    const params = { title, ...(opts.key ? { key: opts.key } : {}), ...(opts.specs ? { items: opts.specs } : {}), ...(opts.assignees ? { assignees: opts.assignees } : {}) }
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
      const specs = this.prepare(preset.build(text), 'human')
      if (!specs.length) throw new Error('Nothing to send.')
      this.add(itemId, specs, actor)
      return this.idle()
    }
    if (this.must(itemId).closed) throw new Error('This task is closed.')
    const live = (id: string) => this.isUser(id) || (this.agent(id) !== null && !this.agent(id)!.deleted)
    const to = [...new Set([...(assignTo ? [this.resolveAssignee(assignTo)] : []), ...refsIn(text).filter(live)])].filter((id) => id !== actor)
    const specs = this.prepare(preset.build(text), 'human')
    if (!specs.length && !to.length) throw new Error('Nothing to send.')
    this.store.batch(() => {
      if (specs.length) this.add(itemId, specs, actor)
      for (const a of to) this.setAssignee(itemId, a, true, actor)
      // Handing it on takes it off whoever sent it.
      if (to.length) this.setAssignee(itemId, actor, false, actor)
    })
    await this.idle()
  }

  /** Adds items in a task (or tasks in none): from a person, or from an agent through MCP (author 'bot', so its rules apply). */
  async addItems(parentId: string | null, specs: ItemSpec[], actor = this.me(), author: Author = 'human'): Promise<string[]> {
    if (parentId && this.row(parentId)?.type !== 'bot' && this.must(parentId).closed) throw new Error('This task is closed.')
    const ids = this.add(parentId, this.prepare(specs, author), actor)
    // A script is run from inside a task's queue, so waiting for the queues to empty would wait for itself.
    if (actor !== 'script') await this.idle()
    return ids
  }

  /** Runs an approval's action with a person's inputs, which resolves it. */
  async approve(itemId: string, inputs: Record<string, unknown>, actor = this.me()): Promise<void> {
    const info = this.info(itemId)
    if (!info || info.type !== 'approval' || info.resolved) throw new Error('No approval waiting with that id.')
    const proposed = info.params.inputs as Record<string, unknown>
    const changed = Object.keys({ ...proposed, ...inputs }).filter((k) => String(inputs[k] ?? '') !== String(proposed[k] ?? ''))
    const name = String(info.params.action)
    const output = await this.callAction(name, inputs, { engine: this, author: 'human', actor, itemId: info.parentId, runId: null, approving: true })
    this.resolve(itemId, { ran: true, inputs, changed, output }, actor)
    this.emit('change')
    await this.idle()
  }

  /** Ends a blocker without it happening; a task is closed as cancelled, and a bot deleted. */
  async cancel(itemId: string, actor = this.me()): Promise<void> {
    const info = this.info(itemId)
    if (!info || info.resolved) throw new Error('That item is not waiting.')
    if (info.type === 'task') return this.close(itemId, { actor, how: 'cancelled' })
    if (info.type === 'bot') return this.deleteAgent(itemId, actor)
    this.resolve(itemId, { cancelled: true }, actor, true)
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
      this.store.reopenItem(itemId, actor, this.now())
      this.add(itemId, [{ type: 'note', params: { text: `Reopened by ${this.actorName(actor)}.` } }], 'system')
    })
    this.emit('change')
    if (actor !== 'script') await this.idle()
  }

  /** Adds `contains` links from a task to items that sit elsewhere (other tasks, notes, anything), for traceability. */
  async link(taskId: string, itemIds: string[], actor = this.me()): Promise<void> {
    // Any item can hold others: a message holding a reply, as well as a task its context.
    if (!this.row(taskId)) throw new Error(`No item ${taskId}.`)
    this.store.batch(() => {
      for (const id of itemIds) this.store.linkItem(taskId, id, actor, this.now())
    })
    this.emit('change')
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
    // A script is due by the one rule (`scriptDue`); a press (runNow) makes it due at once.
    for (const r of this.store.ofType('script')) if (!r.resolved && !this.pressed.has(r.id)) r.next_check_at = this.scriptDue(r)
    const due = [...this.store.items.values()]
      .filter((r) => !r.resolved && r.next_check_at !== null && r.next_check_at <= now && !this.checking.has(r.id))
      .sort((a, b) => a.next_check_at! - b.next_check_at!)
    // At most `scripts.max_running` scripts at once (MAX_SCRIPTS by default): those due longest go first; the rest stay
    // due, for the next turn of the loop.
    const max = Number(this.settings.get('scripts.max_running', MAX_SCRIPTS))
    let room = max - [...this.checking].filter((id) => this.row(id)?.type === 'script').length
    const allowed = due.filter((r) => r.type !== 'script' || room-- > 0)
    await Promise.all(allowed.map((r) => this.checkOne(r.parent_id!, r.id)))
  }

  private checkOne(taskId: string, itemId: string): Promise<void> {
    this.checking.add(itemId)
    return this.serial(taskId, async () => {
      const row = this.row(itemId)
      if (!row || row.resolved || (row.next_check_at ?? 0) > this.now()) return
      const info = this.toInfo(row)
      const type = this.type(info.type)
      let data: Record<string, unknown> | null = null
      let error: string | null = null
      const woke = this.now()
      const pressedBy = this.pressed.get(itemId) ?? null
      this.pressed.delete(itemId)
      // Undefined: as its type's schedule says. A number or null: what the check asked for.
      let next: number | null | undefined
      try {
        data = await type.check!(info.params, {
          trigger: pressedBy ? 'press' : 'schedule',
          pressedBy,
          setNext: (at) => (next = at),
          engine: this,
          item: this.must(taskId),
          info,
          author: authorOf(info.createdBy),
          memo: row.memo ?? null,
          runs: row.runs,
          setMemo: (value) => this.store.setState(itemId, value, this.now()),
        })
      } catch (e) {
        error = (e as Error).message
      }
      row.runs++
      const timeout = Number(info.params.timeout_minutes ?? 0)
      if (!data && timeout && this.now() >= info.createdAt + timeout * MINUTE)
        data = { outcome: 'timed_out', detail: `Nothing after ${timeout} minutes.` }
      if (data) {
        this.resolve(itemId, data, 'system')
        this.emit('change')
        return
      }
      // A script keeps when it last woke (so a restart knows), and is due again by the rule.
      if (row.type === 'script') {
        this.store.setParams(itemId, { ...row.params, last_wake: woke }, 'script', this.now())
        Object.assign(row, { last_error: error })
        row.next_check_at = this.scriptDue(row)
        this.emit('change')
        return
      }
      if (next !== undefined) {
        Object.assign(row, { next_check_at: next, last_error: error })
        // A script's wake is kept, so a restart wakes it when it asked.
        if (row.type === 'script' && row.params.wake_at !== (next ?? undefined)) {
          const { wake_at: _w, ...rest } = row.params
          this.store.setParams(itemId, next === null ? rest : { ...rest, wake_at: next }, 'script', this.now())
        }
        this.emit('change')
        return
      }
      const schedule = this.scheduleOf(type, info.params)
      if (!schedule) Object.assign(row, { next_check_at: null, last_error: error })
      else {
        const step = Math.min(row.check_step + 1, schedule.length - 1)
        Object.assign(row, { check_step: step, next_check_at: this.now() + schedule[step] * 1000, last_error: error })
      }
      this.emit('change')
    }).finally(() => this.checking.delete(itemId))
  }
}
