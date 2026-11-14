import { runScript } from './script'
import { github } from './github'
import { slackActions } from './slack'
import { Files, isText } from './files'
import { randomUUID } from 'node:crypto'
import { EventEmitter } from 'node:events'
import { mkdirSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { z } from 'zod'
import type { Agent, ItemInfoDetail, ItemSpec, Item, ItemDetail, ItemInfo, LastRun, Overview, User } from '../shared/types'
import { current, itemActions } from './actions'
import { asTopic, ITEM_TYPES, scriptResult } from './itemTypes'
import { execCli, type Action, type ActionContext, type Author, type ItemType, type Exec, type Integration } from './integrations'
import { PRESETS, type Preset } from './presets'
import { IDLE_MINUTES, MAX_SCRIPTS, plainSecrets, Settings, type Secrets } from './settings'
export { IDLE_MINUTES, MAX_SCRIPTS } from './settings'
import { STATE_KEYS, Store, type ItemRow, type Inspection, type Via } from './store'
import { viewText, type Size, type Who } from './views'
import { focusKind } from '../shared/views'
import { inferType, inferUrl } from '../shared/infer'

/** What the app keeps on items for itself: never set by `setValue`. */
const OWN_KEYS = new Set(['session_id', 'seen', 'last_run', 'last_wake', 'last_seen', 'handoff', 'key', 'last_owner', 'last_assignees'])

const MINUTE = 60_000
/** A bot's model, unless it says another. */
export const DEFAULT_MODEL = 'opus'
/** How long a bot or script waits before running on an item again that it still holds, unless it says otherwise (`cycle_minutes`). */
export const CYCLE_MINUTES = 30
/** Who the engine acts as when nobody is said and there is nobody yet (the tests, mostly). */
export const NOBODY = 'human'

/** What a bot says about one task (or its own chat), in its run's output. */
export interface BotUpdate {
  /** The task's id (or the bot's own, for its chat). */
  item: string
  /** To the people in it: what it did, or one direct question. */
  message?: string
  /** Items to add in it: comments, scripts, child topics. */
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
  /** Due again at once (a person wrote in the item while the run went on), though what came before `n` is not news. */
  again?: boolean
}
/** Whether something at (`n`, `at`) came after `mark`. */
const after = (n: number, at: number, mark: Mark) => n > mark.n || (n === mark.n && at > mark.at)

/** A run going now: in memory only. */
interface Run {
  id: string
  agentId: string
  itemIds: string[]
  startedAt: number
  /** The items whose assignees the run changed. */
  assigned: Set<string>
  /** Where the bot stood before it: put back if the run never really happened (a usage limit). */
  before: { mark: Mark | undefined; seen: string[] }
  /** Its own mark on the item: what came after is news it has not seen. */
  mark: Mark
}

/** A person added it, or the system did; a bot, or a bot's script, is a bot. */
const authorOf = (createdBy: string): Author => (createdBy === 'bot' || createdBy === 'script:bot' || createdBy.startsWith('bot:') ? 'bot' : 'human')

/** `[id]()` in text: a reference to an item (a task, a bot, or a person by their username). */
/** The item a URL is pulled in as: a Slack message, a Slack channel (its last day), a pull request, or a link (`inferUrl`). */
function pulledIn(url: string, at: number): ItemSpec {
  const got = inferUrl(url, at)!
  return { type: got.type as ItemSpec['type'], params: got.params }
}

export const refsIn = (text: string): string[] => [
  ...[...text.matchAll(/\[([\w.-]+)\]\(\)/g)].map((m) => m[1]),
  // `[label](id)`, as markdown writes a link, refers to the item `id` too.
  ...[...text.matchAll(/\[[^\]]*\]\(([\w.-]+)\)/g)].map((m) => m[1]),
]

/** Every URL a text mentions: bare, `[label](url)`, or `[url]()`; each once, in order. */
export const urlsIn = (text: string): string[] => [...new Set([...text.matchAll(/https?:\/\/[^\s<>()\[\]]+[^\s<>()\[\].,;:!?'"]/g)].map((m) => m[0]))]

/** A task as a run's prompt tells it: its items new to the bot, and what was pinned to it lately (each as text: views.ts). */
export interface PromptItem {
  item: Item
  children: ItemInfo[]
  first: boolean
  own?: boolean
  pins?: { info: ItemInfo; text: string }[]
}

/** What may sit in no topic: what is its own (topics, bots, people, scripts, the search item), and what is pinned rather than said (links, Slack, files). */
const SIT_IN_NONE = ['topic', 'bot', 'user', 'script', 'link', 'github_pr', 'slack_message', 'slack_channel', 'file']
/** What has an owner (an `owns` link): who it goes back to. */
const OWNED = ['topic', 'bot', 'script']

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
  /** What file items hold: kept copies under `<dataDir>/files`, or files on this machine they refer to. */
  readonly files: Files
  /** The integrations, each in the one shape (integrations.ts): their tools are among the actions. */
  readonly integrations: Integration[] = [github]
  readonly actions: Action[] = [...itemActions, ...slackActions, ...this.integrations.flatMap((i) => i.tools)]
  readonly itemTypes: ItemType[] = ITEM_TYPES
  readonly presets: Preset[] = PRESETS
  /** People's own actions, per item type (the Types page). */
  /** Stops a bot's run. Set by the runner. */
  onCancel: (agentId: string) => void = () => {}
  /** The run each bot has going, by the bot's id. In memory only: a crash leaves nothing behind. */
  private running = new Map<string, Run>()
  /** When each bot and script last ran on each item it holds (by bot, then item): what came after is new to it. */
  private marks = new Map<string, Map<string, Mark>>()
  /** What each bot's session has seen (the items in its runs since the session began): kept in memory, not the store. */
  private sessionSeen = new Map<string, string[]>()
  /** When each bot was woken by hand: it runs then, on its first item, whatever it last saw. */
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
    this.files = new Files(join(this.dataDir, 'files'))
    for (const r of [...this.store.ofType('bot'), ...this.store.ofType('script')]) {
      const seen = r.params.last_seen && typeof r.params.last_seen === 'object' ? (r.params.last_seen as Record<string, number>) : {}
      this.marks.set(r.id, new Map(Object.entries(seen).map(([item, at]) => [item, { n: 0, at: Number(at) }])))
      if (r.type !== 'bot') continue
      const last = r.params.last_run as LastRun | undefined
      // A run the app was stopped during is over; what it was on goes back to its owner, saying so.
      if (last && last.endedAt === null && !r.resolved) {
        this.store.setParams(r.id, { ...r.params, last_run: { ...last, endedAt: this.now(), error: 'The app stopped during the run.' } }, `bot:${r.id}`, this.now())
        for (const itemId of last.items)
          if (this.row(itemId) && !this.isClosed(itemId) && this.assignees(itemId).includes(r.id)) {
            this.add(itemId, [{ type: 'topic', params: { text: `@${this.agentName(r.id)} was stopped (the app stopped) before it finished.` } }], 'system')
            this.handBack(itemId, r.id, null)
          }
      }
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
    const summary = type.type === 'bot' ? this.title(r.id, params) : type.describe(params)
    return {
      id: r.id,
      parentId: r.parent_id,
      via: r.parent_id ? (r.via ?? 'comments') : null,
      owner: this.owner(r.id),
      type: type.type,
      params,
      createdBy: r.created_by,
      createdAt: r.created_at,
      resolved: r.resolved,
      closed: this.isClosed(r.id),
      resolvedAt: r.resolved_at,
      resolvedBy: r.resolved_by,
      cancelled: r.cancelled,
      data,
      actor: (type.blocking ? r.resolved_by : null) ?? r.created_by,
      summary,
      message: r.type === 'topic' ? summary : !r.resolved ? null : r.cancelled ? `Cancelled: ${summary}` : type.said ? type.said(params, data ?? {}) : summary,
      nextCheckAt: r.next_check_at,
      runs: r.runs,
      lastError: r.last_error,
      pins: this.store.pins(r.id).map((p) => p.id),
      pinnedIn: this.store.pinnedIn(r.id),
    }
  }

  private row(id: string): ItemRow | undefined {
    return this.store.items.get(id)
  }

  info(id: string): ItemInfo | null {
    const r = this.row(id)
    return r ? this.toInfo(r) : null
  }

  /** The comments in an item (its chat), oldest first; only the open blockers among them with `open`. */
  children(itemId: string, open = false): ItemInfo[] {
    return this.store
      .children(itemId, 'comments')
      .filter((r) => !open || (!r.resolved && this.blocking(r.type)))
      .map((r) => this.toInfo(r))
  }

  /** The notes in an item (its tree's first level), oldest first. */
  notes(itemId: string): ItemInfo[] {
    return this.store.children(itemId, 'notes').map((r) => this.toInfo(r))
  }

  /** An item's name: a topic's first line, a bot's @name, a script's name; anything else as its type describes it. */
  private title(id: string, params: Record<string, unknown>): string {
    const r = this.row(id)
    if (r?.type === 'bot') return this.agentName(id)
    if (r?.type === 'script') return String(params.name ?? params.description ?? 'Script')
    if (r?.type === 'topic') return String(params.text ?? '').split('\n')[0]
    return r ? this.type(r.type).describe(params).split('\n')[0] : id
  }

  /** Whether an item is closed: its status closed (anything can be closed), or a blocker that resolved (a bot deleted, a script cancelled). */
  isClosed(id: string): boolean {
    const r = this.row(id)
    return !r || r.params.open === false || (r.type !== 'topic' && this.blocking(r.type) && r.resolved)
  }

  /** Who owns an item (its `owns` link), or null. */
  owner(itemId: string): string | null {
    return this.store.owner(itemId)?.owner ?? null
  }

  /** Who a new item is owned by: the person who made it; else the owner of what it is in; else the owner of the bot that made it. */
  private ownerFor(createdBy: string, parentId: string | null): string | null {
    if (this.isUser(createdBy)) return createdBy
    if (parentId && this.owner(parentId)) return this.owner(parentId)
    const bot = /^bot:(.+)$/.exec(createdBy)?.[1]
    return bot ? this.owner(bot) : null
  }

  /**
   * Gives an item to an owner (a person or a bot), or to nobody (null). An item type (`topic`, `bot`…) may own scripts:
   * they are offered in the right pane of every item of that type (`typeScripts`).
   */
  async setOwner(itemId: string, owner: string | null, actor = this.me()): Promise<void> {
    if (!this.row(itemId)) throw new Error(`No item ${itemId}.`)
    const isType = owner !== null && this.itemTypes.some((t) => t.type === owner)
    if (isType && this.row(itemId)!.type !== 'script') throw new Error(`Only a script can be owned by an item type (${owner}).`)
    const to = owner === null ? null : isType ? owner : this.resolveAssignee(owner)
    this.store.setOwner(itemId, to, actor, this.now())
    this.emit('change')
  }

  /** What someone owns that is still open: open topics, and bots and scripts that last. */
  ownedBy(owner: string): string[] {
    return this.store.owned(owner).filter((id) => {
      const r = this.row(id)
      return r && !this.isClosed(id) && (r.type !== 'topic' || r.params.open === true)
    })
  }

  /**
   * What the finder reads from the store for whoever it is for (any of `who`): what is pinned to them (their `pins`
   * links), and what was assigned to them and no longer is, the latest first, at most `limit` (from egdb's history of
   * `assignees` links: one removed is how an item left them; none in a store with no history, in memory).
   */
  async finder(who: string[], limit = 16, viewer = this.me()): Promise<{ pinned: Item[]; unassigned: Item[] }> {
    const items = (ids: string[]) => ids.flatMap((id) => this.get(id, viewer) ?? [])
    const pinned = [...new Set(who.flatMap((w) => this.store.pins(w).map((p) => p.id)))]
    if (!who.length) return { pinned: [], unassigned: [] }
    const log = await this.store.log({ entities: who, keys: ['assignees'], limit: 1000 })
    const ids: string[] = []
    for (const e of log) {
      if (ids.length >= limit) break
      if (e.kind !== 'link' || !e.removed || !e.other || !who.includes(e.other) || ids.includes(e.entity)) continue
      if (this.row(e.entity) && !this.assignees(e.entity).some((a) => who.includes(a))) ids.push(e.entity)
    }
    return { pinned: items(pinned), unassigned: items(ids) }
  }

  /** The open scripts an item type owns: offered in the right pane of every item of that type. */
  typeScripts(type: string): ItemInfo[] {
    return this.store.owned(type).flatMap((id) => {
      const r = this.row(id)
      return r?.type === 'script' && !r.resolved ? [this.toInfo(r)] : []
    })
  }

  /**
   * The scripts the loop runs: those the person signed in owns, or a bot they own does, or a bot that bot owns, and so
   * on (a walk along `owns` links, through bots). Anyone else's, or nobody's, run only when pressed.
   */
  runnableScripts(): Set<string> {
    const scripts = new Set<string>()
    const seen = new Set<string>()
    const walk = (owner: string) => {
      if (seen.has(owner)) return
      seen.add(owner)
      for (const id of this.store.owned(owner)) {
        const r = this.row(id)
        if (!r || r.resolved) continue
        if (r.type === 'script') scripts.add(id)
        else if (r.type === 'bot') walk(id)
      }
    }
    walk(this.me())
    return scripts
  }

  /** Who an item is assigned to: its `assignees` links, the earliest first. */
  assignees(itemId: string): string[] {
    return this.store.assignees(itemId)
  }

  private toItem(r: ItemRow, viewer = this.me()): Item {
    const id = r.id
    const kids = this.store.children(id, 'comments')
    // Neither a script nor a topic sits in another as a dependency: a topic waits on something by being assigned to it.
    const blockers = kids.filter((k) => !k.resolved && k.type !== 'script' && this.blocking(k.type)).map((k) => this.toInfo(k))
    const assignees = this.assignees(id)
    const touched = Math.max(
      r.created_at,
      ...kids.map((k) => Math.max(k.created_at, k.resolved_at ?? 0)),
      ...assignees.map((a) => this.store.assignment(id, a)!.at),
    )
    const closed = this.isClosed(id)
    const bot = [...this.running.values()].find((run) => run.itemIds.includes(id))?.agentId ?? null
    return {
      id,
      type: r.type as Item['type'],
      title: this.title(id, r.params),
      parentId: r.parent_id,
      open: r.type === 'topic' ? (typeof r.params.open === 'boolean' ? r.params.open : null) : !closed,
      closed,
      owner: this.owner(id),
      blockers,
      assignees,
      withYou: !closed && assignees.includes(viewer) && blockers.length === 0,
      bot,
      since: touched,
      createdAt: r.created_at,
      closedAt: r.resolved_at,
      ...(typeof r.params.display === 'string' ? { display: r.params.display } : {}),
    }
  }

  /** The item with this id as the lists read it (a topic, or anything else someone may have), or null; `withYou` for `viewer`. */
  get(id: string, viewer = this.me()): Item | null {
    const r = this.row(id)
    return r ? this.toItem(r, viewer) : null
  }

  private must(id: string, viewer = this.me()): Item {
    const item = this.get(id, viewer)
    if (!item) throw new Error(`No item ${id}.`)
    return item
  }

  /**
   * What the lists show, as `viewer` sees them: every topic with a status that is open, or closed in the last day, and
   * anything else someone has; the bots and the people.
   */
  overview(viewer = this.me()): Overview {
    const since = this.now() - 24 * 60 * MINUTE
    const held = new Set(this.store.allAssignments().map((a) => a.item))
    // What anyone owns is listed too (bots and scripts among it): a search page can be anyone's.
    for (const r of [...this.store.ofType('bot'), ...this.store.ofType('script')]) if (this.owner(r.id)) held.add(r.id)
    const items = [...this.store.items.values()]
      .filter((r) =>
        r.type === 'topic'
          ? r.params.open === true || (r.params.open === false && (r.resolved_at ?? 0) > since) || (held.has(r.id) && r.params.open !== false)
          : held.has(r.id) && !this.isClosed(r.id),
      )
      .map((r) => this.toItem(r, viewer))
      // The most recently touched first: what is moving stays on top.
      .sort((a, b) => b.since - a.since)
    const scripts = this.store
      .ofType('script')
      .filter((r) => !r.resolved && (r.params.name || this.assignedTo(r.id).length))
      .map((r) => ({ id: r.id, named: !!r.params.name, name: String(r.params.name ?? r.params.description ?? 'Script'), wakesAt: r.next_check_at, running: this.checking.has(r.id) }))
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

  /** Any item in full, its comments being its chat (a bot's or script's own chat; a person's, a file's: any item has one). */
  detail(id: string, viewer = this.me()): ItemDetail | null {
    const r = this.row(id)
    if (!r) return null
    const item = this.toItem(r, viewer)
    const parent = item.parentId ? this.get(item.parentId) : null
    return { item, parent: parent && { id: parent.id, title: parent.title }, children: this.children(id) }
  }

  /** Any item, with what its views need. */
  itemInfoDetail(id: string, viewer = this.me()): ItemInfoDetail | null {
    // An id no item has (a URL in text, say) is shown as what it looks like: `inferType`.
    const info = this.info(id) ?? this.inferredInfo(id)
    if (!info) return null
    const parent = info.parentId ? this.get(info.parentId) : null
    const infos = (ids: string[]) => ids.flatMap((i) => (this.row(i) ? [this.toInfo(this.row(i)!)] : []))
    return {
      info,
      parent: parent && { id: parent.id, title: parent.title },
      pins: infos(info.pins),
      pinnedIn: infos(info.pinnedIn),
      item: this.detail(id, viewer),
      typeScripts: this.typeScripts(info.type),
    }
  }

  /** An item as egdb holds it: its values and links, and who made it and when. */
  /** An item as egdb holds it, with the type it looks like (`inferType`), for the debug view to offer. */
  async inspect(id: string): Promise<Inspection> {
    const it = await this.store.inspect(id)
    const got = inferType(id, it.values)
    return { ...it, inferred: got && this.typeOrNull(got.type) ? got.type : null }
  }

  /**
   * What needs `viewer` next, of the open tasks assigned to them that nothing blocks: the one most
   * recently touched, so the few in hand get closed out before the rest come round. Those in `later` (what the window
   * says was viewed lately) go after all the rest.
   */
  next(skip: string[] = [], viewer = this.me(), later: string[] = []): string | null {
    const open = this.store
      .assignedTo(viewer)
      .flatMap((id) => this.get(id, viewer) ?? [])
      .filter((i) => i.withYou && !skip.includes(i.id))
    const last = (id: string) => (later.includes(id) ? 1 : 0)
    return open.sort((a, b) => last(a.id) - last(b.id) || b.since - a.since)[0]?.id ?? null
  }

  /** Renames a topic: the first line of its `text` changes (egdb keeps the old one, and who changed it). */
  async rename(itemId: string, title: string, actor = this.me()): Promise<void> {
    const t = title.trim()
    if (this.row(itemId)?.type !== 'topic') throw new Error(`No topic ${itemId}.`)
    if (!t || this.must(itemId).title === t) return
    const rest = String(this.row(itemId)!.params.text ?? '').split('\n').slice(1)
    this.store.setParams(itemId, { ...this.row(itemId)!.params, text: [t, ...rest].join('\n') }, actor, this.now())
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
      this.add(itemId, [{ type: 'topic', params: { text: `Ran \`${name}\`: it failed: ${(e as Error).message}` } }], actor)
      throw e
    }
    const shown = typeof out === 'string' ? out : JSON.stringify(out, null, 2)
    const text = shown && shown !== 'null' ? `Ran \`${name}\`:\n\n\`\`\`${typeof out === 'string' ? '' : 'json'}\n${shown.length > 3000 ? `${shown.slice(0, 3000)}\n…` : shown}\n\`\`\`` : `Ran \`${name}\`.`
    this.add(itemId, [{ type: 'topic', params: { text } }], actor)
    await this.idle()
    return out
  }

  /**
   * Sets one value on an item (null clears it): any key, any value (a topic's text, status or display, a bot's
   * instructions, a script's state on a topic it holds); a type's params are advice. Not how it stands (`type`, `resolved` and the like: those
   * have their own ways), nor what the app keeps for itself on a bot. egdb keeps every value it had. Whatever shows it
   * is told at once (every change is).
   */
  async setValue(id: string, key: string, value: unknown, actor = this.me()): Promise<void> {
    const row = this.row(id)
    if (!row) throw new Error(`No item ${id}.`)
    if (STATE_KEYS.has(key) || OWN_KEYS.has(key)) throw new Error(`${key} can't be set this way.`)
    // An item's status is how it stands too: closing one is `close`, reopening one `reopen`, and the rest of what each does.
    if (key === 'open') {
      if (value === false) return this.close(id, { actor })
      if (value === true && this.isClosed(id)) return this.reopen(id, actor)
      if (value !== true && value !== null && value !== undefined) throw new Error('An item is open (true), closed (false) or has no status (null).')
      if (JSON.stringify(row.params.open ?? null) === JSON.stringify(value ?? null)) return
      this.store.setOpen(id, (value as boolean | null) ?? null, actor, this.now())
      this.emit('change')
      return
    }
    // Duck typed: any key, any value. A type's params are advice, not a check.
    const next = { ...row.params, [key]: value ?? undefined }
    if (JSON.stringify(row.params[key] ?? null) === JSON.stringify(value ?? null)) return
    this.store.setParams(id, Object.fromEntries(Object.entries(next).filter(([, v]) => v !== undefined)), actor, this.now())
    this.emit('change')
  }

  /**
   * Sets a topic's status (open, closed, or none) and nothing else: unlike `close` and `reopen`, its owner and who has
   * it stay as they are, and nothing is cancelled or said. The notes view's, where a status is a checkbox.
   */
  async setOpen(id: string, open: boolean | null, actor = this.me()): Promise<void> {
    const row = this.row(id)
    if (!row) throw new Error(`No item ${id}.`)
    if (row.type !== 'topic') throw new Error('Only a topic has a status of its own; close or reopen anything else.')
    if (open !== true && open !== false && open !== null) throw new Error('An item is open (true), closed (false) or has no status (null).')
    if ((row.params.open ?? null) === open) return
    this.store.setOpen(id, open, actor, this.now())
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
    if (this.isOpenScript(r)) return r
    const named = this.store.ofType('script').find((x) => !x.resolved && String(x.params.name ?? '').toLowerCase() === r.toLowerCase())
    return this.findAgent(r)?.id ?? named?.id ?? null
  }

  /** A bot or script that could once hold tasks but is cancelled or closed now, by its name or id: how it is named; else null. */
  goneHolder(ref: string): string | null {
    const r = ref.trim().replace(/^@/, '').toLowerCase()
    const x = [...this.store.ofType('bot'), ...this.store.ofType('script')].find(
      (x) => x.resolved && (x.id === ref.trim() || String(x.params.name ?? '').toLowerCase() === r),
    )
    return x ? (x.type === 'bot' ? `@${String(x.params.name ?? x.id)}` : `The script ${String(x.params.name ?? x.id)}`) : null
  }

  resolveAssignee(ref: string): string {
    const id = this.holder(ref)
    if (id) return id
    const gone = this.goneHolder(ref)
    if (gone) throw new Error(`${gone} (${ref}) is ${gone.startsWith('@') ? 'cancelled' : 'closed'}, so it can't have tasks now.`)
    throw new Error(`Nobody is called ${ref}, to assign. People: ${this.users().map((u) => u.id).join(', ') || 'none'}; bots: ${this.agents().map((a) => a.name).join(', ') || 'none'}.`)
  }

  /** Whether an id is a script that is still open: one that can have tasks. */
  isOpenScript(id: string): boolean {
    const r = this.row(id)
    return r?.type === 'script' && !r.resolved
  }

  /**
   * Assigns an item to someone (an `assignees` link from it), or takes it off them. Assigning someone who has it
   * already writes the link again: for a bot, that makes it due on the item again.
   */
  setAssignee(itemId: string, to: string, on: boolean, actor: string, runId: string | null = null): void {
    if (!this.row(itemId)) throw new Error(`No item ${itemId}.`)
    if (on && this.holder(to) !== to) throw new Error(`Nobody is ${to}, to assign.`)
    if (!on && !this.assignees(itemId).includes(to)) return
    this.store.setAssigned(itemId, to, on, actor, this.now())
    for (const run of this.running.values()) if (run.id === runId) run.assigned.add(itemId)
    this.emit('change')
  }

  /** Assigns an item to a person or a bot (by username, name or id), or takes it off them. */
  async assign(itemId: string, to: string, on: boolean, actor = this.me(), runId: string | null = null): Promise<void> {
    if (this.must(itemId).closed && on) throw new Error('That item is closed.')
    this.setAssignee(itemId, on ? this.resolveAssignee(to) : (this.assignees(itemId).find((a) => a === to) ?? this.resolveAssignee(to)), on, actor, runId)
    // A script is run from inside a task's queue, so waiting for the queues to empty would wait for itself.
    if (actor !== 'script') await this.idle()
  }

  // ---- bots ----

  /** A bot's settings: its params (egdb keeps every value they had). A script's too, where both run alike. */
  private agentParams(id: string): Record<string, unknown> {
    const r = this.row(id)
    return r?.type === 'bot' || r?.type === 'script' ? r.params : {}
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
    const dir = this.agent(id)!.folder
    mkdirSync(dir, { recursive: true })
    return dir
  }

  /** The open items assigned to someone (their `assignees` links, followed backwards), the longest held first. */
  assignedTo(assignee: string): string[] {
    return this.store
      .assignedTo(assignee)
      .filter((id) => this.row(id) && !this.isClosed(id))
      .sort((a, b) => {
        const x = this.store.assignment(a, assignee)!
        const y = this.store.assignment(b, assignee)!
        return x.at - y.at || x.n - y.n
      })
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
      cycleMinutes: this.cycleMinutes(id),
      folder: p.folder ? String(p.folder) : join(this.dataDir, 'bots', `${slug(name)}-${id.slice(0, 8)}`),
      sessionId: (p.session_id as string) ?? null,
      lastRun: (p.last_run as LastRun) ?? null,
      createdAt: r.created_at,
      deleted: r.resolved,
      runId: this.running.get(id)?.id ?? null,
      wakesAt: r.resolved ? null : (this.due(id)?.at ?? null),
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
    a: { id?: string; name: string; instructions: string; provider: string; model: string; debounceMinutes?: number; cycleMinutes?: number; folder?: string },
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

  /** Runs a bot now, on the first item it holds, whatever it last saw. */
  wakeAgent(id: string): void {
    if (!this.agent(id)) throw new Error(`No bot ${id}.`)
    this.woken.set(id, this.now())
    this.emit('change')
  }

  /** How long a bot or script waits before running on an item again that it still holds. */
  private cycleMinutes(id: string): number {
    const c = Number(this.agentParams(id).cycle_minutes ?? CYCLE_MINUTES)
    return Number.isFinite(c) ? Math.max(0, c) : CYCLE_MINUTES
  }

  /** When a bot or script last ran on an item (its mark there), if it has. */
  private markOn(agentId: string, itemId: string): Mark | null {
    return this.marks.get(agentId)?.get(itemId) ?? null
  }

  /**
   * What a bot or script runs on next, and when: the one rule for both. Of the open items assigned to it, the longest
   * held first, the first that is due: one it has not run on yet, or that was assigned to it again (by anyone else) since
   * it last ran on it, is due at once; one it ran on is due again its cycle (`cycle_minutes`, CYCLE_MINUTES) after. Woken
   * by hand, its first item is due at once. Null when it holds nothing.
   */
  due(agentId: string): { item: string; at: number } | null {
    // Who it is when it writes: a bot as itself, a script as `script` (any script: they write by one name).
    const self = this.row(agentId)?.type === 'bot' ? [`bot:${agentId}`] : [agentId, 'script']
    const cycle = this.cycleMinutes(agentId) * MINUTE
    let next: { item: string; at: number } | null = null
    const held = this.assignedTo(agentId)
    for (const itemId of held) {
      const mark = this.markOn(agentId, itemId)
      const given = this.store.assignment(itemId, agentId)!
      const at = !mark || (after(given.n, given.at, mark) && !self.includes(given.by)) ? given.at : mark.again ? mark.at : cycle ? mark.at + cycle : null
      if (at !== null && (!next || at < next.at)) next = { item: itemId, at }
      // The first that is due now is the one: the longest held go first.
      if (at !== null && at <= this.now()) return { item: itemId, at }
    }
    const woken = this.woken.get(agentId)
    if (woken !== undefined && held.length) return { item: held[0], at: woken }
    return next
  }

  /**
   * Starts a bot's run, if it is due and not running: on one item, with what is new in it since the bot last ran on it
   * (or, for an item its session has not seen, its whole history). Its session is its own: resumed if it has one,
   * otherwise new.
   */
  claim(agentId: string): { agent: Agent; items: PromptItem[]; resume: boolean; handoff: string | null; recap: boolean } | null {
    const agent = this.agent(agentId)
    if (!agent || agent.deleted || this.running.has(agentId)) return null
    const due = this.due(agentId)
    if (!due || due.at > this.now()) return null
    const itemId = due.item
    // Idle past `bots.idle_minutes` since its last run ended, it starts afresh: resuming would re-read the whole session uncached.
    const last = this.agentParams(agentId).last_run as LastRun | undefined
    const idle = Number(this.settings.get('bots.idle_minutes', IDLE_MINUTES)) * MINUTE
    if (agent.sessionId !== null && last?.endedAt && this.now() - last.endedAt > idle) this.setParam(agentId, 'session_id', null, `bot:${agentId}`)
    const seen = this.seen(agentId)
    const self = `bot:${agentId}`
    const mark = this.markOn(agentId, itemId) ?? { n: 0, at: 0 }
    const own = itemId === agentId
    const first = !own && !seen.includes(itemId)
    // What is new: what was added (or a blocker that resolved) since its mark, less what it did itself.
    const news = (e: ItemRow) =>
      (after(e.added_n, e.created_at, mark) && e.created_by !== self) || (e.resolved && after(e.resolved_n, e.resolved_at ?? 0, mark) && e.resolved_by !== self)
    const children = this.store.children(itemId, 'comments').filter((e) => first || news(e)).map((e) => this.toInfo(e))
    // What is pinned to it: all of it the first time, then what was pinned since it last ran on it. Each at its focus
    // (a topic showing its notes, with its tree), but a chat as its pill: its history is long, and get_item reads it.
    const pins = own
      ? []
      : this.store
          .pins(itemId)
          .filter((p) => first || p.at >= mark.at)
          .flatMap((p) => {
            const info = this.info(p.id)
            return info ? [{ info, text: this.view(p.id, focusKind(info) === 'chat' ? 'pill' : 'focus')! }] : []
          })
    const resume = this.agentParams(agentId).session_id != null
    // A fresh session hears the handoff its last one left, once.
    const handoff = !resume && typeof this.agentParams(agentId).handoff === 'string' ? String(this.agentParams(agentId).handoff) : null
    if (!resume) {
      this.setParam(agentId, 'session_id', randomUUID(), `bot:${agentId}`)
      this.sessionSeen.set(agentId, [])
      if (handoff !== null) this.setParam(agentId, 'handoff', null, `bot:${agentId}`)
    }
    this.woken.delete(agentId)
    return { agent: this.agent(agentId)!, items: [{ item: this.must(itemId), children, first: !resume || first, own, pins }], resume, handoff, recap: !resume && !!last }
  }

  /**
   * What a bot has been doing, for a fresh session: what it wrote lately, and what was given to it or taken from it,
   * the newest first, each thing named beside its id. Null if there is nothing.
   */
  async recap(agentId: string, limit = 40): Promise<string | null> {
    const actor = `bot:${agentId}`
    const [did, given] = await Promise.all([
      this.store.log({ authors: [actor], limit: limit * 2 }),
      this.store.log({ entities: [agentId], keys: ['assignees', 'assigned_to'], limit }),
    ])
    const name = (id: string | null): string => {
      const r = id ? this.row(id) : undefined
      if (!id || !r) return String(id)
      if (r.type === 'user') return `@${this.actorName(id)}`
      if ((r.type === 'topic' && r.params.open !== undefined) || r.type === 'bot' || r.type === 'script') return `"${this.title(id, r.params)}" [${id}]`
      return `a ${r.type} [${id}]${r.parent_id ? ` in ${name(r.parent_id)}` : ''}`
    }
    const brief = (v: unknown) => {
      const t = (typeof v === 'string' ? v : JSON.stringify(v)).replace(/\s+/g, ' ')
      return t.length > 120 ? `${t.slice(0, 120)}…` : t
    }
    const when = (at: number) => new Date(at).toISOString().slice(0, 16).replace('T', ' ')
    // Its own bookkeeping, and the type a new item is made with, say nothing.
    const wrote = did
      .filter((e) => !(e.entity === agentId && OWN_KEYS.has(e.key)) && e.key !== 'type' && !(e.kind === 'link' && ['contains', 'comments', 'notes', 'owns'].includes(e.key)))
      .slice(0, limit)
      .map((e) =>
        e.kind === 'link'
          ? e.key === 'assignees' || e.key === 'assigned_to'
            ? `- ${when(e.at)} ${e.removed ? 'took' : 'assigned'} ${name(e.entity)} ${e.removed ? 'off' : 'to'} ${name(e.other)}`
            : `- ${when(e.at)} ${e.removed ? 'unlinked' : 'linked'} ${name(e.entity)} → ${name(e.other)} (${e.key})`
          : `- ${when(e.at)} ${e.value === null ? `cleared ${e.key} on` : `set ${e.key} on`} ${name(e.entity)}${e.value === null ? '' : `: ${brief(e.value)}`}`,
      )
    const got = given
      .filter((e) => e.other === agentId && e.by !== actor)
      .map((e) => `- ${when(e.at)} ${this.actorName(e.by)} ${e.removed ? 'took' : 'gave you'} ${name(e.entity)}`)
    if (!wrote.length && !got.length) return null
    return [
      got.length ? `Topics given to you or taken from you, newest first:\n${got.join('\n')}` : '',
      wrote.length ? `What you did lately, newest first:\n${wrote.join('\n')}` : '',
    ].filter(Boolean).join('\n\n')
  }

  /**
   * What a bot's session has seen: the items in its runs since the session began. In memory only, so it is not in the
   * store's history: after a restart a resumed session is told an item's whole history again, the first time.
   */
  private seen(agentId: string): string[] {
    return this.agentParams(agentId).session_id ? (this.sessionSeen.get(agentId) ?? []) : []
  }

  /**
   * Marks that a bot or script ran on an item (null: as if it never had): what comes after is new to it, and it is not
   * due there again for its cycle. In memory only (not the store's history), for the items it still holds; a store an
   * older build wrote has `last_seen` values, read once as it loads.
   */
  private setMark(agentId: string, itemId: string, mark: Mark | null): void {
    const marks = this.marks.get(agentId) ?? new Map<string, Mark>()
    if (mark) marks.set(itemId, mark)
    else marks.delete(itemId)
    this.marks.set(agentId, marks)
    const held = new Set(this.store.assignedTo(agentId))
    for (const k of marks.keys()) if (!held.has(k) && k !== itemId) marks.delete(k)
  }

  /** A run begins: the bot's `last_run` says so, and what comes after now in its item is new to it. */
  startRun(agentId: string, itemIds: string[]): string {
    const id = randomUUID()
    const at = this.now()
    const mark = { n: this.store.moment(), at }
    this.running.set(agentId, { id, agentId, itemIds, startedAt: at, assigned: new Set(), before: { mark: this.markOn(agentId, itemIds[0]) ?? undefined, seen: this.seen(agentId) }, mark })
    for (const itemId of itemIds) this.setMark(agentId, itemId, mark)
    const lastRun: LastRun = { id, startedAt: at, endedAt: null, items: itemIds, costUsd: null, turns: null, error: null, output: null }
    this.setParam(agentId, 'last_run', lastRun, `bot:${agentId}`)
    const seen = this.seen(agentId)
    const unseen = itemIds.filter((i) => i !== agentId && !seen.includes(i))
    if (unseen.length) this.sessionSeen.set(agentId, [...seen, ...unseen])
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
                  items: z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional(), pin: z.boolean().optional() })).optional(),
                  // The old name (from when items were called items).
                  events: z.array(z.object({ type: z.string(), params: z.record(z.unknown()), ref: z.string().optional(), pin: z.boolean().optional() })).optional(),
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
        for (const u of updates) if (!this.row(u.item)) throw new Error(`No item ${u.item}.`)
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
        if (this.isClosed(itemId) || !this.assignees(itemId).includes(agentId)) return
        this.add(itemId, [{ type: 'topic', params: { text: `@${this.agentName(agentId)} failed: ${why}` } }], 'system')
        this.handBack(itemId, agentId, runId)
      }
      // Stopped by a usage limit, the run never really happened: the bot keeps its tasks, and what was new stays new, for
      // when the runner lets it run again.
      if (error && end.limited) {
        for (const itemId of run.itemIds) this.setMark(agentId, itemId, run.before.mark ?? null)
        this.sessionSeen.set(agentId, run.before.seen)
        return
      }
      if (error) {
        for (const itemId of run.itemIds) failed(itemId, error)
        return
      }
      const broken = new Set<string>()
      // What a person said in a task while the run went on, the run did not see: a run's update can't hand that task
      // away from the bot. The bot keeps it (beside whoever the update gave it to), and wakes again for it.
      const mark = run.mark
      const heard = (itemId: string) =>
        run.assigned.has(itemId) || this.store.children(itemId).some((e) => after(e.added_n, e.created_at, mark) && this.isUser(e.created_by))
      for (const u of updates) {
        if (this.isClosed(u.item)) continue
        try {
          // Each update is checked on its own: one that is wrong fails only its task.
          // Handing a task to a bot or script that is gone (cancelled since) drops it from the list, rather than failing the update.
          const gone = (u.assign ?? []).filter((a) => !this.holder(a) && this.goneHolder(a))
          const kept = (u.assign ?? []).filter((a) => !gone.includes(a))
          const to = u.assign && (kept.length || !gone.length) ? new Set(kept.map((a) => this.resolveAssignee(a))) : null
          if (gone.length) {
            this.add(u.item, [{ type: 'topic', params: { text: `@${this.agentName(agentId)} gave this to ${gone.map((a) => this.goneHolder(a)).join(', ')}, which can't have tasks now (cancelled or closed).` } }], 'system')
            if (!to && this.assignees(u.item).includes(agentId)) this.handBack(u.item, agentId, runId)
          }
          if (to && !to.has(agentId) && !u.close && heard(u.item)) {
            to.add(agentId)
            this.setMark(agentId, u.item, { ...mark, again: true })
          }
          const specs: ItemSpec[] = [...(u.message ? [{ type: 'topic' as const, params: { text: u.message } }] : []), ...this.prepare(u.items ?? [], 'bot')]
          if (specs.length) this.add(u.item, specs, actor)
          if (to) {
            const now = this.assignees(u.item)
            for (const a of to) if (!now.includes(a)) this.setAssignee(u.item, a, true, actor, runId)
            for (const a of now) if (!to.has(a)) this.setAssignee(u.item, a, false, actor, runId)
          }
          if (u.close && this.row(u.item)?.type === 'topic') closing.push(u.item)
        } catch (e) {
          broken.add(u.item)
          failed(u.item, `its update could not be applied: ${(e as Error).message}`)
        }
      }
      // A task it said who has is settled; the rest are settled here.
      const said = new Set([...updates.filter((u) => u.assign).map((u) => u.item), ...broken])
      for (const itemId of run.itemIds) if (!closing.includes(itemId) && !said.has(itemId) && this.row(itemId) && !this.isClosed(itemId)) this.settle(itemId, run)
    })
    // Done for now: its next run starts a fresh session (with the handoff said once), rather than carrying this one on.
    if (handoff && !error && !this.row(agentId)?.resolved) {
      this.setParam(agentId, 'handoff', handoff, actor)
      this.setParam(agentId, 'session_id', null, actor)
      this.sessionSeen.set(agentId, [])
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
    if (blockers.some((b) => b.createdBy === `bot:${agentId}`)) return
    // A person wrote in it while it ran: the run did not see that, so the bot keeps it, and is due on it again.
    if (this.store.children(itemId).some((e) => after(e.added_n, e.created_at, run.mark) && this.isUser(e.created_by))) return void this.setMark(agentId, itemId, { ...run.mark, again: true })
    this.handBack(itemId, agentId, run.id)
  }

  /** Takes a task off a bot; if that leaves it with nobody, it goes to its owner, if it has one. */
  private handBack(itemId: string, agentId: string, runId: string | null, actor = `bot:${agentId}`): void {
    this.setAssignee(itemId, agentId, false, actor, runId)
    const owner = this.owner(itemId)
    if (!this.assignees(itemId).length && owner && owner !== agentId && this.holder(owner) === owner) this.setAssignee(itemId, owner, true, actor, runId)
  }

  // ---- actions ----

  /** The one way into an action, from MCP, a script or the UI: checked, then run. */
  async callAction(called: string, given: Record<string, unknown>, ctx: ActionContext): Promise<unknown> {
    // Old names and parameters (from when items were called items) are taken as the new.
    const { name, args } = current(called, given ?? {})
    const action = this.actions.find((a) => a.name === name)
    if (!action) throw new Error(`No action ${name}.`)
    const parsed = z.object(action.input).safeParse(args ?? {})
    if (!parsed.success) throw new Error(`Bad arguments for ${name}: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
    return action.run(parsed.data, ctx)
  }

  // ---- items ----

  /** Checks items and expands them as their types say, without touching the store. An older build's `task` and `note` are topics. */
  prepare(specs: ItemSpec[], author: Author): ItemSpec[] {
    return specs.flatMap((given) => {
      const s = { ...given, ...asTopic(given) } as ItemSpec
      const type = this.type(s?.type)
      if (author === 'bot' && !type.bots) throw new Error(`A bot may not add a ${s.type} item.`)
      const parsed = z.object(type.params).safeParse(s.params ?? {})
      if (!parsed.success)
        throw new Error(`Bad params for a ${s.type} item: ${parsed.error.issues.map((i) => `${i.path.join('.')}: ${i.message}`).join('; ')}`)
      let params = parsed.data as Record<string, unknown>
      if (s.type === 'file') params = this.files.prepare(params)
      // Who a new topic goes to, as ids.
      if (s.type === 'topic' && params.assignees) params.assignees = (params.assignees as string[]).map((a) => this.resolveAssignee(a))
      if (s.type === 'user' && this.row(String(params.username))) throw new Error(`The username ${params.username} is taken.`)
      // Prepared now, so a bad one fails the whole list; added in the topic as it is.
      for (const k of ['comments', 'notes'] as const) if (s.type === 'topic' && params[k]) params[k] = this.prepare(params[k] as ItemSpec[], author)
      const expanded = type.expand ? type.expand(params, author) : [{ type: s.type, params }]
      // An id of its own: a person's (the app's), never a bot's.
      const id = s.id && author === 'human' ? checkedId(s.id) : undefined
      return expanded.map((e, i) => ({
        type: e.type as ItemSpec['type'],
        params: e.params,
        ref: i === 0 ? (s.ref ?? e.ref) : e.ref,
        ...(i === 0 && s.pin ? { pin: true } : {}),
        ...(i === 0 && s.bare ? { bare: true } : {}),
        ...(i === 0 && id ? { id } : {}),
      }))
    })
  }

  /**
   * Adds items, already prepared, in a topic (its comments, or its notes with `via`) or a bot's or script's own chat
   * (or, for what may, in none). Plain ones resolve at once; blockers wait. A topic's first comments and notes are
   * added in it, and its `assignees` assigned; neither is kept in its values. A topic whose key is already taken is left
   * out. A topic, bot or script is owned by whoever made it if a person, else by the owner of what it is in, else by the
   * owner of the bot that made it. Answers the ids added.
   */
  add(parentId: string | null, specs: ItemSpec[], createdBy: string, via: Via = 'comments'): string[] {
    return this.store.batch(() => this.addNow(parentId, specs, createdBy, parentId, via))
  }

  /** Adds one plain item with an id of its own choosing (a URL), in no item. */
  private addAs(id: string, spec: ItemSpec, createdBy: string): void {
    const at = this.now()
    this.store.insertItem({
      id, parent_id: null, via: null, seq: this.store.nextSeq(null), type: spec.type, params: this.prepare([spec], 'human')[0].params, created_by: createdBy, created_at: at,
      resolved: true, resolved_at: at, resolved_by: createdBy, cancelled: false, data: null, memo: null, next_check_at: null,
      last_error: null, runs: 0, added_n: 0, resolved_n: 0,
    })
  }

  private addNow(parentId: string | null, specs: ItemSpec[], createdBy: string, pinTo = parentId, via: Via = 'comments'): string[] {
    // A detached topic sits in no topic, wherever it is added (and is pinned there, if it says so).
    // So does anything else added with `pin` that may sit in none (a link, a file, a Slack message): it is pinned, not said.
    const detached = parentId ? specs.filter((s) => (s.type === 'topic' && s.params.detached) || (s.pin && s.type !== 'topic' && SIT_IN_NONE.includes(s.type))) : []
    if (detached.length) return [...this.addNow(parentId, specs.filter((s) => !detached.includes(s)), createdBy, pinTo, via), ...this.addNow(null, detached, createdBy, parentId)]
    const keyed = (s: ItemSpec) => s.type === 'topic' && s.params.key && this.store.ofType('topic').some((r) => r.params.key === s.params.key)
    // One with an id that is taken was added already (the same change, sent again).
    specs = specs.filter((s) => !keyed(s) && !(s.id && this.row(s.id)))
    if (!parentId && specs.some((s) => !SIT_IN_NONE.includes(s.type))) throw new Error(`Only these can sit in no topic: ${SIT_IN_NONE.join(', ')}.`)
    if (parentId && specs.some((s) => s.type === 'bot' || s.type === 'user')) throw new Error('Bots and people sit in no topic.')
    if (parentId && this.row(parentId)?.type === 'bot' && specs.some((s) => s.type !== 'topic')) throw new Error("A bot's own chat holds only topics.")
    const ids: string[] = []
    const seq = this.store.nextSeq(parentId)
    const at = this.now()
    specs.forEach((spec, i) => {
      const type = this.type(spec.type)
      // A person's id is their username.
      const id = spec.type === 'user' ? String(spec.params.username) : (spec.id ?? randomUUID())
      ids.push(id)
      const { assignees, comments, notes, detached: _d, ...params } = spec.params
      const topic = spec.type === 'topic'
      const closed = topic && params.open === false
      this.store.insertItem({
        id, parent_id: parentId, via: parentId ? via : null, seq: seq + i, type: spec.type, params, created_by: createdBy, created_at: at,
        resolved: topic ? closed : !type.blocking, resolved_at: topic ? (closed ? at : null) : type.blocking ? null : at,
        resolved_by: topic ? (closed ? createdBy : null) : type.blocking ? null : createdBy, cancelled: false,
        data: null, memo: null, next_check_at: null, last_error: null,
        runs: 0, added_n: 0, resolved_n: 0,
      })
      if (OWNED.includes(spec.type) && !spec.bare) {
        const owner = this.ownerFor(createdBy, parentId)
        if (owner) this.store.setOwner(id, owner, createdBy, at)
      }
      if (topic) {
        if (Array.isArray(comments) && comments.length) this.addNow(id, comments as ItemSpec[], createdBy, id, 'comments')
        if (Array.isArray(notes) && notes.length) this.addNow(id, notes as ItemSpec[], createdBy, id, 'notes')
        // A new open topic goes to those given; else, made by a person, to them; else (a bot made it) to nobody.
        const to = Array.isArray(assignees) && assignees.length ? (assignees as string[]) : params.open === true && this.isUser(createdBy) && !spec.bare ? [createdBy] : []
        for (const a of to) this.setAssignee(id, a, true, createdBy)
      }
      // An open topic made in another is pinned to it (a subtask), as is anything added with `pin`.
      if (pinTo && (spec.pin || (topic && params.open === true && via === 'comments' && this.row(pinTo)?.type === 'topic'))) this.store.pin(pinTo, id, createdBy, at)
    })
    // Whatever a topic's text links to is pulled in, as an item whose id is its URL (a Slack message or channel, or a
    // link), in no topic: made the first time, so `[url]()` (or the URL itself) opens it. It shows in no chat; pin it
    // where it is wanted.
    for (const spec of specs.filter((s) => s.type === 'topic'))
      for (const url of urlsIn(String(spec.params.text ?? ''))) if (!this.row(url)) this.addAs(url, pulledIn(url, at), createdBy)
    // New waits are checked at once.
    if (specs.some((s) => s.type === 'script')) queueMicrotask(() => void this.checkDue())
    this.emit('change')
    return ids
  }

  /** Resolves a blocker, as `by`. */
  private resolve(itemId: string, data: Record<string, unknown>, by: string, cancelled = false): void {
    const r = this.row(itemId)
    if (!r || r.resolved) return
    this.store.resolveItem(itemId, data, by, this.now(), cancelled)
    // A script cancelled lets go of the topics it still has: each goes back to its owner, if nobody else has it.
    if (r.type === 'script') for (const t of this.assignedTo(itemId)) this.handBack(t, itemId, null, 'script')
  }

  /**
   * Runs a script now, once: its play button, or its card in an item's right pane. A person's press gives it the unsafe
   * tools. `item` is what it runs on (left out: the first topic it holds) and `focus` the item on screen.
   */
  async runNow(itemId: string, actor = this.me(), given: { item?: string | null; focus?: string | null } = {}): Promise<void> {
    const r = this.row(itemId)
    if (r?.type !== 'script' || r.resolved) throw new Error('There is nothing to run.')
    this.pressed.set(itemId, actor)
    const error = await this.runScriptItem(itemId, given)
    if (error) throw new Error(error)
  }

  /** An item as a script is given it (`item`, `focus`): its id, type, title, values, owner, who has it and where it sits. */
  private scriptItem(id: string | null | undefined): Record<string, unknown> | null {
    const r = id ? this.row(id) : null
    if (!r) return null
    return { id: r.id, type: r.type, title: this.title(r.id, r.params), values: r.params, owner: this.owner(r.id), assignees: this.assignees(r.id), parentId: r.parent_id, closed: this.isClosed(r.id) }
  }

  /** Writes one param of an item: for the app's own bookkeeping, such as a bot's session and last run. */
  private setParam(itemId: string, key: string, value: unknown, by: string): void {
    this.store.setParams(itemId, { ...this.row(itemId)!.params, [key]: value }, by, this.now())
  }

  // ---- people ----

  /** A new open topic, in `parentId` or in none, with comments to start it off. */
  async create(
    title: string,
    opts: { parentId?: string | null; key?: string; specs?: ItemSpec[]; assignees?: string[]; actor?: string; author?: Author; id?: string } = {},
  ): Promise<string | null> {
    const author = opts.author ?? 'human'
    // Made already (the same change sent again): it is that one.
    if (opts.id && this.row(checkedId(opts.id))) return opts.id
    const params = { text: title, open: true, ...(opts.key ? { key: opts.key } : {}), ...(opts.specs ? { comments: opts.specs } : {}), ...(opts.assignees ? { assignees: opts.assignees } : {}) }
    const [spec] = this.prepare([{ type: 'topic', params, ...(opts.id ? { id: opts.id } : {}) }], author)
    const [id] = this.add(opts.parentId ?? null, [spec], author === 'bot' ? 'bot' : (opts.actor ?? this.me()))
    await this.idle()
    return id ?? null
  }

  /**
   * The chat input: `text` through a preset, as `actor`. Whoever (a bot or a person) is `[id]()`-referred to in the
   * text is assigned the item, as is whoever an `assign:<id>` preset names; then the sender is taken off it. In a bot's
   * own chat, the bot is, when nobody else is. Assigning a bot that has the item already assigns it again, which makes it
   * due there again.
   */
  async send(itemId: string, presetId: string, text: string, actor = this.me(), commentId?: string): Promise<void> {
    // Sent already (the phone sends again what it can't tell landed): nothing more.
    if (commentId && this.row(checkedId(commentId))) return
    const assignTo = presetId.startsWith('assign:') ? presetId.slice('assign:'.length) : null
    const preset = this.presets.find((p) => p.id === (assignTo ? 'note' : presetId))
    if (!preset) throw new Error(`No preset ${presetId}.`)
    if (this.must(itemId).closed) throw new Error('This is closed.')
    const live = (id: string) => this.holder(id) === id
    const named = [...new Set([...(assignTo ? [this.resolveAssignee(assignTo)] : []), ...refsIn(text).filter(live)])].filter((id) => id !== actor)
    // Written in a bot's own chat, it goes to the bot (as any item goes to whoever is named in what is sent).
    const own = this.row(itemId)?.type === 'bot' ? [itemId] : []
    const to = named.length ? named : own
    const specs = this.prepare(preset.build(text), 'human')
    if (commentId && specs.length) specs[0] = { ...specs[0], id: commentId }
    if (!specs.length && !to.length) throw new Error('Nothing to send.')
    this.store.batch(() => {
      if (specs.length) this.add(itemId, specs, actor)
      for (const a of to) this.setAssignee(itemId, a, true, actor)
      // Handing it on takes it off whoever sent it.
      if (to.length) this.setAssignee(itemId, actor, false, actor)
    })
    await this.idle()
  }

  /**
   * Adds items in a topic, its comments (or its notes, with `via`), or topics and the like in none: from a person, or from
   * an agent through MCP (author 'bot', so its rules apply).
   */
  async addItems(parentId: string | null, specs: ItemSpec[], actor = this.me(), author: Author = 'human', via: Via = 'comments'): Promise<string[]> {
    // A bot's or a script's own chat takes topics too, and any other item holds what is put in it.
    if (parentId && !this.row(parentId)) throw new Error(`No item ${parentId}.`)
    if (parentId && this.row(parentId)!.type === 'topic' && this.isClosed(parentId) && via === 'comments') throw new Error('This topic is closed.')
    const ids = this.add(parentId, this.prepare(specs, author), actor, via)
    // A script is run from inside a task's queue, so waiting for the queues to empty would wait for itself.
    if (actor !== 'script') await this.idle()
    return ids
  }

  // ---- files ----

  private fileParams(itemId: string): Record<string, unknown> {
    const r = this.row(itemId)
    if (r?.type !== 'file') throw new Error(`${itemId} is not a file.`)
    return r.params
  }

  /** A person's upload (or paste, or drop): kept with the source, as a file item in the task. */
  async uploadFile(parentId: string, name: string, base64: string, actor = this.me()): Promise<string> {
    const bytes = Buffer.from(base64, 'base64')
    const hash = this.files.keep(bytes)
    const [id] = await this.addItems(parentId, [{ type: 'file', params: { name: name || 'file', hash, size: bytes.length } }], actor)
    return id
  }

  /** A file item's bytes, for the window to show: base64, with its name and type. */
  fileData(itemId: string): { name: string; mime: string; base64: string } {
    const p = this.fileParams(itemId)
    return { name: String(p.name), mime: String(p.mime), base64: this.files.read(p).toString('base64') }
  }

  /** A file item as text, with whether it can be edited (a file here, not a kept copy), how big it is and if it is gone. */
  fileText(itemId: string): { text: string | null; editable: boolean; path: string | null; size: number | null; missing: boolean } {
    const p = this.fileParams(itemId)
    const { size, missing } = this.files.stat(p)
    const text = !missing && isText(String(p.name), String(p.mime)) ? this.files.read(p).toString('utf8') : null
    return { text, editable: !p.hash && !!p.path && !missing, path: p.path ? String(p.path) : null, size, missing }
  }

  /** Writes a person's edit over the file a file item refers to. */
  saveFileText(itemId: string, text: string): void {
    this.files.saveText(this.fileParams(itemId), text)
    this.emit('change')
  }

  /** The old name for `close`: ends a blocker without it happening, closes a topic, deletes a bot. */
  async cancel(itemId: string, actor = this.me()): Promise<void> {
    return this.close(itemId, { actor })
  }

  /**
   * Opens a closed item again (its status open): its owner and who had it come back, as they were when it closed
   * (`last_owner`, `last_assignees`), but for bots and scripts gone since. What its closing cancelled stays cancelled.
   * A topic's chat says who reopened it. A cancelled script or bot comes back as it was, holding nothing.
   */
  async reopen(itemId: string, actor = this.me()): Promise<void> {
    const row = this.row(itemId)
    if (!row) throw new Error(`No item ${itemId}.`)
    if (!this.isClosed(itemId)) return
    this.store.batch(() => {
      if (row.params.open === false) this.store.setOpen(itemId, true, actor, this.now())
      if (row.resolved && row.type !== 'topic' && this.blocking(row.type)) this.store.reopenItem(itemId, actor, this.now())
      const { last_owner: owner, last_assignees: had } = row.params
      if (typeof owner === 'string' && !this.owner(itemId) && this.holder(owner) === owner) this.store.setOwner(itemId, owner, actor, this.now())
      for (const a of Array.isArray(had) ? had.map(String) : []) if (this.holder(a) === a && !this.assignees(itemId).includes(a)) this.setAssignee(itemId, a, true, actor)
      if (owner !== undefined || had !== undefined) {
        const { last_owner: _o, last_assignees: _a, ...rest } = this.row(itemId)!.params
        this.store.setParams(itemId, rest, actor, this.now())
      }
      if (row.type === 'topic') this.add(itemId, [{ type: 'topic', params: { text: `Reopened by ${this.actorName(actor)}.` } }], 'system')
    })
    this.emit('change')
    if (actor !== 'script') await this.idle()
  }

  /**
   * Pins items to an item (`pins` links), or unpins them: what is pinned to an item is what is useful in actioning it
   * (subtasks, pull requests, files, links); followed backwards, where an item is referenced.
   */
  async pin(itemId: string, itemIds: string[], on = true, actor = this.me()): Promise<void> {
    if (!this.row(itemId)) throw new Error(`No item ${itemId}.`)
    for (const id of itemIds) if (on && !this.row(id)) throw new Error(`No item ${id}.`)
    this.store.batch(() => {
      for (const id of itemIds) on ? this.store.pin(itemId, id, actor, this.now()) : this.store.unpin(itemId, id, actor, this.now())
    })
    this.emit('change')
  }

  /**
   * Moves an item into another (null: into none), into its notes unless `via` says its comments: the notes view's move
   * and link, and taking a note out of its parent. Who made it, and when, stay.
   */
  async moveItem(itemId: string, to: string | null, actor = this.me(), via: Via = 'notes'): Promise<void> {
    this.store.batch(() => this.store.move(itemId, to, via, actor, this.now()))
    this.emit('change')
  }

  /**
   * A set of notes: a topic titled `title` that shows its notes, in no item, pinned to `pinTo` (if given). Its notes are
   * the topics in its notes (`notes` links), however deep: the notes view draws them as a tree. Answers its id.
   */
  newNotes(title: string, pinTo: string | null, actor = this.me()): string {
    if (pinTo && !this.row(pinTo)) throw new Error(`No item ${pinTo}.`)
    return this.store.batch(() => {
      const [id] = this.addNow(null, [{ type: 'topic', params: { text: title, display: 'notes' } }], actor)
      if (pinTo) this.store.pin(pinTo, id, actor, this.now())
      this.emit('change')
      return id
    })
  }

  /**
   * An item and its notes, the notes view's tree: the topic first, then each item in its notes, depth first, oldest
   * first (each with its `parentId`). Below it, only topics' notes are walked into: anything else is a leaf. At most `limit`.
   */
  subtree(id: string, limit = 20_000): ItemInfo[] {
    const root = this.row(id)
    if (!root) return []
    const out: ItemInfo[] = []
    const walk = (r: ItemRow) => {
      if (out.length >= limit) return
      out.push(this.toInfo(r))
      // Any item has notes; within them, only topics' are walked into.
      if (r === root || r.type === 'topic') for (const k of this.store.children(r.id, 'notes')) walk(k)
    }
    walk(root)
    return out
  }

  /** An item at a size, as text (views.ts): for a bot unless told, with ids; an id no item has, as what it looks like. */
  view(id: string, size: Size, opts: { ids?: boolean; offset?: number; limit?: number } = {}): string | null {
    const read = { info: (i: string) => this.info(i) ?? this.inferredInfo(i), children: (i: string) => this.children(i), notes: (i: string) => this.notes(i) }
    return viewText(read, id, size, { ids: true, who: this.who(), ...opts })
  }

  /**
   * An id no item has, as the item it looks like (`inferType`: a URL a Slack message, a pull request, a link), made up
   * for showing and not kept; null if it looks like nothing.
   */
  inferredInfo(id: string): ItemInfo | null {
    if (this.row(id)) return null
    const got = inferType(id)
    if (!got || !this.typeOrNull(got.type)) return null
    const at = this.now()
    return this.toInfo({
      id, parent_id: null, via: null, seq: 0, type: got.type, params: got.params, created_by: 'system', created_at: at,
      resolved: true, resolved_at: at, resolved_by: 'system', cancelled: false, data: null, memo: null, next_check_at: null,
      last_error: null, runs: 0, added_n: 0, resolved_n: 0,
    })
  }

  /**
   * Sets an item's type to what it looks like (`inferType`, from its id and values), with the params that type needs
   * (what it has wins): the debug view's correction. An id no item has is made the item it looks like.
   */
  async setInferredType(id: string, actor = this.me()): Promise<void> {
    const row = this.row(id)
    const got = inferType(id, row?.params ?? {})
    if (!got || !this.typeOrNull(got.type)) throw new Error(`${id} looks like no type.`)
    const params = this.prepare([{ type: got.type as ItemSpec['type'], params: got.params }], 'human')[0].params
    this.store.batch(() => {
      if (!row) return this.addAs(id, { type: got.type as ItemSpec['type'], params }, actor)
      this.store.setType(id, got.type, actor, this.now())
      this.store.setParams(id, params, actor, this.now())
    })
    this.emit('change')
  }

  /** Who did something, as a bot is told: a name, and whether a person said it. */
  who(): Who {
    return { name: (a) => this.actorName(a), person: (a) => this.isUser(a) || a === 'mcp' }
  }

  /**
   * Closes any item (its status closed), and lets it float free: its owner and assignees are taken off it, kept as
   * `last_owner` and `last_assignees` for `reopen`. A topic's open blockers are cancelled (topics and scripts in it are
   * left as they are: they don't depend on it); a bot is deleted, and a script or other blocker cancelled, handing back
   * what they had.
   */
  async close(itemId: string, opts: { actor?: string } = {}): Promise<void> {
    const actor = opts.actor ?? this.me()
    const row = this.row(itemId)
    if (!row) throw new Error(`No item ${itemId}.`)
    if (this.isClosed(itemId)) return
    if (row.type === 'bot') await this.deleteAgent(itemId, actor)
    this.store.batch(() => {
      const owner = this.owner(itemId)
      const had = this.assignees(itemId)
      if (row.type === 'topic')
        for (const b of this.children(itemId, true)) if (b.type !== 'script') this.resolve(b.id, { cancelled: true, detail: 'Its topic closed.' }, actor, true)
      if (owner || had.length)
        this.store.setParams(itemId, { ...row.params, ...(owner ? { last_owner: owner } : {}), ...(had.length ? { last_assignees: had } : {}) }, actor, this.now())
      for (const a of had) this.setAssignee(itemId, a, false, actor)
      if (owner) this.store.setOwner(itemId, null, actor, this.now())
      // A blocker resolves (cancelled), as it always has; anything else takes the status closed.
      if (row.type !== 'topic' && this.blocking(row.type)) {
        if (!row.resolved) this.resolve(itemId, { cancelled: true }, actor, true)
      } else this.store.setOpen(itemId, false, actor, this.now())
    })
    this.emit('change')
    if (actor !== 'script') await this.idle() // a script waiting on its own queue would wait for itself
  }

  // ---- the loop ----

  /** One turn of the central loop: every blocker check that is due. */
  async tick(): Promise<void> {
    await this.checkDue()
  }

  /**
   * Runs every script the loop runs (`runnableScripts`: owned from the person signed in, through bots) that is due
   * (`due`, as bots are), at most `scripts.max_running` (MAX_SCRIPTS) at once, those due longest first.
   */
  async checkDue(): Promise<void> {
    const now = this.now()
    const runnable = this.runnableScripts()
    const open = this.store.ofType('script').filter((r) => !r.resolved && runnable.has(r.id))
    for (const r of open) r.next_check_at = this.due(r.id)?.at ?? null
    const due = open.filter((r) => r.next_check_at !== null && r.next_check_at <= now && !this.checking.has(r.id)).sort((a, b) => a.next_check_at! - b.next_check_at!)
    const room = Math.max(0, Number(this.settings.get('scripts.max_running', MAX_SCRIPTS)) - this.checking.size)
    await Promise.all(due.slice(0, room).map((r) => this.runScriptItem(r.id)))
  }

  /**
   * Runs a script once: its source, given the script itself (its values, state and runs), the one topic it is due on (or,
   * pressed, the first it holds), and the tools; then what it returned. Unsafe tools only for a person's own press. A
   * script never ends on its own: a run that throws is said in its chat (once, till the error changes) and it carries on,
   * on its cycle. Answers the run's error, if any.
   */
  private runScriptItem(id: string, given: { item?: string | null; focus?: string | null } = {}): Promise<string | null> {
    this.checking.add(id)
    return this.serial(id, async () => {
      const row = this.row(id)
      if (!row || row.resolved) return null
      const pressedBy = this.pressed.get(id) ?? null
      this.pressed.delete(id)
      const pressed = !!pressedBy && this.isUser(pressedBy)
      const author: Author = pressed ? 'human' : 'bot'
      const actor = pressed ? pressedBy! : 'script'
      const woke = this.now()
      const due = this.due(id)
      // Pressed on an item, it runs on that; else on the topic it is due on (pressed: the first it holds), marked as seen.
      const topic = given.item ?? (due && (pressed || due.at <= woke) ? due.item : pressed ? (this.assignedTo(id)[0] ?? null) : null)
      if (topic && !given.item) this.setMark(id, topic, { n: this.store.moment(), at: woke })
      const { source, last_wake: _w, last_seen: _s, ...own } = row.params
      const context = {
        ...own,
        id,
        name: own.name ?? own.description ?? null,
        runs: row.runs,
        state: row.memo ?? null,
        topics: topic ? [{ id: topic, title: this.get(topic)?.title ?? topic }] : [],
        now: new Date(woke).toISOString(),
        trigger: pressed ? 'press' : 'wake',
      }
      let error: string | null = null
      try {
        const ran = await runScript(String(source ?? ''), context, (name, args) => this.callAction(name, args, { engine: this, author, actor, itemId: row.parent_id ?? id, runId: null }), {
          item: this.scriptItem(topic),
          focus: this.scriptItem(given.focus),
        })
        const parsed = scriptResult.safeParse(ran.value)
        if (!parsed.success) throw new Error(`It returned ${JSON.stringify(ran.value)?.slice(0, 200)}, not { messages?, assign?, state? }.`)
        const r = parsed.data ?? {}
        for (const m of r.messages ?? []) await this.addItems(m.item ?? row.parent_id ?? id, [{ type: 'topic', params: { text: m.text } }], 'script', author)
        for (const a of r.assign ?? []) {
          const to = new Set(a.to.map((x) => (x === id ? x : this.resolveAssignee(x))))
          const now = this.assignees(a.item)
          for (const x of to) if (!now.includes(x)) await this.assign(a.item, x, true, 'script')
          for (const x of now) if (!to.has(x)) await this.assign(a.item, x, false, 'script')
        }
        if ('state' in r) this.store.setState(id, r.state, this.now())
      } catch (e) {
        error = (e as Error).message
      }
      row.runs++
      // By `script`, so saying it is not something happening to it (which would wake it again).
      if (error && error !== row.last_error) await this.addItems(row.parent_id ?? id, [{ type: 'topic', params: { text: `${this.title(id, row.params)} failed: ${error}` } }], 'script', author)
      row.last_error = error
      row.next_check_at = this.due(id)?.at ?? null
      this.emit('change')
      return error
    }).finally(() => this.checking.delete(id))
  }

}

/** An id the app chose: a UUID, or it is refused. */
function checkedId(id: string): string {
  if (!/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(id)) throw new Error(`Not an id: ${id}.`)
  return id
}
