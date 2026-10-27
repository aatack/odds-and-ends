// The shapes the main process and the renderer both read. docs/events.md is the
// long form of what an event is.

/**
 * Everything is an event, items included. Every event but an item, a bot or a
 * person sits in an item or a bot (its parent), and an item may sit in another.
 * Some events resolve as they are added (a note); the rest are **blockers**,
 * which stay open until something happens (a script says it is done, an approval
 * is given, a child item closes).
 */
export type EventKind = 'task' | 'note' | 'approval' | 'script' | 'bot' | 'user'

/** An event as it is added. `ref` names it, so another added alongside can point at it. */
export interface EventSpec {
  type: EventKind
  params: Record<string, unknown>
  ref?: string
}

/**
 * One event on one item: the interface every type shares. docs/events.md has
 * the table. What an event *is* (its params, data, and how it reads) depends
 * on its type; everything else here is the same for all of them.
 */
export interface ItemEvent {
  id: string
  /** The item it sits in; null only for an item that sits in none. */
  parentId: string | null
  type: EventKind
  /** What it was given, as its type's params schema says. */
  params: Record<string, unknown>

  /** Who added it: 'human', 'bot', 'system', 'script', 'script:bot', 'mcp', or a source's id. */
  createdBy: string
  createdAt: number

  /** It has happened (or was cancelled; for an item, closed). A plain event is resolved as it is added. */
  resolved: boolean
  /** When, known for what resolved since the app started: egdb's history of `resolved` has the rest. */
  resolvedAt: number | null
  /** Who resolved it: the human (approving), a bot (its run ending), 'system' (a check), or as createdBy for a plain event. */
  resolvedBy: string | null
  /** Resolved without happening. */
  cancelled: boolean
  /** What it resolved with, as its type's data says; null while open. */
  data: Record<string, unknown> | null

  /** Who made it happen: resolvedBy for a blocker, createdBy for a plain event. The chat puts the human's on the right. */
  actor: string
  /** One line saying what it is, from its type's `describe`: a blocker's row while open, and how prompts name it. */
  summary: string
  /** What it says in the chat once resolved, from its type's `said`; null while open, or when it says nothing. */
  message: string | null

  /** For the blockers the loop checks: when next, and what went wrong last. */
  nextCheckAt: number | null
  lastError: string | null
  /** How many times it has been checked (a script, run) since the app started. */
  runs: number
  /** Who reacted to it (a thumbs-up): people's usernames, or bots' actors (`bot:<id>`). */
  reactions: string[]
}

/** A task (or a bot, for its own chat), as the lists and its view read it. */
export interface Item {
  id: string
  title: string
  /** The item it sits in, if any. It blocks that item until it closes. */
  parentId: string | null
  closed: boolean
  /** How it closed: 'closed' or 'cancelled'. */
  how: string | null
  /** Its open children, oldest first. */
  blockers: ItemEvent[]
  /**
   * Who it is assigned to: people (their usernames) and bots (their ids), the earliest assigned first. These are
   * `assigned_to` links from the item, not events in it.
   */
  assignees: string[]
  /** Open, assigned to the person looking, and blocked by nothing but people's own approvals: it is their turn. */
  withYou: boolean
  /** The bot running on it now, if one is. Held in memory only. */
  bot: string | null
  /** Since when it has been as it is: the last child added or resolved. */
  since: number
  createdAt: number
  closedAt: number | null
}

export interface ItemDetail {
  item: Item
  /** The item it sits in, if any. */
  parent: { id: string; title: string } | null
  /** Its children, oldest first: every event in it, child items included. */
  events: ItemEvent[]
}

/** Any item, with what its views need: the task it sits in, and (for a task, or a bot) it in full. */
export interface EventDetail {
  event: ItemEvent
  parent: { id: string; title: string } | null
  /** For an item, or a bot (whose own chat it is): it in full. */
  item: ItemDetail | null
}

/**
 * A bot: a `bot` event, in no item, its settings in its params, and its own chat in it. It is assigned items, and
 * wakes when something happens to it. One bot runs one thing at a time, in one Claude session of its own, in a
 * folder of its own.
 */
export interface Agent {
  id: string
  name: string
  /** What it is for and how it works: its system prompt, after the app's own instructions. */
  instructions: string
  /** Who runs it: 'claude' (Claude Code) for now. */
  provider: string
  /** The model, as the provider names it (for Claude: opus, sonnet, haiku or a full id). */
  model: string
  /** How long it waits after the last thing that happened in its items before it wakes, so a burst of work goes in one run. */
  debounceMinutes: number
  /** Where it runs: its own folder, which lasts, so it can keep repositories there. */
  folder: string
  /** Its Claude session, once it has run. */
  sessionId: string | null
  /** How its last run went (its `last_run` value; egdb keeps every one before), or null before it has run. */
  lastRun: LastRun | null
  createdAt: number
  /** Deleted (its agent event cancelled): it is assigned nothing more and never runs again. */
  deleted: boolean
  /** The run going now, if any. */
  runId: string | null
  /** When it wakes next, if something new is waiting for it (it may already be due). */
  wakesAt: number | null
  /** The open items assigned to it. */
  assigned: string[]
}

/** A bot's last run, as its `last_run` value keeps it. */
export interface LastRun {
  id: string
  startedAt: number
  /** Null while it runs. */
  endedAt: number | null
  /** The tasks it was woken for (and its own id, for its chat). */
  items: string[]
  costUsd: number | null
  turns: number | null
  error: string | null
  /** What it wrote along the way, cut short. */
  output: string | null
}

/** An item as egdb holds it, for the debug view: its values, its links (each with who wrote it and when), and who made it. */
export interface Inspection {
  id: string
  values: Record<string, unknown>
  /** When each value was set, and by whom (egdb's event for it); empty with no egdb. */
  set: Record<string, { at: number; by: string }>
  links: { type: string; direction: 'outbound' | 'inbound'; other: string; sort: number | null; at: number | null; by: string | null }[]
  createdAt: number | null
  createdBy: string | null
}

/** A person: a `user` event in no item, whose id is their username. */
export interface User {
  id: string
  name: string
}

export interface Overview {
  /** Who is looking: `withYou` and the recent lists are theirs. */
  me: User
  items: Item[]
  bots: Agent[]
  users: User[]
  /** Whoever (people and bots) the person looking assigned things to, the most recent first: the chat input offers them first. */
  recentAssignees: string[]
}

/** A JSON Schema for a flat object, as far as the forms read it. */
export interface ObjectSchema {
  properties?: Record<string, PropertySchema>
  required?: string[]
}

export interface PropertySchema {
  type?: string | string[]
  description?: string
  enum?: string[]
  items?: PropertySchema
  default?: unknown
}

/** One action, as MCP lists it. */
export interface ToolInfo {
  name: string
  description: string
  inputSchema: ObjectSchema
}

export interface ActionInfo extends ToolInfo {
  /** Changes things outside this app: bots, and scripts bots wrote, never get it. */
  unsafe: boolean
}

export interface EventTypeInfo {
  type: EventKind
  description: string
  params: ObjectSchema
  data: string
  blocking: boolean
}

/** A way to send from the chat input: what the typed text becomes. */
export interface PresetInfo {
  id: string
  name: string
  /** What happens, after the name, in the empty input. */
  hint: string
  /** The event type whose icon stands for it. */
  icon: EventKind
  /** What it does instead with nothing typed, if that differs: the input shows this while it is empty. */
  whenEmpty?: { name: string; hint: string; icon: EventKind }
}

export interface Catalog {
  actions: ActionInfo[]
  events: EventTypeInfo[]
  presets: PresetInfo[]
}

export interface Check {
  ok: boolean
  detail: string
}

export interface IntegrationsView {
  github: Check
  claude: Check & { bots: number }
  slack: { id: string; name: string; team: string; user: string; tokenHint: string }[]
}

export interface McpView {
  url: string
  token: string
  port: number
  error: string | null
}

/** The renderer's whole view of the main process. */
export interface WorkflowsAPI {
  overview: () => Promise<Overview>
  detail: (id: string) => Promise<ItemDetail | null>
  /** Any event, item or not, with what its views need. */
  eventDetail: (id: string) => Promise<EventDetail | null>
  catalog: () => Promise<Catalog>
  /** The item that has waited longest for the human, or null. */
  next: (skip: string[]) => Promise<string | null>
  /** An item as egdb holds it: its values and links. */
  inspect: (id: string) => Promise<Inspection>
  /** A new item, in `parentId` or in none. */
  create: (title: string, parentId: string | null) => Promise<string>
  /** Sends from the chat input: `text` through a preset. */
  send: (id: string, preset: string, text: string) => Promise<void>
  /** Runs an approval blocker's action with the human's inputs. */
  approve: (eventId: string, inputs: Record<string, unknown>) => Promise<void>
  /** Ends a blocker without it happening. */
  cancel: (eventId: string) => Promise<void>
  /** Runs an open script that has no schedule, once. */
  runNow: (eventId: string) => Promise<void>
  close: (id: string) => Promise<void>
  /** Turns the person's thumbs-up on an event on or off. */
  react: (eventId: string, on: boolean) => Promise<void>
  customActions: () => Promise<CustomAction[]>
  saveCustomAction: (a: Omit<CustomAction, 'id'> & { id?: string }) => Promise<CustomAction>
  removeCustomAction: (id: string) => Promise<void>
  /** Runs a custom action on an event. */
  runCustomAction: (id: string, eventId: string) => Promise<void>
  /** Writes a custom action's code from its description and notes, with a model. */
  generateCustomAction: (a: { type: string; name: string; description: string; notes: string; source?: string }) => Promise<string>
  /** Renames an item: its title changes (egdb keeps the old one). */
  rename: (id: string, title: string) => Promise<void>
  /** Makes a bot (no id) or changes its settings. */
  saveBot: (bot: { id?: string; name: string; instructions: string; provider: string; model: string; debounceMinutes: number; folder?: string }) => Promise<string>
  /** Deletes a bot: what it was assigned goes back to the human. */
  deleteBot: (id: string) => Promise<void>
  /** Assigns an item to a person or a bot (on), or takes the assignment away (off). */
  assign: (itemId: string, to: string, on: boolean) => Promise<void>
  /** Every person. */
  users: () => Promise<User[]>
  /** Adds a person (or renames one): their username is their id. */
  saveUser: (username: string, name: string) => Promise<User>
  /** Checks a username, to sign in with: no password, as the app is local. */
  signIn: (username: string) => Promise<User>
  /** Who the calls from here on are made as (kept by the preload, sent with each call). */
  setUser: (username: string | null) => void
  /** Opens a source: a file (made if it is not there; `~/` is the home folder), on a backend of its own. Answers its absolute path. */
  openSource: (path: string) => Promise<string>
  /** Lets go of the source open now (its backend stops, if the app started it). */
  closeSource: () => Promise<void>
  /** Wakes a bot now, whatever its debounce says. */
  wakeBot: (id: string) => Promise<void>
  integrations: () => Promise<IntegrationsView>
  /** Keeps a Slack workspace's user token (encrypted), once Slack says whose it is. Nothing uses it yet. */
  addSlack: (token: string) => Promise<void>
  removeSlack: (id: string) => Promise<void>
  mcp: () => Promise<McpView>
  regenerateMcpToken: () => Promise<void>
  setMcpPort: (port: number) => Promise<void>
  onChange: (cb: () => void) => () => void
  /** Ctrl+W, caught before the window's menu can close the window: take the top view off the stack. */
  onBack: (cb: () => void) => () => void
  /** Alt+Right: put the view last taken off the stack back on. */
  onForward: (cb: () => void) => () => void
}

/** One of the human's own actions, for events of one type: a script, usually written by a model from its description and notes. */
export interface CustomAction {
  id: string
  /** The event type it is for. */
  type: string
  name: string
  /** What it does: shown, and what its code is written from. */
  description: string
  /** More for the model writing its code; shown only on the Types page. */
  notes: string
  /** async (context, actions) => null | string | { message?, events? } */
  source: string
  /** A lucide icon, by its name in the Types page's list. */
  icon: string
  /** Shown as a button in an event's header; the rest are in the menu beside them. */
  important: boolean
}

/** WorkflowsAPI's calls (all but the subscriptions), by name: each goes to the backend as it is. */
export const API_METHODS = [
  'overview', 'detail', 'eventDetail', 'inspect', 'catalog', 'next', 'create', 'send', 'approve', 'cancel', 'runNow', 'close', 'react',
  'customActions', 'saveCustomAction', 'removeCustomAction', 'runCustomAction', 'generateCustomAction', 'rename', 'saveBot', 'deleteBot',
  'assign', 'wakeBot', 'users', 'saveUser', 'signIn', 'integrations', 'addSlack', 'removeSlack', 'mcp', 'regenerateMcpToken', 'setMcpPort',
] as const satisfies readonly ApiMethod[]

type ApiMethod = Exclude<keyof WorkflowsAPI, 'onChange' | 'onBack' | 'onForward' | 'setUser' | 'openSource' | 'closeSource'>
/** Fails to compile while a call is left out of API_METHODS. */
export const API_METHODS_COMPLETE: Exclude<ApiMethod, (typeof API_METHODS)[number]> extends never ? true : never = true
