// The shapes the main process and the renderer both read. docs/events.md is the
// long form of what an event is.

/**
 * Everything is an event, items included. Every event but an item sits in an
 * item (its parent), and an item may sit in another. Some events resolve as
 * they are added (a message, an action run); the rest are **blockers**, which
 * stay open until something happens (a bot replies, CI finishes, an approval is
 * given, a child item closes).
 */
export type EventKind =
  | 'item' | 'message' | 'attachment' | 'priority' | 'rename' | 'summary' | 'action' | 'approval' | 'bot' | 'github' | 'slack'
  | 'after' | 'script' | 'directive' | 'snooze' | 'agent' | 'configure' | 'assign' | 'watch'

/** The human, as an assignee. Every other assignee is a bot, by its id. */
export const HUMAN = 'human'

/** An event as it is added. `ref` names it, so an `after` added alongside can point at it. */
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
  /** Its timeline, read from the trace. */
  times: EventTimes
}

/** An event's timeline, read from the trace. */
export interface EventTimes {
  added: number | null
  firstChecked: number | null
  lastChecked: number | null
  checks: number
  firstRunStarted: number | null
  firstRunEnded: number | null
  runs: number
  resolved: number | null
}

/** An item event, as the lists and the item view read it. */
export interface Item {
  id: string
  title: string
  /** The item it sits in, if any. It blocks that item until it closes. */
  parentId: string | null
  closed: boolean
  /** How it closed: 'closed', 'handled_outside', or 'cancelled'. */
  how: string | null
  /** Its open children, oldest first. */
  blockers: ItemEvent[]
  /**
   * Who it is assigned to: 'human' and bots' ids, by the latest assign event for each. An item with no assignee is
   * the human's.
   */
  assignees: string[]
  /** Open, assigned to the human (or to nobody), and blocked by nothing but the human's own approvals: it is the human's turn. */
  withHuman: boolean
  /** Marked as a priority (by its latest priority event): Next takes it before the rest. */
  priority: boolean
  /** When it wakes, while a snooze blocks it (the latest, if several). */
  snoozedUntil: number | null
  /** The bot running on it now, if one is. Held in memory only. */
  bot: string | null
  /** Its own Claude session, from before bots were their own items: kept for the record. */
  sessionId: string | null
  /** Since when it has been as it is: the last child added or resolved. */
  since: number
  createdAt: number
  closedAt: number | null
}

/** One row of the trace: one thing that happened. */
export interface TraceEntry {
  id: number
  at: number
  /** The item it happened in (for an item's own creation and closing, the item itself). */
  itemId: string
  eventId: string | null
  runId: string | null
  actor: string
  kind: string
  data: Record<string, unknown> | null
}

export interface BotRun {
  id: string
  /** The bot that ran; null for a run from before bots were their own items. */
  agentId: string | null
  /** The items it was woken for. */
  itemIds: string[]
  eventIds: string[]
  startedAt: number
  endedAt: number | null
  costUsd: number | null
  turns: number | null
  result: string | null
}

export interface ItemDetail {
  item: Item
  /** The item it sits in, if any. */
  parent: { id: string; title: string } | null
  /** Its children, oldest first: every event in it, child items included. */
  events: ItemEvent[]
  runs: BotRun[]
  trace: TraceEntry[]
}

/** Any event, with what its views need: the item it sits in, its runs, its trace, and (for an item) the item in full. */
export interface EventDetail {
  event: ItemEvent
  parent: { id: string; title: string } | null
  /** The bot runs that answered it (for an item, every run on it). */
  runs: BotRun[]
  /** Every trace row about it. */
  trace: TraceEntry[]
  /** For an item event: the item in full. */
  item: ItemDetail | null
}

/**
 * A bot: an `agent` event, in no item, configured by the `configure` events in it. It is assigned items (and may
 * watch others), and wakes when anything happens in them. One bot runs one thing at a time, in one Claude session
 * of its own, in a folder of its own.
 */
export interface Agent {
  id: string
  name: string
  /** What it is for and how it works: given with every run, after the app's own instructions. */
  instructions: string
  /** Who runs it: 'claude' (Claude Code) for now. */
  provider: string
  /** The model, as the provider names it (for Claude: opus, sonnet, haiku or a full id); empty for the default. */
  model: string
  /** How long it waits after the last thing that happened in its items before it wakes, so a burst of work goes in one run. */
  debounceMinutes: number
  /** Where it runs: its own folder, which lasts, so it can keep repositories there. */
  folder: string
  /** Its Claude session, once it has run. */
  sessionId: string | null
  createdAt: number
  /** Deleted (its agent event cancelled): it is assigned nothing more and never runs again. */
  deleted: boolean
  /** The run going now, if any. */
  runId: string | null
  /** When it wakes next, if something new is waiting for it (it may already be due). */
  wakesAt: number | null
  /** The open items assigned to it. */
  assigned: string[]
  /** The open items it watches. */
  watching: string[]
}

/** An assign event, as the "recently assigned" lists read it. */
export interface Assignment {
  itemId: string
  title: string
  assignee: string
  on: boolean
  by: string
  at: number
}

export interface Overview {
  items: Item[]
  bots: Agent[]
  /** Items the human had on screen in the last week, the most recent first. */
  viewed: string[]
  /** Items the human attached something to, or attached, in the last week, the most recent first. */
  linked: string[]
  /** What the human attached in the last week, and to which item (`target`), the most recent first: a new item suggests them. */
  attachments: RecentAttachment[]
}

/** One of the human's recent attachments: the event attached (`event`, its `title`, the `item` to read it in) and where it went. */
export interface RecentAttachment {
  event: string
  title: string
  item: string
  target: string
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
  /** The human has the item on screen. */
  viewed: (id: string) => Promise<void>
  /** The human passed over the item for now. */
  skipped: (id: string) => Promise<void>
  /** A new item, in `parentId` or in none. */
  create: (title: string, parentId: string | null) => Promise<string>
  /** Sends from the chat input: `text` through a preset. */
  send: (id: string, preset: string, text: string) => Promise<void>
  /** Runs an approval blocker's action with the human's inputs. */
  approve: (eventId: string, inputs: Record<string, unknown>) => Promise<void>
  /** Ends a blocker without it happening. */
  cancel: (eventId: string) => Promise<void>
  close: (id: string) => Promise<void>
  /** Attaches any event to an item (`targetId`), as an attachment event in it. */
  attach: (eventId: string, targetId: string) => Promise<void>
  /** Asks a cheap model where an item stands: a summary event in it, answered in its chat. */
  summarise: (id: string) => Promise<void>
  customActions: () => Promise<CustomAction[]>
  saveCustomAction: (a: Omit<CustomAction, 'id'> & { id?: string }) => Promise<CustomAction>
  removeCustomAction: (id: string) => Promise<void>
  /** Runs a custom action on an event. */
  runCustomAction: (id: string, eventId: string) => Promise<void>
  /** Writes a custom action's code from its description and notes, with a model. */
  generateCustomAction: (a: { type: string; name: string; description: string; notes: string; source?: string }) => Promise<string>
  /** The directives that hold (not yet deleted), the newest first. */
  directives: () => Promise<ItemEvent[]>
  /** A new directive: markdown on what the human is working on. */
  addDirective: (text: string) => Promise<string>
  /** Edits a directive: a new one replaces it, and its age starts again. */
  editDirective: (id: string, text: string) => Promise<string>
  /** Marks an item as a priority, or not. */
  setPriority: (id: string, on: boolean) => Promise<void>
  /** Renames an item. */
  rename: (id: string, title: string) => Promise<void>
  /** Puts an item away until a time (ms), with a snooze event in it. */
  snooze: (id: string, until: number) => Promise<void>
  /** Wakes a snoozed item now: its snoozes are cancelled. */
  unsnooze: (id: string) => Promise<void>
  /** The human did it outside the tool. Recorded, so reviews can find it. */
  handledOutside: (id: string) => Promise<void>
  pollSources: () => Promise<void>
  /** Makes a bot (no id) or changes one: a configure event in it. */
  saveBot: (bot: { id?: string; name: string; instructions: string; provider: string; model: string; debounceMinutes: number; folder?: string }) => Promise<string>
  /** Deletes a bot: what it was assigned goes back to the human. */
  deleteBot: (id: string) => Promise<void>
  /** Assigns an item to the human or a bot (on), or takes the assignment away (off). */
  assign: (itemId: string, to: string, on: boolean) => Promise<void>
  /** Assign events, the newest first: for one assignee (or anyone), in the last `days`. */
  assignments: (assignee: string | null, days: number) => Promise<Assignment[]>
  /** The open items the human owns or had assigned to them, untouched for `hours`, the longest untouched first. */
  stale: (hours: number) => Promise<Item[]>
  /** Wakes a bot now, whatever its debounce says. */
  wakeBot: (id: string) => Promise<void>
  integrations: () => Promise<IntegrationsView>
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
  'overview', 'detail', 'eventDetail', 'catalog', 'next', 'viewed', 'skipped', 'create', 'send', 'approve', 'cancel', 'close', 'attach',
  'summarise', 'customActions', 'saveCustomAction', 'removeCustomAction', 'runCustomAction', 'generateCustomAction', 'directives',
  'addDirective', 'editDirective', 'setPriority', 'rename', 'snooze', 'unsnooze', 'handledOutside', 'pollSources', 'saveBot', 'deleteBot',
  'assign', 'assignments', 'stale', 'wakeBot', 'integrations', 'addSlack', 'removeSlack', 'mcp', 'regenerateMcpToken', 'setMcpPort',
] as const satisfies readonly ApiMethod[]

type ApiMethod = Exclude<keyof WorkflowsAPI, 'onChange' | 'onBack' | 'onForward'>
/** Fails to compile while a call is left out of API_METHODS. */
export const API_METHODS_COMPLETE: Exclude<ApiMethod, (typeof API_METHODS)[number]> extends never ? true : never = true
