// The shapes the main process and the renderer both read. docs/events.md is the
// long form of what an event is.

/**
 * Everything is an event, items included. Every event but an item sits in an
 * item (its parent), and an item may sit in another. Some events resolve as
 * they are added (a message, an action run); the rest are **blockers**, which
 * stay open until something happens (a bot replies, CI finishes, an approval is
 * given, a child item closes).
 */
export type EventKind = 'item' | 'message' | 'attachment' | 'priority' | 'summary' | 'action' | 'approval' | 'bot' | 'github' | 'slack' | 'after' | 'script' | 'directive' | 'snooze'

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
  /** Open, and blocked by nothing but the human's own approvals: it is the human's turn. */
  withHuman: boolean
  /** Marked as a priority (by its latest priority event): Next takes it before the rest. */
  priority: boolean
  /** When it wakes, while a snooze blocks it (the latest, if several). */
  snoozedUntil: number | null
  /** The worker running a bot on it, while one is. Held in memory only. */
  worker: number | null
  /** Its Claude session, once it has one. */
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
  itemId: string
  eventIds: string[]
  worker: number
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

export interface Overview {
  items: Item[]
  workers: { id: number; itemId: string | null }[]
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
  claude: Check & { workers: number }
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
  /** Marks an item as a priority, or not. */
  setPriority: (id: string, on: boolean) => Promise<void>
  /** Puts an item away until a time (ms), with a snooze event in it. */
  snooze: (id: string, until: number) => Promise<void>
  /** Wakes a snoozed item now: its snoozes are cancelled. */
  unsnooze: (id: string) => Promise<void>
  /** The human did it outside the tool. Recorded, so reviews can find it. */
  handledOutside: (id: string) => Promise<void>
  pollSources: () => Promise<void>
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
