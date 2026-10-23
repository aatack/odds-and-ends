// The shapes the main process and the renderer both read. docs/events.md is the
// long form of what an event is.

/**
 * An item's history is its events. Some resolve as they are added (a message, an
 * action run); the rest are **blockers**, which stay unresolved until something
 * happens (a bot replies, CI finishes, an approval is given).
 */
export type EventKind = 'message' | 'action' | 'closed' | 'approval' | 'bot' | 'github' | 'slack' | 'items' | 'after'

/** An event as it is added. `ref` names it, so an `after` added alongside can point at it. */
export interface EventSpec {
  type: EventKind
  params: Record<string, unknown>
  ref?: string
}

export interface ItemEvent {
  id: string
  itemId: string
  type: EventKind
  params: Record<string, unknown>
  /** Who added it: 'human', 'bot', 'system', 'script', 'mcp', or a source's id. */
  createdBy: string
  createdAt: number
  resolved: boolean
  resolvedAt: number | null
  resolvedBy: string | null
  cancelled: boolean
  /** What it resolved with. */
  data: Record<string, unknown> | null
  /** For the blockers the loop checks. */
  nextCheckAt: number | null
  lastError: string | null
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

export interface Item {
  id: string
  title: string
  source: string
  url: string | null
  closed: boolean
  /** Its unresolved events, oldest first. */
  blockers: ItemEvent[]
  /** Open, and blocked by nothing but the human's own approvals: it is the human's turn. */
  withHuman: boolean
  /** The worker running a bot on it, while one is. */
  worker: number | null
  /** Since when it has been as it is: the last event added or resolved. */
  since: number
  createdAt: number
  updatedAt: number
}

/** One row of the trace: one thing that happened. */
export interface TraceEntry {
  id: number
  at: number
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
  /** Every event, oldest first. */
  events: ItemEvent[]
  runs: BotRun[]
  trace: TraceEntry[]
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
  catalog: () => Promise<Catalog>
  /** The item that has waited longest for the human, or null. */
  next: (skip: string[]) => Promise<string | null>
  /** The human has the item on screen. */
  viewed: (id: string) => Promise<void>
  /** The human passed over the item for now. */
  skipped: (id: string) => Promise<void>
  create: (title: string) => Promise<string>
  /** Sends from the chat input: `text` through a preset. */
  send: (id: string, preset: string, text: string) => Promise<void>
  /** Runs an approval blocker's action with the human's inputs. */
  approve: (eventId: string, inputs: Record<string, unknown>) => Promise<void>
  /** Ends a blocker without it happening. */
  cancel: (eventId: string) => Promise<void>
  close: (id: string) => Promise<void>
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
}
