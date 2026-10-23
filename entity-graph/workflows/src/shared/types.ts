// The shapes the main process and the renderer both read. docs/handoffs.md is
// the long form of what a handoff is.

export type HandoffKind = 'human' | 'bot' | 'github' | 'slack' | 'items' | 'script'

/** A handoff as it is set: what is to happen next, and so who does it. */
export interface HandoffSpec {
  type: HandoffKind
  params: Record<string, unknown>
}

export type HandoffStatus = 'queued' | 'active' | 'resolved' | 'cancelled'

/**
 * One handoff on one item. An item's handoffs are a queue: the active one is
 * what it waits for, and resolving it starts the next. A resolved handoff keeps
 * its resolution, which is what the next one reads.
 */
export interface Handoff extends HandoffSpec {
  id: string
  itemId: string
  status: HandoffStatus
  /** Who set it: 'human', 'bot', 'system', or a source's id. */
  createdBy: string
  createdAt: number
  startedAt: number | null
  resolvedAt: number | null
  resolution: Record<string, unknown> | null
  resolvedBy: string | null
  /** For the ones the loop checks. */
  nextCheckAt: number | null
  lastError: string | null
  /** When each thing happened to it, from the trace. */
  times: HandoffTimes
}

/** A handoff's timeline: the first (and where it matters, last) time of each thing, and how often. */
export interface HandoffTimes {
  set: number | null
  started: number | null
  firstViewed: number | null
  lastViewed: number | null
  views: number
  skips: number
  firstRunStarted: number | null
  firstRunEnded: number | null
  runs: number
  firstChecked: number | null
  lastChecked: number | null
  checks: number
  resolved: number | null
  cancelled: number | null
}

/** One row of the trace: one thing that happened. */
export interface TraceEntry {
  id: number
  at: number
  itemId: string
  handoffId: string | null
  runId: string | null
  actor: string
  kind: string
  data: Record<string, unknown> | null
}

export interface Item {
  id: string
  title: string
  source: string
  url: string | null
  closed: boolean
  /** The active handoff; null once closed. */
  current: Handoff | null
  /** How many handoffs wait behind the current one. */
  queued: number
  /** The worker running the item, while one is. */
  worker: number | null
  createdAt: number
  updatedAt: number
}

/** Something that happened between handoffs: an action run, a check that failed, an item created. */
export interface ItemEvent {
  id: number
  itemId: string
  at: number
  actor: string
  kind: string
  text: string
  data: Record<string, unknown> | null
}

export interface BotRun {
  id: string
  itemId: string
  handoffId: string
  worker: number
  startedAt: number
  endedAt: number | null
  costUsd: number | null
  turns: number | null
  result: string | null
}

export interface ItemDetail {
  item: Item
  /** Every handoff, in order: resolved, active, queued. */
  handoffs: Handoff[]
  events: ItemEvent[]
  runs: BotRun[]
  /** Everything that happened to the item, in order. */
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
  /** Seen by other people: a bot, or a bot's script, must hand off for approval instead. */
  unsafe: boolean
}

export interface HandoffTypeInfo {
  type: HandoffKind
  description: string
  params: ObjectSchema
  resolution: string
}

export interface Catalog {
  actions: ActionInfo[]
  handoffs: HandoffTypeInfo[]
  /** A list of handoffs of any type, as a JSON Schema: what the forms edit a queue with. */
  handoffList: Record<string, unknown>
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
  /** The human has the item on screen: recorded against its current handoff. */
  viewed: (id: string) => Promise<void>
  /** The human passed over the item for now. */
  skipped: (id: string) => Promise<void>
  /** The item whose human handoff has waited longest, or null. */
  next: (skip: string[]) => Promise<string | null>
  create: (title: string, next: HandoffSpec[]) => Promise<string>
  /** Resolves the current handoff and puts `next` at the front of the queue. */
  handOff: (id: string, next: HandoffSpec[]) => Promise<void>
  /** Runs the action the current human handoff asks approval for, with the human's inputs. */
  approve: (id: string, inputs: Record<string, unknown>) => Promise<void>
  close: (id: string) => Promise<void>
  /** The human did it outside the tool. Recorded, so reviews can find it. */
  handledOutside: (id: string) => Promise<void>
  takeOver: (id: string) => Promise<void>
  pollSources: () => Promise<void>
  integrations: () => Promise<IntegrationsView>
  addSlack: (name: string, token: string) => Promise<void>
  removeSlack: (id: string) => Promise<void>
  mcp: () => Promise<McpView>
  regenerateMcpToken: () => Promise<void>
  setMcpPort: (port: number) => Promise<void>
  onChange: (cb: () => void) => () => void
}
