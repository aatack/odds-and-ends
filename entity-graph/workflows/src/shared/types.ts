// The shapes the main process and the renderer both read.

/** What an item waits for. Every open item has exactly one blocker. */
export type Blocker = 'operator' | 'agent' | 'external' | 'items' | 'closed'

/** One input of an approval form. */
export interface Field {
  name: string
  label: string
  kind: 'text' | 'multiline' | 'select'
  options?: string[]
}

/** What an agent asks of the operator when it blocks on them. */
export interface Request {
  question: string
  /** Set when the agent wants an action run: the form, filled in as far as it could. */
  approval?: { action: string; label: string; fields: Field[]; inputs: Record<string, string> }
}

export interface Wait {
  kind: string
  description: string
  params: Record<string, unknown>
  step: number
  nextCheckAt: number
  deadline: number | null
  lastError: string | null
}

export interface Item {
  id: string
  title: string
  source: string
  url: string | null
  /** The current state of the item, in a few lines. Always kept up to date. */
  state: string
  blocker: Blocker
  request: Request | null
  /** The operator's latest instruction to the agents. */
  instruction: string | null
  /** The worker running the item, while one is. */
  worker: number | null
  createdAt: number
  updatedAt: number
  /** When the item took its current blocker. */
  blockedAt: number
}

export interface ItemEvent {
  id: number
  itemId: string
  at: number
  actor: string
  kind: string
  text: string
  data: Record<string, unknown> | null
}

export interface AgentRun {
  id: string
  itemId: string
  worker: number
  startedAt: number
  endedAt: number | null
  outcome: string | null
  costUsd: number | null
  turns: number | null
  result: string | null
}

export interface ItemDetail {
  item: Item
  wait: Wait | null
  /** The items this one waits for, while it does. */
  dependsOn: { id: string; title: string; blocker: Blocker }[]
  events: ItemEvent[]
  runs: AgentRun[]
}

export interface Overview {
  items: Item[]
  workers: { id: number; itemId: string | null }[]
}

/** One MCP tool, as a client lists it. */
export interface ToolInfo {
  name: string
  description: string
  inputSchema: { properties?: Record<string, { type?: string; description?: string; enum?: string[] }>; required?: string[] }
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
  /** What an agent running an item sees. */
  runTools: ToolInfo[]
  /** What anything connected at `url` sees. */
  operatorTools: ToolInfo[]
}

/** The renderer's whole view of the main process. */
export interface WorkflowsAPI {
  overview: () => Promise<Overview>
  detail: (id: string) => Promise<ItemDetail | null>
  /** The operator-blocked item that has waited longest, or null. */
  next: (skip: string[]) => Promise<string | null>
  create: (title: string, instruction: string) => Promise<string>
  dispatch: (id: string, instruction: string) => Promise<void>
  execute: (id: string, inputs: Record<string, string>) => Promise<void>
  close: (id: string, note: string) => Promise<void>
  /** The operator did it outside the tool. Recorded, so reviews can find it. */
  handledOutside: (id: string, note: string) => Promise<void>
  setState: (id: string, state: string) => Promise<void>
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
