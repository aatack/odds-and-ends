/**
 * The wire format. The backend owns these shapes; the frontend only ever reads
 * them. Nothing in here imports React, so the backend can `import type` from it
 * without ever loading the module at runtime.
 */

export type TopicId = string

/** The central unit of the app. Everything is one of these, including events. */
export interface Topic {
  id: TopicId
  type: string
  text: string
  metadata: Record<string, unknown>
  createdAt: string
  updatedAt: string
  open: boolean
  resolved: boolean
}

/**
 * `watch` means activity in the child is activity in the parent: it walks up and
 * wakes the ancestors. `reference` is inert, and only exists to group topics into
 * meta-topics.
 */
export type LinkType = 'watch' | 'reference'

export interface Link {
  parentId: TopicId
  parentType: string
  childId: TopicId
  childType: string
  type: LinkType
  createdAt: string
}

/** A reason a topic is closed, and the condition that reopens it. */
export interface Blocker {
  id: string
  topicId: TopicId
  type: string
  label: string
  config: Record<string, unknown>
  createdAt: string
  dueAt: string | null
  satisfiedAt: string | null
  cancelledAt: string | null
  lastError: string | null
}

/** A blocker the backend thinks makes sense for this topic, most useful first. */
export interface BlockerSuggestion {
  type: string
  label: string
  config: Record<string, unknown>
  /** Set when the suggestion needs a value from the user before it can be added. */
  prompt?: { field: string; label: string; placeholder?: string }
}

/** One entry in a topic's feed. Children are only sent for expanded nodes. */
export interface TopicNode {
  topic: Topic
  link: LinkType
  childCount: number
  children: TopicNode[] | null
}

export interface TopicDetail {
  topic: Topic
  parents: Topic[]
  children: TopicNode[]
  blockers: Blocker[]
  suggestions: BlockerSuggestion[]
  blocked: boolean
}

export interface QueueEntry {
  topic: Topic
  childCount: number
  /** Why it is here: the blocker that fired, or the activity that woke it. */
  reason: string
}

export interface WaitingEntry {
  topic: Topic
  blockers: Blocker[]
}

export interface QueueView {
  open: QueueEntry[]
  waiting: WaitingEntry[]
}

export interface RunSummary {
  id: string
  topicId: TopicId
  kind: string
  status: 'running' | 'succeeded' | 'failed'
  startedAt: string
  finishedAt: string | null
  error: string | null
}

export interface IntegrationStatus {
  name: string
  enabled: boolean
  state: 'idle' | 'polling' | 'error' | 'disabled'
  detail: string
  lastRunAt: string | null
}

export interface ServerStatus {
  revision: number
  integrations: IntegrationStatus[]
  runs: RunSummary[]
}

/** Pushed down the event stream whenever anything changes. */
export interface ChangeEvent {
  revision: number
  topics: TopicId[]
  queue: boolean
}

export interface ActionRequest {
  /** Client-generated, so a retry of the same mutation is not applied twice. */
  id: string
  name: string
  args: Record<string, unknown>
}

export interface ActionResult {
  ok: boolean
  /** Topics the action created or moved focus to. */
  focus?: TopicId
  error?: string
}
