// The wire shapes of a source: what `query` returns, what `writeEvents` takes.
//
// Copied from the desktop app's `src/core` rather than imported from it. The
// duplication is deliberate and small (these are the only shapes that cross the
// wire): this app is meant to build, install and deploy with nothing of the
// Electron project in its graph, and a type-only import would still tie the two
// tsconfigs together. If the server's contract changes, both clients change —
// which is the honest situation whether or not they share a file.

/** 0=add, 1=remove, 2=move toward index 0, 3=move toward the end. */
export type LinkAction = 0 | 1 | 2 | 3

export interface ValueEvent {
  type: 'value'
  timestamp: number
  author: string
  entityId: string
  key: string
  value: unknown
}

export interface LinkEvent {
  type: 'link'
  timestamp: number
  author: string
  sourceId: string
  destinationId: string
  action: LinkAction
}

export type AppEvent = ValueEvent | LinkEvent

export interface Entity {
  id: string
  createdAt: number
  editedAt: number
  createdBy: string
  editedBy: string
  values: Record<string, unknown>
  inboundLinks: string[]
  /** Ordered — this is the child order an outline reads top to bottom. */
  outboundLinks: string[]
}

export interface QueryResult {
  entity: Entity
  depth: number
  parentId: string | null
}

export interface StackFrame {
  id: string
  depth: number
  parentId: string | null
  path: string[]
}

export interface QueryPage {
  results: QueryResult[]
  /** Non-null when the limit was hit; pass back as `continuationStack` to resume. */
  continuationStack: StackFrame[] | null
}

/**
 * Which way a traversal follows links. `out` is the ordinary tree of children;
 * `in` walks inbound links, so the same query answers "what links to this?".
 */
export type LinkDirection = 'out' | 'in'

/** Bytes stored under an entity id, as `readResource` returns them. */
export interface ResourceRecord {
  id: string
  mimeType: string
  /** Base64. */
  data: string
  name: string | null
  author: string
  timestamp: number
}

/** One entry of the source's tool list (`GET /:sourceId/tools`). */
export interface ToolMeta {
  id: string
  name: string
  description: string
  safety: string
  args: Record<string, unknown>
}
