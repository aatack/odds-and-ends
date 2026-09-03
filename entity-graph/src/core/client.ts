// The shapes the renderer and the main process agree on: the graph of pensives
// the user has drawn, and Tailscale, which is how a phone reaches any of it.
//
// There are no servers here any more, and no source connections. A pensive is a
// *node* in one graph the app owns — a SQLite file, several of them joined, one
// reached over HTTP — and how it is exposed is another node downstream of it. So
// what used to be "which server, which source, which token" is now an edge.

/** The kinds of node the sources page can draw. */
export type NodeKind = 'sqlite' | 'combined' | 'broadcast' | 'connect' | 'mcp' | 'desktop'

/**
 * What one node holds, over and above its name and where it sits.
 *
 * A node's *inputs* are edges rather than configuration: a combiner does not
 * list its children, it has children drawn into it. What is left is the part
 * that is nobody else's business — a path on disk, a port, a URL.
 */
export type NodeConfig =
  /** A file on this machine. The only node that holds notes itself. */
  | { kind: 'sqlite'; path: string }
  /**
   * Its inputs read as one store. `writeTo` names the input every edit lands
   * in — null until the user says, at which point the node cannot be written to.
   */
  | { kind: 'combined'; writeTo: string | null }
  /** Its input, published over HTTP on `port`, to whoever holds a token. */
  | { kind: 'broadcast'; port: number }
  /** Its input, published over HTTP as an MCP server, for an agent. */
  | { kind: 'mcp'; port: number }
  /** A pensive somewhere else, broadcast by another copy of this app. */
  | { kind: 'connect'; url: string; token: string }
  /** This app's own window. Its input is the pensive the outliner shows. */
  | { kind: 'desktop' }

/** One node of the graph, as it is stored and as the page draws it. */
export interface SourceNode {
  id: string
  label: string
  x: number
  y: number
  /** Switched off: every call through it fails, and it draws grey. */
  paused: boolean
  config: NodeConfig
}

/** A node's output plugged into another node's input. */
export interface SourceEdge {
  id: string
  /** The node being read. */
  from: string
  /** The node reading it. */
  to: string
}

/**
 * A bearer token on a broadcast or MCP node, issued to one person by name.
 *
 * The name is not a label: every write that arrives with the token is recorded
 * as that author, whatever the client asks for. Pausing a token refuses it
 * without forgetting it; revoking takes it off for good.
 */
export interface SourceToken {
  token: string
  nodeId: string
  /** Who writes made with it are attributed to. */
  name: string
  paused: boolean
}

/** What the app can say about a node beyond what the user wrote on it. */
export interface NodeStatus {
  /** Where a broadcast or MCP node answers, once it is listening. */
  url: string | null
  /**
   * The same server on loopback. What a tailnet mount proxies to, and so what
   * the page compares against to decide whether the switch is on.
   */
  localUrl: string | null
  /** Why this node isn't working — an actionable sentence, or null. */
  problem: string | null
}

/** The whole page in one answer: the graph, plus how each node is getting on. */
export interface SourceGraph {
  nodes: SourceNode[]
  edges: SourceEdge[]
  status: Record<string, NodeStatus>
}

/**
 * What each kind of node is, in the one place both ends read it: the page draws
 * its handles from `inputs` and `output`, the main process refuses an edge that
 * disagrees with them, and the "add a node" menu is this list.
 */
export interface NodeKindInfo {
  kind: NodeKind
  /** What it is called, and the name a new one is given. */
  label: string
  /** One line saying what it is for, read while picking one. */
  blurb: string
  /** How many outputs may be plugged into it. */
  inputs: 0 | 1 | 'many'
  /** Whether it can be read from at all. */
  output: boolean
  /** False for the one node that is always there and cannot be deleted. */
  addable: boolean
  /** What a new one holds. A port of 0 means "pick a free one". */
  config: NodeConfig
}

export const NODE_KINDS: NodeKindInfo[] = [
  {
    kind: 'sqlite',
    label: 'SQLite file',
    blurb: 'Notes in a file on this machine.',
    inputs: 0,
    output: true,
    addable: true,
    config: { kind: 'sqlite', path: '' },
  },
  {
    kind: 'combined',
    label: 'Combined',
    blurb: 'Several pensives read as one, written to whichever you choose.',
    inputs: 'many',
    output: true,
    addable: true,
    config: { kind: 'combined', writeTo: null },
  },
  {
    kind: 'connect',
    label: 'Connection',
    blurb: 'A pensive broadcast by another machine.',
    inputs: 0,
    output: true,
    addable: true,
    config: { kind: 'connect', url: '', token: '' },
  },
  {
    kind: 'broadcast',
    label: 'Broadcast',
    blurb: 'Publish one pensive over HTTP, to whoever holds a token.',
    inputs: 1,
    output: false,
    addable: true,
    config: { kind: 'broadcast', port: 0 },
  },
  {
    kind: 'mcp',
    label: 'MCP',
    blurb: 'Publish one pensive to an agent, as an MCP server.',
    inputs: 1,
    output: false,
    addable: true,
    config: { kind: 'mcp', port: 0 },
  },
  {
    kind: 'desktop',
    label: 'This app',
    blurb: 'Whatever is plugged in here is what the outliner shows.',
    inputs: 1,
    output: false,
    addable: false,
    config: { kind: 'desktop' },
  },
]

export const nodeKind = (kind: NodeKind): NodeKindInfo =>
  NODE_KINDS.find((k) => k.kind === kind) ?? NODE_KINDS[0]

/** Whether a node of this kind holds tokens — the two that publish. */
export const publishes = (kind: NodeKind): boolean => kind === 'broadcast' || kind === 'mcp'

/** Fields of a node the user may change. */
export interface NodePatch {
  label?: string
  x?: number
  y?: number
  paused?: boolean
  config?: NodeConfig
}

/** The pensive the outliner is showing: whatever is plugged into `desktop`. */
export interface CurrentPensive {
  id: string
  label: string
}

// ---------------------------------------------------------------------------
// Tailscale — putting the phone app and one broadcast on the tailnet
// ---------------------------------------------------------------------------

/**
 * One thing `tailscale serve` is publishing on this machine's HTTPS name: a
 * directory read off disk (`path`), a reverse proxy (`proxy`), or a literal
 * string (`text`).
 */
export interface TailscaleHandler {
  /** The URL path it answers on, e.g. `/` or `/api/flow`. */
  mount: string
  kind: 'path' | 'proxy' | 'text'
  /** The directory, upstream URL, or literal, depending on `kind`. */
  target: string
}

/** What the app knows about Tailscale on this machine, refreshed on demand. */
export interface TailscaleView {
  /** The `tailscale` command exists and its daemon is up. */
  running: boolean
  /** Why phone access isn't available — an actionable sentence, or null. */
  problem: string | null
  /** The HTTPS name serve answers on, e.g. `laptop.tail1234.ts.net`. */
  domain: string | null
  /** Everything currently served, as tailscale reports it. */
  handlers: TailscaleHandler[]
  /**
   * False when the serve config holds something the app can't put back after
   * the `reset` that removing a mount requires — Funnel, a service, a
   * foreground serve. Adding is still safe; removing is refused.
   */
  editable: boolean
  /** What made it uneditable. */
  locked: string | null
  /** The phone app's build directory, and whether anything has been built into it. */
  app: { path: string; built: boolean }
}

/**
 * Where each thing sits on the tailnet name. These are shared rather than
 * written out at each end because they are the contract between what the main
 * process serves and what the renderer reads back as "on".
 */

/** The phone app itself, at the root of the name. */
export const APP_MOUNT = '/'

/**
 * One broadcast node, mounted under the id of the node it publishes rather than
 * at a plain `/api`: the phone only ever needs the one pensive, and the path is
 * what the app it is handed appends.
 */
export const sourceMount = (nodeId: string): string => `/api/${nodeId}`

/**
 * The base URL the phone app is given. `--set-path` strips its prefix before
 * proxying, so the app can carry an `/api` segment the broadcast never sees, and
 * the app appends the node id itself.
 */
export const phoneBaseUrl = (domain: string): string => `https://${domain}/api`

/** Where the phone app is opened to install it. */
export const phoneAppUrl = (domain: string): string => `https://${domain}/`

// Re-export the tool metadata shape the renderer prompts arguments from.
export type { ToolMeta } from './pensive/types'
