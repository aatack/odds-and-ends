import { randomBytes } from 'crypto'
import { mkdirSync } from 'fs'
import { dirname, resolve } from 'path'
import Database from 'better-sqlite3'
import { v4 as uuidv4 } from 'uuid'
import type {
  NodeConfig,
  NodePatch,
  SourceEdge,
  SourceNode,
  SourceToken,
} from '../../core/client'

// Where the graph of pensives is kept: a SQLite file of the app's own, beside
// its other local state. It holds *what the user drew* and nothing else — no
// notes, no events. The stores it points at are the sqlite nodes.

interface RawNode {
  id: string
  label: string
  x: number
  y: number
  paused: number
  config_json: string
}

interface RawEdge {
  id: string
  from_id: string
  to_id: string
}

interface RawToken {
  token: string
  node_id: string
  name: string
  paused: number
}

/** The node standing for the app's own window. Fixed, and there is one. */
export const DESKTOP_NODE_ID = 'desktop'

export class GraphDb {
  private db: Database.Database

  constructor(path: string) {
    const dir = dirname(resolve(path))
    if (dir !== dirname(dir)) mkdirSync(dir, { recursive: true })
    this.db = new Database(path)
    this.db.pragma('journal_mode = WAL')
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS nodes (
        id          TEXT PRIMARY KEY,
        label       TEXT NOT NULL,
        x           REAL NOT NULL DEFAULT 0,
        y           REAL NOT NULL DEFAULT 0,
        paused      INTEGER NOT NULL DEFAULT 0,
        config_json TEXT NOT NULL
      );
      CREATE TABLE IF NOT EXISTS edges (
        id      TEXT PRIMARY KEY,
        from_id TEXT NOT NULL,
        to_id   TEXT NOT NULL
      );
      CREATE TABLE IF NOT EXISTS tokens (
        token   TEXT PRIMARY KEY,
        node_id TEXT NOT NULL,
        name    TEXT NOT NULL DEFAULT '',
        paused  INTEGER NOT NULL DEFAULT 0
      );
      CREATE INDEX IF NOT EXISTS idx_edge_to ON edges(to_id);
      CREATE INDEX IF NOT EXISTS idx_token_node ON tokens(node_id);
    `)
    this.ensureDesktop()
  }

  /**
   * The desktop node exists before anybody asks for it, and cannot be added or
   * removed: it is where the app plugs itself in, so a graph with none would
   * have no way of ever showing anything.
   */
  private ensureDesktop(): void {
    if (this.node(DESKTOP_NODE_ID)) return
    this.db
      .prepare('INSERT INTO nodes (id, label, x, y, paused, config_json) VALUES (?, ?, 0, 0, 0, ?)')
      .run(DESKTOP_NODE_ID, 'This app', JSON.stringify({ kind: 'desktop' }))
  }

  private toNode(r: RawNode): SourceNode {
    return {
      id: r.id,
      label: r.label,
      x: r.x,
      y: r.y,
      paused: !!r.paused,
      config: JSON.parse(r.config_json) as NodeConfig,
    }
  }

  nodes(): SourceNode[] {
    return this.db
      .prepare<[], RawNode>('SELECT * FROM nodes')
      .all()
      .map((r) => this.toNode(r))
  }

  node(id: string): SourceNode | undefined {
    const r = this.db.prepare<[string], RawNode>('SELECT * FROM nodes WHERE id = ?').get(id)
    return r ? this.toNode(r) : undefined
  }

  edges(): SourceEdge[] {
    return this.db
      .prepare<[], RawEdge>('SELECT * FROM edges')
      .all()
      .map((r) => ({ id: r.id, from: r.from_id, to: r.to_id }))
  }

  /** The nodes feeding one node's input, in the order they were connected. */
  inputs(id: string): string[] {
    return this.db
      .prepare<[string], { from_id: string }>('SELECT from_id FROM edges WHERE to_id = ? ORDER BY rowid')
      .all(id)
      .map((r) => r.from_id)
  }

  addNode(node: { label: string; x: number; y: number; config: NodeConfig }): SourceNode {
    const id = uuidv4()
    this.db
      .prepare('INSERT INTO nodes (id, label, x, y, paused, config_json) VALUES (?, ?, ?, ?, 0, ?)')
      .run(id, node.label, node.x, node.y, JSON.stringify(node.config))
    return { id, ...node, paused: false }
  }

  updateNode(id: string, patch: NodePatch): SourceNode {
    const existing = this.node(id)
    if (!existing) throw new Error(`no node "${id}"`)
    const next: SourceNode = {
      ...existing,
      ...(patch.label !== undefined ? { label: patch.label } : {}),
      ...(patch.x !== undefined ? { x: patch.x } : {}),
      ...(patch.y !== undefined ? { y: patch.y } : {}),
      ...(patch.paused !== undefined ? { paused: patch.paused } : {}),
      // A node cannot change kind: the shape of what it holds, and what may be
      // drawn into it, are the same decision. Delete it and add the other one.
      ...(patch.config !== undefined && patch.config.kind === existing.config.kind
        ? { config: patch.config }
        : {}),
    }
    this.db
      .prepare('UPDATE nodes SET label = ?, x = ?, y = ?, paused = ?, config_json = ? WHERE id = ?')
      .run(next.label, next.x, next.y, next.paused ? 1 : 0, JSON.stringify(next.config), id)
    return next
  }

  /** Forget a node, the edges at either end of it, and its tokens. */
  removeNode(id: string): void {
    if (id === DESKTOP_NODE_ID) throw new Error('the desktop node cannot be removed')
    this.db.prepare('DELETE FROM nodes WHERE id = ?').run(id)
    this.db.prepare('DELETE FROM edges WHERE from_id = ? OR to_id = ?').run(id, id)
    this.db.prepare('DELETE FROM tokens WHERE node_id = ?').run(id)
    // A combiner that wrote to it no longer has a write source. Left naming a
    // node that is gone, it would refuse every write with an id nobody can see.
    for (const node of this.nodes()) {
      if (node.config.kind === 'combined' && node.config.writeTo === id) {
        this.updateNode(node.id, { config: { kind: 'combined', writeTo: null } })
      }
    }
  }

  addEdge(from: string, to: string): SourceEdge {
    const id = uuidv4()
    this.db.prepare('INSERT INTO edges (id, from_id, to_id) VALUES (?, ?, ?)').run(id, from, to)
    return { id, from, to }
  }

  removeEdge(id: string): void {
    const edge = this.edges().find((e) => e.id === id)
    this.db.prepare('DELETE FROM edges WHERE id = ?').run(id)
    if (!edge) return
    const target = this.node(edge.to)
    if (target?.config.kind === 'combined' && target.config.writeTo === edge.from) {
      this.updateNode(target.id, { config: { kind: 'combined', writeTo: null } })
    }
  }

  /** Every edge into `to`, dropped. How a one-input node's plug is replaced. */
  clearInputs(to: string): void {
    for (const edge of this.edges().filter((e) => e.to === to)) this.removeEdge(edge.id)
  }

  // --- Tokens --------------------------------------------------------------

  tokens(nodeId: string): SourceToken[] {
    return this.db
      .prepare<[string], RawToken>('SELECT * FROM tokens WHERE node_id = ? ORDER BY rowid')
      .all(nodeId)
      .map((r) => ({ token: r.token, nodeId: r.node_id, name: r.name, paused: !!r.paused }))
  }

  /** The live token issued to `name` on this node, if there is one. */
  tokenFor(nodeId: string, name: string): SourceToken | undefined {
    return this.tokens(nodeId).find((t) => t.name === name && !t.paused)
  }

  issueToken(nodeId: string, name: string): SourceToken {
    const token = randomBytes(24).toString('hex')
    this.db
      .prepare('INSERT INTO tokens (token, node_id, name, paused) VALUES (?, ?, ?, 0)')
      .run(token, nodeId, name)
    return { token, nodeId, name, paused: false }
  }

  pauseToken(token: string, paused: boolean): void {
    this.db.prepare('UPDATE tokens SET paused = ? WHERE token = ?').run(paused ? 1 : 0, token)
  }

  /** For good: a revoked token is gone rather than remembered as refused. */
  revokeToken(token: string): void {
    this.db.prepare('DELETE FROM tokens WHERE token = ?').run(token)
  }

  /** Who a token says a write is by, or null when it is not one of ours. */
  authorOf(nodeId: string, token: string): string | null {
    const row = this.db
      .prepare<[string, string], RawToken>('SELECT * FROM tokens WHERE token = ? AND node_id = ?')
      .get(token, nodeId)
    if (!row || row.paused) return null
    return row.name || 'anonymous'
  }

  close(): void {
    this.db.close()
  }
}
