import { isAbsolute, join } from 'path'
import { homedir } from 'os'
import {
  CombinedPensive,
  ConnectPensive,
  PausedPensive,
  SqlitePensive,
  type Pensive,
} from '../../core/pensive/index'
import type { SourceNode } from '../../core/client'
import { DESKTOP_NODE_ID, type GraphDb } from './graph'

// Turning the drawing into the thing. One node becomes one pensive, its inputs
// resolved by following the edges backwards, and the answer is kept until the
// graph changes.
//
// Two rules are enforced here rather than trusted to the page: a loop is refused
// while it is being built, and a paused node yields a pensive that refuses
// everything. Both have to hold on this side, because the page is not the only
// caller — a broadcast answers requests from other machines.

/** A path as it is written down: `~` for home, relative to the app's own folder. */
export function resolveStorePath(path: string, base: string): string {
  const trimmed = path.trim()
  if (trimmed === '~' || trimmed.startsWith('~/')) return join(homedir(), trimmed.slice(1))
  return isAbsolute(trimmed) ? trimmed : join(base, trimmed)
}

export class NodeNotFoundError extends Error {
  constructor(public id: string) {
    super(`No node "${id}"`)
    this.name = 'NodeNotFoundError'
  }
}

export interface RegistryOptions {
  /** Where a relative sqlite path is resolved against. */
  storeRoot: string
  /** The author a write that names none is recorded as. */
  author: () => string
}

export class PensiveRegistry {
  private cache = new Map<string, Pensive>()
  private building = new Set<string>()
  /** Why a node failed to build, from the last time it was asked for. */
  private problems = new Map<string, string>()

  constructor(
    private db: GraphDb,
    private opts: RegistryOptions,
  ) {}

  /** The pensive for a node, built if need be. Throws if it cannot be. */
  async get(id: string): Promise<Pensive> {
    const cached = this.cache.get(id)
    if (cached) return cached

    const node = this.db.node(id)
    if (!node) throw new NodeNotFoundError(id)
    if (this.building.has(id)) {
      throw new Error(`"${node.label}" is downstream of itself — that would be a loop`)
    }
    if (node.paused) return new PausedPensive(node.id, node.label)

    this.building.add(id)
    try {
      const pensive = await this.build(node)
      // Whatever a pensive discovers rather than declares — the tools written as
      // notes, a remote registry. A failure here is the node's problem to
      // report, not a reason for it not to exist.
      await pensive.refresh?.().catch(() => undefined)
      this.cache.set(id, pensive)
      this.problems.delete(id)
      return pensive
    } catch (e) {
      this.problems.set(id, e instanceof Error ? e.message : String(e))
      throw e
    } finally {
      this.building.delete(id)
    }
  }

  /** The one input a single-input node has, or the reason it hasn't got one. */
  private async only(node: SourceNode): Promise<Pensive> {
    const [input] = this.db.inputs(node.id)
    if (!input) throw new Error(`Nothing is plugged into "${node.label}"`)
    return this.get(input)
  }

  private async build(node: SourceNode): Promise<Pensive> {
    const config = node.config
    switch (config.kind) {
      case 'sqlite':
        if (!config.path.trim()) throw new Error(`"${node.label}" has no file path`)
        return new SqlitePensive(
          node.id,
          node.label,
          resolveStorePath(config.path, this.opts.storeRoot),
          this.opts.author(),
        )
      case 'combined': {
        const inputs = this.db.inputs(node.id)
        if (inputs.length === 0) throw new Error(`Nothing is plugged into "${node.label}"`)
        const children = await Promise.all(inputs.map((i) => this.get(i)))
        const writeTo = config.writeTo
          ? (children.find((c) => c.id === config.writeTo) ?? null)
          : null
        return new CombinedPensive(node.id, node.label, children, writeTo, this.opts.author())
      }
      case 'connect':
        if (!config.url.trim()) throw new Error(`"${node.label}" has no URL`)
        return new ConnectPensive(node.id, node.label, config.url, config.token)
      // The three that only pass a pensive on: what they *are* is the pensive
      // plugged into them, and what makes them different is what the app does
      // with them — listens on a port, hands it to the window.
      case 'broadcast':
      case 'mcp':
      case 'desktop':
        return this.only(node)
    }
  }

  /** The pensive, or the sentence saying why there isn't one. */
  async tryGet(id: string): Promise<{ pensive: Pensive } | { problem: string }> {
    try {
      return { pensive: await this.get(id) }
    } catch (e) {
      return { problem: e instanceof Error ? e.message : String(e) }
    }
  }

  /** What the outliner shows: whatever is plugged into the desktop node. */
  desktop(): Promise<{ pensive: Pensive } | { problem: string }> {
    return this.tryGet(DESKTOP_NODE_ID)
  }

  /** Why a node isn't working, as of the last attempt to build it. */
  problem(id: string): string | null {
    return this.problems.get(id) ?? null
  }

  /**
   * Drop every built pensive so the next call rebuilds from the graph. Closing
   * the SQLite handles is the point as much as the invalidation: two handles on
   * one file, one of them stale, is how a write ends up somewhere nobody is
   * reading.
   */
  invalidate(): void {
    for (const pensive of this.cache.values()) {
      const closable = pensive as { close?: () => void }
      if (typeof closable.close === 'function') closable.close()
    }
    this.cache.clear()
    this.problems.clear()
  }
}

/**
 * Whether connecting `from` to `to` would put a node downstream of itself.
 * Checked before an edge is written as well as while one is built, so the page
 * can refuse the gesture rather than draw something that then fails.
 */
export function wouldCycle(edges: { from: string; to: string }[], from: string, to: string): boolean {
  if (from === to) return true
  // Walk downstream of `to`: if `from` is down there, the new edge closes a ring.
  const seen = new Set<string>([to])
  const queue = [to]
  while (queue.length) {
    const at = queue.shift()!
    for (const edge of edges.filter((e) => e.from === at)) {
      if (edge.to === from) return true
      if (!seen.has(edge.to)) {
        seen.add(edge.to)
        queue.push(edge.to)
      }
    }
  }
  return false
}
