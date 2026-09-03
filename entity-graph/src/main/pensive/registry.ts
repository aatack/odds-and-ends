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
//
// **A loop is a node that is downstream of itself along one path**, which is why
// the check is a path passed down the recursion rather than a set of what is
// being built. A set cannot tell a loop from two callers wanting the same node at
// the same moment — the page reading the graph while the window reads a note —
// and the second of those is not a loop, it is a Tuesday. What that case wants is
// to wait for the build already under way, which is why the cache holds the
// *promise* rather than the pensive.

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
  /**
   * The build in flight or already finished, per node. A promise rather than a
   * pensive so that two callers asking at once get one build between them.
   */
  private cache = new Map<string, Promise<Pensive>>()
  /** Why a node failed to build, from the last time it was asked for. */
  private problems = new Map<string, string>()
  /** What a node that *did* build is having to do without. */
  private warnings = new Map<string, string>()

  constructor(
    private db: GraphDb,
    private opts: RegistryOptions,
  ) {}

  /**
   * The pensive for a node, built if need be. Throws if it cannot be.
   *
   * `upstream` is the path taken to get here, innermost last. It is what makes a
   * loop a loop: a node already on the path is being asked to read itself.
   */
  get(id: string, upstream: readonly string[] = []): Promise<Pensive> {
    const node = this.db.node(id)
    if (!node) return Promise.reject(new NodeNotFoundError(id))
    if (upstream.includes(id)) {
      return Promise.reject(
        new Error(`"${node.label}" is downstream of itself — that would be a loop`),
      )
    }
    // Paused is not cached: it is a property of the node as it stands now, and
    // pressing play must not have to wait for anything to be invalidated.
    if (node.paused) return Promise.resolve(new PausedPensive(node.id, node.label))

    const cached = this.cache.get(id)
    if (cached) return cached

    const building = this.begin(node, upstream)
    this.cache.set(id, building)
    return building
  }

  /** One build, from scratch. Its failure is remembered and then thrown on. */
  private async begin(node: SourceNode, upstream: readonly string[]): Promise<Pensive> {
    try {
      const pensive = await this.build(node, [...upstream, node.id])
      // Whatever a pensive discovers rather than declares — the tools written as
      // notes, a remote registry. A failure here is the node's problem to
      // report, not a reason for it not to exist.
      await pensive.refresh?.().catch(() => undefined)
      // It works now, whatever it did last time it was asked.
      this.problems.delete(node.id)
      return pensive
    } catch (e) {
      this.problems.set(node.id, e instanceof Error ? e.message : String(e))
      // A failed build is not kept: the answer to a path with a typo in it is to
      // fix the typo and be asked again, not to restart the app.
      this.cache.delete(node.id)
      throw e
    }
  }

  /** The one input a single-input node has, or the reason it hasn't got one. */
  private async only(node: SourceNode, upstream: readonly string[]): Promise<Pensive> {
    const [input] = this.db.inputs(node.id)
    if (!input) throw new Error(`Nothing is plugged into "${node.label}"`)
    return this.get(input, upstream)
  }

  private async build(node: SourceNode, upstream: readonly string[]): Promise<Pensive> {
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
        // An input that cannot be built at all — a path with a typo, a loop — is
        // left out rather than taking the others down with it: a combiner is
        // several stores read as one, and one of them being broken is not a
        // reason to lose the rest. What it *is* is worth saying, so the ones
        // skipped become this node's own problem line. (A paused input builds
        // fine; refusing to read is its business, and `CombinedPensive` reads
        // around it.)
        const built = await Promise.all(
          inputs.map(async (i) => ({ id: i, ...(await this.tryGet(i, upstream)) })),
        )
        const children = built.flatMap((b) => ('pensive' in b ? [b.pensive] : []))
        if (!children.length) {
          throw new Error(
            `Nothing plugged into "${node.label}" can be read: ` +
              built.map((b) => ('problem' in b ? b.problem : '')).join('; '),
          )
        }
        const skipped = built.filter((b) => 'problem' in b)
        if (skipped.length) {
          this.warnings.set(
            node.id,
            `Reading without ${skipped.length} of its inputs: ` +
              skipped.map((b) => ('problem' in b ? b.problem : '')).join('; '),
          )
        } else {
          this.warnings.delete(node.id)
        }
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
        return this.only(node, upstream)
    }
  }

  /** The pensive, or the sentence saying why there isn't one. */
  async tryGet(
    id: string,
    upstream: readonly string[] = [],
  ): Promise<{ pensive: Pensive } | { problem: string }> {
    try {
      return { pensive: await this.get(id, upstream) }
    } catch (e) {
      return { problem: e instanceof Error ? e.message : String(e) }
    }
  }

  /** What the outliner shows: whatever is plugged into the desktop node. */
  desktop(): Promise<{ pensive: Pensive } | { problem: string }> {
    return this.tryGet(DESKTOP_NODE_ID)
  }

  /**
   * Why a node isn't working, as of the last attempt to build it — or, for one
   * that built with something missing, what it is doing without.
   */
  problem(id: string): string | null {
    return this.problems.get(id) ?? this.warnings.get(id) ?? null
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
    this.warnings.clear()
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
