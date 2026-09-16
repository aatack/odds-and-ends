import type { NodeConfig, SourceNode } from '../../core/client'
import { watches } from '../../core/client'
import type { GraphDb } from '../pensive/graph'
import type { PensiveRegistry } from '../pensive/registry'
import type { FeedOptions, RunningFeed } from './feed'
import { GithubFeed, type GithubFeedConfig } from './github'
import { SlackFeed, type SlackFeedConfig } from './slack'

// The feeds, kept in step with the drawing — the same job `PensiveServers` does
// for the listeners, and for the same reason: a node exists ⇔ the thing it
// stands for is running, so adding one starts it and deleting one stops it.
//
// Pausing is where the two differ. A paused broadcast keeps its server and
// answers 403, because "somebody switched this off" is worth being able to tell
// from "there is nothing here". A paused feed is *stopped*: it has nobody to
// tell, the point of it is that it is doing something, and the socket it holds
// open is exactly what should not survive being switched off. Pressing play
// starts it again, and it begins with the catch-up, so nothing said while it was
// off is lost.

/**
 * What about a node means "restart the feed". Everything except the two values
 * the feed writes itself — a cursor that moved is the feed working, not a reason
 * to throw away the connection it moved with.
 */
export function feedSignature(node: SourceNode): string {
  const config = node.config as Record<string, unknown>
  const { cursor: _cursor, lastModified: _lastModified, ...rest } = config
  return JSON.stringify([node.paused, node.label, rest])
}

export class EventFeeds {
  private feeds = new Map<string, { feed: RunningFeed; signature: string }>()

  constructor(
    private db: GraphDb,
    private registry: PensiveRegistry,
    /** Something the page would want to redraw for: a feed said a new thing. */
    private changed: () => void,
  ) {}

  /** Start, stop and restart feeds until they match the graph. */
  async sync(): Promise<void> {
    const wanted = new Map(
      this.db
        .nodes()
        .filter((n) => watches(n.config.kind) && !n.paused)
        .map((n) => [n.id, n]),
    )

    for (const [id, running] of [...this.feeds]) {
      const node = wanted.get(id)
      if (node && feedSignature(node) === running.signature) continue
      this.feeds.delete(id)
      await running.feed.stop()
    }

    for (const [id, node] of wanted) {
      if (this.feeds.has(id)) continue
      const feed = this.build(node)
      if (!feed) continue
      this.feeds.set(id, { feed, signature: feedSignature(node) })
      // Not awaited: a feed's first act is to read everything it missed, which
      // for a cursor a week old is minutes of work, and the app must not wait
      // for it to finish before its window will draw.
      void feed.start()
    }
  }

  private build(node: SourceNode): RunningFeed | null {
    // Typed per kind rather than once over a union: a feed's config is the shape
    // of its own node, and the node's kind is what has just been checked.
    const options = <C>(): FeedOptions<C> => ({
      nodeId: node.id,
      label: node.label,
      // Read per call rather than captured, so editing a token takes effect on
      // the feed that is already running rather than only on the next one.
      config: () => (this.db.node(node.id)?.config ?? node.config) as C,
      advance: (patch: Partial<C>) => this.write(node.id, patch as Partial<NodeConfig>),
      pensive: () => this.registry.tryGet(node.id),
      changed: this.changed,
    })
    if (node.config.kind === 'slackEvents') return new SlackFeed(options<SlackFeedConfig>())
    if (node.config.kind === 'githubEvents') return new GithubFeed(options<GithubFeedConfig>())
    return null
  }

  /**
   * A cursor, written back. Straight onto the node rather than through the IPC
   * handler that ordinarily edits one, because that handler rebuilds every
   * pensive downstream and re-syncs the servers — which for a value that moves
   * once a minute would be the most expensive thing the app does, and would
   * restart the very feed that wrote it.
   */
  private write(nodeId: string, patch: Partial<NodeConfig>): void {
    const node = this.db.node(nodeId)
    if (!node) return
    this.db.updateNode(nodeId, { config: { ...node.config, ...patch } as NodeConfig })
  }

  /** What a feed is doing and what is wrong with it, for the node to draw. */
  status(nodeId: string): { problem: string | null; activity: string | null } {
    return this.feeds.get(nodeId)?.feed.status() ?? { problem: null, activity: null }
  }

  async stopAll(): Promise<void> {
    const running = [...this.feeds.values()]
    this.feeds.clear()
    await Promise.all(running.map((r) => r.feed.stop()))
  }
}

export type { SlackFeedConfig, GithubFeedConfig }
