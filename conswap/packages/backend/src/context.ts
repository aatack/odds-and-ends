import type { ChangeEvent, IntegrationStatus, TopicId } from '@conswap/common/types'
import type { Config } from './config.js'
import type { Db } from './database.js'

export interface Integration {
  name: string
  status(): IntegrationStatus
  start(): void
  stop(): void
}

type Listener = (event: ChangeEvent) => void

/**
 * Everything the rest of the backend is allowed to reach for. Passing it around
 * explicitly keeps the logic testable against an in-memory database.
 */
export class Context {
  readonly config: Config
  readonly db: Db
  readonly integrations = new Map<string, Integration>()

  private listeners = new Set<Listener>()
  private pendingTopics = new Set<TopicId>()
  private pendingQueue = false
  private flushHandle: NodeJS.Timeout | null = null

  revision = 0

  constructor(config: Config, db: Db) {
    this.config = config
    this.db = db
  }

  log(scope: string, message: string, detail?: unknown): void {
    const suffix = detail === undefined ? '' : ` ${typeof detail === 'string' ? detail : JSON.stringify(detail)}`
    process.stdout.write(`[${new Date().toISOString()}] ${scope}: ${message}${suffix}\n`)
  }

  /**
   * Announces a change. Calls are coalesced over a tick so that a poll writing a
   * hundred messages produces one push rather than a hundred.
   */
  changed(topics: TopicId[] = [], queue = true): void {
    for (const id of topics) this.pendingTopics.add(id)
    this.pendingQueue = this.pendingQueue || queue
    if (this.flushHandle) return
    this.flushHandle = setTimeout(() => {
      this.flushHandle = null
      this.revision += 1
      const event: ChangeEvent = {
        revision: this.revision,
        topics: [...this.pendingTopics],
        queue: this.pendingQueue,
      }
      this.pendingTopics.clear()
      this.pendingQueue = false
      for (const listener of this.listeners) listener(event)
    }, 30)
  }

  subscribe(listener: Listener): () => void {
    this.listeners.add(listener)
    return () => this.listeners.delete(listener)
  }

  register(integration: Integration): void {
    this.integrations.set(integration.name, integration)
  }
}
