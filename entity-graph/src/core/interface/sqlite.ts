import { mkdirSync } from 'fs'
import { resolve, dirname } from 'path'
import Database from 'better-sqlite3'
import type { AppEvent, ValueEvent, LinkEvent } from '../events'
import { POP_AGE_LIMIT_MS, POP_GROUP_MS, type ResourceRecord } from '../pensive/types'
import type { DumpableInterface } from './index'

interface ValueRow {
  timestamp: number
  author: string
  entity_id: string
  key: string
  value: string
}

interface LinkRow {
  timestamp: number
  author: string
  source_id: string
  destination_id: string
  action: number
}

interface ResourceRow {
  id: string
  timestamp: number
  author: string
  mime_type: string
  name: string | null
  bytes: Buffer
}

export class SqliteInterface implements DumpableInterface {
  private db: Database.Database

  constructor(path: string) {
    const dir = dirname(resolve(path))
    if (dir !== dirname(dir)) mkdirSync(dir, { recursive: true })
    this.db = new Database(path)
    this.db.pragma('journal_mode = WAL')
    this.init()
  }

  private init(): void {
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS value_events (
        id        INTEGER PRIMARY KEY AUTOINCREMENT,
        timestamp INTEGER NOT NULL,
        author    TEXT    NOT NULL,
        entity_id TEXT    NOT NULL,
        key       TEXT    NOT NULL,
        value     TEXT    NOT NULL
      );
      CREATE TABLE IF NOT EXISTS link_events (
        id             INTEGER PRIMARY KEY AUTOINCREMENT,
        timestamp      INTEGER NOT NULL,
        author         TEXT    NOT NULL,
        source_id      TEXT    NOT NULL,
        destination_id TEXT    NOT NULL,
        action         INTEGER NOT NULL
      );
      CREATE TABLE IF NOT EXISTS resources (
        id        TEXT    PRIMARY KEY,
        timestamp INTEGER NOT NULL,
        author    TEXT    NOT NULL,
        mime_type TEXT    NOT NULL,
        name      TEXT,
        bytes     BLOB    NOT NULL
      );
      CREATE INDEX IF NOT EXISTS idx_value_entity ON value_events(entity_id);
      CREATE INDEX IF NOT EXISTS idx_link_source  ON link_events(source_id);
      CREATE INDEX IF NOT EXISTS idx_link_dest    ON link_events(destination_id);
    `)
  }

  /**
   * Store bytes under an entity id, replacing whatever was there.
   *
   * Resources are not events: nothing versions them, and popping events off the
   * store leaves them where they are. An undone paste therefore leaves its blob
   * behind, unreferenced — harmless, and cheaper than making bytes replayable.
   */
  async writeResource(resource: ResourceRecord): Promise<void> {
    this.db
      .prepare(
        `INSERT OR REPLACE INTO resources (id, timestamp, author, mime_type, name, bytes)
         VALUES (?, ?, ?, ?, ?, ?)`
      )
      .run(
        resource.id,
        resource.timestamp,
        resource.author,
        resource.mimeType,
        resource.name,
        // Decoded at rest: base64 is the transport's business, not the store's.
        Buffer.from(resource.data, 'base64')
      )
  }

  async readResource(id: string): Promise<ResourceRecord | null> {
    const row = this.db
      .prepare<[string], ResourceRow>(
        `SELECT id, timestamp, author, mime_type, name, bytes FROM resources WHERE id = ?`
      )
      .get(id)
    if (!row) return null
    return {
      id: row.id,
      timestamp: row.timestamp,
      author: row.author,
      mimeType: row.mime_type,
      name: row.name,
      data: Buffer.from(row.bytes).toString('base64'),
    }
  }

  /**
   * Every event touching any of `entityIds`, in the order they were written.
   *
   * Order matters and is the point of this being flat. Within one timestamp the
   * events are ordered by insertion (`id`), because a rollup's own sort is
   * stable and ties are common: one action writes an entity's values and its
   * parent link at the same instant, and a batch that adds a child and then
   * moves it into place is several link events sharing one. Read them back in
   * another order and the entity rolls up with a different child order.
   *
   * Value and link events are not interleaved by id — separate tables, separate
   * sequences — which costs nothing: the two kinds don't interact in a rollup,
   * and each keeps its own order within the timestamp it shares.
   */
  private eventsTouching(entityIds: string[]): AppEvent[] {
    if (entityIds.length === 0) return []
    const ph = entityIds.map(() => '?').join(',')

    const valueRows = this.db
      .prepare<string[], ValueRow>(
        `SELECT timestamp, author, entity_id, key, value
         FROM value_events WHERE entity_id IN (${ph})
         ORDER BY timestamp, id`
      )
      .all(...entityIds)

    const linkRows = this.db
      .prepare<string[], LinkRow>(
        `SELECT timestamp, author, source_id, destination_id, action
         FROM link_events
         WHERE source_id IN (${ph}) OR destination_id IN (${ph})
         ORDER BY timestamp, id`
      )
      .all(...entityIds, ...entityIds)

    return [
      ...valueRows.map(
        (row): AppEvent => ({
          type: 'value',
          timestamp: row.timestamp,
          author: row.author,
          entityId: row.entity_id,
          key: row.key,
          value: JSON.parse(row.value),
        })
      ),
      ...linkRows.map(
        (row): AppEvent => ({
          type: 'link',
          timestamp: row.timestamp,
          author: row.author,
          sourceId: row.source_id,
          destinationId: row.destination_id,
          action: row.action as 0 | 1 | 2 | 3,
        })
      ),
    ]
  }

  /** {@link eventsTouching}, deduplicated and flat — the shape `EventBacking` takes. */
  async readEventsFlat(entityIds: string[]): Promise<AppEvent[]> {
    return this.eventsTouching(entityIds)
  }

  async readEvents(entityIds: string[]): Promise<Map<string, AppEvent[]>> {
    const result = new Map<string, AppEvent[]>()
    if (entityIds.length === 0) return result
    for (const id of entityIds) result.set(id, [])

    for (const event of this.eventsTouching(entityIds)) {
      if (event.type === 'value') {
        result.get(event.entityId)?.push(event)
        continue
      }
      result.get(event.sourceId)?.push(event)
      if (event.destinationId !== event.sourceId) result.get(event.destinationId)?.push(event)
    }

    return result
  }

  /**
   * Remove the last action's events, returning what was removed (oldest first,
   * ready to be written back).
   *
   * Grouping is what makes this useful as an undo step: one user action often
   * writes several events at the same instant — creating an entity writes its
   * values and the link to its parent together — and they should come off as a
   * unit. Select and delete share one transaction, so a concurrent write can't
   * slip between them.
   *
   * **Both windows are this store's**: {@link POP_GROUP_MS} for what counts as
   * one action and {@link POP_AGE_LIMIT_MS} for how far back it reaches, so
   * there is nothing a caller can widen. Past the age limit the history is
   * settled and this is a no-op; the limit is a floor on the cutoff rather than a
   * refusal, so an action straddling it takes the recent half and leaves the rest.
   *
   * `author` narrows it to that person's own events — the *latest* is then their
   * latest, which may be older than somebody else's. That is the point: on a
   * store two people are writing to, undo should reach your own last edit and
   * not theirs.
   */
  async popLatestEvents(author?: string): Promise<AppEvent[]> {
    // Read before the transaction so the clock is sampled once, not per query.
    const oldestPoppable = Date.now() - POP_AGE_LIMIT_MS
    // Spliced into each statement rather than branched around: the alternative is
    // two of everything below, differing in one clause.
    const mine = author === undefined ? '' : ' AND author = :author'
    const whose = author === undefined ? {} : { author }
    return this.db.transaction(() => {
      const latest = this.db
        .prepare<{ author?: string }, { ts: number | null }>(
          `SELECT MAX(ts) AS ts FROM (
             SELECT MAX(timestamp) AS ts FROM value_events WHERE 1 = 1${mine}
             UNION ALL
             SELECT MAX(timestamp) AS ts FROM link_events WHERE 1 = 1${mine}
           )`
        )
        .get(whose)
      if (latest?.ts == null) return []

      const cutoff = Math.max(latest.ts - POP_GROUP_MS, oldestPoppable)
      const from = { ...whose, cutoff }
      const valueRows = this.db
        .prepare<{ cutoff: number; author?: string }, ValueRow>(
          `SELECT timestamp, author, entity_id, key, value
           FROM value_events WHERE timestamp >= :cutoff${mine}`
        )
        .all(from)
      const linkRows = this.db
        .prepare<{ cutoff: number; author?: string }, LinkRow>(
          `SELECT timestamp, author, source_id, destination_id, action
           FROM link_events WHERE timestamp >= :cutoff${mine}`
        )
        .all(from)

      this.db
        .prepare(`DELETE FROM value_events WHERE timestamp >= :cutoff${mine}`)
        .run(from)
      this.db.prepare(`DELETE FROM link_events WHERE timestamp >= :cutoff${mine}`).run(from)

      const events: AppEvent[] = [
        ...valueRows.map(
          (row): ValueEvent => ({
            type: 'value',
            timestamp: row.timestamp,
            author: row.author,
            entityId: row.entity_id,
            key: row.key,
            value: JSON.parse(row.value),
          })
        ),
        ...linkRows.map(
          (row): LinkEvent => ({
            type: 'link',
            timestamp: row.timestamp,
            author: row.author,
            sourceId: row.source_id,
            destinationId: row.destination_id,
            action: row.action as 0 | 1 | 2 | 3,
          })
        ),
      ]
      return events.sort((a, b) => a.timestamp - b.timestamp)
    })()
  }

  async readAllEvents(): Promise<AppEvent[]> {
    const events: AppEvent[] = []

    const valueRows = this.db
      .prepare<[], ValueRow>(
        `SELECT timestamp, author, entity_id, key, value FROM value_events ORDER BY timestamp, id`
      )
      .all()
    for (const row of valueRows) {
      events.push({
        type: 'value',
        timestamp: row.timestamp,
        author: row.author,
        entityId: row.entity_id,
        key: row.key,
        value: JSON.parse(row.value),
      })
    }

    const linkRows = this.db
      .prepare<[], LinkRow>(
        `SELECT timestamp, author, source_id, destination_id, action FROM link_events ORDER BY timestamp, id`
      )
      .all()
    for (const row of linkRows) {
      events.push({
        type: 'link',
        timestamp: row.timestamp,
        author: row.author,
        sourceId: row.source_id,
        destinationId: row.destination_id,
        action: row.action as 0 | 1 | 2 | 3,
      })
    }

    return events
  }

  async writeEvents(events: AppEvent[]): Promise<void> {
    const insertValue = this.db.prepare(
      `INSERT INTO value_events (timestamp, author, entity_id, key, value)
       VALUES (?, ?, ?, ?, ?)`
    )
    const insertLink = this.db.prepare(
      `INSERT INTO link_events (timestamp, author, source_id, destination_id, action)
       VALUES (?, ?, ?, ?, ?)`
    )

    this.db.transaction(() => {
      for (const e of events) {
        if (e.type === 'value') {
          insertValue.run(e.timestamp, e.author, e.entityId, e.key, JSON.stringify(e.value))
        } else {
          insertLink.run(e.timestamp, e.author, e.sourceId, e.destinationId, e.action)
        }
      }
    })()
  }

  close(): void {
    this.db.close()
  }
}
