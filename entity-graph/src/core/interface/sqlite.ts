import { mkdirSync } from 'fs'
import { resolve, dirname } from 'path'
import Database from 'better-sqlite3'
import type { AppEvent, ValueEvent, LinkEvent } from '../events'
import { POP_AGE_LIMIT_MS, type ResourceBacking, type ResourceRecord } from '../source/permissions'
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

export class SqliteInterface implements DumpableInterface, ResourceBacking {
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

  async readEvents(entityIds: string[]): Promise<Map<string, AppEvent[]>> {
    const result = new Map<string, AppEvent[]>()
    if (entityIds.length === 0) return result
    for (const id of entityIds) result.set(id, [])

    const ph = entityIds.map(() => '?').join(',')

    const valueRows = this.db
      .prepare<string[], ValueRow>(
        `SELECT timestamp, author, entity_id, key, value
         FROM value_events WHERE entity_id IN (${ph})`
      )
      .all(...entityIds)

    for (const row of valueRows) {
      const event: ValueEvent = {
        type: 'value',
        timestamp: row.timestamp,
        author: row.author,
        entityId: row.entity_id,
        key: row.key,
        value: JSON.parse(row.value),
      }
      result.get(row.entity_id)!.push(event)
    }

    const linkRows = this.db
      .prepare<string[], LinkRow>(
        `SELECT timestamp, author, source_id, destination_id, action
         FROM link_events
         WHERE source_id IN (${ph}) OR destination_id IN (${ph})`
      )
      .all(...entityIds, ...entityIds)

    for (const row of linkRows) {
      const event: LinkEvent = {
        type: 'link',
        timestamp: row.timestamp,
        author: row.author,
        sourceId: row.source_id,
        destinationId: row.destination_id,
        action: row.action as 0 | 1 | 2 | 3,
      }
      if (result.has(row.source_id)) result.get(row.source_id)!.push(event)
      if (result.has(row.destination_id) && row.destination_id !== row.source_id) {
        result.get(row.destination_id)!.push(event)
      }
    }

    return result
  }

  /**
   * Remove the most recent event, and any within `windowMs` of it, returning
   * what was removed (oldest first, ready to be written back).
   *
   * The window is what makes this useful as an undo step: one user action often
   * writes several events at the same instant — creating an entity writes its
   * values and the link to its parent together — and they should come off as a
   * unit. Select and delete share one transaction, so a concurrent write can't
   * slip between them.
   *
   * Nothing older than {@link POP_AGE_LIMIT_MS} is touched, whatever window is
   * asked for: past that the history is settled and this call is a no-op. The
   * limit is a floor on the cutoff rather than a refusal, so a window straddling
   * it takes the recent half and leaves the rest.
   */
  async popLatestEvents(windowMs: number): Promise<AppEvent[]> {
    // Read before the transaction so the clock is sampled once, not per query.
    const oldestPoppable = Date.now() - POP_AGE_LIMIT_MS
    return this.db.transaction(() => {
      const latest = this.db
        .prepare<[], { ts: number | null }>(
          `SELECT MAX(ts) AS ts FROM (
             SELECT MAX(timestamp) AS ts FROM value_events
             UNION ALL
             SELECT MAX(timestamp) AS ts FROM link_events
           )`
        )
        .get()
      if (latest?.ts == null) return []

      const cutoff = Math.max(latest.ts - windowMs, oldestPoppable)
      const valueRows = this.db
        .prepare<[number], ValueRow>(
          `SELECT timestamp, author, entity_id, key, value
           FROM value_events WHERE timestamp >= ?`
        )
        .all(cutoff)
      const linkRows = this.db
        .prepare<[number], LinkRow>(
          `SELECT timestamp, author, source_id, destination_id, action
           FROM link_events WHERE timestamp >= ?`
        )
        .all(cutoff)

      this.db.prepare(`DELETE FROM value_events WHERE timestamp >= ?`).run(cutoff)
      this.db.prepare(`DELETE FROM link_events WHERE timestamp >= ?`).run(cutoff)

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
        `SELECT timestamp, author, entity_id, key, value FROM value_events`
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
        `SELECT timestamp, author, source_id, destination_id, action FROM link_events`
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
