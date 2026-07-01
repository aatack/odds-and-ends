import { mkdirSync, dirname } from 'fs'
import { resolve } from 'path'
import Database from 'better-sqlite3'
import type { AppEvent, ValueEvent, LinkEvent } from '../events'
import type { EntityInterface } from './index'

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

export class SqliteInterface implements EntityInterface {
  private db: Database.Database

  constructor(path: string) {
    mkdirSync(dirname(resolve(path)), { recursive: true })
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
      CREATE INDEX IF NOT EXISTS idx_value_entity ON value_events(entity_id);
      CREATE INDEX IF NOT EXISTS idx_link_source  ON link_events(source_id);
      CREATE INDEX IF NOT EXISTS idx_link_dest    ON link_events(destination_id);
    `)
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
