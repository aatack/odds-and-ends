import type { DatabaseSync } from 'node:sqlite'
import type { AppEvent, LinkAction } from './graph/events.ts'

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

const fromValue = (row: ValueRow): AppEvent => ({
  type: 'value',
  timestamp: row.timestamp,
  author: row.author,
  entityId: row.entity_id,
  key: row.key,
  value: JSON.parse(row.value),
})

const fromLink = (row: LinkRow): AppEvent => ({
  type: 'link',
  timestamp: row.timestamp,
  author: row.author,
  sourceId: row.source_id,
  destinationId: row.destination_id,
  action: row.action as LinkAction,
})

export interface WriteOptions {
  /**
   * Cache only: sources whose links are exactly those in this write. Any
   * other link from one of them is dropped, so a list that lost an item
   * loses its row. `within` narrows it to destinations starting with that,
   * for a source whose links come from more than one list.
   */
  replaceLinksFrom?: (string | { source: string; within: string })[]
}

/**
 * Events in one SQLite file. `log` appends, and is what I own. `snapshot`
 * keeps one event per value and per link, replacing it on the next write, and
 * is the cache of what other services said: a reload states what is true now
 * rather than adding to a history nothing needs.
 */
export class EventStore {
  private readonly db: DatabaseSync
  private readonly mode: 'log' | 'snapshot'
  private readonly onChange: (ids: string[]) => void
  private depth = 0

  constructor(db: DatabaseSync, mode: 'log' | 'snapshot', onChange: (ids: string[]) => void = () => {}) {
    this.db = db
    this.mode = mode
    this.onChange = onChange
  }

  /**
   * Every event touching any of `ids`, oldest first. Ties keep the order they
   * were written in, which is what a rollup's stable sort relies on.
   */
  read(ids: readonly string[]): AppEvent[] {
    if (!ids.length) return []
    const list = JSON.stringify([...new Set(ids)])
    const values = this.db
      .prepare(
        `SELECT timestamp, author, entity_id, key, value FROM value_events
         WHERE entity_id IN (SELECT value FROM json_each(?)) ORDER BY timestamp, id`,
      )
      .all(list) as unknown as ValueRow[]
    const links = this.db
      .prepare(
        `SELECT timestamp, author, source_id, destination_id, action FROM link_events
         WHERE source_id IN (SELECT value FROM json_each(?1)) OR destination_id IN (SELECT value FROM json_each(?1))
         ORDER BY timestamp, id`,
      )
      .all(list) as unknown as LinkRow[]
    return [...values.map(fromValue), ...links.map(fromLink)]
  }

  /** Writes events, and says which entities changed. A snapshot write that changes nothing says nothing. */
  write(events: readonly AppEvent[], options: WriteOptions = {}): void {
    const touched = new Set<string>()
    const snapshot = this.mode === 'snapshot'
    const insertValue = this.db.prepare(
      `INSERT INTO value_events (timestamp, author, entity_id, key, value) VALUES (?, ?, ?, ?, ?)` +
        (snapshot
          ? ` ON CONFLICT (entity_id, key) DO UPDATE SET timestamp = excluded.timestamp, author = excluded.author, value = excluded.value
              WHERE value IS NOT excluded.value OR timestamp IS NOT excluded.timestamp`
          : ''),
    )
    const insertLink = this.db.prepare(
      `INSERT INTO link_events (timestamp, author, source_id, destination_id, action) VALUES (?, ?, ?, ?, ?)` +
        (snapshot
          ? ` ON CONFLICT (source_id, destination_id) DO UPDATE SET timestamp = excluded.timestamp, author = excluded.author, action = excluded.action
              WHERE action IS NOT excluded.action OR timestamp IS NOT excluded.timestamp`
          : ''),
    )
    this.transaction(() => {
      for (const e of events) {
        if (e.type === 'value') {
          const { changes } = insertValue.run(e.timestamp, e.author, e.entityId, e.key, JSON.stringify(e.value ?? null))
          if (changes) touched.add(e.entityId)
        } else {
          const { changes } = insertLink.run(e.timestamp, e.author, e.sourceId, e.destinationId, e.action)
          if (changes) touched.add(e.sourceId).add(e.destinationId)
        }
      }
      for (const replace of options.replaceLinksFrom ?? []) {
        if (!snapshot) throw new Error('only the cache replaces links')
        const { source, within } = typeof replace === 'string' ? { source: replace, within: '' } : replace
        const keep = events.flatMap((e) => (e.type === 'link' && e.sourceId === source ? [e.destinationId] : []))
        const dropped = this.db
          .prepare(
            `DELETE FROM link_events WHERE source_id = ? AND substr(destination_id, 1, length(?)) = ?
             AND destination_id NOT IN (SELECT value FROM json_each(?))
             RETURNING destination_id`,
          )
          .all(source, within, within, JSON.stringify(keep)) as { destination_id: string }[]
        if (dropped.length) touched.add(source)
        for (const row of dropped) touched.add(row.destination_id)
      }
    })
    if (touched.size) this.onChange([...touched])
  }

  /**
   * Takes the last action off the log and hands it back, oldest first: what
   * undo is. One action is everything within `group` ms of the newest event
   * (a note and its link are written together). Nothing older than `horizon`
   * ms is ever taken: past that an edit is settled.
   */
  pop(now: number, group = 100, horizon = 5 * 60_000): AppEvent[] {
    if (this.mode !== 'log') throw new Error('only the owned log pops')
    let popped: AppEvent[] = []
    this.transaction(() => {
      const latest = (
        this.db
          .prepare('SELECT MAX(ts) AS ts FROM (SELECT MAX(timestamp) AS ts FROM value_events UNION ALL SELECT MAX(timestamp) AS ts FROM link_events)')
          .get() as { ts: number | null }
      ).ts
      if (latest === null) return
      const cutoff = Math.max(latest - group, now - horizon)
      const values = this.db
        .prepare('SELECT timestamp, author, entity_id, key, value FROM value_events WHERE timestamp >= ? ORDER BY timestamp, id')
        .all(cutoff) as unknown as ValueRow[]
      const links = this.db
        .prepare('SELECT timestamp, author, source_id, destination_id, action FROM link_events WHERE timestamp >= ? ORDER BY timestamp, id')
        .all(cutoff) as unknown as LinkRow[]
      this.db.prepare('DELETE FROM value_events WHERE timestamp >= ?').run(cutoff)
      this.db.prepare('DELETE FROM link_events WHERE timestamp >= ?').run(cutoff)
      popped = [...values.map(fromValue), ...links.map(fromLink)].sort((a, b) => a.timestamp - b.timestamp)
    })
    if (popped.length) {
      const ids = new Set<string>()
      for (const e of popped) {
        if (e.type === 'value') ids.add(e.entityId)
        else ids.add(e.sourceId).add(e.destinationId)
      }
      this.onChange([...ids])
    }
    return popped
  }

  /** Empties the store: the cache's weekly clear-out. */
  clear(): void {
    this.transaction(() => {
      this.db.exec('DELETE FROM value_events; DELETE FROM link_events;')
    })
  }

  transaction(body: () => void): void {
    if (this.depth > 0) return body()
    this.depth += 1
    this.db.exec('BEGIN')
    try {
      body()
      this.db.exec('COMMIT')
    } catch (error) {
      this.db.exec('ROLLBACK')
      throw error
    } finally {
      this.depth -= 1
    }
  }
}

/** Secrets and settings, in the owned file. */
export class Settings {
  private readonly db: DatabaseSync

  constructor(db: DatabaseSync) {
    this.db = db
  }

  get(key: string): string | null {
    const row = this.db.prepare('SELECT value FROM settings WHERE key = ?').get(key) as { value: string } | undefined
    return row?.value ?? null
  }

  set(key: string, value: string | null): void {
    if (value === null) this.db.prepare('DELETE FROM settings WHERE key = ?').run(key)
    else
      this.db
        .prepare('INSERT INTO settings (key, value) VALUES (?, ?) ON CONFLICT (key) DO UPDATE SET value = excluded.value')
        .run(key, value)
  }
}

export interface Blob {
  mime: string
  data: Uint8Array
}

/** Bytes fetched from a service, such as an image, in the cache file. */
export class Blobs {
  private readonly db: DatabaseSync

  constructor(db: DatabaseSync) {
    this.db = db
  }

  get(key: string): Blob | null {
    const row = this.db.prepare('SELECT mime, data FROM blobs WHERE key = ?').get(key) as Blob | undefined
    return row ? { mime: row.mime, data: row.data } : null
  }

  put(key: string, blob: Blob): void {
    this.db
      .prepare(
        `INSERT INTO blobs (key, mime, data) VALUES (?, ?, ?)
         ON CONFLICT (key) DO UPDATE SET mime = excluded.mime, data = excluded.data`,
      )
      .run(key, blob.mime, blob.data)
  }

  clear(): void {
    this.db.exec('DELETE FROM blobs')
  }
}
