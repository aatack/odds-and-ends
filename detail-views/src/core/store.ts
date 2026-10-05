import type { DatabaseSync } from 'node:sqlite'
import type { Entity } from './types.ts'

interface EntityRow {
  id: string
  type: string
  data: string
  created_at: number
  updated_at: number
  expires_at: number | null
}

function toEntity(row: EntityRow): Entity {
  return {
    id: row.id,
    type: row.type,
    data: JSON.parse(row.data) as Record<string, unknown>,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    expiresAt: row.expires_at,
  }
}

/** How long a cached write lives. Leave it out for owned data. */
export interface CacheOptions {
  ttl?: number
}

export interface ChildLink {
  id: string
  rank: number
}

/**
 * Entities and the directional links between them. Owned rows have no expiry;
 * cached rows do, and `sweep` removes them once it passes.
 */
export class Store {
  private readonly db: DatabaseSync
  private readonly now: () => number
  private readonly onChange: (ids: string[]) => void

  constructor(db: DatabaseSync, options: { now?: () => number; onChange?: (ids: string[]) => void } = {}) {
    this.db = db
    this.now = options.now ?? Date.now
    this.onChange = options.onChange ?? (() => {})
  }

  private expiry(options: CacheOptions | undefined): number | null {
    return options?.ttl === undefined ? null : this.now() + options.ttl
  }

  get(id: string): Entity | null {
    const row = this.db.prepare('SELECT * FROM entities WHERE id = ?').get(id) as EntityRow | undefined
    return row ? toEntity(row) : null
  }

  /** The first entity of `type` whose top-level `field` equals `value`. */
  findBy(type: string, field: string, value: string): Entity | null {
    const row = this.db
      .prepare(`SELECT * FROM entities WHERE type = ? AND json_extract(data, '$.' || ?) = ? LIMIT 1`)
      .get(type, field, value) as EntityRow | undefined
    return row ? toEntity(row) : null
  }

  getMany(ids: string[]): Map<string, Entity> {
    const found = new Map<string, Entity>()
    if (ids.length === 0) return found
    const statement = this.db.prepare(`SELECT * FROM entities WHERE id IN (SELECT value FROM json_each(?))`)
    for (const row of statement.all(JSON.stringify(ids)) as unknown as EntityRow[]) found.set(row.id, toEntity(row))
    return found
  }

  /**
   * Writes an entity whole. A cached write to something owned keeps it owned:
   * the data is replaced but the row never gains an expiry.
   */
  put(id: string, type: string, data: Record<string, unknown>, options?: CacheOptions): Entity {
    const now = this.now()
    this.db
      .prepare(
        `INSERT INTO entities (id, type, data, created_at, updated_at, expires_at)
         VALUES (?, ?, ?, ?, ?, ?)
         ON CONFLICT (id) DO UPDATE SET
           type = excluded.type,
           data = excluded.data,
           updated_at = excluded.updated_at,
           expires_at = CASE WHEN entities.expires_at IS NULL THEN NULL ELSE excluded.expires_at END`,
      )
      .run(id, type, JSON.stringify(data), now, now, this.expiry(options))
    this.onChange([id])
    return this.get(id)!
  }

  /** Merges fields into an existing entity. Returns null if it is not there. */
  patch(id: string, fields: Record<string, unknown>, options?: CacheOptions): Entity | null {
    const existing = this.get(id)
    if (!existing) return null
    return this.put(id, existing.type, { ...existing.data, ...fields }, existing.expiresAt === null ? undefined : options)
  }

  remove(id: string): void {
    this.db.prepare('DELETE FROM links WHERE parent = ? OR child = ?').run(id, id)
    this.db.prepare('DELETE FROM entities WHERE id = ?').run(id)
    this.onChange([id])
  }

  children(parent: string): Entity[] {
    const rows = this.db
      .prepare(
        `SELECT e.* FROM links l JOIN entities e ON e.id = l.child
         WHERE l.parent = ? ORDER BY l.rank, l.created_at`,
      )
      .all(parent) as unknown as EntityRow[]
    return rows.map(toEntity)
  }

  parents(child: string): Entity[] {
    const rows = this.db
      .prepare(`SELECT e.* FROM links l JOIN entities e ON e.id = l.parent WHERE l.child = ? ORDER BY l.created_at`)
      .all(child) as unknown as EntityRow[]
    return rows.map(toEntity)
  }

  /** An owned link stays owned, like an owned entity. Without a rank it goes last. */
  link(parent: string, child: string, options: CacheOptions & { rank?: number } = {}): void {
    const rank =
      options.rank ??
      (this.db.prepare('SELECT COALESCE(MAX(rank), 0) + 1 AS next FROM links WHERE parent = ?').get(parent) as { next: number })
        .next
    this.db
      .prepare(
        `INSERT INTO links (parent, child, rank, created_at, expires_at) VALUES (?, ?, ?, ?, ?)
         ON CONFLICT (parent, child) DO UPDATE SET
           rank = excluded.rank,
           expires_at = CASE WHEN links.expires_at IS NULL THEN NULL ELSE excluded.expires_at END`,
      )
      .run(parent, child, rank, this.now(), this.expiry(options))
    this.onChange([parent, child])
  }

  unlink(parent: string, child: string): void {
    this.db.prepare('DELETE FROM links WHERE parent = ? AND child = ?').run(parent, child)
    this.onChange([parent, child])
  }

  /**
   * Replaces the cached children of `parent` with `children`. Owned links
   * under it are left alone, so my own notes survive a refresh.
   */
  setCachedChildren(parent: string, children: ChildLink[], options: Required<CacheOptions>): void {
    this.transaction(() => {
      const keep = JSON.stringify(children.map((child) => child.id))
      this.db
        .prepare(
          `DELETE FROM links WHERE parent = ? AND expires_at IS NOT NULL
           AND child NOT IN (SELECT value FROM json_each(?))`,
        )
        .run(parent, keep)
      for (const child of children) this.link(parent, child.id, { rank: child.rank, ttl: options.ttl })
    })
    this.onChange([parent])
  }

  /**
   * Deletes expired links, then expired entities that no owned link still
   * points at or from, then any link left without an end. Returns how many
   * entities went.
   */
  sweep(): number {
    const now = this.now()
    let removed = 0
    this.transaction(() => {
      this.db.prepare('DELETE FROM links WHERE expires_at IS NOT NULL AND expires_at <= ?').run(now)
      removed = Number(
        this.db
          .prepare(
            `DELETE FROM entities WHERE expires_at IS NOT NULL AND expires_at <= ?
             AND id NOT IN (SELECT child FROM links WHERE expires_at IS NULL)
             AND id NOT IN (SELECT parent FROM links WHERE expires_at IS NULL)`,
          )
          .run(now).changes,
      )
      this.db
        .prepare(
          `DELETE FROM links WHERE parent NOT IN (SELECT id FROM entities)
           OR child NOT IN (SELECT id FROM entities)`,
        )
        .run()
    })
    if (removed > 0) this.onChange([])
    return removed
  }

  getSetting(key: string): string | null {
    const row = this.db.prepare('SELECT value FROM settings WHERE key = ?').get(key) as { value: string } | undefined
    return row?.value ?? null
  }

  setSetting(key: string, value: string | null): void {
    if (value === null) this.db.prepare('DELETE FROM settings WHERE key = ?').run(key)
    else
      this.db
        .prepare('INSERT INTO settings (key, value) VALUES (?, ?) ON CONFLICT (key) DO UPDATE SET value = excluded.value')
        .run(key, value)
  }

  private depth = 0

  transaction(body: () => void): void {
    if (this.depth > 0) {
      body()
      return
    }
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
