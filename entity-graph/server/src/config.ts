import { randomBytes } from 'crypto'
import { mkdirSync } from 'fs'
import { dirname, resolve } from 'path'
import Database from 'better-sqlite3'
import type { Safety } from '../../src/core/source/index'

/** Type-specific configuration stored (as JSON) in the `sources` table. */
export type SourceConfig =
  | { type: 'sqlite'; path: string; defaultAuthor?: string }
  | { type: 'combined'; children: string[] }
  | { type: 'frozen'; child: string; beforeTs: number }
  | { type: 'filter'; child: string; allow?: string[]; deny?: string[]; maxSafety?: Safety }
  | { type: 'remote'; url: string; token?: string }

export type SourceType = SourceConfig['type']

export interface SourceRow {
  id: string
  label: string
  type: SourceType
  config: SourceConfig
  createdAt: number
}

export interface TokenRow {
  token: string
  sourceId: string
  label: string
  revoked: boolean
}

interface RawSourceRow {
  id: string
  label: string
  type: string
  config_json: string
  created_at: number
}

interface RawTokenRow {
  token: string
  source_id: string
  label: string
  revoked: number
}

/** Reserved source ids that would collide with fixed routes. */
export const RESERVED_IDS = new Set(['admin', 'tokens'])

export class ConfigDb {
  private db: Database.Database

  constructor(path: string) {
    const dir = dirname(resolve(path))
    if (dir !== dirname(dir)) mkdirSync(dir, { recursive: true })
    this.db = new Database(path)
    this.db.pragma('journal_mode = WAL')
    this.db.exec(`
      CREATE TABLE IF NOT EXISTS sources (
        id          TEXT PRIMARY KEY,
        label       TEXT NOT NULL,
        type        TEXT NOT NULL,
        config_json TEXT NOT NULL,
        created_at  INTEGER NOT NULL
      );
      CREATE TABLE IF NOT EXISTS source_tokens (
        token     TEXT PRIMARY KEY,
        source_id TEXT NOT NULL,
        label     TEXT NOT NULL DEFAULT '',
        revoked   INTEGER NOT NULL DEFAULT 0
      );
      CREATE INDEX IF NOT EXISTS idx_token_source ON source_tokens(source_id);
    `)
  }

  private toRow(r: RawSourceRow): SourceRow {
    return {
      id: r.id,
      label: r.label,
      type: r.type as SourceType,
      config: JSON.parse(r.config_json) as SourceConfig,
      createdAt: r.created_at,
    }
  }

  listSources(): SourceRow[] {
    return this.db
      .prepare<[], RawSourceRow>('SELECT * FROM sources ORDER BY created_at')
      .all()
      .map((r) => this.toRow(r))
  }

  getSource(id: string): SourceRow | undefined {
    const r = this.db
      .prepare<[string], RawSourceRow>('SELECT * FROM sources WHERE id = ?')
      .get(id)
    return r ? this.toRow(r) : undefined
  }

  createSource(row: { id: string; label: string; config: SourceConfig }): SourceRow {
    const createdAt = Date.now()
    this.db
      .prepare(
        `INSERT INTO sources (id, label, type, config_json, created_at)
         VALUES (?, ?, ?, ?, ?)`
      )
      .run(row.id, row.label, row.config.type, JSON.stringify(row.config), createdAt)
    return { id: row.id, label: row.label, type: row.config.type, config: row.config, createdAt }
  }

  updateSource(id: string, patch: { label?: string; config?: SourceConfig }): SourceRow {
    const existing = this.getSource(id)
    if (!existing) throw new Error(`source "${id}" not found`)
    const label = patch.label ?? existing.label
    const config = patch.config ?? existing.config
    this.db
      .prepare(`UPDATE sources SET label = ?, type = ?, config_json = ? WHERE id = ?`)
      .run(label, config.type, JSON.stringify(config), id)
    return { ...existing, label, type: config.type, config }
  }

  deleteSource(id: string): void {
    this.db.prepare('DELETE FROM sources WHERE id = ?').run(id)
    this.db.prepare('DELETE FROM source_tokens WHERE source_id = ?').run(id)
  }

  /** Sources that directly reference `id` as a child (blocks deletion). */
  dependents(id: string): string[] {
    return this.listSources()
      .filter((s) => {
        const c = s.config
        if (c.type === 'combined') return c.children.includes(id)
        if (c.type === 'frozen' || c.type === 'filter') return c.child === id
        return false
      })
      .map((s) => s.id)
  }

  issueToken(sourceId: string, label = ''): string {
    const token = randomBytes(24).toString('hex')
    this.db
      .prepare('INSERT INTO source_tokens (token, source_id, label, revoked) VALUES (?, ?, ?, 0)')
      .run(token, sourceId, label)
    return token
  }

  listTokens(sourceId: string): TokenRow[] {
    return this.db
      .prepare<[string], RawTokenRow>('SELECT * FROM source_tokens WHERE source_id = ?')
      .all(sourceId)
      .map((r) => ({ token: r.token, sourceId: r.source_id, label: r.label, revoked: !!r.revoked }))
  }

  revokeToken(token: string): void {
    this.db.prepare('UPDATE source_tokens SET revoked = 1 WHERE token = ?').run(token)
  }

  /** True if `token` is a live (non-revoked) token for `sourceId`. */
  verifyToken(sourceId: string, token: string): boolean {
    const row = this.db
      .prepare<[string, string], { revoked: number }>(
        'SELECT revoked FROM source_tokens WHERE token = ? AND source_id = ?'
      )
      .get(token, sourceId)
    return !!row && row.revoked === 0
  }

  close(): void {
    this.db.close()
  }
}
