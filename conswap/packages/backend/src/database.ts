import Database from 'better-sqlite3'
import { mkdirSync } from 'node:fs'
import { dirname } from 'node:path'

export type Db = Database.Database

/**
 * Each migration runs once, in order, inside a transaction. Adding a migration
 * means appending to this list and never editing the ones above it.
 */
const migrations: { name: string; sql: string }[] = [
  {
    name: '001-topics',
    sql: `
      CREATE TABLE topics (
        id TEXT PRIMARY KEY,
        type TEXT NOT NULL,
        text TEXT NOT NULL DEFAULT '',
        metadata TEXT NOT NULL DEFAULT '{}',
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        open INTEGER NOT NULL DEFAULT 1,
        resolved INTEGER NOT NULL DEFAULT 0
      );
      CREATE INDEX topics_open ON topics (open, resolved, updated_at);
      CREATE INDEX topics_type ON topics (type);

      CREATE TABLE links (
        parent_id TEXT NOT NULL,
        parent_type TEXT NOT NULL,
        child_id TEXT NOT NULL,
        child_type TEXT NOT NULL,
        type TEXT NOT NULL DEFAULT 'watch',
        created_at TEXT NOT NULL,
        PRIMARY KEY (parent_id, child_id, type)
      );
      CREATE INDEX links_child ON links (child_id);
      CREATE INDEX links_parent ON links (parent_id, created_at);

      CREATE TABLE blockers (
        id TEXT PRIMARY KEY,
        topic_id TEXT NOT NULL,
        type TEXT NOT NULL,
        label TEXT NOT NULL DEFAULT '',
        config TEXT NOT NULL DEFAULT '{}',
        state TEXT NOT NULL DEFAULT '{}',
        created_at TEXT NOT NULL,
        due_at TEXT,
        satisfied_at TEXT,
        cancelled_at TEXT,
        last_error TEXT
      );
      CREATE INDEX blockers_topic ON blockers (topic_id);
      CREATE INDEX blockers_live ON blockers (satisfied_at, cancelled_at, due_at);

      CREATE TABLE runs (
        id TEXT PRIMARY KEY,
        topic_id TEXT NOT NULL,
        kind TEXT NOT NULL,
        status TEXT NOT NULL,
        started_at TEXT NOT NULL,
        finished_at TEXT,
        prompt TEXT NOT NULL DEFAULT '',
        output TEXT NOT NULL DEFAULT '',
        error TEXT
      );
      CREATE INDEX runs_topic ON runs (topic_id, started_at);

      -- Every mutation the frontend asks for, kept forever, so that a failed
      -- optimistic update can always be replayed rather than lost.
      CREATE TABLE mutations (
        id TEXT PRIMARY KEY,
        name TEXT NOT NULL,
        args TEXT NOT NULL,
        created_at TEXT NOT NULL,
        status TEXT NOT NULL,
        result TEXT,
        error TEXT
      );

      CREATE TABLE kv (
        key TEXT PRIMARY KEY,
        value TEXT NOT NULL
      );
    `,
  },
]

export function openDatabase(path: string): Db {
  if (path !== ':memory:') mkdirSync(dirname(path), { recursive: true })
  const db = new Database(path)
  db.pragma('journal_mode = WAL')
  db.pragma('foreign_keys = ON')
  db.exec('CREATE TABLE IF NOT EXISTS migrations (name TEXT PRIMARY KEY, applied_at TEXT NOT NULL)')

  const applied = new Set(
    db.prepare('SELECT name FROM migrations').all().map((row) => (row as { name: string }).name),
  )
  const record = db.prepare('INSERT INTO migrations (name, applied_at) VALUES (?, ?)')
  for (const migration of migrations) {
    if (applied.has(migration.name)) continue
    db.transaction(() => {
      db.exec(migration.sql)
      record.run(migration.name, new Date().toISOString())
    })()
  }
  return db
}

export function readKey(db: Db, key: string): string | null {
  const row = db.prepare('SELECT value FROM kv WHERE key = ?').get(key) as { value: string } | undefined
  return row?.value ?? null
}

export function writeKey(db: Db, key: string, value: string): void {
  db.prepare('INSERT INTO kv (key, value) VALUES (?, ?) ON CONFLICT (key) DO UPDATE SET value = excluded.value').run(
    key,
    value,
  )
}

export function readJson<T>(db: Db, key: string, fallback: T): T {
  const raw = readKey(db, key)
  if (raw === null) return fallback
  try {
    return JSON.parse(raw) as T
  } catch {
    return fallback
  }
}

export function writeJson(db: Db, key: string, value: unknown): void {
  writeKey(db, key, JSON.stringify(value))
}
