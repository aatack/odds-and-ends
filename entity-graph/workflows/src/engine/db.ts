// The store: one SQLite file. Written against the part of the API that
// better-sqlite3 (the app) and node:sqlite (the tests) share, with positional
// parameters only, so either can be handed in.

export interface Statement {
  run(...params: unknown[]): unknown
  get(...params: unknown[]): unknown
  all(...params: unknown[]): unknown[]
}

export interface Db {
  exec(sql: string): void
  prepare(sql: string): Statement
}

const SCHEMA = `
CREATE TABLE IF NOT EXISTS items (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  source TEXT NOT NULL,
  source_ref TEXT UNIQUE,
  url TEXT,
  state TEXT NOT NULL DEFAULT '',
  closed INTEGER NOT NULL DEFAULT 0,
  worker INTEGER,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);

-- Each item's handoffs: a queue, in seq order. One is active while the item is
-- open; resolved ones keep their resolution, which is what chains them.
CREATE TABLE IF NOT EXISTS handoffs (
  id TEXT PRIMARY KEY,
  item_id TEXT NOT NULL,
  seq REAL NOT NULL,
  status TEXT NOT NULL,
  type TEXT NOT NULL,
  params TEXT NOT NULL,
  created_by TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  started_at INTEGER,
  resolved_at INTEGER,
  state_at_start TEXT,
  resolution TEXT,
  resolved_by TEXT,
  check_step INTEGER NOT NULL DEFAULT 0,
  next_check_at INTEGER,
  last_error TEXT
);
CREATE INDEX IF NOT EXISTS handoffs_item ON handoffs (item_id, seq);
CREATE INDEX IF NOT EXISTS handoffs_active ON handoffs (status, type, started_at);

-- What happened between handoffs. Append-only: it is what the bots read back
-- to find what could have gone better.
CREATE TABLE IF NOT EXISTS events (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  item_id TEXT NOT NULL,
  at INTEGER NOT NULL,
  actor TEXT NOT NULL,
  kind TEXT NOT NULL,
  text TEXT NOT NULL,
  data TEXT
);
CREATE INDEX IF NOT EXISTS events_item ON events (item_id, id);

-- Every claude -p run, with its whole transcript.
CREATE TABLE IF NOT EXISTS runs (
  id TEXT PRIMARY KEY,
  item_id TEXT NOT NULL,
  handoff_id TEXT NOT NULL,
  worker INTEGER NOT NULL,
  started_at INTEGER NOT NULL,
  ended_at INTEGER,
  cost_usd REAL,
  turns INTEGER,
  result TEXT,
  prompt TEXT NOT NULL,
  transcript TEXT NOT NULL DEFAULT ''
);
CREATE INDEX IF NOT EXISTS runs_item ON runs (item_id, started_at);
`

export function migrate(db: Db): void {
  db.exec(SCHEMA)
  // Agents were renamed bots; rows written before that say 'agent'.
  db.exec(`
    UPDATE handoffs SET type = 'bot' WHERE type = 'agent';
    UPDATE handoffs SET created_by = 'bot' WHERE created_by = 'agent';
    UPDATE handoffs SET created_by = 'script:bot' WHERE created_by = 'script:agent';
    UPDATE handoffs SET resolved_by = 'bot' WHERE resolved_by = 'agent';
    UPDATE events SET actor = 'bot' WHERE actor = 'agent';
  `)
}

export function transaction<T>(db: Db, fn: () => T): T {
  db.exec('BEGIN')
  try {
    const out = fn()
    db.exec('COMMIT')
    return out
  } catch (e) {
    db.exec('ROLLBACK')
    throw e
  }
}
