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
  blocker TEXT NOT NULL,
  request TEXT,
  instruction TEXT,
  worker INTEGER,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL,
  blocked_at INTEGER NOT NULL
);
CREATE INDEX IF NOT EXISTS items_blocker ON items (blocker, blocked_at);

-- The full history of every item. Nothing is deleted or rewritten here: it is
-- what the agents read back to find what could have gone better.
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
CREATE INDEX IF NOT EXISTS events_at ON events (at);

-- One row per external wait, while the item waits.
CREATE TABLE IF NOT EXISTS waits (
  item_id TEXT PRIMARY KEY,
  kind TEXT NOT NULL,
  description TEXT NOT NULL,
  params TEXT NOT NULL,
  step INTEGER NOT NULL DEFAULT 0,
  next_check_at INTEGER NOT NULL,
  deadline INTEGER,
  last_error TEXT
);

-- Items waiting for other items to close.
CREATE TABLE IF NOT EXISTS deps (
  item_id TEXT NOT NULL,
  depends_on TEXT NOT NULL,
  PRIMARY KEY (item_id, depends_on)
);

-- Every claude -p run, with its whole transcript.
CREATE TABLE IF NOT EXISTS runs (
  id TEXT PRIMARY KEY,
  item_id TEXT NOT NULL,
  worker INTEGER NOT NULL,
  started_at INTEGER NOT NULL,
  ended_at INTEGER,
  outcome TEXT,
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
