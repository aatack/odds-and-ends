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
  closed INTEGER NOT NULL DEFAULT 0,
  worker INTEGER,
  -- The item's own Claude session: every bot run on it continues the one conversation.
  session_id TEXT,
  created_at INTEGER NOT NULL,
  updated_at INTEGER NOT NULL
);

-- An item's events, oldest first. One that resolves as it is added is a plain
-- event; one that does not is a blocker until it resolves. Rows are added and
-- resolved, never deleted.
CREATE TABLE IF NOT EXISTS events (
  id TEXT PRIMARY KEY,
  item_id TEXT NOT NULL,
  seq INTEGER NOT NULL,
  type TEXT NOT NULL,
  params TEXT NOT NULL,
  created_by TEXT NOT NULL,
  created_at INTEGER NOT NULL,
  resolved INTEGER NOT NULL DEFAULT 0,
  resolved_at INTEGER,
  resolved_by TEXT,
  cancelled INTEGER NOT NULL DEFAULT 0,
  -- Its type's start hook has run (or it has none).
  started INTEGER NOT NULL DEFAULT 0,
  data TEXT,
  check_step INTEGER NOT NULL DEFAULT -1,
  next_check_at INTEGER,
  last_error TEXT
);
CREATE INDEX IF NOT EXISTS events_item ON events (item_id, seq);
CREATE INDEX IF NOT EXISTS events_open ON events (resolved, type, created_at);

-- Everything that happened, when, and who did it: one row a moment, never
-- changed or deleted. Events' timelines (ItemEvent.times) are read from it.
CREATE TABLE IF NOT EXISTS trace (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  at INTEGER NOT NULL,
  item_id TEXT NOT NULL,
  event_id TEXT,
  run_id TEXT,
  actor TEXT NOT NULL,
  kind TEXT NOT NULL,
  data TEXT
);
CREATE INDEX IF NOT EXISTS trace_item ON trace (item_id, id);
CREATE INDEX IF NOT EXISTS trace_event ON trace (event_id, kind);

-- Every claude -p run, with its whole transcript. A run answers every bot
-- blocker on its item that was waiting as it started.
CREATE TABLE IF NOT EXISTS runs (
  id TEXT PRIMARY KEY,
  item_id TEXT NOT NULL,
  event_ids TEXT NOT NULL,
  worker INTEGER NOT NULL,
  started_at INTEGER NOT NULL,
  -- The last trace row as the bot was prompted: the next run is told everything after it.
  trace_mark INTEGER NOT NULL DEFAULT 0,
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
