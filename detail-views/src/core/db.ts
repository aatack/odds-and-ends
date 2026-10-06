import { DatabaseSync } from 'node:sqlite'

/** Append-only. Never edit one that has shipped. */
const migrations: string[] = [
  `
  CREATE TABLE entities (
    id TEXT PRIMARY KEY,
    type TEXT NOT NULL,
    data TEXT NOT NULL DEFAULT '{}',
    created_at INTEGER NOT NULL,
    updated_at INTEGER NOT NULL,
    expires_at INTEGER
  );
  CREATE INDEX entities_expires ON entities (expires_at) WHERE expires_at IS NOT NULL;

  CREATE TABLE links (
    parent TEXT NOT NULL,
    child TEXT NOT NULL,
    rank REAL NOT NULL DEFAULT 0,
    created_at INTEGER NOT NULL,
    expires_at INTEGER,
    PRIMARY KEY (parent, child)
  );
  CREATE INDEX links_child ON links (child);

  CREATE TABLE settings (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL
  );
  `,
  `
  CREATE TABLE blobs (
    key TEXT PRIMARY KEY,
    mime TEXT NOT NULL,
    data BLOB NOT NULL,
    created_at INTEGER NOT NULL,
    expires_at INTEGER NOT NULL
  );
  `,
  // findBy on a conversation's user runs for every author and mention shown;
  // without this it scans every cached row.
  `
  CREATE INDEX entities_user ON entities (type, json_extract(data, '$.user'));
  `,
]

export function openDatabase(path: string): DatabaseSync {
  const db = new DatabaseSync(path)
  db.exec('PRAGMA journal_mode = WAL; PRAGMA foreign_keys = ON;')
  const { user_version: version } = db.prepare('PRAGMA user_version').get() as { user_version: number }
  for (let index = version; index < migrations.length; index += 1) {
    db.exec('BEGIN')
    try {
      db.exec(migrations[index])
      db.exec(`PRAGMA user_version = ${index + 1}`)
      db.exec('COMMIT')
    } catch (error) {
      db.exec('ROLLBACK')
      throw error
    }
  }
  return db
}
