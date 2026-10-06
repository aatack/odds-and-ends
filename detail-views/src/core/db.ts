import { DatabaseSync } from 'node:sqlite'

/*
 * Two files. Migrations are append-only per file: never edit one that has
 * shipped. (The file before these, `detail-views.sqlite`, is only ever read,
 * once, by `legacy.ts`.)
 */

const events = `
  CREATE TABLE value_events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp INTEGER NOT NULL,
    author TEXT NOT NULL,
    entity_id TEXT NOT NULL,
    key TEXT NOT NULL,
    value TEXT NOT NULL
  );
  CREATE TABLE link_events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    timestamp INTEGER NOT NULL,
    author TEXT NOT NULL,
    source_id TEXT NOT NULL,
    destination_id TEXT NOT NULL,
    action INTEGER NOT NULL
  );
  CREATE INDEX link_events_source ON link_events (source_id);
  CREATE INDEX link_events_destination ON link_events (destination_id);
`

/** What I made: an event log nothing in the app ever deletes from. */
export const ownedMigrations: string[] = [
  `${events}
  CREATE INDEX value_events_entity ON value_events (entity_id);
  CREATE TABLE settings (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL
  );
  `,
]

/**
 * What was fetched from elsewhere. One row per value and per link, replaced
 * by the next load rather than piled up, and the whole file may be emptied at
 * any time: everything in it can be loaded again.
 */
export const cacheMigrations: string[] = [
  `${events}
  CREATE UNIQUE INDEX value_events_key ON value_events (entity_id, key);
  CREATE UNIQUE INDEX link_events_pair ON link_events (source_id, destination_id);
  CREATE TABLE blobs (
    key TEXT PRIMARY KEY,
    mime TEXT NOT NULL,
    data BLOB NOT NULL
  );
  CREATE TABLE meta (
    key TEXT PRIMARY KEY,
    value TEXT NOT NULL
  );
  `,
]

export function openDatabase(path: string, migrations: string[]): DatabaseSync {
  const db = new DatabaseSync(path)
  db.exec('PRAGMA journal_mode = WAL;')
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
