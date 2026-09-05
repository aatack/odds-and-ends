/**
 * The store. One table of events; everything else -- the balance, the staircase,
 * the plot -- is worked out from them, so the events are the only thing worth
 * keeping.
 */

import { DatabaseSync } from "node:sqlite";
import { dirname, join, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import type { DebtEvent, EventKind } from "./debt.ts";
import { instantOf } from "./time.ts";

const HERE = dirname(fileURLToPath(import.meta.url));

/** Where the database lives; `RUNNING_DEBT_DB` moves it. */
export function databasePath(): string {
  const set = process.env.RUNNING_DEBT_DB;
  return set ? resolve(set) : join(HERE, "..", "debt.db");
}

/** The events written down in the note this was built from. */
const SEED: [month: number, day: number, kind: EventKind, km: number][] = [
  [6, 4, "penalty", 0],
  [6, 7, "run", 3],
  [8, 14, "cycle", 7.21],
  [8, 16, "cycle", 8.37],
  [8, 16, "cycle", 8.4],
  [8, 18, "cycle", 12.88],
  [8, 20, "cycle", 15.43],
  [8, 22, "cycle", 11.1],
  [8, 23, "cycle", 18.19],
  [8, 26, "cycle", 20.01],
  [8, 28, "cycle", 7.75],
  [8, 29, "cycle", 13.81],
  [8, 30, "cycle", 33.57],
  [9, 2, "cycle", 18.56],
  [9, 4, "cycle", 14.27],
];

const SEED_YEAR = 2026;

export function open(path = databasePath()): DatabaseSync {
  const db = new DatabaseSync(path);
  db.exec(`
    CREATE TABLE IF NOT EXISTS events (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      at INTEGER NOT NULL,
      kind TEXT NOT NULL CHECK (kind IN ('penalty', 'run', 'cycle')),
      km REAL NOT NULL DEFAULT 0
    )
  `);
  db.exec("CREATE INDEX IF NOT EXISTS events_at ON events (at)");
  return db;
}

/** Write the seed in, once, so a fresh database opens onto the real history. */
export function seed(db: DatabaseSync): number {
  const already = db.prepare("SELECT COUNT(*) AS n FROM events").get() as {
    n: number;
  };
  if (already.n > 0) return 0;
  for (const [month, day, kind, km] of SEED) {
    add(db, kind, km, instantOf(SEED_YEAR, month, day, 12));
  }
  return SEED.length;
}

export function add(
  db: DatabaseSync,
  kind: EventKind,
  km: number,
  at: number,
): DebtEvent {
  const inserted = db
    .prepare("INSERT INTO events (at, kind, km) VALUES (?, ?, ?)")
    .run(at, kind, km);
  return { id: Number(inserted.lastInsertRowid), at, kind, km };
}

export function remove(db: DatabaseSync, id: number): boolean {
  return db.prepare("DELETE FROM events WHERE id = ?").run(id).changes > 0;
}

export function events(db: DatabaseSync): DebtEvent[] {
  return db
    .prepare("SELECT id, at, kind, km FROM events ORDER BY at, id")
    .all() as unknown as DebtEvent[];
}
