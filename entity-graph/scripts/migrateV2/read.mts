// Getting the old store off disk without touching it.
//
// The connection is opened `readonly` and `fileMustExist`, so SQLite refuses a
// write before this script has the chance to attempt one, and a mistyped path
// fails rather than quietly conjuring an empty database to migrate. Everything
// is read in one go: a v2 store is one person's notes, small enough to hold, and
// holding it is what lets the rest of this be pure functions over rows.

import Database from 'better-sqlite3'
import type { Json, V2Row } from './v2Reducer.mjs'

/** One row of the v2 `resources` table. */
export interface V2Resource {
  timestamp: number
  uuid: string
  bytes: Buffer
}

export interface V2Store {
  rows: V2Row[]
  /** One per uuid — the newest, which is the one v2 would have shown. */
  resources: V2Resource[]
  /** Rows whose JSON wouldn't parse; v2 would have thrown on these. */
  unreadable: { timestamp: number; uuid: string; key: string; value: string }[]
}

interface EntityRow {
  timestamp: number
  uuid: string
  key: string
  value: string
}

interface ResourceRow {
  timestamp: number
  uuid: string
  data: Buffer
}

const hasTable = (db: Database.Database, name: string): boolean =>
  db.prepare<[string], { n: number }>(`SELECT COUNT(*) AS n FROM sqlite_master WHERE name = ?`).get(name)!.n > 0

export function readV2(path: string): V2Store {
  const db = new Database(path, { readonly: true, fileMustExist: true })
  try {
    const rows: V2Row[] = []
    const unreadable: V2Store['unreadable'] = []

    // By rowid, which is write order: the sort v2 actually read in is applied
    // afterwards and is stable, so this is what settles rows it considers equal.
    const entityRows = hasTable(db, 'entities')
      ? db
          .prepare<[], EntityRow>(`SELECT timestamp, uuid, key, value FROM entities ORDER BY rowid`)
          .all()
      : []

    for (const row of entityRows) {
      let value: Json
      try {
        value = JSON.parse(row.value)
      } catch {
        unreadable.push(row)
        continue
      }
      rows.push({ timestamp: row.timestamp, uuid: row.uuid, key: row.key, value })
    }

    // v2 read every version of a resource and let the last one win, so only the
    // newest is worth carrying. Ties go to whichever was written last, since
    // that is the one an entity would have been showing.
    const newest = new Map<string, V2Resource>()
    const resourceRows = hasTable(db, 'resources')
      ? db
          .prepare<[], ResourceRow>(`SELECT timestamp, uuid, data FROM resources ORDER BY rowid`)
          .all()
      : []

    for (const row of resourceRows) {
      const held = newest.get(row.uuid)
      if (!held || row.timestamp >= held.timestamp) {
        newest.set(row.uuid, { timestamp: row.timestamp, uuid: row.uuid, bytes: row.data })
      }
    }

    return { rows, resources: [...newest.values()], unreadable }
  } finally {
    db.close()
  }
}
