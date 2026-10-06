import { DatabaseSync } from 'node:sqlite'
import { existsSync } from 'node:fs'
import { link, values, type AppEvent } from './graph/events.ts'
import type { EventStore, Settings } from './store.ts'

const imported = 'legacy.imported'

/**
 * Brings what I owned in the first database (`detail-views.sqlite`: rows with
 * no expiry, and settings) into the owned event log, once. The old file is
 * opened read-only and left exactly as it was, so going back to it is a
 * matter of checking out the old code. What was cached there is not brought
 * over: it loads again on its own.
 */
export function importLegacy(path: string, owned: EventStore, settings: Settings): void {
  if (settings.get(imported) || !existsSync(path)) return
  const db = new DatabaseSync(path, { readOnly: true })
  try {
    const events: AppEvent[] = []
    const entities = db
      .prepare(`SELECT id, type, data, created_at, updated_at FROM entities WHERE expires_at IS NULL AND type NOT LIKE '%.home'`)
      .all() as { id: string; type: string; data: string; created_at: number; updated_at: number }[]
    for (const row of entities) {
      events.push(...values(row.id, { type: row.type }, row.created_at, 'me'))
      events.push(...values(row.id, JSON.parse(row.data) as Record<string, unknown>, row.updated_at, 'me'))
    }
    const links = db
      .prepare('SELECT parent, child, created_at FROM links WHERE expires_at IS NULL ORDER BY rank, created_at')
      .all() as { parent: string; child: string; created_at: number }[]
    for (const row of links) events.push(link(row.parent, row.child, row.created_at, 'me'))
    owned.transaction(() => {
      owned.write(events)
      for (const row of db.prepare('SELECT key, value FROM settings').all() as { key: string; value: string }[]) {
        if (settings.get(row.key) === null) settings.set(row.key, row.value)
      }
      settings.set(imported, path)
    })
  } finally {
    db.close()
  }
}
