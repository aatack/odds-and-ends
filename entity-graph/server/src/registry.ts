import { fileURLToPath } from 'url'
import { resolve } from 'path'
import {
  CombinedSource,
  FilterSource,
  FrozenSource,
  RemoteSource,
  SqliteSource,
  type Source,
} from '../../src/core/source/index'
import type { ConfigDb, SourceRow } from './config'

/**
 * Base directory for sqlite source databases (`server/databases`). A source's
 * configured `path` is resolved against this, so relative paths live here while
 * an absolute path still overrides it. Anchored to the package, not the cwd.
 */
const DATABASES_DIR = fileURLToPath(new URL('../databases', import.meta.url))

export class SourceNotFoundError extends Error {
  constructor(public id: string) {
    super(`No source with id "${id}"`)
    this.name = 'SourceNotFoundError'
  }
}

/**
 * Builds live `Source` objects from config rows, resolving child references
 * recursively. Results are cached; CRUD writes call `invalidate()` to rebuild.
 */
export class Registry {
  private cache = new Map<string, Source>()
  private building = new Set<string>()

  constructor(private db: ConfigDb) {}

  async get(id: string): Promise<Source> {
    const cached = this.cache.get(id)
    if (cached) return cached
    if (this.building.has(id)) throw new Error(`cycle detected involving source "${id}"`)
    const row = this.db.getSource(id)
    if (!row) throw new SourceNotFoundError(id)

    this.building.add(id)
    try {
      const src = await this.build(row)
      // (Re)load any async tools: user-defined tools (Sqlite) or a remote
      // registry (Remote). Tolerate failure so a broken source still builds.
      await src.refresh?.().catch(() => undefined)
      this.cache.set(id, src)
      return src
    } finally {
      this.building.delete(id)
    }
  }

  private async build(row: SourceRow): Promise<Source> {
    const cfg = row.config
    switch (cfg.type) {
      case 'sqlite':
        return new SqliteSource(
          row.id,
          row.label,
          resolve(DATABASES_DIR, cfg.path),
          cfg.defaultAuthor,
        )
      case 'combined': {
        const children = await Promise.all(cfg.children.map((c) => this.get(c)))
        return new CombinedSource(row.id, row.label, children)
      }
      case 'frozen':
        return new FrozenSource(row.id, row.label, await this.get(cfg.child), cfg.beforeTs)
      case 'filter':
        return new FilterSource(row.id, row.label, await this.get(cfg.child), {
          allow: cfg.allow,
          deny: cfg.deny,
          maxSafety: cfg.maxSafety,
        })
      case 'remote':
        // `get()` calls `refresh()` after build to populate the tool cache.
        return new RemoteSource(row.id, row.label, cfg.url, cfg.token)
    }
  }

  /** Drop cached sources (and close any SQLite handles) so they rebuild lazily. */
  invalidate(): void {
    for (const src of this.cache.values()) {
      const closable = src as { close?: () => void }
      if (typeof closable.close === 'function') closable.close()
    }
    this.cache.clear()
  }
}
