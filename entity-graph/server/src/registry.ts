import {
  CombinedSource,
  FilterSource,
  FrozenSource,
  RemoteSource,
  SqliteSource,
  type Source,
} from '../../src/core/source/index'
import type { ConfigDb, SourceRow } from './config'

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
        return new SqliteSource(row.id, row.label, cfg.path, cfg.defaultAuthor)
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
      case 'remote': {
        const remote = new RemoteSource(row.id, row.label, cfg.url, cfg.token)
        // Populate the tool cache; tolerate an unreachable remote at build time.
        await remote.refresh().catch(() => undefined)
        return remote
      }
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
