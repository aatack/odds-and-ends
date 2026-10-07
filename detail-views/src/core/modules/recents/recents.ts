import { link } from '../../graph/events.ts'
import { typeOf } from '../../present.ts'
import type { Module, ModuleContext } from '../module.ts'
import { recentsIds as ids, recentsView } from './view.ts'

/** How many items the list keeps. */
const keep = 200

/** Recents: each view I push, of an item with a type, moved to the front of a list in the cache store. */
export class Recents implements Module {
  readonly view = recentsView

  private readonly context: ModuleContext

  constructor(context: ModuleContext) {
    this.context = context
  }

  /**
   * A view of `id` was pushed. An item with no type (a plain note) isn't
   * kept. The link is written again at the time of the visit, which (the
   * cache store keeping one row per link) takes it out and puts it back at the
   * newest end; past `keep`, the oldest go.
   */
  visit(id: string, values: Record<string, unknown>): void {
    if (id === ids.root || !typeOf(id, values)) return
    const listed = this.context.lens.children(ids.root).filter((other) => other !== id)
    // The cache keeps one row per link: dropping one is writing it removed.
    const dropped = listed.slice(0, Math.max(0, listed.length - (keep - 1)))
    this.context.cache.write([
      ...dropped.map((old) => link(ids.root, old, 0, 'app', 1)),
      link(ids.root, id, this.context.now(), 'app'),
    ])
  }
}
