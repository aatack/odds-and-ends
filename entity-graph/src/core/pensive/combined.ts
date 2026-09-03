import type { AppEvent } from '../events'
import { BasePensive } from './base'
import { NotSupportedError, type Pensive, type ResourceRecord } from './types'

/**
 * Several pensives read as one, and written to one of them.
 *
 * Reading is the union, in the order the children were given: a rollup sorts by
 * timestamp anyway, so two stores holding parts of the same note read as that
 * note. Writing is not a union — there is nowhere for "both" to mean anything —
 * so one child is the **write source** and every edit lands there, whichever
 * store the note being edited came from. That is the setting worth changing
 * often: the same outline, with today's notes going somewhere else.
 *
 * Undo pops from the write source alone, which is the only honest answer: it is
 * where the edits went.
 */
export class CombinedPensive extends BasePensive {
  constructor(
    readonly id: string,
    readonly label: string,
    private children: Pensive[],
    /** Which child edits land in. Null when the user hasn't said. */
    private writeTo: Pensive | null,
    defaultAuthor?: string,
  ) {
    super()
    this.defaultAuthor = defaultAuthor
  }

  /** The write source, or the reason there isn't one. */
  private target(): Pensive {
    if (!this.writeTo) {
      throw new NotSupportedError(
        `"${this.label}" has no write source — pick which of its inputs edits should go to`,
      )
    }
    return this.writeTo
  }

  async readEvents(entityIds?: string[]): Promise<AppEvent[]> {
    const perChild = await Promise.all(this.children.map((c) => c.readEvents(entityIds)))
    return perChild.flat()
  }

  writeEvents(events: AppEvent[]): Promise<void> {
    return this.target().writeEvents(events)
  }

  popEvents(author?: string): Promise<AppEvent[]> {
    return this.target().popEvents(author)
  }

  /**
   * The first child that has the bytes. A resource is stored under the id of the
   * entity describing it, and that entity lives in exactly one child, so there
   * is no ambiguity to resolve — only stores to ask in turn.
   */
  async readResource(id: string): Promise<ResourceRecord | null> {
    for (const child of this.children) {
      const found = await child.readResource(id).catch(() => null)
      if (found) return found
    }
    return null
  }

  writeResource(resource: ResourceRecord): Promise<void> {
    return this.target().writeResource(resource)
  }
}
