import type { AppEvent } from '../events'
import { BasePensive } from './base'
import type { Pensive, ResourceRecord } from './types'

/**
 * A pensive whose writes are all recorded as one person, whatever the caller
 * says.
 *
 * This is what a bearer token means. A token is issued to somebody by name, and
 * every edit that arrives with it is theirs — the client is not asked and is not
 * believed, since a client that could name its own author could name anybody's.
 * Reads are untouched.
 *
 * The tool layer is rebuilt here rather than delegated, so a call that names no
 * author gets this one as its default and a call that names one has it
 * overwritten on the way past. Undo is the cost: an event put back by redo is
 * re-attributed, having gone out through the same door.
 */
export class AttributedPensive extends BasePensive {
  constructor(
    private inner: Pensive,
    private author: string,
  ) {
    super()
    this.defaultAuthor = author
  }

  get id(): string {
    return this.inner.id
  }

  get label(): string {
    return this.inner.label
  }

  readEvents(entityIds?: string[]): Promise<AppEvent[]> {
    return this.inner.readEvents(entityIds)
  }

  writeEvents(events: AppEvent[]): Promise<void> {
    return this.inner.writeEvents(events.map((e) => ({ ...e, author: this.author })))
  }

  popEvents(windowMs: number): Promise<AppEvent[]> {
    return this.inner.popEvents(windowMs)
  }

  readResource(id: string): Promise<ResourceRecord | null> {
    return this.inner.readResource(id)
  }

  writeResource(resource: ResourceRecord): Promise<void> {
    return this.inner.writeResource({ ...resource, author: this.author })
  }
}
