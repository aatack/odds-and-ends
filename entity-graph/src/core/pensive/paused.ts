import { PausedError, type Pensive, type ResourceRecord, type ToolMeta } from './types'

/**
 * What a switched-off node is, from the outside.
 *
 * Pausing is a property of the node rather than of the store behind it, so it is
 * a pensive of its own: whatever is downstream gets one of these in place of the
 * real thing, and every call it makes fails the same way, with a sentence saying
 * who is paused. A combiner one of whose inputs is paused is therefore broken
 * exactly as far as that input, and the answer for all of it is to press play.
 */
export class PausedPensive implements Pensive {
  constructor(
    readonly id: string,
    readonly label: string,
  ) {}

  private refuse(): never {
    throw new PausedError(this.label)
  }

  async readEvents(): Promise<never> {
    return this.refuse()
  }

  async writeEvents(): Promise<never> {
    return this.refuse()
  }

  async popEvents(): Promise<never> {
    return this.refuse()
  }

  async readResource(): Promise<ResourceRecord | null> {
    return this.refuse()
  }

  async writeResource(): Promise<never> {
    return this.refuse()
  }

  /** Empty rather than a refusal: "nothing is callable" is the honest answer. */
  async listTools(): Promise<ToolMeta[]> {
    return []
  }

  async callTool(): Promise<never> {
    return this.refuse()
  }
}
