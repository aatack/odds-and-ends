import type { Blocker, Topic, TopicId } from '@conswap/common/types'
import type { Context } from '../context.js'

/** Something new turning up underneath a blocked topic. */
export interface ActivitySignal {
  /** The topic that was just added. */
  topic: Topic
  /** The blocked topic's descendant chain down to the new topic. */
  path: TopicId[]
  at: string
}

export interface CheckOutcome {
  satisfied: boolean
  /** Seconds to wait before checking again. */
  retryIn?: number
  error?: string
  /** Appended to the topic as a system event when the blocker clears. */
  note?: string
}

export interface BlockerDefinition {
  type: string
  /** Written into the blocker when it is made, so the UI never has to guess. */
  describe(context: Context, config: Record<string, unknown>): string
  /** Cleared the moment matching activity arrives, with no polling at all. */
  wakesOn?(context: Context, blocker: Blocker, signal: ActivitySignal): boolean
  /** Polled on the scheduler. Absent means the blocker only wakes on activity. */
  check?(context: Context, blocker: Blocker): Promise<CheckOutcome>
  /** Default seconds between checks. */
  interval?: number
}
