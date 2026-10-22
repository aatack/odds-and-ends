import type { CallOutcome } from '../state/types'
import type { BadgeColor } from './ui/Badge'

// How a call's outcome reads: the word and the colour the activity log gives
// each of the four states.

export const CALL_STATUS: Record<CallOutcome['kind'], { label: string; color: BadgeColor }> = {
  running: { label: 'Running', color: 'brand' },
  success: { label: 'Done', color: 'success' },
  error: { label: 'Failed', color: 'error' },
  cancelled: { label: 'Cancelled', color: 'gray' },
}
