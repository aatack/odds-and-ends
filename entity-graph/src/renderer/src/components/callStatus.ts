import type { CallOutcome } from '../state/types'
import type { BadgeColor } from './ui/Badge'

// How a call's outcome reads, in one place: the activity log says it in a row,
// and a `[@tool:callId]` field says it in the middle of a sentence. Two things
// showing the same four states, so the word and the colour are shared rather than
// written out twice and allowed to drift.

export const CALL_STATUS: Record<CallOutcome['kind'], { label: string; color: BadgeColor }> = {
  running: { label: 'Running', color: 'brand' },
  success: { label: 'Done', color: 'success' },
  error: { label: 'Failed', color: 'error' },
  cancelled: { label: 'Cancelled', color: 'gray' },
}
