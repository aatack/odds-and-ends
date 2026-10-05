import { useSyncExternalStore } from 'react'
import type { Session, Snapshot } from './session.ts'

/** The one hook: the session as plain data. */
export function useSnapshot(session: Session): Snapshot {
  return useSyncExternalStore(session.subscribe, session.get)
}
