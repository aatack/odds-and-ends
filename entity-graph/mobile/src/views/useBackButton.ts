import { useEffect } from 'react'
import * as A from '../state/actions'
import { useSheet, useView } from '../state/hooks'
import { getView } from '../state/store'
import { closeSheet, sheetAtom } from '../state/ui'

// The system back gesture, which on a phone is what Escape is on a keyboard.
//
// The desktop app gives Escape a pecking order without anything spelling one out —
// a pending call, then an edit, then a find field — because each step is a tool that
// isn't enabled unless there is something for it to do. The same order is written by
// hand here, since a back gesture arrives at the window rather than at the tool
// router, and it has one more layer under it than Escape does: leaving the app.
//
// The mechanism is a guard entry in the history. Whenever there is something to
// dismiss, an extra entry sits on top of the stack; a back gesture pops it, we
// dismiss one layer, and put another guard back if anything remains. When nothing
// does, no guard is pushed and the next back leaves the app — which is what a back
// gesture at the top of an app should do.

const GUARD = { egGuard: true }

const hasGuard = (): boolean => (history.state as { egGuard?: boolean } | null)?.egGuard === true

/** Whether anything on screen would answer a back gesture. */
function dismissible(): boolean {
  const view = getView()
  return sheetAtom.get() != null || view.edit != null || view.find != null || view.stack.length > 1
}

/** Dismiss the topmost layer. Returns false when there was nothing to dismiss. */
function dismissTop(): boolean {
  if (sheetAtom.get() != null) {
    closeSheet()
    return true
  }
  const view = getView()
  if (view.edit != null) {
    // Back abandons rather than commits: a gesture that means "get me out of here"
    // shouldn't write anything.
    A.setEdit(null)
    return true
  }
  if (view.find != null) {
    A.setFind(null)
    return true
  }
  if (view.stack.length > 1) {
    A.popLevel()
    return true
  }
  return false
}

export function useBackButton(): void {
  const view = useView()
  const sheet = useSheet()

  useEffect(() => {
    const onPop = (): void => {
      if (dismissTop() && dismissible()) history.pushState(GUARD, '')
    }
    window.addEventListener('popstate', onPop)
    return () => window.removeEventListener('popstate', onPop)
  }, [])

  useEffect(() => {
    if (dismissible() && !hasGuard()) history.pushState(GUARD, '')
  }, [view, sheet])
}
