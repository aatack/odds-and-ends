import { createContext } from 'react'

// The dialogs that are open, innermost last. Dropdown and Modal register here
// as they open, and Escape closes the innermost — what entity-graph's key router
// does for them there.

const open: (() => void)[] = []

window.addEventListener('keydown', (e) => {
  if (e.key !== 'Escape' || !open.length) return
  e.preventDefault()
  e.stopPropagation()
  open[open.length - 1]()
}, true)

/** Records a dialog as open; answers how to record it closed. */
export function openModal(close: () => void): () => void {
  open.push(close)
  return () => {
    const i = open.indexOf(close)
    if (i >= 0) open.splice(i, 1)
  }
}

/**
 * Whether a dialog is up: views that take keys from the window leave them to it meanwhile. A view inside a dialog
 * passes that dialog's close (`DialogContext`): then only one opened over it counts.
 */
export const modalOpen = (mine?: (() => void) | null): boolean => open.length > 0 && open[open.length - 1] !== mine

/** The dialog a view is drawn in, as it registered (`openModal`); null outside any. */
export const DialogContext = createContext<(() => void) | null>(null)
