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
