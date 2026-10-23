import { useAtom, useAtomValue } from 'jotai'
import { Search, X } from 'lucide-react'
import { useEffect, useRef } from 'react'
import { findAtom, findFocusAtom } from './views'

/**
 * Ctrl+F's line, over the view it finds in, while it is open: what is typed
 * filters the view to what shows it. Escape, or the ×, closes it.
 */
export function FindLine() {
  const [find, setFind] = useAtom(findAtom)
  const focusAsked = useAtomValue(findFocusAtom)
  const box = useRef<HTMLInputElement>(null)
  useEffect(() => box.current?.focus(), [focusAsked])
  if (find === null) return null
  return (
    <div className="builder-row compact search find-line">
      <span className="builder-icon" title="Find">
        <span className="type-icon">
          <Search className="icon" />
        </span>
      </span>
      <div className="builder-fields hinted">
        <input
          ref={box}
          autoFocus
          value={find}
          aria-label="Find"
          onChange={(e) => setFind(e.target.value)}
          onKeyDown={(e) => {
            if (e.key === 'Escape') {
              e.preventDefault()
              setFind(null)
            }
          }}
        />
        {!find && <span className="hint">Find</span>}
      </div>
      <span className="builder-icon right" title="Close (Escape)" role="button" aria-label="Close" onClick={() => setFind(null)}>
        <span className="type-icon">
          <X className="icon" />
        </span>
      </span>
    </div>
  )
}
