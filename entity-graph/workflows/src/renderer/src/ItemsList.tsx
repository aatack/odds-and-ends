import fuzzysort from 'fuzzysort'
import { useAtomValue } from 'jotai'
import { Search } from 'lucide-react'
import { useEffect, useMemo, useRef, useState } from 'react'
import type { Item } from '../../shared/types'
import { minutes } from './Chat'
import { useEventMenu } from './EventMenu'
import { GROUPS } from './groups'
import { ItemIcon } from './icons'
import { searchFocusAtom } from './views'

/** What an item is waiting on, in a few words. */
function status(i: Item): string {
  if (i.closed) return i.how === 'handled_outside' ? 'done outside' : (i.how ?? 'closed')
  if (i.withHuman) return 'needs you'
  const b = i.blockers.find((b) => b.type === 'bot')
  if (b) return i.worker !== null ? 'a bot is working' : 'waiting for a bot'
  return i.blockers.map((b) => b.summary).join(', ')
}

/**
 * Every item, grouped, under a search line like the chat input: typing filters
 * the titles fuzzily, the arrow keys move through what is left, Enter opens one,
 * and Tab makes a new item with what was typed as its title.
 */
export function ItemsList(props: { items: Item[]; onOpen: (id: string) => void; onCreate: (title: string) => void }) {
  const { items, onOpen, onCreate } = props
  const [text, setText] = useState('')
  const [at, setAt] = useState(0)
  const box = useRef<HTMLInputElement>(null)
  const menu = useEventMenu()
  const focusAsked = useAtomValue(searchFocusAtom)
  useEffect(() => box.current?.focus(), [focusAsked])

  // The groups, each filtered (best match first) when something is typed; and the rows in order, for the arrow keys.
  const groups = useMemo(() => {
    const matched = text.trim()
      ? new Map(fuzzysort.go(text, items, { key: 'title', threshold: 0.2 }).map((r, i) => [r.obj.id, i]))
      : null
    return GROUPS.map((g) => ({
      ...g,
      items: items
        .filter((i) => g.has(i) && (!matched || matched.has(i.id)))
        .sort((a, b) => (matched ? matched.get(a.id)! - matched.get(b.id)! : 0)),
    })).filter((g) => g.items.length)
  }, [items, text])
  const flat = groups.flatMap((g) => g.items)
  const current = Math.min(at, Math.max(flat.length - 1, 0))
  // The highlighted row stays in sight as the arrows move it.
  const list = useRef<HTMLDivElement>(null)
  useEffect(() => {
    list.current?.querySelector('.row.selected')?.scrollIntoView({ block: 'nearest' })
  }, [current, text])
  const now = Date.now()

  return (
    <div className="items-view" ref={list}>
      <div
        className="builder-row compact search"
        onMouseDown={(e) => {
          if ((e.target as HTMLElement).closest('input')) return
          e.preventDefault()
          box.current?.focus()
        }}
      >
        <span className="builder-icon" title="Search">
          <span className="type-icon">
            <Search className="icon" />
          </span>
        </span>
        <div className="builder-fields hinted">
          <input
            ref={box}
            value={text}
            aria-label="Search items"
            onChange={(e) => {
              setText(e.target.value)
              setAt(0)
            }}
            onKeyDown={(e) => {
              if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
                e.preventDefault()
                setAt(Math.max(0, Math.min(flat.length - 1, current + (e.key === 'ArrowDown' ? 1 : -1))))
              }
              if (e.key === 'Enter' && flat[current]) {
                e.preventDefault()
                onOpen(flat[current].id)
              }
              if (e.key === 'Tab') {
                e.preventDefault()
                if (text.trim()) onCreate(text.trim())
              }
              if (e.key === 'Escape') setText('')
            }}
          />
          {!text && <span className="hint">Search</span>}
        </div>
        {/* On touch there is no Enter/Tab: the pills do the same when tapped. */}
        <span className="keys">
          <span
            className="key-pill"
            role="button"
            aria-label="Open the highlighted item"
            onMouseDown={(e) => e.preventDefault()}
            onClick={() => flat[current] && onOpen(flat[current].id)}
          >
            <kbd>Enter</kbd> Open
          </span>
          {text.trim() && (
            <span
              className="key-pill"
              role="button"
              aria-label="Create an item with the typed title"
              onMouseDown={(e) => e.preventDefault()}
              onClick={() => onCreate(text.trim())}
            >
              <kbd>Tab</kbd> New
            </span>
          )}
        </span>
      </div>

      {groups.map((g) => (
        <section key={g.label} className="list items-group">
          <h3>
            {g.label} <span className="count">{g.items.length}</span>
          </h3>
          {g.items.map((i) => (
            <div
              key={i.id}
              className={`row ${flat[current]?.id === i.id ? 'selected' : ''}`}
              onClick={() => onOpen(i.id)}
              onMouseMove={() => setAt(flat.indexOf(i))}
              {...menu(i.id)}
            >
              <ItemIcon item={i} />
              <span className="row-title">{i.title}</span>
              <span className="muted row-status">{status(i)}</span>
              <span className="muted row-age">{minutes(Math.max(0, now - i.since))}</span>
            </div>
          ))}
        </section>
      ))}
      {!flat.length && <p className="muted">{text.trim() ? 'Nothing matches. Tab makes it a new item.' : 'No items yet.'}</p>}
    </div>
  )
}
