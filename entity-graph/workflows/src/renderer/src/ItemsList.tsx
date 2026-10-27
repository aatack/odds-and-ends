import fuzzysort from 'fuzzysort'
import { useAtomValue } from 'jotai'
import { Bot, Check, Search, User } from 'lucide-react'
import { useEffect, useMemo, useRef, useState } from 'react'
import type { Item } from '../../shared/types'
import { minutes } from './Chat'
import { useEventMenu } from './EventMenu'
import { ItemIcon, waitingOn } from './icons'
import { SECTIONS, type Recent, type SectionId } from './sections'
import { searchFocusAtom } from './views'

/**
 * What a row says beside its icon: only what the icon can't. What a script
 * waits for, the approval asked for; for a bot, a child task, or your turn
 * alone, nothing.
 */
function detail(i: Item): string {
  if (i.closed) return i.how === 'cancelled' ? 'cancelled' : ''
  if (i.withYou) return i.blockers.find((b) => b.type === 'approval' && b.params.reason)?.summary ?? ''
  const b = waitingOn(i)
  if (b?.type === 'script' && b.params.description) return b.summary
  return ''
}

/** A line of the finder: an item in a section, a bot, or the "n more" a section ends with when it has more than it shows. */
/** A bot or person a finder offers: `status` is what its row says (by default, Assign). */
type Someone = { id: string; name: string; bot?: boolean; status?: string }

type Line =
  | { key: string; item: Item; chosen?: boolean }
  | { key: string; bot: Someone; chosen?: boolean }
  | { key: string; more: SectionId; n: number }

/**
 * The finder: items in sections (`sections`, in order; see `SECTIONS`), under a
 * search line like the chat input. Each section shows `perSection` rows, then
 * "n more", which shows the rest. Typing filters the titles fuzzily (an item
 * then shows in only the first section that has it); the arrow keys move through
 * what is left, Enter opens one (or shows a section's rest), and Tab makes a new
 * item with what was typed as its title. The pickers are this too, with Enter
 * (and a click) picking instead: `verb` says which. With `selected`, what is
 * selected comes first, in a section of its own, ticked (a multiselect picker:
 * picking a row toggles it). Escape with nothing typed calls `onEscape`.
 */
export function ItemsList(props: {
  items: Item[]
  recent: Recent
  sections: SectionId[]
  perSection?: number
  onOpen: (id: string) => void
  onCreate: (title: string) => void
  verb?: string
  /** Bots and people to offer too, first, in a section of their own (`@` in the chat input). */
  bots?: Someone[]
  /** Given, the bots' section is called this and comes after the others (the Tasks view's Bots). */
  botsLabel?: string
  /** For a multiselect: the ids selected, shown first. */
  selected?: string[]
  /** Escape with nothing typed. */
  onEscape?: () => void
}) {
  const { items, recent, sections, perSection = 12, onOpen, onCreate, verb = 'Open', bots = [], botsLabel, selected, onEscape } = props
  const [text, setText] = useState('')
  const [at, setAt] = useState(0)
  const [expanded, setExpanded] = useState<SectionId[]>([])
  const box = useRef<HTMLInputElement>(null)
  const menu = useEventMenu()
  const focusAsked = useAtomValue(searchFocusAtom)
  useEffect(() => box.current?.focus(), [focusAsked])

  // The sections, each filtered (best match first) when something is typed, and cut to `perSection` unless expanded.
  const groups = useMemo(() => {
    const matched = text.trim()
      ? new Map(fuzzysort.go(text, items, { key: 'title', threshold: 0.2 }).map((r, i) => [r.obj.id, i]))
      : null
    const chosen = new Set(selected ?? [])
    const seen = new Set<string>(chosen)
    const chosenLines: Line[] = [
      ...bots.filter((b) => chosen.has(b.id)).map((bot) => ({ key: `chosen:${bot.id}`, bot, chosen: true })),
      ...items.filter((i) => chosen.has(i.id)).map((item) => ({ key: `chosen:${item.id}`, item, chosen: true })),
    ]
    const chosenGroup = chosenLines.length ? [{ id: 'selected', label: 'Selected', count: chosenLines.length, lines: chosenLines }] : []
    const others = bots.filter((b) => !chosen.has(b.id))
    const botLines: Line[] = (text.trim() ? fuzzysort.go(text, others, { key: 'name', threshold: 0.2 }).map((r) => r.obj) : others).map((bot) => ({
      key: `bot:${bot.id}`,
      bot,
    }))
    const botGroup = botLines.length
      ? [{ id: 'bots', label: botsLabel ?? (selected ? 'Others' : 'People and bots'), count: botLines.length, lines: botLines }]
      : []
    const itemGroups = sections
      .map((id) => {
        let all = SECTIONS[id].items(items, recent)
        all = all.filter((i) => !chosen.has(i.id))
        if (matched) {
          all = all.filter((i) => matched.has(i.id) && !seen.has(i.id)).sort((a, b) => matched.get(a.id)! - matched.get(b.id)!)
          all.forEach((i) => seen.add(i.id))
        }
        const shown = expanded.includes(id) ? all : all.slice(0, perSection)
        const lines: Line[] = shown.map((item) => ({ key: `${id}:${item.id}`, item }))
        if (shown.length < all.length) lines.push({ key: `${id}:more`, more: id, n: all.length - shown.length })
        return { id: id as string, label: SECTIONS[id].label, count: all.length, lines }
      })
      .filter((g) => g.count)
    return botsLabel ? [...chosenGroup, ...itemGroups, ...botGroup] : [...chosenGroup, ...botGroup, ...itemGroups]
  }, [items, recent, sections, perSection, expanded, text, bots, botsLabel, selected])
  const flat = groups.flatMap((g) => g.lines)
  const current = Math.min(at, Math.max(flat.length - 1, 0))
  const choose = (line: Line) => ('item' in line ? onOpen(line.item.id) : 'bot' in line ? onOpen(line.bot.id) : setExpanded([...expanded, line.more]))
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
            aria-label="Search tasks"
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
                choose(flat[current])
              }
              if (e.key === 'Tab') {
                e.preventDefault()
                if (text.trim()) onCreate(text.trim())
              }
              if (e.key === 'Escape') {
                if (!text && onEscape) {
                  e.preventDefault()
                  onEscape()
                } else setText('')
              }
            }}
          />
          {!text && <span className="hint">Search</span>}
        </div>
        <span className="keys">
          <span className="key-pill">
            <kbd>Enter</kbd> {verb}
          </span>
          {text.trim() && (
            <span className="key-pill">
              <kbd>Tab</kbd> New
            </span>
          )}
        </span>
      </div>

      {groups.map((g) => (
        <section key={g.id} className="list items-group">
          <h3>
            {g.label} <span className="count">{g.count}</span>
          </h3>
          {g.lines.map((line) => {
            const selected = flat[current]?.key === line.key ? 'selected' : ''
            const hover = () => setAt(flat.indexOf(line))
            if ('bot' in line)
              return (
                <div key={line.key} className={`row ${selected}`} onClick={() => choose(line)} onMouseMove={hover}>
                  {line.chosen ? <Check className="icon" /> : line.bot.bot === false ? <User className="icon" /> : <Bot className="icon" />}
                  <span className="row-title">@{line.bot.name}</span>
                  <span className="muted row-status">{line.chosen ? 'Remove' : (line.bot.status ?? 'Assign')}</span>
                </div>
              )
            if (!('item' in line))
              return (
                <div key={line.key} className={`row more ${selected}`} onClick={() => choose(line)} onMouseMove={hover}>
                  <span className="muted">…{line.n} more</span>
                </div>
              )
            const i = line.item
            return (
              <div key={line.key} className={`row ${selected}`} onClick={() => onOpen(i.id)} onMouseMove={hover} {...menu(i.id)}>
                {line.chosen ? <Check className="icon" /> : <ItemIcon item={i} />}
                <span className="row-title">{i.title}</span>
                <span className="muted row-status">{detail(i)}</span>
                <span className="muted row-age">{minutes(Math.max(0, now - i.since))}</span>
              </div>
            )
          })}
        </section>
      ))}
      {!flat.length && <p className="muted">{text.trim() ? 'Nothing matches.' : 'No tasks.'}</p>}
    </div>
  )
}
