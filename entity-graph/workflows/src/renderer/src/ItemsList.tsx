import fuzzysort from 'fuzzysort'
import { useAtomValue } from 'jotai'
import { Bot, Check, Plus, Search, User } from 'lucide-react'
import { useEffect, useMemo, useRef, useState } from 'react'
import type { Item } from '../../shared/types'
import { minutes } from './Chat'
import { useItemMenu } from './ItemMenu'
import { ItemPicker } from './ItemPicker'
import { ItemIcon, waitingOn } from './icons'
import { FINDER_SECTIONS, SECTIONS, type Recent, type SectionId } from './sections'
import { api } from './useWorkflows'
import { Marked } from './components/ui/Marked'
import { searchFocusAtom } from './views'

/**
 * What a row says beside its icon: only what the icon can't. What a script
 * waits for, the approval asked for; for a bot, a child task, or your turn
 * alone, nothing.
 */
function detail(i: Item): string {
  if (i.closed) return ''
  if (i.withYou) return i.blockers.find((b) => b.type === 'approval' && b.params.reason)?.summary ?? ''
  const b = waitingOn(i)
  if (b?.type === 'script' && b.params.description) return b.summary
  return ''
}

/** A line of the finder: an item in a section, a bot, or the "n more" a section ends with when it has more than it shows. */
/** A bot or person a finder offers: `status` is what its row says (by default, Assign). */
type Someone = { id: string; name: string; bot?: boolean; status?: string }

type Line = { key: string; item: Item; chosen?: boolean } | { key: string; bot: Someone; chosen?: boolean } | { key: string; more: string; n: number }

type Act = (fn: () => Promise<unknown>) => Promise<void>
/** The sections that show a link from whoever the finder is for, and so can be added to (its + opens the picker). */
const ADDABLE: Partial<Record<SectionId, { verb: string; title: string }>> = {
  assigned: { verb: 'Assign', title: 'Assign something' },
  owned: { verb: 'Own', title: 'Own something' },
  pinned: { verb: 'Pin', title: 'Pin something' },
}

/**
 * The finder, one view wherever items are found: home, an item's Assignments, and every item picker (`ItemPicker`,
 * this in a modal). Its sections (`FINDER_SECTIONS`, see `SECTIONS`) are the same everywhere: what is assigned to
 * whoever it is for (`recent.who`, else you), what they own, what is pinned to them, what was assigned to them lately,
 * then, while something is typed, everything else that matches. Each shows `perSection` rows, then "n more", which
 * shows the rest; an item shows in the first section that has it. Typing filters the titles fuzzily; the arrow keys
 * move through what is left, Enter opens one (or shows a section's rest), and Tab makes a new item with what was typed
 * as its title (`onTab`; a picker has none). A picker picks rather than opens: `verb` says which. With `selected`,
 * what is selected comes first, ticked (a multiselect: picking a row toggles it). Given `act`, the link sections show
 * even when empty, each with a + that picks something to link. Escape with nothing typed calls `onEscape`.
 */
export function ItemsList(props: {
  items: Item[]
  recent: Recent
  /** Its sections, in order; left out, every finder's. Empty: the bots and people alone (a picker of people). */
  sections?: SectionId[]
  perSection?: number
  /** Enter, or a click: the row chosen. */
  onOpen: (id: string) => void
  /** Tab: with what is typed (perhaps nothing). Left out, Tab does nothing: a finder for looking things up, not starting them. */
  onTab?: (text: string) => void
  verb?: string
  /** Bots and people to offer too, first, in a section of their own (`@` in the chat input). */
  bots?: Someone[]
  /** Given, the bots' section is called this and comes after the others (the Tasks view's Bots). */
  botsLabel?: string
  /** For a multiselect: the ids selected, shown first. */
  selected?: string[]
  /** Escape with nothing typed. */
  onEscape?: () => void
  /** Given, the link sections have a + (assign, own, pin something), done through this. */
  act?: Act
  /** On a phone: buttons rather than keys (a New button for Tab), rows of two lines, and the search line focused only when asked (it raises the keyboard). */
  mobile?: boolean
}) {
  const { items, recent: given, sections = FINDER_SECTIONS, perSection = 12, onOpen, onTab, verb = 'Open', bots = [], botsLabel, selected, onEscape, act, mobile = false } = props
  const [text, setText] = useState('')
  const [at, setAt] = useState(0)
  const [expanded, setExpanded] = useState<string[]>([])
  // What is pinned to whoever the finder is for, and what was assigned to them lately: read from the store (the latter
  // from its history), again as anything changes.
  const [extra, setExtra] = useState<{ pinned: Item[]; unassigned: Item[] }>({ pinned: [], unassigned: [] })
  const who = given.who?.length ? given.who : [given.me]
  const whoKey = who.join(',')
  const wantsExtra = sections.includes('pinned') || sections.includes('unassigned')
  useEffect(() => {
    if (!wantsExtra) return setExtra({ pinned: [], unassigned: [] })
    let live = true
    void api()
      .finder(whoKey.split(','))
      .then((got) => live && setExtra(got), () => live && setExtra({ pinned: [], unassigned: [] }))
    return () => {
      live = false
    }
  }, [whoKey, wantsExtra, items])
  const recent = useMemo(() => ({ ...given, ...extra }), [given, extra])
  // The + of a link section: which link is being made, once something is picked.
  const [adding, setAdding] = useState<SectionId | null>(null)
  const add = (id: string) => {
    const kind = adding
    setAdding(null)
    if (!act || !kind) return
    void act(async () => {
      if (kind === 'assigned') for (const w of who) await api().assign(id, w, true)
      if (kind === 'owned') await api().setOwner(id, who[0])
      if (kind === 'pinned') for (const w of who) await api().pin(w, [id], true)
    })
  }
  const box = useRef<HTMLInputElement>(null)
  const menu = useItemMenu()
  const focusAsked = useAtomValue(searchFocusAtom)
  useEffect(() => {
    if (!mobile || focusAsked) box.current?.focus()
  }, [focusAsked, mobile])

  // The sections, each filtered (best match first) when something is typed, and cut to `perSection` unless expanded.
  const groups = useMemo(() => {
    // What is typed is matched against every item the finder has, and those only the store gave it.
    const known = [...items, ...[...extra.pinned, ...extra.unassigned].filter((u) => !items.some((i) => i.id === u.id))]
    const matched = text.trim() ? new Map(fuzzysort.go(text, known, { key: 'title', threshold: 0.2 }).map((r, i) => [r.obj.id, i])) : null
    const chosen = new Set(selected ?? [])
    const seen = new Set<string>(chosen)
    const chosenLines: Line[] = [
      ...bots.filter((b) => chosen.has(b.id)).map((bot) => ({ key: `chosen:${bot.id}`, bot, chosen: true })),
      ...known.filter((i) => chosen.has(i.id)).map((item) => ({ key: `chosen:${item.id}`, item, chosen: true })),
    ]
    const chosenGroup = chosenLines.length ? [{ id: 'selected', label: 'Selected', count: chosenLines.length, lines: chosenLines, add: false }] : []
    const others = bots.filter((b) => !chosen.has(b.id))
    const botLines: Line[] = (text.trim() ? fuzzysort.go(text, others, { key: 'name', threshold: 0.2 }).map((r) => r.obj) : others).map((bot) => ({
      key: `bot:${bot.id}`,
      bot,
    }))
    const botGroup = botLines.length
      ? [{ id: 'bots', label: botsLabel ?? (selected ? 'Others' : 'People and bots'), count: botLines.length, lines: botLines, add: false }]
      : []
    const itemGroups = sections
      .map((id) => {
        // Everything else is only for what is typed; each item shows in the first section that has it.
        let all = id === 'everything' && !matched ? [] : SECTIONS[id].items(items, recent)
        all = all.filter((i) => !chosen.has(i.id) && !seen.has(i.id))
        if (matched) all = all.filter((i) => matched.has(i.id)).sort((a, b) => matched.get(a.id)! - matched.get(b.id)!)
        all.forEach((i) => seen.add(i.id))
        const shown = expanded.includes(id) ? all : all.slice(0, perSection)
        const lines: Line[] = shown.map((item) => ({ key: `${id}:${item.id}`, item }))
        if (shown.length < all.length) lines.push({ key: `${id}:more`, more: id, n: all.length - shown.length })
        return { id, label: SECTIONS[id].label, count: all.length, lines, add: !!act && !!ADDABLE[id] }
      })
      // A link section that can be added to shows even when empty, but not while something is typed.
      .filter((g) => g.count || (g.add && !matched))
    return botsLabel ? [...chosenGroup, ...itemGroups, ...botGroup] : [...chosenGroup, ...botGroup, ...itemGroups]
  }, [items, recent, extra, sections, perSection, expanded, text, bots, botsLabel, selected, act])
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
    <div className={`items-view ${mobile ? 'mobile' : ''}`} ref={list}>
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
                onTab?.(text.trim())
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
        {mobile ? (
          onTab && (
            <button data-ui className="search-new" onClick={() => onTab(text.trim())}>
              New
            </button>
          )
        ) : (
          <span className="keys">
            <span className="key-pill">
              <kbd>Enter</kbd> {verb}
            </span>
            {onTab && (
              <span className="key-pill">
                <kbd>Tab</kbd> New
              </span>
            )}
          </span>
        )}
      </div>

      {groups.map((g) => (
        <section key={g.id} className="list items-group">
          <h3>
            {g.label} <span className="count">{g.count}</span>
            {g.add && (
              <button data-ui className="side-add" title={ADDABLE[g.id as SectionId]!.title} aria-label={ADDABLE[g.id as SectionId]!.title} onClick={() => setAdding(g.id as SectionId)}>
                <Plus className="icon" />
              </button>
            )}
          </h3>
          {g.lines.map((line) => {
            const selected = flat[current]?.key === line.key ? 'selected' : ''
            const hover = () => setAt(flat.indexOf(line))
            if ('bot' in line)
              return (
                <div key={line.key} className={`row ${selected}`} onClick={() => choose(line)} onMouseMove={hover}>
                  {line.chosen ? <Check className="icon" /> : line.bot.bot === false ? <User className="icon" /> : <Bot className="icon" />}
                  <span className="row-title">@<Marked text={line.bot.name} phrase={text} /></span>
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
                <span className="row-title"><Marked text={i.title} phrase={text} /></span>
                <span className="muted row-status">{detail(i)}</span>
                <span className="muted row-age">{minutes(Math.max(0, now - i.since))}</span>
              </div>
            )
          })}
        </section>
      ))}
      {!flat.length && !groups.length && <p className="muted">{text.trim() ? 'Nothing matches.' : 'No tasks.'}</p>}
      {adding && (
        <ItemPicker
          title={ADDABLE[adding]!.title}
          verb={ADDABLE[adding]!.verb}
          items={items.filter((i) => !SECTIONS[adding].items(items, recent).some((h) => h.id === i.id) && !who.includes(i.id))}
          recent={given}
          onPick={add}
          onClose={() => setAdding(null)}
        />
      )}
    </div>
  )
}
