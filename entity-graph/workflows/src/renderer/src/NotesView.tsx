// A note's focus: the notes in it as a dense tree, worked from the keyboard as entity-graph's outliner is. Its
// structure is only the items in each note (`contains` links); anything else in a note is a leaf, opened with `d`.
// Every change shows at once (notes.ts) and goes to the backend after.

import fuzzysort from 'fuzzysort'
import { useAtom } from 'jotai'
import { atomWithStorage, createJSONStorage } from 'jotai/utils'
import { ChevronDown, ChevronRight, Square, SquareCheck } from 'lucide-react'
import { useEffect, useMemo, useRef, useState } from 'react'
import type { Item, ItemInfo } from '../../shared/types'
import { Markdown } from './components/ui/Markdown'
import { Modal } from './components/ui/Modal'
import { FindLine } from './FindLine'
import { GrowingTextarea } from './GrowingTextarea'
import { ItemInfoIcon } from './icons'
import { useItemMenu } from './ItemMenu'
import { clearNotesError, createNote, moveNote, resolve, setNoteValue, useNotes, type NotesTree } from './notes'
import { findAtom } from './views'

/** Where each tree was left, by its root: the row selected, and the notes folded shut. Kept across reloads. */
type Place = { path: string[]; collapsed: string[] }
const placesAtom = atomWithStorage<Record<string, Place>>('workflows.notes', {}, createJSONStorage(() => localStorage), { getOnInit: true })

type Row = { key: string; id: string; path: string[]; depth: number; info: ItemInfo; hasKids: boolean; collapsed: boolean }
type Edit = { mode: 'edit' | 'create'; path: string[]; draft: string; values?: Record<string, unknown> }
type Filters = { find: string; sectionsOnly: boolean; openOnly: boolean }

const textOf = (i: ItemInfo) => (i.type === 'note' ? String(i.params.text ?? '') : i.summary)

/**
 * The rows on screen, depth first from the root. Folded notes hide what is in them, unless something is being looked
 * for: then every row that shows it is kept, with what it sits in. Sections only keeps the headings; open only keeps
 * the unticked boxes, and walks no further into a ticked one.
 */
function rowsOf(tree: NotesTree, root: string, collapsed: Set<string>, f: Filters): Row[] {
  const phrase = f.find.trim().toLowerCase()
  const filtering = !!phrase || f.sectionsOnly || f.openOnly
  const kept = new Map<string, boolean>()
  const matches = (i: ItemInfo) =>
    (!phrase || textOf(i).toLowerCase().includes(phrase)) && (!f.sectionsOnly || i.params.section === true) && (!f.openOnly || i.params.open === true)
  const keep = (id: string): boolean => {
    if (kept.has(id)) return kept.get(id)!
    kept.set(id, false)
    const i = tree.info(id)
    const yes = !!i && !(f.openOnly && i.params.open === false) && (matches(i) || tree.kids(id).some(keep))
    kept.set(id, yes)
    return yes
  }
  const out: Row[] = []
  const walk = (id: string, path: string[], depth: number) => {
    const info = tree.info(id)
    if (!info) return
    const kids = info.type === 'note' ? tree.kids(id) : []
    const shut = collapsed.has(resolve(id))
    out.push({ key: path.map(resolve).join('/'), id: resolve(id), path, depth, info, hasKids: kids.length > 0, collapsed: shut && kids.length > 0 })
    if (shut && !filtering) return
    for (const k of kids) if (!filtering || keep(k)) walk(k, [...path, resolve(k)], depth + 1)
  }
  walk(root, [root], 0)
  return out
}

const same = (a: string[], b: string[]) => a.length === b.length && a.every((x, i) => resolve(x) === resolve(b[i]))
const isTyping = (t: EventTarget | null) => t instanceof HTMLElement && (t.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].includes(t.tagName))

export function NotesView(props: { info: ItemInfo; tasks: Item[]; onOpen: (id: string) => void; onBack: () => void; mobile?: boolean }) {
  const { info, tasks, onOpen, onBack, mobile = false } = props
  const root = info.id
  const { tree, error } = useNotes(root, info)
  const [places, setPlaces] = useAtom(placesAtom)
  const place: Place = places[root] ?? { path: [root], collapsed: [] }
  const setPlace = (change: (p: Place) => Place) => setPlaces((all) => ({ ...all, [root]: change(all[root] ?? { path: [root], collapsed: [] }) }))
  const collapsed = useMemo(() => new Set(place.collapsed.map(resolve)), [place.collapsed, tree])
  const [find, setFind] = useAtom(findAtom)
  const [sectionsOnly, setSectionsOnly] = useState(false)
  const [openOnly, setOpenOnly] = useState(false)
  const rows = useMemo(() => rowsOf(tree, root, collapsed, { find: find ?? '', sectionsOnly, openOnly }), [tree, root, collapsed, find, sectionsOnly, openOnly])
  const [edit, setEdit] = useState<Edit | null>(null)
  const editRef = useRef(edit)
  editRef.current = edit
  const [picking, setPicking] = useState<null | 'move' | 'link'>(null)

  // The row selected: the path kept, or as much of it as is still on screen.
  const at = useMemo(() => {
    for (let n = place.path.length; n > 0; n--) {
      const i = rows.findIndex((r) => same(r.path, place.path.slice(0, n)))
      if (i >= 0) return i
    }
    return 0
  }, [rows, place.path])
  const selected = rows[at]
  const select = (path: string[]) => setPlace((p) => ({ ...p, path: path.map(resolve) }))
  const setCollapsed = (id: string, shut: boolean) =>
    setPlace((p) => ({ ...p, collapsed: shut ? [...new Set([...p.collapsed, id])] : p.collapsed.filter((c) => resolve(c) !== id) }))

  const startCreate = (values?: Record<string, unknown>) => {
    if (!selected || selected.info.type !== 'note') return
    setCollapsed(selected.id, false)
    setEdit({ mode: 'create', path: selected.path, draft: '', values })
  }
  const commit = () => {
    const e = editRef.current
    if (!e) return
    setEdit(null)
    editRef.current = null
    const subject = e.path[e.path.length - 1]
    if (!e.draft.trim()) return
    if (e.mode === 'edit') {
      if (e.draft !== textOf(tree.info(subject)!)) setNoteValue(subject, 'text', e.draft)
      return
    }
    const id = createNote(subject, { text: e.draft, ...e.values })
    select([...e.path, id])
  }
  const cancel = () => {
    editRef.current = null
    setEdit(null)
  }

  const scroller = useRef<HTMLDivElement>(null)
  useEffect(() => {
    scroller.current?.querySelector('.note-row.selected')?.scrollIntoView({ block: 'nearest' })
  }, [at, rows.length])

  // The keys, as entity-graph's: only while nothing is being typed into.
  const keys = useRef<(e: KeyboardEvent) => void>(() => {})
  keys.current = (e: KeyboardEvent) => {
    if (isTyping(e.target) || picking || edit || e.altKey || e.metaKey) return
    const k = e.key
    const mod = e.ctrlKey
    const run = (fn: () => void) => {
      e.preventDefault()
      fn()
    }
    if (mod) {
      if (k === '/' && selected?.info.type === 'note') run(() => setNoteValue(selected.id, 'section', selected.info.params.section ? null : true))
      return
    }
    if (!selected) return
    const parentPath = selected.path.slice(0, -1)
    if (k === 'w' || k === 'ArrowUp') run(() => at > 0 && select(rows[at - 1].path))
    else if (k === 's' || k === 'ArrowDown') run(() => at < rows.length - 1 && select(rows[at + 1].path))
    else if (k === 'a') run(() => parentPath.length && select(parentPath))
    else if (k === 'Home') run(() => select([root]))
    else if (k === 'End') run(() => select(rows[rows.length - 1].path))
    else if (k === 'ArrowLeft') run(() => selected.hasKids && setCollapsed(selected.id, true))
    else if (k === 'ArrowRight') run(() => setCollapsed(selected.id, false))
    else if (k === 'e') run(() => selected.info.type === 'note' && setEdit({ mode: 'edit', path: selected.path, draft: textOf(selected.info) }))
    else if (k === 'Enter') run(() => startCreate())
    else if (k === '/') run(() => startCreate({ section: true }))
    else if (k === '?') run(() => startCreate({ open: true }))
    else if (k === '>') run(() => setNoteValue(selected.id, 'open', selected.info.params.open === undefined ? true : selected.info.params.open === true ? false : null))
    else if (k === 'q') run(() => setSectionsOnly((v) => !v))
    else if (k === 'Q') run(() => setOpenOnly((v) => !v))
    else if (k === 'd') run(() => selected.id !== root && onOpen(selected.id))
    else if (k === 'A') run(onBack)
    else if (k === 'x') run(() => parentPath.length && setPicking('move'))
    else if (k === 'r') run(() => selected.info.type === 'note' && setPicking('link'))
    else if ((k === 'Delete' || k === 'Backspace') && parentPath.length)
      // Out of its note, into none; the row above is selected, as in entity-graph.
      run(() => {
        moveNote(selected.id, null)
        select(rows[at - 1].path)
      })
    else if (k === 'Escape') {
      if (find !== null) run(() => setFind(null))
      else if (sectionsOnly || openOnly)
        run(() => {
          setSectionsOnly(false)
          setOpenOnly(false)
        })
    }
  }
  useEffect(() => {
    const on = (e: KeyboardEvent) => keys.current(e)
    window.addEventListener('keydown', on)
    return () => window.removeEventListener('keydown', on)
  }, [])

  // The box a new note is typed into sits at the end of what is in its note.
  const createAt = useMemo(() => {
    if (edit?.mode !== 'create') return -1
    const from = rows.findIndex((r) => same(r.path, edit.path))
    if (from < 0) return -1
    let i = from + 1
    while (i < rows.length && rows[i].depth > rows[from].depth) i++
    return i
  }, [edit, rows])

  const box = (depth: number, section: boolean) => (
    <GrowingTextarea
      autoFocus
      className={`note-input ${section ? 'note-section' : ''}`}
      style={section ? sectionStyle(depth) : undefined}
      value={edit?.draft ?? ''}
      placeholder={edit?.mode === 'create' ? (section ? 'New section…' : 'New note…') : undefined}
      onChange={(e) => setEdit((x) => x && { ...x, draft: e.target.value })}
      onFocus={(e) => e.target.setSelectionRange(e.target.value.length, e.target.value.length)}
      onBlur={commit}
      onKeyDown={(e) => {
        if (e.key === 'Enter' && !e.shiftKey) {
          e.preventDefault()
          commit()
        } else if (e.key === 'Escape') {
          e.preventDefault()
          e.stopPropagation()
          cancel()
        }
      }}
    />
  )

  const menu = useItemMenu()
  const rendered = rows.map((r, i) => {
    const editing = edit?.mode === 'edit' && same(edit.path, r.path)
    const section = r.info.params.section === true
    const open = r.info.params.open
    return (
      <div
        key={r.key}
        className={`note-row ${i === at ? 'selected' : ''} ${r.collapsed ? 'shut' : ''} ${open === false ? 'ticked' : ''}`}
        style={{ paddingLeft: r.depth * 20 + 4 }}
        onClick={() => select(r.path)}
        onDoubleClick={() => r.info.type === 'note' ? setEdit({ mode: 'edit', path: r.path, draft: textOf(r.info) }) : r.id !== root && onOpen(r.id)}
        {...menu(r.id)}
      >
        <span
          className="note-mark"
          onClick={(e) => {
            e.stopPropagation()
            if (open !== undefined && open !== null) setNoteValue(r.id, 'open', open === true ? false : true)
            else if (r.hasKids) setCollapsed(r.id, !r.collapsed)
          }}
        >
          {r.info.type !== 'note' ? (
            <ItemInfoIcon info={r.info} />
          ) : open === true ? (
            <Square className="icon" />
          ) : open === false ? (
            <SquareCheck className="icon" />
          ) : !r.hasKids ? (
            <span className="note-bullet" />
          ) : r.collapsed ? (
            <ChevronRight className="icon" />
          ) : (
            <ChevronDown className="icon" />
          )}
        </span>
        <div className="note-body">
          {editing ? (
            box(r.depth, section)
          ) : r.info.type !== 'note' ? (
            <span className="note-item">{r.info.summary}</span>
          ) : textOf(r.info) ? (
            <div className={section ? 'note-section' : undefined} style={section ? sectionStyle(r.depth) : undefined}>
              <Markdown text={textOf(r.info)} highlight={find ?? undefined} className="note-text" />
            </div>
          ) : (
            <span className="note-empty">Empty</span>
          )}
        </div>
      </div>
    )
  })
  if (createAt >= 0 && edit) {
    const depth = (rows.find((r) => same(r.path, edit.path))?.depth ?? 0) + 1
    rendered.splice(
      createAt,
      0,
      <div key="new" className="note-row" style={{ paddingLeft: depth * 20 + 4 }}>
        <span className="note-mark">{edit.values?.open ? <Square className="icon" /> : <span className="note-bullet" />}</span>
        <div className="note-body">{box(depth, edit.values?.section === true)}</div>
      </div>,
    )
  }

  return (
    <div className={`notes ${mobile ? 'mobile' : ''}`}>
      {info.parentId && (
        <div className="meta item-parent">
          in{' '}
          <button data-ui className="link" onClick={() => onOpen(info.parentId!)}>
            {tree.info(info.parentId)?.summary ?? 'its parent'}
          </button>
        </div>
      )}
      <FindLine />
      {(sectionsOnly || openOnly) && (
        <div className="notes-filters">
          {sectionsOnly && (
            <button data-ui className="pill" onClick={() => setSectionsOnly(false)} title="Show everything (q)">
              Sections only ×
            </button>
          )}
          {openOnly && (
            <button data-ui className="pill" onClick={() => setOpenOnly(false)} title="Show finished items too (Shift+Q)">
              Open only ×
            </button>
          )}
        </div>
      )}
      {error && (
        <div className="error" onClick={clearNotesError}>
          {error}
        </div>
      )}
      <div className="notes-scroll" ref={scroller}>
        {rendered}
      </div>
      {picking && selected && (
        <NotePicker
          title={picking === 'move' ? 'Move into…' : 'Put in this note…'}
          tree={tree}
          root={root}
          tasks={tasks}
          // Nothing goes inside itself.
          exclude={picking === 'move' ? selected.id : null}
          onClose={() => setPicking(null)}
          onPick={(id) => {
            setPicking(null)
            if (picking === 'move') {
              moveNote(selected.id, id)
              select(rows[at - 1]?.path ?? [root])
            } else {
              moveNote(id, selected.id)
              setCollapsed(selected.id, false)
            }
          }}
        />
      )}
    </div>
  )
}

/** Headings grow with prominence: half as big again at the top, easing to a tenth as they nest (entity-graph's). */
const sectionStyle = (depth: number) => ({ fontSize: `${Math.max(1.1, 1.5 - depth * 0.1)}em`, lineHeight: 1.3 })

/**
 * Picks where a note goes (x), or what to put in one (r): the notes in this tree and the open tasks, filtered as
 * typed; the arrow keys and Enter, or a click.
 */
function NotePicker(props: {
  title: string
  tree: NotesTree
  root: string
  tasks: Item[]
  exclude: string | null
  onPick: (id: string) => void
  onClose: () => void
}) {
  const { title, tree, root, tasks, exclude, onPick, onClose } = props
  const [text, setText] = useState('')
  const [at, setAt] = useState(0)
  const options = useMemo(() => {
    const out: { id: string; label: string; info?: ItemInfo }[] = []
    const walk = (id: string) => {
      const i = tree.info(id)
      if (!i || resolve(id) === exclude) return
      if (i.type === 'note') out.push({ id: resolve(id), label: textOf(i) || 'Empty', info: i })
      for (const k of tree.kids(id)) walk(k)
    }
    walk(root)
    for (const t of tasks) if (!t.closed) out.push({ id: t.id, label: t.title })
    return out
  }, [tree, root, tasks, exclude])
  const shown = useMemo(
    () => (text.trim() ? fuzzysort.go(text, options, { key: 'label', limit: 50 }).map((r) => r.obj) : options.slice(0, 50)),
    [text, options],
  )
  useEffect(() => setAt(0), [text])
  return (
    <Modal title={title} size="large" blur onClose={onClose}>
      <input
        className="note-picker-input"
        autoFocus
        value={text}
        placeholder="Find a note or a task"
        onChange={(e) => setText(e.target.value)}
        onKeyDown={(e) => {
          if (e.key === 'ArrowDown') {
            e.preventDefault()
            setAt((n) => Math.min(n + 1, shown.length - 1))
          } else if (e.key === 'ArrowUp') {
            e.preventDefault()
            setAt((n) => Math.max(n - 1, 0))
          } else if (e.key === 'Enter' && shown[at]) {
            e.preventDefault()
            onPick(shown[at].id)
          }
        }}
      />
      <div className="note-picker-list">
        {shown.map((o, i) => (
          <button data-ui key={o.id} className={`row ${i === at ? 'selected' : ''}`} onMouseEnter={() => setAt(i)} onClick={() => onPick(o.id)}>
            {o.info ? <ItemInfoIcon info={o.info} /> : <ItemInfoIcon info={{ type: 'task', createdBy: '' }} />}
            <span className="row-title">{o.label}</span>
          </button>
        ))}
        {!shown.length && <p className="muted">Nothing matches.</p>}
      </div>
    </Modal>
  )
}
