// A note's focus: the notes in it as a dense tree, worked from the keyboard as entity-graph's outliner is. Its
// structure is only the items in each note (`contains` links); anything else in a note is a leaf, opened with `d`.
// Every change shows at once (store.ts) and goes to the backend after. Everything of the notes view is in this folder;
// what it shares with the rest of the app (the picker, the text box and its `@`, the find line, markdown) it takes from there.

import { atom, useAtom } from 'jotai'
import { atomWithStorage, createJSONStorage } from 'jotai/utils'
import { ChevronDown, ChevronRight, Square, SquareCheck, X } from 'lucide-react'
import { useContext, useEffect, useMemo, useRef, useState } from 'react'
import type { Item, ItemInfo } from '../../../shared/types'
import { Markdown } from '../components/ui/Markdown'
import { FindLine } from '../FindLine'
import { ItemInfoIcon } from '../icons'
import { useItemMenu } from '../ItemMenu'
import { ItemPicker } from '../ItemPicker'
import type { Recent } from '../sections'
import { DialogContext, modalOpen } from '../state/ui'
import { TextInput, type Mentions } from '../TextInput'
import { findAtom } from '../views'
import { clearNotesError, createNote, notesError, moveNote, resolve, setNoteOpen, setNoteValue, useNotes, type NotesTree } from './store'
import { Margin } from '../Margin'
import { pasteHere } from '../paste'
import './notes.css'

const storage = <T,>() => createJSONStorage<T>(() => localStorage)

/** Where each tree was left, by its root: the row selected, and the notes folded shut. Kept across reloads. */
type Place = { path: string[]; collapsed: string[] }
const placesAtom = atomWithStorage<Record<string, Place>>('workflows.notes', {}, storage(), { getOnInit: true })

/**
 * What is being typed in each tree, by its root, until it is written (Enter, or pressing another row): kept when the
 * window loses focus or the view is left, so it is there to finish on coming back, as the chat input's drafts are.
 */
type Edit = { mode: 'edit' | 'create'; path: string[]; draft: string; values?: Record<string, unknown> }
const editsAtom = atomWithStorage<Record<string, Edit>>('workflows.notes.edits', {}, storage(), { getOnInit: true })

/**
 * A move waiting for where to (`x`: the row picked goes into the one chosen), or a link for what (`r`: the one chosen
 * goes into the row picked; `shift+r` the other way, the row picked into the one chosen, as entity-graph's are). Chosen
 * by selecting a row, in this tree or another reached with `d`, and pressing the key again (either `r` for a link: its
 * way is settled); or in the picker, from the corner. Not kept: a reload forgets it.
 */
type Picking = { kind: 'move' | 'link'; reverse?: boolean; subject: string; label: string }
const pickingAtom = atom<Picking | null>(null)

type Row = { key: string; id: string; path: string[]; depth: number; info: ItemInfo; hasKids: boolean; collapsed: boolean }
type Filters = { find: string; sectionsOnly: boolean; openOnly: boolean }

const textOf = (i: ItemInfo) => (i.type === 'topic' ? String(i.params.text ?? '') : i.summary)

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
    const kids = info.type === 'topic' || id === root ? tree.kids(id) : []
    const shut = collapsed.has(resolve(id))
    out.push({ key: path.map(resolve).join('/'), id: resolve(id), path, depth, info, hasKids: kids.length > 0, collapsed: shut && kids.length > 0 })
    if (shut && !filtering) return
    for (const k of kids) if (!filtering || keep(k)) walk(k, [...path, resolve(k)], depth + 1)
  }
  walk(root, [root], 0)
  return out
}

/** Whether `id` is `outer`, or sits somewhere inside it, as the tree now has it. */
function within(tree: NotesTree, id: string, outer: string): boolean {
  for (let at: string | null | undefined = resolve(id); at; at = tree.info(at)?.parentId) if (at === resolve(outer)) return true
  return false
}

const same = (a: string[], b: string[]) => a.length === b.length && a.every((x, i) => resolve(x) === resolve(b[i]))
const isTyping = (t: EventTarget | null) => t instanceof HTMLElement && (t.isContentEditable || ['INPUT', 'TEXTAREA', 'SELECT'].includes(t.tagName))

/** `bare`: the tree alone, without the line saying what it is in, nor a find line (in a margin's dropdown). */
export function NotesView(props: { info: ItemInfo; tasks: Item[]; recent: Recent; bots: Mentions['bots']; onOpen: (id: string) => void; onBack: () => void; mobile?: boolean; bare?: boolean }) {
  const { info, tasks, recent, bots, onOpen, onBack, mobile = false, bare = false } = props
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
  const [edits, setEdits] = useAtom(editsAtom)
  const edit = edits[root] ?? null
  const setEdit = (e: Edit | null | ((x: Edit | null) => Edit | null)) =>
    setEdits((all) => {
      const next = typeof e === 'function' ? e(all[root] ?? null) : e
      const { [root]: _, ...rest } = all
      return next ? { ...rest, [root]: next } : rest
    })
  const editRef = useRef<Edit | null>(edit)
  editRef.current = edit
  const [picking, setPicking] = useAtom(pickingAtom)
  const [choosing, setChoosing] = useState(false)

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

  // Where a kept edit's box goes: its row (editing), or the end of what is in its note (a new one). Not on screen (its
  // row filtered out, or gone), the draft waits, and the keys are the tree's.
  const editAt = edit ? rows.findIndex((r) => same(r.path, edit.path)) : -1
  const createAt = useMemo(() => {
    if (edit?.mode !== 'create' || editAt < 0) return -1
    let i = editAt + 1
    while (i < rows.length && rows[i].depth > rows[editAt].depth) i++
    return i
  }, [edit?.mode, editAt, rows])
  const editing = editAt >= 0

  const startCreate = (values?: Record<string, unknown>) => {
    if (!selected || selected.info.type !== 'topic') return
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
      if (e.draft !== textOf(tree.info(subject) ?? info)) setNoteValue(subject, 'text', e.draft)
      return
    }
    const id = createNote(subject, { text: e.draft, ...e.values })
    select([...e.path, id])
  }
  const cancel = () => {
    editRef.current = null
    setEdit(null)
  }

  // Dropped open from a margin on a topic with no notes yet: the box for its first one is open at once.
  useEffect(() => {
    if (bare && !info.notes && !edit && info.type === 'topic') startCreate()
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  /** Whether the row picked goes into the one chosen (a move, or a link made with `shift+r`), or the other way. */
  const pickingInto = picking?.kind === 'move' || !!picking?.reverse
  /** Finishes a move or a link with what was chosen; refuses what would put a note inside itself. */
  const finish = (chosen: string) => {
    if (!picking) return
    setPicking(null)
    setChoosing(false)
    const [what, into] = pickingInto ? [picking.subject, chosen] : [chosen, picking.subject]
    if (within(tree, into, what)) return notesError("A note can't go inside itself.")
    moveNote(what, into)
    setCollapsed(resolve(into), false)
  }

  // The selected row stays in view, clear of the edges (its scroll-margin, in notes.css).
  const scroller = useRef<HTMLDivElement>(null)
  useEffect(() => {
    scroller.current?.querySelector('.note-row.selected')?.scrollIntoView({ block: 'nearest' })
  }, [at, rows.length])
  // The wheel anywhere on the page scrolls the tree, not only over it: the margins, the header. Not in a dialog (it
  // scrolls as anything does), nor under one.
  const inDialog = useContext(DialogContext)
  const dialog = useRef(inDialog)
  dialog.current = inDialog
  useEffect(() => {
    const on = (e: WheelEvent) => {
      const el = scroller.current
      const t = e.target as HTMLElement | null
      if (dialog.current || modalOpen() || !el || e.defaultPrevented || t?.closest('.notes-scroll, .side-pane, .side-nav, .modal, [role="dialog"], .menu, textarea')) return
      el.scrollBy({ top: e.deltaY, left: 0 })
    }
    window.addEventListener('wheel', on, { passive: true })
    return () => window.removeEventListener('wheel', on)
  }, [])

  // The keys, as entity-graph's: only while nothing is being typed into.
  const keys = useRef<(e: KeyboardEvent) => void>(() => {})
  keys.current = (e: KeyboardEvent) => {
    if (isTyping(e.target) || modalOpen(dialog.current) || e.altKey || e.metaKey) return
    // A draft waiting (the window or the view was left mid-edit): Escape drops it, any other key goes back to it.
    if (editing) {
      if (e.key === 'Escape') {
        e.preventDefault()
        cancel()
      } else scroller.current?.querySelector<HTMLTextAreaElement>('textarea.note-input')?.focus()
      return
    }
    const k = e.key
    const run = (fn: () => void) => {
      e.preventDefault()
      fn()
    }
    if (e.ctrlKey) {
      if (k === '/' && selected?.info.type === 'topic') run(() => setNoteValue(selected.id, 'section', selected.info.params.section ? null : true))
      return
    }
    if (!selected) return
    const parentPath = selected.path.slice(0, -1)
    // Waiting for a pick: the same key again takes the row selected; Escape gives up. Everything else moves as ever.
    if (picking && ((k === 'x' && picking.kind === 'move') || ((k === 'r' || k === 'R') && picking.kind === 'link'))) return run(() => finish(selected.id))
    if (picking && k === 'Escape') return run(() => setPicking(null))
    if (k === 'w' || k === 'ArrowUp') run(() => at > 0 && select(rows[at - 1].path))
    else if (k === 's' || k === 'ArrowDown') run(() => at < rows.length - 1 && select(rows[at + 1].path))
    else if (k === 'a') run(() => parentPath.length && select(parentPath))
    else if (k === 'Home') run(() => select([root]))
    else if (k === 'End') run(() => select(rows[rows.length - 1].path))
    else if (k === 'ArrowLeft') run(() => selected.hasKids && setCollapsed(selected.id, true))
    else if (k === 'ArrowRight') run(() => setCollapsed(selected.id, false))
    else if (k === 'e') run(() => selected.info.type === 'topic' && setEdit({ mode: 'edit', path: selected.path, draft: textOf(selected.info) }))
    else if (k === 'Enter') run(() => startCreate())
    else if (k === '/') run(() => startCreate({ section: true }))
    else if (k === '?') run(() => startCreate({ open: true }))
    else if (k === '>') run(() => selected.info.type === 'topic' && setNoteOpen(selected.id, selected.info.params.open === undefined ? true : selected.info.params.open === true ? false : null))
    else if (k === 'q') run(() => setSectionsOnly((v) => !v))
    else if (k === 'Q') run(() => setOpenOnly((v) => !v))
    else if (k === 'd') run(() => selected.id !== root && onOpen(selected.id))
    else if (k === 'A') run(onBack)
    else if (k === 'x') run(() => parentPath.length && setPicking({ kind: 'move', subject: selected.id, label: textOf(selected.info) }))
    else if (k === 'r' || k === 'R') run(() => selected.info.type === 'topic' && setPicking({ kind: 'link', reverse: k === 'R', subject: selected.id, label: textOf(selected.info) }))
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

  const box = (depth: number, section: boolean) => (
    <TextInput
      autoFocus
      className={`note-input ${section ? `note-section depth-${Math.min(depth, 3)}` : ''}`}
      value={edit?.draft ?? ''}
      mentions={{ items: tasks, recent, bots }}
      placeholder={edit?.mode === 'create' ? (section ? 'New section…' : 'New note…') : undefined}
      onText={(draft) => setEdit((x) => x && { ...x, draft })}
      onFocus={(e) => e.target.setSelectionRange(e.target.value.length, e.target.value.length)}
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
    const typing = edit?.mode === 'edit' && i === editAt
    const section = r.info.params.section === true
    const open = r.info.params.open
    return (
      <div
        key={r.key}
        className={`note-row ${i === at ? 'selected' : ''} ${r.collapsed ? 'shut' : ''} ${open === false ? 'ticked' : ''} ${picking?.subject === r.id ? 'picking' : ''}`}
        style={{ paddingLeft: r.depth * 20 + 4 }}
        // Pressing another row writes what is being typed, as Enter does; going elsewhere keeps it, to finish later.
        // A press in a modal opened from the box (the `@` finder) bubbles here through React's portal: not a press on a
        // row. A double press selects a word, as in any text, and doesn't edit (that is `e`); on an item that isn't a
        // note, it opens it.
        onMouseDown={(e) => {
          if (!e.currentTarget.contains(e.target as Node)) return
          if (edit && !(e.target as HTMLElement).closest('textarea')) commit()
        }}
        onClick={() => select(r.path)}
        onDoubleClick={() => r.info.type !== 'topic' && r.id !== root && onOpen(r.id)}
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
          {r.info.type !== 'topic' ? (
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
          {typing ? (
            box(r.depth, section)
          ) : r.info.type !== 'topic' ? (
            <span className="note-item">{r.info.summary}</span>
          ) : textOf(r.info) ? (
            <div className={section ? `note-section depth-${Math.min(r.depth, 3)}` : undefined}>
              <Markdown text={textOf(r.info)} highlight={find ?? undefined} className="note-text" />
            </div>
          ) : (
            <span className="note-empty">Empty</span>
          )}
        </div>
        {r.id !== root && <Margin info={r.info} other="comments" />}
      </div>
    )
  })
  if (createAt >= 0 && edit) {
    const depth = rows[editAt].depth + 1
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
    // What is pasted here (not into a box) goes in the selected row's notes (`pasteHere`).
    <div className={`notes ${mobile ? 'mobile' : ''}`} {...pasteHere({ into: selected?.id ?? root, via: 'notes' })}>
      {info.parentId && !bare && (
        <div className="meta item-parent">
          in{' '}
          <button data-ui className="link" onClick={() => onOpen(info.parentId!)}>
            {tree.info(info.parentId)?.summary ?? 'its parent'}
          </button>
        </div>
      )}
      {!bare && <FindLine />}
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
      {picking && (
        <div className="note-picking" role="status">
          <div className="note-picking-head">
            <strong>
              {picking.kind === 'move' ? 'Move' : picking.reverse ? 'Put' : 'Put in'} “{picking.label.split('\n')[0] || 'Empty'}”{picking.reverse ? ' in…' : ''}
            </strong>
            <button data-ui onClick={() => setChoosing(true)} title={pickingInto ? 'Pick where it goes' : 'Pick what goes in it'}>
              Pick…
            </button>
            <button data-ui title="Cancel (Escape)" aria-label="Cancel" onClick={() => setPicking(null)}>
              <X className="icon" />
            </button>
          </div>
          <p>
            {pickingInto ? 'Select where it goes' : 'Select what goes in it'}, then press {picking.kind === 'move' ? 'x' : 'r'} again.
          </p>
        </div>
      )}
      {picking && choosing && (
        <ItemPicker
          title={picking.kind === 'move' ? 'Move into…' : picking.reverse ? 'Put it in…' : 'Put in it…'}
          items={tasks}
          recent={recent}
          verb={picking.kind === 'move' ? 'Move here' : picking.reverse ? 'Put it here' : 'Put in'}
          onClose={() => setChoosing(false)}
          onPick={finish}
        />
      )}
    </div>
  )
}
