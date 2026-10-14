import { createContext, memo, useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react'
import type { ComponentType, CSSProperties, ReactNode } from 'react'
import type { Entity, NoteValues, ViewRow } from '../../../core/types.ts'
import { Checkbox } from './Notes.tsx'
import { Text } from './messages.tsx'
import type { Edit, ShownRow } from '../state.ts'
import { ItemPill, KindsContext, Status, tintClass, tintOf } from './primitives.tsx'
import { Composer } from './primitives.tsx'
import type { OverviewProps } from './kindTypes.ts'
import type { ViewProps } from './types.ts'

/** Rows mounted beyond each edge of the viewport. */
const overscan = 8
/** The height assumed for a row not yet measured, in px. */
const estimate = 30
/** How far in from the edge a row the view jumps to lands, as a share of the viewport. */
const margin = 0.4

/**
 * Every view: its root's overview and its tree, or its type's `Detail` in
 * their place. The root is the first row and selectable like any other, so
 * Enter, `e` and links work on it too.
 */
export function ViewPane(props: ViewProps) {
  const kinds = useContext(KindsContext)!
  const { view } = props
  if (!view.root) {
    return (
      <div className="pane centred">
        {view.loading ? <span className="muted">Loading…</span> : <Status error={view.error} />}
      </div>
    )
  }
  const root = view.root
  // A whole page of its own only in the main view; a peek or the side panel shows the tree.
  const Detail = kinds[root.type].Detail
  if (Detail && props.interactive) return <Detail {...props} entity={root} />
  return <TreePane {...props} root={root} />
}

/**
 * A view as its root's overview and its tree, whatever its type: what a
 * view is without a `Detail`, and what a `Detail` may show as part of its
 * page (a topic's thoughts, with `look` for their look).
 */
export function TreePane(props: ViewProps & { root: Entity; look?: string }) {
  const kinds = useContext(KindsContext)!
  const { view, root } = props
  const Overview = kinds[root.type].Overview
  const action = view.actions.find((offered) => offered.id === props.acting)
  // Typing the next prompt, the chat doesn't mark a row: the box is where I am.
  const prompting = view.startsAtEnd && (props.prompt?.composing ?? props.composing) && !action
  return (
    <div className={`pane${props.look ? ` ${props.look}` : ''}${prompting ? ' prompting' : ''}${props.focused ? '' : ' unfocused'}`}>
      {props.interactive && props.find !== null && (
        <FindField find={props.find} focus={props.findFocus} onFind={props.onFind} onFocused={props.onFindFocused} />
      )}
      <ThoughtsContext.Provider value={props.look === 'thoughts'}>
        <TreeList props={props} root={root} Overview={Overview} />
      </ThoughtsContext.Provider>
      {view.loading && view.rows.length <= 1 && <div className="loading">Loading…</div>}
      <Status error={view.error} />
      {action && props.composing && (
        <Composer
          kind="action"
          placeholder={action.prompt}
          draft={props.draft}
          composing={props.composing}
          onDraft={props.onDraft}
          onCompose={props.onCompose}
        />
      )}
      {/* A Claude session's chat: the next prompt, typed below it. It has the keyboard as the view opens. */}
      {view.compose === 'claude' && props.interactive && !action && (
        <Composer
          key={root.id}
          kind="claude"
          autoFocus
          placeholder="Prompt"
          draft={props.draft}
          composing={props.composing}
          onDraft={props.onDraft}
          onCompose={props.onCompose}
        />
      )}
      {/* A chat in the side panel or a peek: its own box, apart from the main view's. */}
      {view.compose === 'claude' && !props.interactive && props.prompt && (
        <Composer
          key={root.id}
          kind="claude"
          field="window"
          placeholder="Prompt"
          draft={props.prompt.draft}
          composing={props.prompt.composing}
          onDraft={props.prompt.onDraft}
          onCompose={props.prompt.onCompose}
        />
      )}
    </div>
  )
}

/** The view's find: rows whose text says it, and the rows above them. Enter goes back to the tree, Escape clears it. */
function FindField(props: { find: string; focus: number; onFind(text: string): void; onFocused(): void }) {
  const ref = useRef<HTMLInputElement>(null)
  // Only when asked (Ctrl+F), never just for being on screen.
  const { focus, onFocused } = props
  useEffect(() => {
    if (!focus) return
    ref.current?.focus()
    onFocused()
  }, [focus, onFocused])
  return (
    <input
      ref={ref}
      className="find"
      data-field="find"
      placeholder="Find"
      spellCheck={false}
      value={props.find}
      onChange={(event) => props.onFind(event.target.value)}
    />
  )
}

/** A row's frame: takes the cursor on a click. Keeping it on screen is the list's job. */
function Selectable(props: { shown: Extract<ShownRow, { kind: 'entity' }>; className?: string; style?: CSSProperties; onSelect?(path: string[]): void; children: ReactNode }) {
  const { selected } = props.shown
  const { onSelect } = props
  const path = props.shown.row.path
  return (
    <div
      className={`row${selected ? ' selected' : ''}${props.className ? ` ${props.className}` : ''}`}
      style={props.style}
      onMouseDown={onSelect ? () => onSelect(path) : undefined}
    >
      {props.children}
    </div>
  )
}

// `--depth` too, for a look that sizes headings by it (thoughts).
const indent = (depth: number): CSSProperties => ({ paddingLeft: 10 + (depth - 1) * 20, ['--depth' as string]: depth })

/**
 * A child row: indented by depth, marked when it has children (open or
 * folded), then its type's `Row`. Given only what it uses, all of it stable,
 * so a row redraws only when it changes.
 */
const TreeRow = memo(function TreeRow(props: {
  shown: Extract<ShownRow, { kind: 'entity' }>
  /** Only while this row is being edited. */
  edit: Edit | null
  onSelect(path: string[]): void
  onOpen(id: string): void
  onImage(ref: string | null): void
  onEditDraft(text: string): void
  onCommitEdit(): void
  onToggle?(path: string[]): void
  findText?: string
  /** Chat leads a row with nothing in place of a bullet. */
  bullets: boolean
}) {
  const kinds = useContext(KindsContext)!
  const { row } = props.shown
  const Row = kinds[row.entity.type].Row
  return (
    <Selectable
      shown={props.shown}
      className={`tree${rowClass(row)}${row.hasChildren && !row.open ? ' folded' : ''}${row.entity.data.section ? ' section' : ''}`}
      style={indent(row.depth)}
      onSelect={props.onSelect}
    >
      <Marker row={row} bullets={props.bullets} onToggle={props.onToggle} />
      <div className={`cell${tintClass(tintOf(kinds, row.entity))}${hasPeers(row.entity) ? ' has-context' : ''}`}>
        {props.shown.editing ? (
          <TextBox edit={props.edit} onEditDraft={props.onEditDraft} />
        ) : (
          <Row
            entity={row.entity}
            parent={row.parent}
            above={row.above}
            onOpen={props.onOpen}
            onImage={props.onImage}
            findText={props.findText}
          />
        )}
        <ContextPills entity={row.entity} />
      </div>
    </Selectable>
  )
})

const hasPeers = (entity: Entity): boolean => Array.isArray(entity.data.contextPeers) && entity.data.contextPeers.length > 0

/** How many of a context's other items are named beside one; the rest are counted. */
const contextShown = 6

/**
 * The other items in an item's context (`contextPeers`, from `present`), as
 * pills straight after its text: hover to peek, click to go. Rows and overviews draw them;
 * pills don't, so they never nest.
 */
function ContextPills(props: { entity: Entity }) {
  if (!hasPeers(props.entity)) return null
  const ids = props.entity.data.contextPeers as string[]
  return (
    <span className="context-pills">
      {ids.slice(0, contextShown).map((id) => (
        <ItemPill key={id} id={id} fallback="…" />
      ))}
      {ids.length > contextShown && <span className="muted">+{ids.length - contextShown}</span>}
    </span>
  )
}

function rowClass(row: ViewRow): string {
  return row.entity.type === 'slack.message' && row.entity.data.quiet ? ' quiet' : ''
}

/** The box a new note is typed into, where it will appear. */
function EditRow(props: { depth: number; values?: NoteValues; props: ViewProps }) {
  return (
    <div className={`row tree selected${props.values?.section ? ' section' : ''}`} style={indent(props.depth)}>
      <span className="marker">{typeof props.values?.open === 'boolean' ? <Checkbox open={props.values.open} /> : <span className="bullet">•</span>}</span>
      <div className="cell">
        <EditBox props={props.props} />
      </div>
    </div>
  )
}

/**
 * What leads a row: its box if it has one (ticked with a click, or Space),
 * else its fold mark if it has children, else a bullet (or nothing, in chat).
 */
function Marker(props: { row: ViewRow; bullets: boolean; onToggle?(path: string[]): void }) {
  const { row, onToggle } = props
  const box = boxOf(row)
  if (box !== null) {
    return (
      <span
        className="marker"
        onMouseDown={(event) => {
          if (!onToggle) return
          event.stopPropagation()
          onToggle(row.path)
        }}
      >
        <Checkbox open={box} />
      </span>
    )
  }
  if (!row.hasChildren && !props.bullets) return null
  return <span className="marker">{row.hasChildren ? (row.open ? '▾' : '▸') : <span className="bullet">•</span>}</span>
}

/** A row's box: true while open, false once ticked, null for none (a note's `open`, an old task's `done`). */
function boxOf(row: ViewRow): boolean | null {
  const { data, type } = row.entity
  if (type === 'task') return !data.done
  return typeof data.open === 'boolean' ? data.open : null
}

/** Typing in place: Enter writes, Escape gives up. Clicking away leaves it as it is, to come back to. */
function EditBox(props: { props: ViewProps }) {
  return <TextBox edit={props.props.edit} onEditDraft={props.props.onEditDraft} />
}

/** Whether the tree is drawn as entity-graph's editor draws one (thoughts): its rows, marks, type and typing in place. */
const ThoughtsContext = createContext(false)

export function TextBox(props: { edit: Edit | null; onEditDraft(text: string): void }) {
  const { edit, onEditDraft } = props
  if (useContext(ThoughtsContext)) return <ThoughtsTextBox edit={edit} onEditDraft={onEditDraft} />
  return (
    <input
      className="edit"
      autoFocus
      spellCheck
      value={edit?.draft ?? ''}
      placeholder={edit?.mode === 'create' ? 'Note' : ''}
      data-field="edit"
      onChange={(event) => onEditDraft(event.target.value)}
      onMouseDown={(event) => event.stopPropagation()}
    />
  )
}

// --- Thoughts: entity-graph's editor, copied (its `Editor.tsx` and `TextEditor.tsx`) ---

/** A section's size by depth: about 1.5× at the root, easing to 1.1× as it nests. */
const sectionStyle = (depth: number): CSSProperties => ({ fontSize: `${14 * Math.max(1.1, 1.5 - depth * 0.1)}px`, lineHeight: 1.3 })

/** Untitled UI's icons, as entity-graph draws them: stroked in the colour of the text. */
function Icon(props: { size: number; d: string }) {
  return (
    <svg viewBox="0 0 24 24" width={props.size} height={props.size} stroke="currentColor" strokeWidth="2" fill="none" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true">
      <path d={props.d} />
    </svg>
  )
}

const icons = {
  chevronRight: 'm9 18 6-6-6-6',
  chevronDown: 'm6 9 6 6 6-6',
  square:
    'M3 7.8c0-1.68 0-2.52.327-3.162a3 3 0 0 1 1.311-1.311C5.28 3 6.12 3 7.8 3h8.4c1.68 0 2.52 0 3.162.327a3 3 0 0 1 1.311 1.311C21 5.28 21 6.12 21 7.8v8.4c0 1.68 0 2.52-.327 3.162a3 3 0 0 1-1.311 1.311C18.72 21 17.88 21 16.2 21H7.8c-1.68 0-2.52 0-3.162-.327a3 3 0 0 1-1.311-1.311C3 18.72 3 17.88 3 16.2V7.8Z',
  check: 'M20 6 9 17l-5-5',
}

/** What leads a row: its box (kept even with children), else a chevron while it has children, else a small dot. */
function ThoughtsMark(props: { open: boolean | null; foldable: boolean; collapsed: boolean }) {
  if (props.open === true) return <Icon size={13} d={icons.square} />
  if (props.open === false) return <Icon size={13} d={icons.check} />
  if (props.foldable) return <Icon size={14} d={props.collapsed ? icons.chevronRight : icons.chevronDown} />
  return <span className="eg-dot" />
}

/**
 * A row of thoughts, as entity-graph's editor draws one: indented 20px a
 * level, a 20px square for its mark (a click there folds it), then its text
 * in Lora, faint while its children are hidden; a section larger the nearer
 * the root. The root is a row like the others.
 */
const ThoughtsRow = memo(function ThoughtsRow(props: {
  shown: Extract<ShownRow, { kind: 'entity' }>
  edit: Edit | null
  onSelect(path: string[]): void
  onFold?(path: string[], open: boolean): void
  onOpen(id: string): void
  onImage(ref: string | null): void
  onEditDraft(text: string): void
  findText?: string
}) {
  const kinds = useContext(KindsContext)!
  const { row, selected, editing } = props.shown
  const { entity, depth } = row
  const section = entity.data.section === true
  const heading = section ? sectionStyle(depth) : undefined
  const text = typeof entity.data.text === 'string' ? entity.data.text : ''
  const box = boxOf(row)
  const collapsed = row.hasChildren && !row.open
  // A note (or the topic heading the tree) is its text; anything else linked in is its own row.
  const prose = entity.type === 'note' || depth === 0
  const Row = kinds[entity.type].Row
  return (
    <div className="eg-row-outer">
      <div className={`eg-row${selected ? ' selected' : ''}`} style={{ paddingLeft: depth * 20 + 4 }} onMouseDown={() => props.onSelect(row.path)}>
        <span
          className="eg-mark"
          onMouseDown={(event) => {
            event.stopPropagation()
            if (row.hasChildren && depth > 0) props.onFold?.(row.path, collapsed)
          }}
        >
          <ThoughtsMark open={box} foldable={row.hasChildren} collapsed={collapsed} />
        </span>
        <div className="eg-content">
          {editing ? (
            <ThoughtsTextBox edit={props.edit} onEditDraft={props.onEditDraft} section={section} style={heading} />
          ) : !prose ? (
            <div className={`eg-text${collapsed ? ' hides' : ''}`}>
              <Row entity={entity} parent={row.parent} above={row.above} onOpen={props.onOpen} onImage={props.onImage} findText={props.findText} />
            </div>
          ) : text ? (
            <div className={`eg-text eg-markdown${section ? ' section' : ''}${collapsed ? ' hides' : ''}`} style={heading}>
              <Text text={text} find={props.findText} />
            </div>
          ) : (
            <span className="eg-text eg-empty">Empty</span>
          )}
        </div>
      </div>
    </div>
  )
})

/** The row a new note is typed into, where it will appear, showing the mark it will have. */
function ThoughtsInputRow(props: { depth: number; values?: NoteValues; edit: Edit | null; onEditDraft(text: string): void }) {
  const section = props.values?.section === true
  return (
    <div className="eg-row-outer">
      <div className="eg-row selected" style={{ paddingLeft: props.depth * 20 + 4 }}>
        <span className="eg-mark">
          <ThoughtsMark open={typeof props.values?.open === 'boolean' ? props.values.open : null} foldable={false} collapsed={false} />
        </span>
        <div className="eg-content">
          <ThoughtsTextBox edit={props.edit} onEditDraft={props.onEditDraft} section={section} style={section ? sectionStyle(props.depth) : undefined} />
        </div>
      </div>
    </div>
  )
}

/**
 * Typing in place, as entity-graph does: a textarea in the row's own type,
 * with no frame, growing with what is typed, so nothing moves when it opens.
 * Enter writes, Shift+Enter breaks the line.
 */
function ThoughtsTextBox(props: { edit: Edit | null; onEditDraft(text: string): void; section?: boolean; style?: CSSProperties }) {
  const { edit, onEditDraft } = props
  const ref = useRef<HTMLTextAreaElement>(null)
  useEffect(() => {
    const element = ref.current
    if (!element) return
    element.focus()
    element.setSelectionRange(element.value.length, element.value.length)
  }, [])
  useLayoutEffect(() => {
    const element = ref.current
    if (!element) return
    element.style.height = 'auto'
    element.style.height = `${element.scrollHeight}px`
  }, [edit?.draft, props.style?.fontSize, props.style?.lineHeight])
  return (
    <textarea
      ref={ref}
      className={`eg-text eg-edit${props.section ? ' section' : ''}`}
      rows={1}
      spellCheck
      style={props.style}
      value={edit?.draft ?? ''}
      placeholder={edit?.mode === 'create' ? (props.section ? 'New section…' : 'New note…') : ''}
      data-field="edit"
      onChange={(event) => onEditDraft(event.target.value)}
      onMouseDown={(event) => event.stopPropagation()}
    />
  )
}

/**
 * The tree, windowed: only rows near the viewport are mounted, as in
 * entity-graph. Each row measures itself (rows wrap, messages have images),
 * unmeasured ones are guessed, and offsets are worked out from the keys, which
 * stay the same while the tree does, so a cursor move lays out nothing.
 *
 * - The selected row is kept on screen: the view stays put while it is, and
 *   jumps to put it `margin` (40%) in from the edge once it goes past one.
 * - The row being typed into is pinned: mounted wherever it is, so scrolling
 *   away doesn't take the caret with it, and stuck to the view's bottom (or
 *   top) while its own place is out of sight.
 * - Scrolling near the end, or rows that don't fill the screen, walk further
 *   (`onNearEnd`). Chat reads bottom up, so its end is the top.
 * - Rows arriving above (chat growing upwards, guesses becoming heights) don't
 *   move what is on screen: the first visible row is kept where it was.
 * - Chat (`startsAtEnd`) keeps its overview above the list, out of the scroll;
 *   its rows sit at the bottom, a gap apart, and stay at the bottom as they
 *   grow while the list is scrolled to there.
 */
function TreeList(props: { props: ViewProps; root: Entity; Overview: ComponentType<OverviewProps> }) {
  const kinds = useContext(KindsContext)!
  const view = props.props
  const { rows, keys, selectedIndex } = view.shown
  const chat = view.view.startsAtEnd
  const thoughts = useContext(ThoughtsContext)
  // The rows the list lays out: in chat, all but the overview, which sits above it.
  const body = chat ? 1 : 0
  const ref = useRef<HTMLDivElement>(null)
  const [scrollTop, setScrollTop] = useState(0)
  const [viewport, setViewport] = useState(0)
  const [heights, setHeights] = useState<Map<string, number>>(new Map())
  const onMeasure = useCallback((key: string, height: number) => {
    setHeights((known) => {
      if (known.get(key) === height) return known
      const next = new Map(known)
      next.set(key, height)
      return next
    })
  }, [])

  useEffect(() => {
    const el = ref.current
    if (!el) return
    const update = () => setViewport(el.clientHeight)
    update()
    const observer = new ResizeObserver(update)
    observer.observe(el)
    return () => observer.disconnect()
  }, [])

  // offsets[i] is the top of row i; offsets[n] the total height.
  const offsets = useMemo(() => {
    const out = new Array<number>(keys.length + 1)
    let total = 0
    for (let i = 0; i < keys.length; i++) {
      out[i] = total
      if (i >= body) total += heights.get(keys[i]) ?? estimate
    }
    out[keys.length] = total
    return out
  }, [keys, heights, body])
  const total = offsets[keys.length]

  // Keep the first visible row where it was when rows above it change: the
  // anchor is recorded after each render and restored when the layout moves.
  // Chat sticks to its bottom instead while it is there, and starts there: a
  // view it hasn't been recorded for yet (just opened) counts as at the end.
  const anchor = useRef<{ root: string; key: string; delta: number; atEnd: boolean } | null>(null)
  const rootId = props.root.id
  useLayoutEffect(() => {
    const el = ref.current
    const held = anchor.current?.root === rootId ? anchor.current : null
    if (!el) return
    if (chat && (held?.atEnd ?? true)) {
      el.scrollTop = el.scrollHeight
      setScrollTop(el.scrollTop)
      return
    }
    if (!held) return
    const at = keys.indexOf(held.key)
    if (at < 0) return
    const want = offsets[at] + held.delta
    if (Math.abs(el.scrollTop - want) > 1) {
      el.scrollTop = want
      setScrollTop(el.scrollTop)
    }
  }, [keys, offsets, chat, viewport, rootId])

  const offsetsRef = useRef(offsets)
  offsetsRef.current = offsets
  const reveal = useCallback((index: number) => {
    const el = ref.current
    if (!el || index < body) return
    const o = offsetsRef.current
    const top = o[index]
    const bottom = o[index + 1]
    const height = el.clientHeight
    if (!height) return
    // Nothing moves while the row is on screen. Once it isn't, the view jumps
    // so it lands `margin` in from the edge it went past: the cursor runs on
    // smoothly, and the page catches up now and then.
    const room = Math.max(0, Math.min(height * margin, height - (bottom - top)))
    if (top < el.scrollTop) el.scrollTop = Math.max(0, top - room)
    else if (bottom > el.scrollTop + height) el.scrollTop = bottom - height + room
    setScrollTop(el.scrollTop)
  }, [body])
  // The cursor is followed as rows around it are measured, until the list is
  // scrolled by hand; moving the cursor follows it again.
  const following = useRef(true)
  useEffect(() => {
    following.current = true
  }, [selectedIndex])
  const selectedTop = selectedIndex < 0 ? -1 : offsets[selectedIndex]
  useEffect(() => {
    if (following.current) reveal(selectedIndex)
  }, [selectedIndex, selectedTop, viewport, reveal])

  // Asked to (the side panel opening): the selected row goes to the middle,
  // again each time the layout moves, until it has settled for a moment.
  const centring = useRef<ReturnType<typeof setTimeout> | null>(null)
  const settled = 250
  useLayoutEffect(() => {
    if (!view.centre) return
    if (centring.current) clearTimeout(centring.current)
    centring.current = setTimeout(() => (centring.current = null), settled)
  }, [view.centre])
  useEffect(() => () => void (centring.current && clearTimeout(centring.current)), [])
  useLayoutEffect(() => {
    const el = ref.current
    if (!el || !centring.current || selectedIndex < body) return
    const middle = (offsets[selectedIndex] + offsets[selectedIndex + 1]) / 2
    el.scrollTop = Math.max(0, middle - el.clientHeight / 2)
    setScrollTop(el.scrollTop)
    // Each move restarts the wait.
    clearTimeout(centring.current)
    centring.current = setTimeout(() => (centring.current = null), settled)
  }, [view.centre, offsets, viewport, selectedIndex, body])

  const editIndex = rows.findIndex((row) => row.kind === 'input' || row.editing)
  // In the list, not the overview above it.
  const pinned = editIndex >= body

  const { onNearEnd } = view
  const near = (el: HTMLDivElement) =>
    chat ? el.scrollTop <= estimate * overscan : el.scrollTop + el.clientHeight >= total - estimate * overscan
  // Rows that don't fill the screen have no scroll to ask for more for them.
  useEffect(() => {
    if (!viewport || view.view.complete || total >= viewport) return
    onNearEnd?.()
  })

  // The window: the first row reaching the viewport's top to the first past its bottom, padded.
  const bottomEdge = scrollTop + (viewport || 600)
  let first = 0
  while (first < rows.length && offsets[first + 1] <= scrollTop) first++
  const firstVisible = first
  first = Math.max(body, first - overscan)
  let last = first
  while (last < rows.length && offsets[last] < bottomEdge) last++
  last = Math.min(rows.length, last + overscan)
  useLayoutEffect(() => {
    const el = ref.current
    anchor.current =
      el && keys[firstVisible] !== undefined
        ? { root: rootId, key: keys[firstVisible], delta: el.scrollTop - offsets[firstVisible], atEnd: el.scrollTop + el.clientHeight >= el.scrollHeight - 2 }
        : null
  })

  const overview = (shown: Extract<ShownRow, { kind: 'entity' }>) => (
    <Selectable shown={shown} className="overview" onSelect={view.onSelect}>
      {/* Its context's pills to the right, level with its first line. */}
      <div className={`overview-frame${tintClass(tintOf(kinds, props.root))}`}>
        <div className="overview-body">
          {shown.editing ? <EditBox props={view} /> : <props.Overview {...view} entity={props.root} findText={view.find ?? undefined} />}
        </div>
        <ContextPills entity={props.root} />
      </div>
    </Selectable>
  )

  const render = (index: number) => {
    const shown = rows[index]
    return (
      <Measured key={shown.key} measureKey={shown.key} onMeasure={onMeasure} gap={chat && shown.kind === 'entity' && shown.row.depth === 1}>
        {thoughts ? (
          shown.kind === 'input' ? (
            <ThoughtsInputRow depth={shown.depth} values={shown.values} edit={view.edit} onEditDraft={view.onEditDraft} />
          ) : (
            <ThoughtsRow
              shown={shown}
              edit={shown.editing ? view.edit : null}
              onSelect={view.onSelect}
              onFold={view.onFold}
              onOpen={view.onOpen}
              onImage={view.onImage}
              onEditDraft={view.onEditDraft}
              findText={view.find ?? undefined}
            />
          )
        ) : shown.kind === 'input' ? (
          <EditRow depth={shown.depth} values={shown.values} props={view} />
        ) : shown.row.depth === 0 ? (
          overview(shown)
        ) : (
          <TreeRow
            shown={shown}
            edit={shown.editing ? view.edit : null}
            onSelect={view.onSelect}
            onOpen={view.onOpen}
            onImage={view.onImage}
            onEditDraft={view.onEditDraft}
            onCommitEdit={view.onCommitEdit}
            onToggle={view.onToggle}
            findText={view.find ?? undefined}
            bullets={!chat}
          />
        )}
      </Measured>
    )
  }

  const slice: ReactNode[] = []
  for (let i = first; i < last; i++) {
    slice.push(i === editIndex && pinned ? <div key="\0edit-slot" style={{ height: offsets[i + 1] - offsets[i] }} /> : render(i))
  }

  const top = chat ? rows[0] : undefined
  return (
    <>
      {/* Over the chat, ruled off from it, so it reads as staying put while the chat scrolls. */}
      {top?.kind === 'entity' && top.row.depth === 0 && <div className="chat-head">{overview(top)}</div>}
    <div
      ref={ref}
      className={`list${chat ? ' chat' : ''}`}
      onWheel={() => (following.current = false)}
      onMouseDown={(event) => {
        // On the scrollbar, not a row.
        if (event.target === event.currentTarget) following.current = false
      }}
      onScroll={(event) => {
        const el = event.currentTarget
        setScrollTop(el.scrollTop)
        if (near(el)) onNearEnd?.()
      }}
    >
      {/* Room to scroll on past the last row, 40% of the view, once the rows overflow it. Chat ends at its last row. */}
      <div className="rows" style={{ height: total, marginBottom: chat ? 'var(--chat-gap)' : total > viewport ? Math.round(viewport * 0.4) : 0 }}>
        <div style={{ height: offsets[first] }} />
        {slice}
        {pinned && (
          // The box being typed into sits at its own place, sticking to the
          // bottom (or top) of the view while that place is out of sight. The
          // browser keeps it there as the list scrolls, so it never lags.
          <div className="pin-track">
            <div style={{ height: offsets[editIndex] }} />
            <div className="edit-pin">{render(editIndex)}</div>
          </div>
        )}
      </div>
    </div>
    </>
  )
}

/** Reports its height while mounted, so the list can lay out the rows around it. */
function Measured(props: { measureKey: string; onMeasure(key: string, height: number): void; gap?: boolean; children: ReactNode }) {
  const ref = useRef<HTMLDivElement>(null)
  const { measureKey, onMeasure } = props
  useLayoutEffect(() => {
    const el = ref.current
    if (!el) return
    // Fractional: rows are ~30.5px, and whole pixels would add up to a row's
    // drift between where the list lays a row and where the offsets say it is.
    const report = () => onMeasure(measureKey, el.getBoundingClientRect().height)
    report()
    const observer = new ResizeObserver(report)
    observer.observe(el)
    return () => observer.disconnect()
  }, [measureKey, onMeasure])
  return (
    <div ref={ref} className={props.gap ? 'gapped' : undefined}>
      {props.children}
    </div>
  )
}
