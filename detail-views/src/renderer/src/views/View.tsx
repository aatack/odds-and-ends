import { memo, useCallback, useContext, useEffect, useLayoutEffect, useMemo, useRef, useState } from 'react'
import type { ComponentType, CSSProperties, ReactNode } from 'react'
import type { Entity, NoteValues, ViewRow } from '../../../core/types.ts'
import { Checkbox } from './Notes.tsx'
import type { Edit, ShownRow } from '../state.ts'
import { KindsContext, Status } from './primitives.tsx'
import { Composer } from './primitives.tsx'
import type { OverviewProps } from './kindTypes.ts'
import type { ViewProps } from './types.ts'

/** Rows mounted beyond each edge of the viewport. */
const overscan = 8
/** The height assumed for a row not yet measured, in px. */
const estimate = 30
/** How far in from the edge a row the cursor moves to lands, as a share of the viewport. */
const margin = 0.3

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
  const Detail = kinds[root.type].Detail
  if (Detail) return <Detail {...props} entity={root} />
  const Overview = kinds[root.type].Overview
  const action = view.actions.find((offered) => offered.id === props.acting)
  return (
    <div className="pane">
      {props.interactive && props.find !== null && (
        <FindField find={props.find} focus={props.findFocus} onFind={props.onFind} onFocused={props.onFindFocused} />
      )}
      <TreeList props={props} root={root} Overview={Overview} />
      {view.loading && view.rows.length <= 1 && <div className="loading">Loading…</div>}
      <Status error={view.error} />
      {props.interactive && action && props.composing && (
        <Composer
          kind="action"
          placeholder={action.prompt}
          draft={props.draft}
          composing={props.composing}
          onDraft={props.onDraft}
          onCompose={props.onCompose}
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

const indent = (depth: number): CSSProperties => ({ paddingLeft: 10 + (depth - 1) * 20 })

/**
 * A child row: indented by depth, marked when it has children (open or
 * folded), then its type's `Row`. Given only what it uses, all of it stable,
 * so a row redraws only when it changes.
 */
const TreeRow = memo(function TreeRow(props: {
  shown: Extract<ShownRow, { kind: 'entity' }>
  interactive: boolean
  /** Only while this row is being edited. */
  edit: Edit | null
  onSelect(path: string[]): void
  onOpen(id: string): void
  onImage(ref: string | null): void
  onEditDraft(text: string): void
  onCommitEdit(): void
  onToggle?(path: string[]): void
  findText?: string
}) {
  const kinds = useContext(KindsContext)!
  const { row } = props.shown
  const Row = kinds[row.entity.type].Row
  return (
    <Selectable
      shown={props.shown}
      className={`tree${rowClass(row)}`}
      style={indent(row.depth)}
      onSelect={props.interactive ? props.onSelect : undefined}
    >
      <Marker row={row} onToggle={props.interactive ? props.onToggle : undefined} />
      <div className="cell">
        {props.shown.editing ? (
          <TextBox edit={props.edit} onEditDraft={props.onEditDraft} onCommitEdit={props.onCommitEdit} />
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
      </div>
    </Selectable>
  )
})

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
 * else its fold mark if it has children, else a bullet.
 */
function Marker(props: { row: ViewRow; onToggle?(path: string[]): void }) {
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
  return <span className="marker">{row.hasChildren ? (row.open ? '▾' : '▸') : <span className="bullet">•</span>}</span>
}

/** A row's box: true while open, false once ticked, null for none (a note's `open`, an old task's `done`). */
function boxOf(row: ViewRow): boolean | null {
  const { data, type } = row.entity
  if (type === 'task') return !data.done
  return typeof data.open === 'boolean' ? data.open : null
}

/** Typing in place: Enter (or leaving it) writes, Escape gives up. */
function EditBox(props: { props: ViewProps }) {
  return <TextBox edit={props.props.edit} onEditDraft={props.props.onEditDraft} onCommitEdit={props.props.onCommitEdit} />
}

function TextBox(props: { edit: Edit | null; onEditDraft(text: string): void; onCommitEdit(): void }) {
  const { edit, onEditDraft, onCommitEdit } = props
  return (
    <input
      className="edit"
      autoFocus
      spellCheck
      value={edit?.draft ?? ''}
      placeholder={edit?.mode === 'create' ? 'Note' : ''}
      onChange={(event) => onEditDraft(event.target.value)}
      onBlur={() => onCommitEdit()}
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
 * - The selected row is kept on screen, landing `margin` in from the edge.
 * - The row being typed into is pinned: mounted wherever it is, so scrolling
 *   away doesn't take the caret with it, and stuck to the view's bottom (or
 *   top) while its own place is out of sight.
 * - Scrolling near the end, or rows that don't fill the screen, walk further
 *   (`onNearEnd`). Chat reads bottom up, so its end is the top.
 * - Rows arriving above (chat growing upwards, guesses becoming heights) don't
 *   move what is on screen: the first visible row is kept where it was.
 */
function TreeList(props: { props: ViewProps; root: Entity; Overview: ComponentType<OverviewProps> }) {
  const view = props.props
  const { rows, keys, selectedIndex } = view.shown
  const atTop = view.view.startsAtEnd
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
      total += heights.get(keys[i]) ?? estimate
    }
    out[keys.length] = total
    return out
  }, [keys, heights])
  const total = offsets[keys.length]

  // Keep the first visible row where it was when rows above it change: the
  // anchor is recorded after each render and restored when the layout moves.
  const anchor = useRef<{ key: string; delta: number } | null>(null)
  useLayoutEffect(() => {
    const el = ref.current
    const held = anchor.current
    if (!el || !held) return
    const at = keys.indexOf(held.key)
    if (at < 0) return
    const want = offsets[at] + held.delta
    if (Math.abs(el.scrollTop - want) > 1) {
      el.scrollTop = want
      setScrollTop(el.scrollTop)
    }
  }, [keys, offsets])

  const offsetsRef = useRef(offsets)
  offsetsRef.current = offsets
  const reveal = useCallback((index: number) => {
    const el = ref.current
    if (!el || index < 0) return
    const o = offsetsRef.current
    const top = o[index]
    const bottom = o[index + 1]
    const height = el.clientHeight
    if (!height) return
    const room = Math.max(0, Math.min(height * margin, (height - (bottom - top)) / 2))
    if (top < el.scrollTop + room) el.scrollTop = Math.max(0, top - room)
    else if (bottom > el.scrollTop + height - room) el.scrollTop = bottom - height + room
    setScrollTop(el.scrollTop)
  }, [])
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

  const editIndex = rows.findIndex((row) => row.kind === 'input' || row.editing)

  const { onNearEnd } = view
  const near = (el: HTMLDivElement) =>
    atTop ? el.scrollTop <= estimate * overscan : el.scrollTop + el.clientHeight >= el.scrollHeight - estimate * overscan
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
  first = Math.max(0, first - overscan)
  let last = first
  while (last < rows.length && offsets[last] < bottomEdge) last++
  last = Math.min(rows.length, last + overscan)
  useLayoutEffect(() => {
    const el = ref.current
    anchor.current = el && keys[firstVisible] !== undefined ? { key: keys[firstVisible], delta: el.scrollTop - offsets[firstVisible] } : null
  })

  const render = (index: number) => {
    const shown = rows[index]
    return (
      <Measured key={shown.key} measureKey={shown.key} onMeasure={onMeasure}>
        {shown.kind === 'input' ? (
          <EditRow depth={shown.depth} values={shown.values} props={view} />
        ) : shown.row.depth === 0 ? (
          <Selectable shown={shown} className="overview" onSelect={view.interactive ? view.onSelect : undefined}>
            {shown.editing ? <EditBox props={view} /> : <props.Overview {...view} entity={props.root} findText={view.find ?? undefined} />}
          </Selectable>
        ) : (
          <TreeRow
            shown={shown}
            interactive={view.interactive}
            edit={shown.editing ? view.edit : null}
            onSelect={view.onSelect}
            onOpen={view.onOpen}
            onImage={view.onImage}
            onEditDraft={view.onEditDraft}
            onCommitEdit={view.onCommitEdit}
            onToggle={view.onToggle}
            findText={view.find ?? undefined}
          />
        )}
      </Measured>
    )
  }

  const slice: ReactNode[] = []
  for (let i = first; i < last; i++) {
    slice.push(i === editIndex ? <div key="\0edit-slot" style={{ height: offsets[i + 1] - offsets[i] }} /> : render(i))
  }

  return (
    <div
      ref={ref}
      className="list"
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
      <div className="rows" style={{ height: total }}>
        <div style={{ height: offsets[first] }} />
        {slice}
        {editIndex >= 0 && (
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
  )
}

/** Reports its height while mounted, so the list can lay out the rows around it. */
function Measured(props: { measureKey: string; onMeasure(key: string, height: number): void; children: ReactNode }) {
  const ref = useRef<HTMLDivElement>(null)
  const { measureKey, onMeasure } = props
  useLayoutEffect(() => {
    const el = ref.current
    if (!el) return
    const report = () => onMeasure(measureKey, el.offsetHeight)
    report()
    const observer = new ResizeObserver(report)
    observer.observe(el)
    return () => observer.disconnect()
  }, [measureKey, onMeasure])
  return <div ref={ref}>{props.children}</div>
}
