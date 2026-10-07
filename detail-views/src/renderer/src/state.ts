import type { AppEvent } from '../../core/graph/events.ts'
import type { NoteValues, View, ViewRow } from '../../core/types.ts'

/**
 * Latent UI state: the minimum, and serialisable. Nothing derivable lives here
 * (no rows, no resolved selection, no entity text) and nothing cached (entities
 * are the session's `EntityCache`). Everything shown is derived from this plus
 * the cache by pure functions below, and **a derived value is never written
 * back**: the resolved selection, for one, is worked out afresh each time, so a
 * selection inside a row that is folded comes back when it is unfolded.
 */
export interface State {
  /** Views pushed, oldest first: each is the id of the entity it is rooted at. `at` is the one on screen. */
  trail: string[]
  at: number
  /**
   * The selection in each view, by its root: a path from that root. It may
   * name a row that isn't there (folded, not loaded yet); `resolveSelection`
   * says which row is actually selected.
   */
  selections: Record<string, string[]>
  /** Rows I have opened (true) or folded (false), by entity id, over their type's default. */
  folds: Record<string, boolean>
  /** Text being typed into a row, in place: editing its text, or a new note under it. Persisted, so a draft survives a reload. */
  edit: Edit | null
  /**
   * What each view is filtered to, by its root: null while it isn't, a string
   * (empty included) while the find field is open.
   */
  finds: Record<string, string | null>
  /**
   * Actions undone, newest last: the only copy of their events (undo takes
   * them off the store), so this is latent and persisted, not history.
   * Any other write clears it, since redoing these would land them after it.
   */
  undone: AppEvent[][]
  /** Prompt text for an action, per view. */
  drafts: Record<string, string>
  /** Whether the action prompt has the keyboard. Not persisted. */
  composing: boolean
  /** An action started on the view's root and waiting for Enter. Not persisted. */
  acting: string | null
  /** An image shown full size over everything, by its ref. Not persisted. */
  viewing: string | null
  /**
   * A move or link waiting for its other end: started on one row, finished by
   * pressing the same key on another (in any view). Not persisted.
   */
  picking: Pick | null
  /**
   * Floating windows peeking at a link or an entity, bottom first. Transient
   * ones follow the hover and stack: one opened from inside another is its
   * child. Pinned ones stay, and are persisted.
   */
  peeks: Peek[]
}

export interface Edit {
  /** The view it is in. */
  root: string
  /** The row: edited in `edit` mode; the parent of the new note in `create`. */
  path: string[]
  mode: 'edit' | 'create'
  draft: string
  /** For a new note: a heading, or a checkbox. */
  values?: NoteValues
}

export type PickTool = 'move' | 'link' | 'linkReverse'

export interface Pick {
  tool: PickTool
  /** The row it started on, as a path in its view: for a move, its parent is where it leaves. */
  path: string[]
}

export interface Rect {
  x: number
  y: number
  width: number
  height: number
}

export type PeekTarget = { kind: 'url'; url: string } | { kind: 'entity'; id: string }

export interface Peek {
  key: string
  target: PeekTarget
  /** Where the hover was; a transient peek is placed beside it. */
  anchor: Rect
  /** Set once the window is moved or resized, which also pins it. */
  rect: Rect | null
  pinned: boolean
  /** The peek this one was opened from, or null for the main view. */
  parent: string | null
}

export function sameTarget(a: PeekTarget, b: PeekTarget): boolean {
  return a.kind === 'url' ? b.kind === 'url' && a.url === b.url : b.kind === 'entity' && a.id === b.id
}

export function transientPeek(state: State): Peek | null {
  return state.peeks.findLast((peek) => !peek.pinned) ?? null
}

/** A peek and the peeks it was opened from, by key. */
export function peekChain(state: State, key: string | null): Set<string> {
  const chain = new Set<string>()
  let current = key
  while (current && !chain.has(current)) {
    chain.add(current)
    current = state.peeks.find((peek) => peek.key === current)?.parent ?? null
  }
  return chain
}

/**
 * Opens a transient peek on top of the one it came from. Transient peeks off
 * that line (siblings and their children) close; pinned ones stay.
 */
export function showPeek(state: State, peek: Peek): State {
  const keep = peekChain(state, peek.parent)
  return { ...state, peeks: [...state.peeks.filter((other) => other.pinned || keep.has(other.key)), peek] }
}

/** Closes transient peeks the pointer is no longer in or under. */
export function keepHovered(state: State, hovering: string | null): State {
  const keep = peekChain(state, hovering)
  const peeks = state.peeks.filter((peek) => peek.pinned || keep.has(peek.key))
  return peeks.length === state.peeks.length ? state : { ...state, peeks }
}

/** Closes one peek, and any transient ones stacked on it. `null` closes the top transient one. */
export function closePeek(state: State, key: string | null): State {
  const target = key ?? transientPeek(state)?.key
  if (!target) return state
  const peeks = state.peeks.filter((peek) => peek.key !== target && !(!peek.pinned && peekChain(state, peek.key).has(target)))
  return peeks.length === state.peeks.length ? state : { ...state, peeks }
}

/** Moving or resizing pins a peek and brings it to the front. */
export function placePeek(state: State, key: string, rect: Rect): State {
  const peek = state.peeks.find((candidate) => candidate.key === key)
  if (!peek) return state
  return { ...state, peeks: [...state.peeks.filter((other) => other !== peek), { ...peek, rect, pinned: true }] }
}

export function raisePeek(state: State, key: string): State {
  const peek = state.peeks.find((candidate) => candidate.key === key)
  if (!peek || state.peeks.at(-1) === peek) return state
  return { ...state, peeks: [...state.peeks.filter((other) => other !== peek), peek] }
}

const trailLimit = 200

export function initialState(root: string): State {
  return {
    trail: [root],
    at: 0,
    selections: {},
    folds: {},
    edit: null,
    finds: {},
    undone: [],
    drafts: {},
    composing: false,
    acting: null,
    viewing: null,
    picking: null,
    peeks: [],
  }
}

export function focused(state: State): string {
  return state.trail[state.at]
}

// --- Rows -------------------------------------------------------------------

/** A row of a view as shown: an entity's row, or the box a new note is typed into. */
export type ShownRow =
  | { kind: 'entity'; key: string; row: ViewRow; selected: boolean; editing: boolean }
  | { kind: 'input'; key: string; depth: number; parent: string[]; values?: NoteValues }

export interface ShownView {
  rows: ShownRow[]
  /** The selection in effect, resolved against the rows. Never stored. */
  selectedPath: string[]
  /** Its index in `rows`, or -1 when nothing is selected. */
  selectedIndex: number
}

const keyOf = (path: readonly string[]): string => path.join('\0')

/**
 * The selection in effect. A stored path that is one of the rows is it; one
 * that isn't is cut back until it is (its row folded or gone), which is never
 * written back. With nothing stored, a view that reads bottom up starts at its
 * last row, any other at its first child, or its root when it has none.
 */
export function resolveSelection(view: View, stored: string[] | undefined): string[] {
  const keys = new Set(view.rows.map((row) => row.key))
  if (stored?.length) {
    let path = stored
    while (path.length > 1 && !keys.has(keyOf(path))) path = path.slice(0, -1)
    // A path still loading may name a row on its way: keep it rather than snap to the root.
    if (keys.has(keyOf(path)) && (path.length === stored.length || !view.loading)) return path
    if (view.loading) return stored
  }
  const fallback = view.startsAtEnd ? view.rows[view.rows.length - 1] : (view.rows[1] ?? view.rows[0])
  return fallback?.path ?? []
}

/**
 * A view's rows with what the UI knows laid over them: which is selected,
 * which is being typed into, and where the box for a new note goes (after its
 * parent's subtree). Rows the same as in `previous` are handed back as they
 * were, so a cursor move changes two rows and memoised rows don't redraw.
 */
export function markRows(view: View, state: State, root: string, previous?: ShownView): ShownView {
  const selectedPath = resolveSelection(view, state.selections[root])
  const selectedKey = keyOf(selectedPath)
  const edit = state.edit?.root === root ? state.edit : null
  const editKey = edit ? keyOf(edit.path) : null
  const before = new Map<string, ShownRow>()
  for (const row of previous?.rows ?? []) before.set(row.key, row)
  const rows: ShownRow[] = view.rows.map((row) => {
    const selected = row.key === selectedKey
    const editing = edit?.mode === 'edit' && row.key === editKey
    const known = before.get(row.key)
    if (known?.kind === 'entity' && known.row === row && known.selected === selected && known.editing === editing) return known
    return { kind: 'entity', key: row.key, row, selected, editing }
  })
  if (edit?.mode === 'create') {
    const at = rows.findIndex((row) => row.key === editKey)
    if (at >= 0) {
      const depth = view.rows[at].depth
      let insert = at + 1
      while (insert < rows.length && rows[insert].kind === 'entity' && (rows[insert] as { row: ViewRow }).row.depth > depth) insert++
      rows.splice(insert, 0, { kind: 'input', key: `\0new\0${editKey}`, depth: depth + 1, parent: edit.path, values: edit.values })
    }
  }
  return { rows, selectedPath, selectedIndex: rows.findIndex((row) => row.kind === 'entity' && row.selected) }
}

/** The entity row under the selection, if any. */
export function selectedRow(shown: ShownView): ViewRow | null {
  const at = shown.rows[shown.selectedIndex]
  return at?.kind === 'entity' ? at.row : null
}

/**
 * A view's rows kept by its find: rows whose text says it, and the rows above
 * them, so the tree still reads. The root always stays. Applied after the
 * walk, to what the walk reached: a folded row's children aren't searched.
 */
export function filterView(view: View, find: string | null | undefined): View {
  const needle = find?.trim().toLowerCase()
  if (!needle) return view
  const keep = new Set<string>()
  for (const row of view.rows) {
    if (!String(row.entity.data.text ?? '').toLowerCase().includes(needle)) continue
    for (let i = 1; i <= row.path.length; i++) keep.add(keyOf(row.path.slice(0, i)))
  }
  return { ...view, rows: view.rows.filter((row) => row.depth === 0 || keep.has(row.key)) }
}

// --- Reducers ---------------------------------------------------------------

export function navigate(state: State, id: string): State {
  if (focused(state) === id) return state
  const trail = [...state.trail.slice(0, state.at + 1), id].slice(-trailLimit)
  return { ...state, trail, at: trail.length - 1, composing: false, acting: null, viewing: null, edit: null, peeks: state.peeks.filter((peek) => peek.pinned) }
}

export function back(state: State): State {
  return state.at > 0 ? { ...state, at: state.at - 1, composing: false, acting: null, edit: null } : state
}

/** Back to a view further down the stack (a breadcrumb). */
export function goTo(state: State, at: number): State {
  return at >= 0 && at < state.at ? { ...state, at, composing: false, acting: null, edit: null } : state
}

export function forward(state: State): State {
  return state.at < state.trail.length - 1 ? { ...state, at: state.at + 1, composing: false, acting: null, edit: null } : state
}

export function select(state: State, path: string[]): State {
  return { ...state, selections: { ...state.selections, [focused(state)]: path } }
}

/** Steps the selection through the view's entity rows. */
export function move(state: State, shown: ShownView, delta: number): State {
  const entities = shown.rows.filter((row) => row.kind === 'entity')
  if (!entities.length) return state
  const at = entities.findIndex((row) => row.selected)
  const index = Math.max(0, Math.min(entities.length - 1, (at < 0 ? 0 : at) + delta))
  return select(state, (entities[index] as { row: ViewRow }).row.path)
}

export function fold(state: State, id: string, open: boolean): State {
  return state.folds[id] === open ? state : { ...state, folds: { ...state.folds, [id]: open } }
}

export function startEdit(state: State, path: string[], text: string): State {
  return { ...state, edit: { root: focused(state), path, mode: 'edit', draft: text } }
}

export function startCreate(state: State, path: string[], values?: NoteValues): State {
  return { ...state, edit: { root: focused(state), path, mode: 'create', draft: '', values } }
}

export function setEditDraft(state: State, draft: string): State {
  return state.edit ? { ...state, edit: { ...state.edit, draft } } : state
}

export function endEdit(state: State): State {
  return state.edit ? { ...state, edit: null } : state
}

export function setFind(state: State, find: string | null): State {
  return { ...state, finds: { ...state.finds, [focused(state)]: find } }
}

export function pushUndone(state: State, events: AppEvent[]): State {
  return { ...state, undone: [...state.undone, events].slice(-50) }
}

export function popUndone(state: State): State {
  return { ...state, undone: state.undone.slice(0, -1) }
}

export function clearUndone(state: State): State {
  return state.undone.length ? { ...state, undone: [] } : state
}

export function startPick(state: State, tool: PickTool, path: string[]): State {
  return { ...state, picking: { tool, path } }
}

export function endPick(state: State): State {
  return state.picking ? { ...state, picking: null } : state
}

export function setDraft(state: State, text: string): State {
  const drafts = { ...state.drafts }
  if (text) drafts[draftKey(state)] = text
  else delete drafts[draftKey(state)]
  return { ...state, drafts }
}

export function view(state: State, ref: string | null): State {
  return state.viewing === ref ? state : { ...state, viewing: ref }
}

export function setComposing(state: State, composing: boolean): State {
  if (state.composing === composing) return state
  // Leaving the prompt abandons the action.
  return { ...state, composing, acting: composing ? state.acting : null }
}

export function startAction(state: State, action: string): State {
  return { ...state, acting: action, composing: true }
}

/** Drafts for an action are kept apart per view and action. */
export function draftKey(state: State): string {
  return state.acting ? `${focused(state)}#${state.acting}` : focused(state)
}

/** What is kept across reloads. */
export function persisted(state: State): Omit<State, 'composing' | 'viewing' | 'acting' | 'picking'> {
  const { composing: _, viewing: __, acting: ___, picking: ____, ...rest } = state
  return { ...rest, peeks: rest.peeks.filter((peek) => peek.pinned) }
}

export function restore(saved: unknown, root: string): State {
  const value = saved as Partial<State> | undefined
  if (!value || !Array.isArray(value.trail) || value.trail.length === 0) return initialState(root)
  return {
    ...initialState(root),
    trail: value.trail,
    at: Math.min(Math.max(value.at ?? 0, 0), value.trail.length - 1),
    selections: value.selections ?? {},
    folds: value.folds ?? {},
    edit: value.edit ?? null,
    finds: value.finds ?? {},
    undone: Array.isArray(value.undone) ? value.undone : [],
    drafts: value.drafts ?? {},
    peeks: Array.isArray(value.peeks) ? value.peeks.filter((peek) => peek.pinned).map((peek) => ({ ...peek, parent: peek.parent ?? null })) : [],
  }
}
