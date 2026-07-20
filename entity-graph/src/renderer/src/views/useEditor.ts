import { useCallback, useEffect, useId, useMemo, useRef, useState } from 'react'
import type { QueryPage, QueryResult, StackFrame } from '../../../core/wrapper'
import { EDITOR_ACTIONS, type EditorController } from '../actions/editorActions'
import { dismissToast, showToast } from '../components/ui/Toast'

// ---------------------------------------------------------------------------
// Public types
// ---------------------------------------------------------------------------

/**
 * Side-effecting actions the editor logic needs. Kept as an injected object so
 * the hook has no knowledge of the transport (IPC, http, …) — the caller wires
 * these to `window.entityGraph`.
 */
export interface EditorActions {
  resolveQuery: (
    rootId: string,
    opts: { maxDepth?: number; collapsed?: string[]; limit?: number; continuationStack?: StackFrame[] },
  ) => Promise<QueryPage>
  /** Set the `text` value of an entity. */
  writeText: (entityId: string, text: string) => Promise<void>
  /** Create a new entity with the given text as a child of `parentId`. Returns its id. */
  createChild: (parentId: string, text: string) => Promise<string>
  /** Unlink `child` from `oldParent` and link it under `newParent`. */
  moveEntity: (entityId: string, fromParent: string, toParent: string) => Promise<void>
  /** Add a directional link `source → dest` without removing anything. */
  linkEntities: (sourceId: string, destId: string) => Promise<void>
  /** Remove the link `parent → child`. */
  unlink: (parentId: string, childId: string) => Promise<void>
}

export interface UseEditorArgs {
  rootId: string
  maxDepth?: number
  actions: EditorActions
  /** Open the raw debug inspector for an entity (from the `debug` action). */
  onDebugEntity: (entityId: string) => void
  /**
   * Entity ids to start collapsed. The canvas uses this to fold the occurrences
   * of entities that appear as their own node elsewhere on the board.
   */
  initialCollapsed?: string[]
}

/** A rendered bullet backed by a real entity. */
export interface EntityRow {
  kind: 'entity'
  id: string
  /** Depth within the query (0 = root). */
  depth: number
  /** Path of entity ids from the root to this row (identifies the row uniquely). */
  path: string[]
  /** The entity's `text` value, or undefined when absent/empty. */
  text?: string
  hasChildren: boolean
  collapsed: boolean
  selected: boolean
  /** True while this row's text is being edited in place. */
  editing: boolean
}

/** A transient input row shown while creating a new child entity. */
export interface InputRow {
  kind: 'input'
  depth: number
}

export type EditorRow = EntityRow | InputRow

export interface UseEditorResult {
  rows: EditorRow[]
  loading: boolean
  /** Path of entity ids to the current selection (last element is the selected id). */
  selectedPath: string[]
  /** Run a registered editor action by id (shared with the command palette). */
  runAction: (id: string) => void
  /** Commit the in-place editor's value (writes an edit or creates a child). */
  commitEdit: (value: string) => void
  /** Abandon the in-place editor without writing. */
  cancelEdit: () => void
  selectRow: (path: string[]) => void
  toggleCollapse: (row: EntityRow) => void
  /** Called by the view when the scroll position nears the end. */
  loadMore: () => void
}

// Format the entity at `startIndex` and its visible descendants as nested
// markdown bullets. Children of collapsed rows aren't in `rows`, so they're
// naturally excluded. Depth sets the indent relative to the starting row.
function subtreeToMarkdown(rows: EditorRow[], startIndex: number): string {
  const start = rows[startIndex]
  if (!start || start.kind !== 'entity') return ''
  const lines: string[] = []
  for (let i = startIndex; i < rows.length; i++) {
    const row = rows[i]
    if (i > startIndex && row.kind === 'entity' && row.depth <= start.depth) break
    if (row.kind !== 'entity') continue
    lines.push(`${'  '.repeat(row.depth - start.depth)}- ${row.text ?? ''}`)
  }
  return lines.join('\n')
}

// ---------------------------------------------------------------------------
// Internals
// ---------------------------------------------------------------------------

const PAGE_SIZE = 200

type EditState = { mode: 'edit' | 'create'; path: string[] } | null
type PendingState = { kind: 'move' | 'link'; path: string[]; reverse: boolean } | null

const pathEq = (a: string[], b: string[]): boolean =>
  a.length === b.length && a.every((x, i) => x === b[i])

// ---------------------------------------------------------------------------
// Hook
// ---------------------------------------------------------------------------

/**
 * All of the editor's derived state and mutating actions. Holds only the latent
 * state (root, depth, collapsed set, selection path, transient edit/move modes)
 * and derives the flat row list from a resolved query page.
 */
export function useEditor({
  rootId,
  maxDepth,
  actions,
  onDebugEntity,
  initialCollapsed,
}: UseEditorArgs): UseEditorResult {
  // Latent state ------------------------------------------------------------
  // Kept in a ref so the reset effect can re-seed collapse without depending on
  // a fresh array identity every render.
  const initialCollapsedRef = useRef(initialCollapsed)
  const [collapsed, setCollapsed] = useState<Set<string>>(() => new Set(initialCollapsed))
  const [selectedPath, setSelectedPath] = useState<string[]>([])
  const [limit, setLimit] = useState(PAGE_SIZE)
  const [edit, setEdit] = useState<EditState>(null)
  const [pending, setPending] = useState<PendingState>(null)

  // Query state -------------------------------------------------------------
  // `results` accumulates across pages; `continuation` is the resume token for
  // the next page (null when the whole tree has been fetched).
  const [results, setResults] = useState<QueryResult[]>([])
  const [continuation, setContinuation] = useState<StackFrame[] | null>(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [reloadToken, setReloadToken] = useState(0)
  // Guards against overlapping fetches (onNearEnd can fire repeatedly).
  const fetching = useRef(false)

  const reload = useCallback(() => setReloadToken((t) => t + 1), [])

  // Stable-ish key so the fetch effect re-runs when the collapsed set changes.
  const collapsedKey = useMemo(() => [...collapsed].sort().join(','), [collapsed])

  // Reset everything when the starting entity changes.
  useEffect(() => {
    setSelectedPath(rootId ? [rootId] : [])
    setCollapsed(new Set(initialCollapsedRef.current))
    setLimit(PAGE_SIZE)
    setEdit(null)
    setPending(null)
  }, [rootId])

  // Fetch the first page from scratch whenever root/depth/collapsed change (or a
  // mutation bumps `reloadToken`). Later pages are appended by `loadMore`.
  useEffect(() => {
    if (!rootId) {
      setResults([])
      setContinuation(null)
      return
    }
    let cancelled = false
    fetching.current = true
    setLoading(true)
    setError(null)
    actions
      .resolveQuery(rootId, { maxDepth, collapsed: [...collapsed], limit })
      .then((p) => {
        if (cancelled) return
        setResults(p.results)
        setContinuation(p.continuationStack)
      })
      .catch((e) => {
        if (!cancelled) setError(e instanceof Error ? e.message : String(e))
      })
      .finally(() => {
        if (!cancelled) setLoading(false)
        fetching.current = false
      })
    return () => {
      cancelled = true
    }
    // collapsedKey stands in for `collapsed`; actions/maxDepth are stable per view.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [rootId, maxDepth, collapsedKey, limit, reloadToken])

  // Derived rows ------------------------------------------------------------
  const rows = useMemo<EditorRow[]>(() => {
    const out: EditorRow[] = []
    const stack: string[] = []
    for (const { entity, depth } of results) {
      stack.length = depth
      stack.push(entity.id)
      const path = stack.slice()
      const text = entity.values.text
      out.push({
        kind: 'entity',
        id: entity.id,
        depth,
        path,
        text: text == null || text === '' ? undefined : String(text),
        hasChildren: entity.outboundLinks.length > 0,
        collapsed: collapsed.has(entity.id),
        selected: pathEq(path, selectedPath),
        editing: edit?.mode === 'edit' && pathEq(path, edit.path),
      })
    }

    // Splice the "new child" input in after the parent's entire subtree.
    if (edit?.mode === 'create') {
      const pi = out.findIndex((r) => r.kind === 'entity' && pathEq(r.path, edit.path))
      if (pi >= 0) {
        const parentDepth = out[pi].depth
        let ins = pi + 1
        while (ins < out.length && out[ins].depth > parentDepth) ins++
        out.splice(ins, 0, { kind: 'input', depth: parentDepth + 1 })
      }
    }
    return out
  }, [results, collapsed, selectedPath, edit])

  // Selection helpers -------------------------------------------------------
  const entityRows = useMemo(
    () => rows.filter((r): r is EntityRow => r.kind === 'entity'),
    [rows],
  )
  const selectedId = selectedPath[selectedPath.length - 1]

  const moveSelection = useCallback(
    (delta: number) => {
      if (entityRows.length === 0) return
      const idx = entityRows.findIndex((r) => r.selected)
      if (idx < 0) {
        setSelectedPath(entityRows[0].path)
        return
      }
      const next = idx + delta
      if (next >= 0 && next < entityRows.length) setSelectedPath(entityRows[next].path)
    },
    [entityRows],
  )

  const selectParent = useCallback(() => {
    setSelectedPath((p) => (p.length > 1 ? p.slice(0, -1) : p))
  }, [])

  const collapseSelected = useCallback(() => {
    if (!selectedId) return
    setCollapsed((prev) => (prev.has(selectedId) ? prev : new Set(prev).add(selectedId)))
  }, [selectedId])

  const expandSelected = useCallback(() => {
    if (!selectedId) return
    setCollapsed((prev) => {
      if (!prev.has(selectedId)) return prev
      const n = new Set(prev)
      n.delete(selectedId)
      return n
    })
  }, [selectedId])

  // Editing / writing -------------------------------------------------------
  const startEdit = useCallback(() => {
    if (selectedPath.length) setEdit({ mode: 'edit', path: selectedPath })
  }, [selectedPath])

  const startCreate = useCallback(() => {
    if (!selectedId) return
    // Make sure the parent is expanded so the new child is visible.
    setCollapsed((prev) => {
      if (!prev.has(selectedId)) return prev
      const n = new Set(prev)
      n.delete(selectedId)
      return n
    })
    setEdit({ mode: 'create', path: selectedPath })
  }, [selectedId, selectedPath])

  const commitEdit = useCallback(
    (value: string) => {
      const cur = edit
      setEdit(null)
      if (!cur) return
      if (cur.mode === 'edit') {
        const id = cur.path[cur.path.length - 1]
        actions.writeText(id, value).then(reload).catch(setErrorMsg)
      } else {
        // Committing an empty create (e.g. blurring the input) is a no-op rather
        // than spawning a blank entity.
        if (!value.trim()) return
        const parentId = cur.path[cur.path.length - 1]
        actions
          .createChild(parentId, value)
          .then((newId) => {
            setSelectedPath([...cur.path, newId])
            reload()
          })
          .catch(setErrorMsg)
      }
    },
    [edit, actions, reload],
  )

  const cancelEdit = useCallback(() => setEdit(null), [])

  // Moving / linking --------------------------------------------------------
  const unlinkSelected = useCallback(() => {
    if (selectedPath.length < 2) return
    const child = selectedPath[selectedPath.length - 1]
    const parent = selectedPath[selectedPath.length - 2]
    actions
      .unlink(parent, child)
      .then(() => {
        setSelectedPath((p) => p.slice(0, -1))
        reload()
      })
      .catch(setErrorMsg)
  }, [selectedPath, actions, reload])

  const toggleMove = useCallback(() => {
    if (pending?.kind === 'move') {
      const moving = pending.path
      const to = selectedPath[selectedPath.length - 1]
      setPending(null)
      if (moving.length < 2 || !to) return
      const entity = moving[moving.length - 1]
      const from = moving[moving.length - 2]
      if (to !== from) actions.moveEntity(entity, from, to).then(reload).catch(setErrorMsg)
    } else if (selectedPath.length >= 2) {
      setPending({ kind: 'move', path: selectedPath, reverse: false })
    }
  }, [pending, selectedPath, actions, reload])

  const toggleLink = useCallback(
    (reverse: boolean) => {
      if (pending?.kind === 'link') {
        const first = pending.path[pending.path.length - 1]
        const second = selectedPath[selectedPath.length - 1]
        const rev = pending.reverse
        setPending(null)
        if (!first || !second) return
        const source = rev ? second : first
        const dest = rev ? first : second
        actions.linkEntities(source, dest).then(reload).catch(setErrorMsg)
      } else if (selectedPath.length) {
        setPending({ kind: 'link', path: selectedPath, reverse })
      }
    },
    [pending, selectedPath, actions, reload],
  )

  const cancelPending = useCallback(() => setPending(null), [])

  const exportSelected = useCallback(() => {
    const index = rows.findIndex((r) => r.kind === 'entity' && pathEq(r.path, selectedPath))
    if (index < 0) return
    const md = subtreeToMarkdown(rows, index)
    void navigator.clipboard.writeText(md)
    const count = md ? md.split('\n').length : 0
    showToast({ message: `Copied ${count} item${count === 1 ? '' : 's'} to the clipboard`, variant: 'success' })
  }, [rows, selectedPath])

  const debugSelected = useCallback(() => {
    if (selectedId) onDebugEntity(selectedId)
  }, [selectedId, onDebugEntity])

  function setErrorMsg(e: unknown) {
    setError(e instanceof Error ? e.message : String(e))
  }

  // Actions & key handling --------------------------------------------------
  // The controller is the seam between the action registry and this hook's
  // state. It's rebuilt each render and held in a ref so the key handler and
  // `runAction` stay stable while always seeing the latest closures.
  const controller: EditorController = {
    moveSelection,
    selectParent,
    collapseSelected,
    expandSelected,
    startEdit,
    startCreate,
    unlinkSelected,
    toggleMove,
    toggleLink,
    cancelPending,
    exportSelected,
    debugSelected,
  }
  const controllerRef = useRef(controller)
  controllerRef.current = controller

  const runAction = useCallback((id: string) => {
    EDITOR_ACTIONS.find((a) => a.id === id)?.run(controllerRef.current)
  }, [])

  // Hotkeys are no longer bound here. With many editors mounted at once (one per
  // frame, plus every canvas panel), a per-editor listener would fire in all of
  // them at once. The layout instead binds the editor keys once at the top and
  // routes them to the focused frame's `runAction`.

  const selectRow = useCallback((path: string[]) => setSelectedPath(path), [])

  const toggleCollapse = useCallback((row: EntityRow) => {
    setSelectedPath(row.path)
    setCollapsed((prev) => {
      const n = new Set(prev)
      if (n.has(row.id)) n.delete(row.id)
      else n.add(row.id)
      return n
    })
  }, [])

  // Fetch and append the next page when the view nears the end. Uses the
  // `continuationStack` resume token so already-fetched rows aren't re-queried.
  const loadMore = useCallback(() => {
    if (fetching.current || !continuation || !rootId) return
    fetching.current = true
    setLoading(true)
    actions
      .resolveQuery(rootId, { maxDepth, collapsed: [...collapsed], limit, continuationStack: continuation })
      .then((p) => {
        setResults((prev) => [...prev, ...p.results])
        setContinuation(p.continuationStack)
      })
      .catch((e) => setError(e instanceof Error ? e.message : String(e)))
      .finally(() => {
        setLoading(false)
        fetching.current = false
      })
  }, [continuation, rootId, maxDepth, collapsed, limit, actions])

  const statusMessage = useMemo(() => {
    if (!pending) return null
    return pending.kind === 'move'
      ? 'Moving — select the new parent and press x again (Esc to cancel)'
      : `Linking${pending.reverse ? ' (reversed)' : ''} — select the target and press r again (Esc to cancel)`
  }, [pending])

  // Surface transient messages as toasts rather than in-tree banners. Errors
  // pop briefly; the pending move/link prompt is a sticky toast (keyed per hook
  // instance) that lives for as long as the operation is in progress.
  useEffect(() => {
    if (error) showToast({ message: error, variant: 'error' })
  }, [error])

  const statusToastId = useId()
  useEffect(() => {
    if (statusMessage) showToast({ id: statusToastId, message: statusMessage, sticky: true })
    else dismissToast(statusToastId)
    return () => dismissToast(statusToastId)
  }, [statusMessage, statusToastId])

  return {
    rows,
    loading,
    selectedPath,
    runAction,
    commitEdit,
    cancelEdit,
    selectRow,
    toggleCollapse,
    loadMore,
  }
}
