import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import type { QueryPage, QueryResult, StackFrame } from '../../../core/wrapper'

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
  error: string | null
  /** True while any in-place text input is open. */
  editing: boolean
  /** Human-readable hint for an in-progress move/link, else null. */
  statusMessage: string | null
  /** Global key handler for the scroll container. */
  onContainerKeyDown: (e: React.KeyboardEvent) => void
  /** Key handler for the in-place text input (edit & create). */
  onEditKeyDown: (e: React.KeyboardEvent<HTMLInputElement>) => void
  selectRow: (path: string[]) => void
  toggleCollapse: (row: EntityRow) => void
  /** Called by the view when the scroll position nears the end. */
  loadMore: () => void
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
export function useEditor({ rootId, maxDepth, actions }: UseEditorArgs): UseEditorResult {
  // Latent state ------------------------------------------------------------
  const [collapsed, setCollapsed] = useState<Set<string>>(new Set())
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
    setCollapsed(new Set())
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

  const onEditKeyDown = useCallback(
    (e: React.KeyboardEvent<HTMLInputElement>) => {
      // Keep keystrokes from reaching the container's navigation handler.
      e.stopPropagation()
      if (e.key === 'Escape') {
        e.preventDefault()
        setEdit(null)
        return
      }
      if (e.key !== 'Enter') return
      e.preventDefault()
      const value = e.currentTarget.value
      const cur = edit
      setEdit(null)
      if (!cur) return
      if (cur.mode === 'edit') {
        const id = cur.path[cur.path.length - 1]
        actions.writeText(id, value).then(reload).catch(setErrorMsg)
      } else {
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

  function setErrorMsg(e: unknown) {
    setError(e instanceof Error ? e.message : String(e))
  }

  // Global key handling -----------------------------------------------------
  const onContainerKeyDown = useCallback(
    (e: React.KeyboardEvent) => {
      if (edit) return // the in-place input owns the keyboard while editing
      // Leave modifier combos (e.g. Ctrl/⌘+R reload, devtools) to the app shell.
      if (e.ctrlKey || e.metaKey || e.altKey) return
      switch (e.key) {
        case 'w':
          e.preventDefault()
          moveSelection(-1)
          break
        case 's':
          e.preventDefault()
          moveSelection(1)
          break
        case 'a':
          e.preventDefault()
          selectParent()
          break
        case 'ArrowLeft':
          e.preventDefault()
          collapseSelected()
          break
        case 'ArrowRight':
          e.preventDefault()
          expandSelected()
          break
        case 'e':
          e.preventDefault()
          startEdit()
          break
        case 'Enter':
          e.preventDefault()
          startCreate()
          break
        case 'Backspace':
        case 'Delete':
          e.preventDefault()
          unlinkSelected()
          break
        case 'x':
          e.preventDefault()
          toggleMove()
          break
        case 'Escape':
          e.preventDefault()
          setPending(null)
          break
        default:
          if (e.key === 'r' || e.key === 'R') {
            e.preventDefault()
            toggleLink(e.shiftKey)
          }
      }
    },
    [
      edit,
      moveSelection,
      selectParent,
      collapseSelected,
      expandSelected,
      startEdit,
      startCreate,
      unlinkSelected,
      toggleMove,
      toggleLink,
    ],
  )

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

  return {
    rows,
    loading,
    error,
    editing: edit !== null,
    statusMessage,
    onContainerKeyDown,
    onEditKeyDown,
    selectRow,
    toggleCollapse,
    loadMore,
  }
}
