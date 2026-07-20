import React, { useEffect, useMemo, useRef } from 'react'
import { Editor } from '../views/Editor'
import { useEditor, type EditorActions } from '../views/useEditor'
import { last, type EntityView } from './types'
import type { ViewHandle } from './useLayout'

export interface EntityFrameProps {
  view: EntityView
  actions: EditorActions
  onDebugEntity: (entityId: string) => void
  /** Double-clicking a row activates it (open in a new frame / add to a canvas). */
  onActivateEntity?: (entityId: string) => void
  /** Extra entity ids to render collapsed — the canvas folds cross-references. */
  collapsed?: string[]
  /** Publish this view's imperative handle to the layout (keyed by the caller). */
  onHandle?: (handle: ViewHandle | null) => void
}

/**
 * A single entity view: the tree rooted at `view.rootId`, rendered through the
 * shared {@link useEditor} logic and dumb {@link Editor} component. When it's
 * the focused frame the layout reads its selection and routes editor hotkeys to
 * it via the handle it publishes here.
 */
export function EntityFrame({
  view,
  actions,
  onDebugEntity,
  onActivateEntity,
  collapsed,
  onHandle,
}: EntityFrameProps): React.JSX.Element {
  const ed = useEditor({
    rootId: view.rootId,
    maxDepth: view.maxDepth ?? undefined,
    actions,
    onDebugEntity,
    initialCollapsed: collapsed,
  })

  // Expose selection + action dispatch through refs so the handle stays stable
  // while always reading the latest values.
  const selectedRef = useRef(ed.selectedPath)
  selectedRef.current = ed.selectedPath
  const runRef = useRef(ed.runAction)
  runRef.current = ed.runAction

  const handle = useMemo<ViewHandle>(
    () => ({
      getSelectedEntityId: () => last(selectedRef.current) ?? null,
      runAction: (id) => runRef.current(id),
    }),
    [],
  )

  useEffect(() => {
    if (!onHandle) return
    onHandle(handle)
    return () => onHandle(null)
  }, [onHandle, handle])

  return (
    <Editor
      rows={ed.rows}
      loading={ed.loading}
      error={ed.error}
      statusMessage={ed.statusMessage}
      notice={ed.notice}
      onSelectRow={ed.selectRow}
      onToggleCollapse={ed.toggleCollapse}
      onCommitEdit={ed.commitEdit}
      onCancelEdit={ed.cancelEdit}
      onExport={() => ed.runAction('export')}
      onDebug={() => ed.runAction('debug')}
      onNearEnd={ed.loadMore}
      onActivateRow={onActivateEntity ? (path) => onActivateEntity(last(path) as string) : undefined}
    />
  )
}
