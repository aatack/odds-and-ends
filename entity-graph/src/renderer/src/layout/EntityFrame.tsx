import React, { useEffect, useMemo, useRef } from 'react'
import { Editor } from '../views/Editor'
import { useEditor, type EditorActions } from '../views/useEditor'
import type { ContextMenuItem } from '../components/ui/ContextMenu'
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
  /** Report the root entity's display text (for tab / panel labels). */
  onRootName?: (id: string, text: string | undefined) => void
  /** Grow to fit rather than scroll within a fixed height (canvas auto nodes). */
  autoHeight?: boolean
  /** Extra right-click menu items (e.g. the canvas "Close panel"). */
  extraMenuItems?: ContextMenuItem[]
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
  onRootName,
  autoHeight,
  extraMenuItems,
}: EntityFrameProps): React.JSX.Element {
  const ed = useEditor({
    rootId: view.rootId,
    maxDepth: view.maxDepth ?? undefined,
    actions,
    onDebugEntity,
    forceCollapsed: collapsed,
  })

  // The selected row's text, and the root entity's text (the first row).
  const selectedRow = ed.rows.find((r) => r.kind === 'entity' && r.selected)
  const selectedText = selectedRow?.kind === 'entity' ? selectedRow.text : undefined
  const rootRow = ed.rows[0]
  const rootText = rootRow?.kind === 'entity' ? rootRow.text : undefined

  // Expose selection + action dispatch through refs so the handle stays stable
  // while always reading the latest values.
  const selectedRef = useRef(ed.selectedPath)
  selectedRef.current = ed.selectedPath
  const selectedTextRef = useRef(selectedText)
  selectedTextRef.current = selectedText
  const runRef = useRef(ed.runAction)
  runRef.current = ed.runAction

  const handle = useMemo<ViewHandle>(
    () => ({
      getSelectedEntityId: () => last(selectedRef.current) ?? null,
      getSelectedText: () => selectedTextRef.current ?? null,
      runAction: (id) => runRef.current(id),
    }),
    [],
  )

  useEffect(() => {
    if (!onHandle) return
    onHandle(handle)
    return () => onHandle(null)
  }, [onHandle, handle])

  useEffect(() => {
    onRootName?.(view.rootId, rootText)
  }, [onRootName, view.rootId, rootText])

  return (
    <Editor
      rows={ed.rows}
      loading={ed.loading}
      onSelectRow={ed.selectRow}
      onToggleCollapse={ed.toggleCollapse}
      onCommitEdit={ed.commitEdit}
      onCancelEdit={ed.cancelEdit}
      onExport={() => ed.runAction('export')}
      onDebug={() => ed.runAction('debug')}
      onNearEnd={ed.loadMore}
      codeRuns={ed.codeRuns}
      onRunCode={ed.runCode}
      onStopCode={ed.stopCode}
      onActivateRow={onActivateEntity ? (path) => onActivateEntity(last(path) as string) : undefined}
      autoHeight={autoHeight}
      extraMenuItems={extraMenuItems}
    />
  )
}
