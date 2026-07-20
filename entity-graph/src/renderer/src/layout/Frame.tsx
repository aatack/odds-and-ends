import React, { useCallback } from 'react'
import type { EditorActions } from '../views/useEditor'
import { Canvas } from './Canvas'
import { EntityFrame } from './EntityFrame'
import type { Frame, View } from './types'
import type { ViewHandle } from './useLayout'

export interface FrameViewProps {
  frame: Frame
  actions: EditorActions
  onDebugEntity: (entityId: string) => void
  registerHandle: (frameId: string, handle: ViewHandle | null) => void
  pushEntityFrame: (tabId: string, entityId: string) => void
  updateView: (frameId: string, view: View) => void
  reportName: (id: string, text: string | undefined) => void
}

/** Renders a frame's view, dispatching on its kind. */
export function FrameView({
  frame,
  actions,
  onDebugEntity,
  registerHandle,
  pushEntityFrame,
  updateView,
  reportName,
}: FrameViewProps): React.JSX.Element {
  const onHandle = useCallback(
    (h: ViewHandle | null) => registerHandle(frame.id, h),
    [registerHandle, frame.id],
  )

  if (frame.view.kind === 'canvas') {
    return (
      <Canvas
        frame={frame}
        actions={actions}
        onDebugEntity={onDebugEntity}
        updateView={updateView}
        onHandle={onHandle}
      />
    )
  }

  return (
    <EntityFrame
      view={frame.view}
      actions={actions}
      onDebugEntity={onDebugEntity}
      onActivateEntity={(id) => pushEntityFrame(frame.tabId, id)}
      onHandle={onHandle}
      onRootName={reportName}
    />
  )
}
