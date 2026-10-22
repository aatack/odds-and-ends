import React from 'react'
import { viewsFor } from '../components/types'
import { useEntitySummary, useLayoutState } from '../state/hooks'
import type { FrameState } from '../state/types'
import { DefaultFocus } from './DefaultFocus'
import { ChatView } from './ChatView'
import { DetailView } from './DetailView'

/**
 * A frame, drawn as its frame type says. A tab's top frame and a peek's are
 * both drawn here, so a new frame type is one more case below.
 */
export function EntityFrame({ frameId }: { frameId: string }): React.JSX.Element {
  const frame: FrameState | undefined = useLayoutState().frames[frameId]
  if (!frame) return <div className="p-8 text-center text-[13px] text-gray-400">No frame.</div>
  switch (frame.type) {
    case 'focus':
      return <Focused frameId={frameId} entityId={frame.rootId} />
    case 'detail':
      return <DetailView frameId={frameId} />
    case 'chat':
      return <ChatView frameId={frameId} />
  }
}

/** A focus frame: the root's type's focus, or the default one's rows. */
function Focused({ frameId, entityId }: { frameId: string; entityId: string }): React.JSX.Element {
  const { Focus = DefaultFocus } = viewsFor(useEntitySummary(entityId).type)
  return <Focus frameId={frameId} entityId={entityId} />
}
