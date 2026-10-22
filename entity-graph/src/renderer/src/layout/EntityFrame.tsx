import React from 'react'
import { viewsFor } from '../components/types'
import { useEntitySummary, useLayoutState } from '../state/hooks'
import { Outline } from './Outline'

/**
 * One frame: the entity at its root, with the whole frame to itself — its
 * type's focus, or the outline where the type has none. Which it is follows the
 * root's `type`, so a note's frame is its outline and a file's is the file.
 */
export function EntityFrame({ frameId }: { frameId: string }): React.JSX.Element {
  const frame = useLayoutState().frames[frameId]
  if (!frame) return <div className="p-8 text-center text-[13px] text-gray-400">No frame.</div>
  return <Focused frameId={frameId} entityId={frame.rootId} />
}

/** Reading the root's type is what loads it, so only for a frame that has one. */
function Focused({ frameId, entityId }: { frameId: string; entityId: string }): React.JSX.Element {
  const { Focus = Outline } = viewsFor(useEntitySummary(entityId).type)
  return <Focus frameId={frameId} entityId={entityId} />
}
