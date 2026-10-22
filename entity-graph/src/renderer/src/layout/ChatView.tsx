import React, { useLayoutEffect, useRef } from 'react'
import type { Entity } from '../../../core/entity'
import { summaryOf } from '../../../core/entity'
import { TypeCard } from '../components/types'
import { cn } from '../helpers/cn'
import { useEntitySource, useLayoutState } from '../state/hooks'
import type { EntityRow } from '../state/derive'
import { currentUser } from '../source/transport'

/**
 * The chat view: the root's children, in order, as the bubbles of a
 * conversation, each drawn with its card. What the user wrote sits on the right
 * in a slight blue; everything else on the left. Short of full width, as a chat
 * is. It holds no selection; a bubble publishes its entity, so right-clicking it
 * opens the tool list on that message.
 */
export function ChatView({ frameId }: { frameId: string }): React.JSX.Element {
  const frame = useLayoutState().frames[frameId]
  const source = useEntitySource()
  const rootId = frame?.rootId ?? ''
  const ids = source.get([rootId])[rootId].outboundLinks
  const messages = source.get(ids)
  const me = currentUser()
  const scroller = useRef<HTMLDivElement>(null)

  // A conversation is read from the end: open at the latest message, and stay
  // there as new ones arrive.
  useLayoutEffect(() => {
    const el = scroller.current
    if (el) el.scrollTop = el.scrollHeight
  }, [ids.length])

  if (!frame) return <div className="p-8 text-center text-[13px] text-gray-400">No frame.</div>

  return (
    <div ref={scroller} className="h-full overflow-y-auto bg-white px-4 py-4">
      {ids.length === 0 ? (
        <p className="py-8 text-center text-[13px] text-gray-400">
          {source.pending(rootId) ? 'Loading…' : 'No messages.'}
        </p>
      ) : (
        <div className="flex flex-col gap-1.5">
          {ids.map((id, i) => (
            <Bubble
              key={`${id}:${i}`}
              entity={messages[id]}
              rootId={rootId}
              mine={messages[id].createdBy === me}
              loading={source.pending(id)}
            />
          ))}
        </div>
      )}
    </div>
  )
}

function Bubble({
  entity,
  rootId,
  mine,
  loading,
}: {
  entity: Entity
  rootId: string
  mine: boolean
  loading: boolean
}): React.JSX.Element {
  // A card is drawn from a row, so the message is made into one: a child of the
  // root, selected by nobody and edited by nobody.
  const row: EntityRow = {
    kind: 'entity',
    id: entity.id,
    ...summaryOf(entity.values),
    depth: 0,
    path: [rootId, entity.id],
    parentId: rootId,
    section: entity.values.section === true,
    open: typeof entity.values.open === 'boolean' ? entity.values.open : undefined,
    hasChildren: entity.outboundLinks.length > 0,
    collapsed: false,
    hidesChildren: false,
    loading,
    selected: false,
    editing: false,
  }
  return (
    <div
      data-entity-id={entity.id}
      data-parent-id={rootId}
      className={cn(
        'max-w-[80%] min-w-0 rounded-2xl px-3 py-1.5',
        mine ? 'self-end bg-brand-50' : 'self-start bg-gray-100',
      )}
    >
      <TypeCard row={row} onDraft={() => undefined} onCommit={() => undefined} />
    </div>
  )
}
