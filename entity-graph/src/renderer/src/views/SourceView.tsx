import React, { useMemo, useState } from 'react'
import type { ActiveSource } from '../../../core/client'
import type { QueryPage } from '../../../core/wrapper'
import { EditorView } from './Editor'
import type { EditorActions } from './useEditor'
import { DebugModal } from '../components/DebugModal'
import { EntityDebugModal } from '../components/EntityDebugModal'
import { Button } from '../components/ui/Button'

const api = window.entityGraph

/** The well-known root entity every source tree starts from. */
const ROOT_ID = '@index'

interface Props {
  active: ActiveSource
  user: string
}

/**
 * The open source's main view: the entity tree rooted at `@index`, plus a
 * top-right button that opens the raw debug editor in a modal. All data flows
 * through `sourceCall` on the active source — the app holds no local state
 * about the graph.
 */
export function SourceView({ active, user }: Props): React.JSX.Element {
  const [debug, setDebug] = useState(false)
  const [debugEntity, setDebugEntity] = useState<string | null>(null)

  // The tree's transport seam. Bound to the source's tools; the hook stays
  // ignorant of HTTP/IPC.
  const actions = useMemo<EditorActions>(() => {
    const call = (tool: string, args: unknown) => api.sourceCall(active.id, tool, args)
    return {
      resolveQuery: (rootId, opts) => call('query', { rootId, ...opts }) as Promise<QueryPage>,
      writeText: (entityId, text) =>
        call('writeValue', { entityId, key: 'text', value: text, author: user }).then(() => undefined),
      createChild: (parentId, text) =>
        call('createEntity', { values: { text }, parentId }) as Promise<string>,
      moveEntity: (entityId, from, to) =>
        call('moveEntity', { entityId, fromParentId: from, toParentId: to }).then(() => undefined),
      linkEntities: (sourceId, destId) =>
        call('writeLink', { sourceId, destinationId: destId, action: 0, author: user }).then(() => undefined),
      unlink: (parentId, childId) =>
        call('writeLink', { sourceId: parentId, destinationId: childId, action: 1, author: user }).then(() => undefined),
    }
  }, [active.id, user])

  return (
    <div className="relative flex flex-col h-full">
      <div className="flex-1 min-h-0">
        <EditorView rootId={ROOT_ID} actions={actions} onDebugEntity={setDebugEntity} />
      </div>

      {/* Debug lives in an unobtrusive corner button rather than a header bar. */}
      <Button
        variant="tertiary"
        size="sm"
        className="absolute bottom-4 right-4 opacity-60 hover:opacity-100"
        onClick={() => setDebug(true)}
      >
        Debug
      </Button>

      {debug && <DebugModal sourceId={active.id} user={user} onClose={() => setDebug(false)} />}
      {debugEntity && (
        <EntityDebugModal
          sourceId={active.id}
          entityId={debugEntity}
          user={user}
          onClose={() => setDebugEntity(null)}
        />
      )}
    </div>
  )
}
