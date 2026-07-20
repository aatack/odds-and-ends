import React, { useMemo, useState } from 'react'
import type { ActiveSource } from '../../../core/client'
import type { QueryPage } from '../../../core/wrapper'
import type { EditorActions } from './useEditor'
import { Layout } from '../layout/Layout'
import { DebugModal } from '../components/DebugModal'
import { EntityDebugModal } from '../components/EntityDebugModal'
import type { Command } from '../components/CommandPalette'
import { Button } from '../components/ui/Button'

const api = window.entityGraph

interface Props {
  active: ActiveSource
  user: string
  /** Publish the view + layout palette commands to the app shell. */
  onRegisterCommands: (commands: Command[] | null) => void
}

/**
 * The open source's shell: builds the transport-bound {@link EditorActions} and
 * hands them to the {@link Layout}, which arranges the actual views (entity
 * trees, canvases) across tab groups. Also owns the raw debug modals. All data
 * flows through `sourceCall` on the active source — no local graph state.
 */
export function SourceView({ active, user, onRegisterCommands }: Props): React.JSX.Element {
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
      createCodeChild: (parentId, text) =>
        call('createEntity', { values: { text, type: 'code' }, parentId }) as Promise<string>,
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
        <Layout
          actions={actions}
          onDebugEntity={setDebugEntity}
          onRegisterCommands={onRegisterCommands}
        />
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
