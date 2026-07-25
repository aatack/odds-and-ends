import React, { useEffect } from 'react'
import type { ActiveSource } from '../../../core/client'
import { Layout } from '../layout/Layout'
import { DebugModal } from '../components/DebugModal'
import { EntityDebugModal } from '../components/EntityDebugModal'
import { ResourceModal } from '../components/Resource'
import { Button } from '../components/ui/Button'
import { useUi } from '../state/hooks'
import { setQueryFetcher } from '../state/query'
import { setResourceFetcher } from '../state/resources'
import { updateUi } from '../state/ui'
import { query, readResource } from '../source/entity'
import { setSourceTransport } from '../source/transport'
import { setIntegrationServer } from '../tools/integrationTools'

const api = window.entityGraph

/**
 * The open source's shell. Its whole job is to point the transport at the source
 * and let the query engine drive: everything below reads state, so no data flows
 * through this component.
 */
export function SourceView({
  active,
  user,
}: {
  active: ActiveSource
  user: string
}): React.JSX.Element {
  const ui = useUi()

  useEffect(() => {
    setSourceTransport({
      call: (tool, args) => api.sourceCall(active.id, tool, args),
      user,
      sourceId: active.id,
    })
    setQueryFetcher(query)
    setResourceFetcher(readResource)
    // The integrations belong to the *server*, not to the source — but this is
    // where the app is pointed at one, so it is where they are picked up.
    setIntegrationServer(active.serverId)
    return () => {
      setQueryFetcher(null)
      setResourceFetcher(null)
      setSourceTransport(null)
      setIntegrationServer(null)
    }
  }, [active.id, active.serverId, user])

  return (
    <div className="relative flex h-full flex-col">
      <div className="min-h-0 flex-1">
        <Layout />
      </div>

      {/* Debug lives in an unobtrusive corner button rather than a header bar. */}
      <Button
        variant="tertiary"
        size="sm"
        className="absolute bottom-4 left-4 opacity-60 hover:opacity-100"
        onClick={() => updateUi({ debugSource: true })}
      >
        Debug
      </Button>

      {ui.debugSource && (
        <DebugModal sourceId={active.id} user={user} onClose={() => updateUi({ debugSource: false })} />
      )}
      {ui.resourceId && <ResourceModal id={ui.resourceId} />}
      {ui.debugEntityId && (
        <EntityDebugModal
          sourceId={active.id}
          entityId={ui.debugEntityId}
          user={user}
          onClose={() => updateUi({ debugEntityId: null })}
        />
      )}
    </div>
  )
}
