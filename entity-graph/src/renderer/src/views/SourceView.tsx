import React, { useEffect } from 'react'
import type { ActiveSource } from '../../../core/client'
import { Layout } from '../layout/Layout'
import { DebugModal } from '../components/DebugModal'
import { EntityDebugModal } from '../components/EntityDebugModal'
import { Button } from '../components/ui/Button'
import { useUi } from '../state/hooks'
import { setQueryFetcher } from '../state/query'
import { updateUi } from '../state/ui'
import { query } from '../source/entity'
import { setSourceTransport } from '../source/transport'

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
    return () => {
      setQueryFetcher(null)
      setSourceTransport(null)
    }
  }, [active.id, user])

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
