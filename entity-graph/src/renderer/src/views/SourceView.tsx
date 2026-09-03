import React, { useEffect } from 'react'
import type { CurrentPensive } from '../../../core/client'
import { Layout } from '../layout/Layout'
import { EntityInspector } from '../components/EntityInspector'
import { ResourceModal } from '../components/Resource'
import { evaluateCode } from '../helpers/codeRunner'
import {
  applyEvents,
  invalidateEntities,
  refreshEntities,
  removeEvents,
  setCodeEvaluator,
  setEntityFetcher,
} from '../../../core/cache'
import { useUi } from '../state/hooks'
import { setResourceFetcher } from '../state/resources'
import { updateUi } from '../state/ui'
import { readResource, scanEvents, setWriteObserver } from '../source/entity'
import { setSourceTransport } from '../source/transport'
import { clearUserTools, loadUserTools } from '../tools/userTools'

const api = window.entityGraph

/**
 * The open pensive's shell. Its whole job is to plug the seams together — the
 * transport, the caches, and the sandbox the entity cache runs `events` scripts
 * in — and then get out of the way: everything below reads state, so no data
 * flows through this component.
 *
 * Which pensive it is comes from the sources page, so this component is keyed on
 * its id: dragging a different store into the desktop node tears the seams down
 * and lays them again over the new one.
 */
export function SourceView({
  pensive,
  user,
}: {
  pensive: CurrentPensive
  user: string
}): React.JSX.Element {
  const ui = useUi()

  useEffect(() => {
    setSourceTransport({
      call: (tool, args) => api.pensiveCall(tool, args),
      user,
      sourceId: pensive.id,
    })
    setEntityFetcher(scanEvents)
    setWriteObserver({
      applied: applyEvents,
      removed: removeEvents,
      touched: invalidateEntities,
    })
    setCodeEvaluator(evaluateCode)
    setResourceFetcher(readResource)
    // Whatever is cached was rolled up from a different store, so it is read
    // again rather than trusted — the rows keep what they have until it lands.
    refreshEntities()
    // The user's own tools belong to the store, and are read through the
    // transport set just above, so this has to come after it.
    void loadUserTools()
    return () => {
      setEntityFetcher(null)
      setWriteObserver(null)
      setCodeEvaluator(null)
      setResourceFetcher(null)
      setSourceTransport(null)
      clearUserTools()
    }
  }, [pensive.id, user])

  return (
    <div className="relative flex h-full flex-col">
      <div className="min-h-0 flex-1">
        <Layout />
      </div>

      {ui.resourceId && <ResourceModal id={ui.resourceId} />}
      {ui.inspectEntityId && (
        <EntityInspector
          entityId={ui.inspectEntityId}
          user={user}
          onClose={() => updateUi({ inspectEntityId: null })}
        />
      )}
    </div>
  )
}
