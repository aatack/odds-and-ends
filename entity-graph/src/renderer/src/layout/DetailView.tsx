import React, { useEffect } from 'react'
import { PillContent } from '../components/EntityPill'
import { EDITORS } from '../components/editors'
import { SchemaForm, isObject, type JsonSchema } from '../components/schema'
import * as A from '../state/actions'
import { useGetEntities, useLayoutState } from '../state/hooks'
import { detailFrame } from '../state/types'
import { runTool } from '../tools/call'

/** How long the typing has to stop for before the form is written. */
const WRITE_AFTER = 600

/** The form an entity of no type, or of a type with no schema, is edited with. */
const TEXT_ONLY: JsonSchema = {
  type: 'object',
  properties: { text: { type: 'string', 'x-multiline': true } },
}

/**
 * The detail view: the root's values as a form, from its type's schema. What is
 * typed is the frame's draft, and the draft is written — the values that
 * changed, as one step to undo — a moment after the typing stops, and when the
 * form loses focus (`detail.commit`).
 */
export function DetailView({ frameId }: { frameId: string }): React.JSX.Element {
  const frame = detailFrame(useLayoutState().frames[frameId])
  const get = useGetEntities()
  const rootId = frame?.rootId ?? ''
  const values = get([rootId])[rootId].values
  const typeId = typeof values.type === 'string' ? values.type : null
  const typed = typeId ? get([typeId])[typeId].values.schema : undefined
  const schema = hasProperties(typed) ? typed : TEXT_ONLY
  const draft = frame?.draft ?? null

  useEffect(() => {
    if (!draft) return
    const timer = setTimeout(() => commit(frameId), WRITE_AFTER)
    return () => clearTimeout(timer)
  }, [draft, frameId])

  if (!frame) return <div className="p-8 text-center text-[13px] text-gray-400">No frame.</div>

  const keys = Object.keys(schema.properties as Record<string, unknown>)
  // Only the keys the form shows, a cleared one as null: so that the write is
  // what changed here, and nothing else on the entity is written back over.
  const setValue = (next: unknown): void => {
    const object = isObject(next) ? next : {}
    A.setDetailDraft(frameId, Object.fromEntries(keys.map((key) => [key, object[key] ?? null])))
  }

  return (
    <div
      className="h-full overflow-auto bg-white px-6 py-5"
      onBlur={(e) => {
        if (!e.currentTarget.contains(e.relatedTarget as Node | null)) commit(frameId)
      }}
    >
      <div className="mb-3 flex min-w-0 items-center gap-2 text-[12px] text-gray-500">
        <PillContent id={rootId} className="text-gray-700" />
        {typeId && !hasProperties(typed) && (
          <span className="text-gray-400">· its type has no schema</span>
        )}
      </div>
      <SchemaForm
        value={draft ? { ...values, ...draft } : values}
        setValue={setValue}
        schema={schema}
        editors={EDITORS}
      />
    </div>
  )
}

const commit = (frameId: string): void => void runTool('detail.commit', { extra: { frameId } })

const hasProperties = (schema: unknown): schema is JsonSchema & { properties: object } =>
  isObject(schema) && isObject(schema.properties) && Object.keys(schema.properties).length > 0
