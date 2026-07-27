import React, { useCallback, useEffect, useState } from 'react'
import { Trash03 } from '@untitledui/icons'
import type { Entity } from '../../../core/entity'
import { entitiesAtom, refreshDerived, refreshEntities, type LoadState } from '../../../core/cache'
import { cn } from '../helpers/cn'
import { useAtomValue } from '../state/hooks'
import { clearUndo } from '../state/undo'
import { Button } from './ui/Button'
import { Field } from './ui/Field'
import { IconButton } from './ui/IconButton'
import { Input } from './ui/Input'
import { Modal } from './ui/Modal'

const api = window.entityGraph

const SECTION = 'text-[11px] font-medium uppercase tracking-[0.09em] text-gray-500'

interface Props {
  /** Id of the open source (the handle `sourceCall` resolves by). */
  sourceId: string
  /** The entity being inspected. */
  entityId: string
  user: string
  onClose: () => void
}

/**
 * Low-level inspector for a single entity: its current values and outbound
 * links, plus forms to write a value and add or remove links. Everything goes
 * through the source's raw tools, mirroring the global debug panel but scoped
 * to one entity.
 */
export function EntityDebugModal({ sourceId, entityId, user, onClose }: Props): React.JSX.Element {
  const [values, setValues] = useState<Record<string, unknown>>({})
  const [links, setLinks] = useState<string[]>([])
  const [inbound, setInbound] = useState<string[]>([])
  const [missing, setMissing] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const load = useCallback(async (): Promise<void> => {
    setError(null)
    try {
      // Straight from the source rather than from the cache, and rolled up by
      // the store: the point of this panel is what is actually written down,
      // without a type's defaults or an `events` script laid over the top.
      const read = (await api.sourceCall(sourceId, 'readEntities', {
        entityIds: [entityId],
      })) as Record<string, Entity>
      const self = read[entityId]
      setValues(self?.values ?? {})
      setLinks(self?.outboundLinks ?? [])
      setInbound(self?.inboundLinks ?? [])
      // No values and nothing pointing either way: an id that was written to by
      // accident, or one that has since been fully unlinked.
      setMissing(
        !!self &&
          Object.keys(self.values).length === 0 &&
          self.outboundLinks.length === 0 &&
          self.inboundLinks.length === 0,
      )
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }, [sourceId, entityId])

  useEffect(() => {
    void load()
  }, [load])

  // The debug panel writes raw events rather than going through a tool — that's
  // the point of it — so it has to do by hand what the call machine would: tell
  // open frames to refetch, and strand the undo stack, whose events are no longer
  // the store's most recent.
  const write = async (fn: () => Promise<unknown>): Promise<void> => {
    setError(null)
    try {
      await fn()
      clearUndo()
      refreshEntities()
      await load()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  const removeLink = (dest: string): Promise<void> =>
    write(() =>
      api.sourceCall(sourceId, 'writeLink', { sourceId: entityId, destinationId: dest, action: 1, author: user }),
    )

  return (
    <Modal title="Debug entity" onClose={onClose}>
      <p className="-mt-1 break-all font-mono text-xs text-gray-500">{entityId}</p>

      {error && <p className="text-[13px] text-error-600">{error}</p>}
      {missing && (
        <p className="text-xs text-gray-400">
          Nothing is recorded against this id — no values, and no links either way.
        </p>
      )}

      <section className="space-y-2">
        <p className={SECTION}>Values</p>
        {Object.keys(values).length === 0 ? (
          <p className="text-xs text-gray-400">No values.</p>
        ) : (
          <div className="overflow-hidden rounded-md bg-gray-50 font-mono text-xs">
            {Object.entries(values).map(([k, v]) => (
              <div key={k} className="flex gap-2 px-3 py-1.5">
                <span className="shrink-0 text-gray-900">{k}</span>
                <span className="truncate text-gray-500">{JSON.stringify(v)}</span>
              </div>
            ))}
          </div>
        )}
        <WriteValue onWrite={(key, value) => write(() => api.sourceCall(sourceId, 'writeValue', { entityId, key, value, author: user }))} />
      </section>

      <Cached entityId={entityId} />

      <section className="space-y-2">
        <p className={SECTION}>Links</p>
        {links.length === 0 ? (
          <p className="text-xs text-gray-400">No outbound links.</p>
        ) : (
          <div className="overflow-hidden rounded-md bg-gray-50 font-mono text-xs">
            {links.map((dest) => (
              <div key={dest} className="flex items-center justify-between gap-2 px-3 py-1">
                <span className="truncate text-gray-900">{dest}</span>
                <IconButton title="Remove link" onClick={() => removeLink(dest)}>
                  <Trash03 size={14} />
                </IconButton>
              </div>
            ))}
          </div>
        )}
        <AddLink onAdd={(dest) => write(() => api.sourceCall(sourceId, 'writeLink', { sourceId: entityId, destinationId: dest, action: 0, author: user }))} />
      </section>

      {/* Inbound is the direction you need to trace a stray link back to whoever
          made it, so it's listed even though it isn't editable from here. */}
      <section className="space-y-2">
        <p className={SECTION}>Linked from</p>
        {inbound.length === 0 ? (
          <p className="text-xs text-gray-400">Nothing links to this entity.</p>
        ) : (
          <div className="overflow-hidden rounded-md bg-gray-50 font-mono text-xs">
            {inbound.map((src) => (
              <div key={src} className="flex items-center justify-between gap-2 px-3 py-1">
                <span className="truncate text-gray-900">{src}</span>
                <IconButton
                  title="Remove this inbound link"
                  onClick={() =>
                    write(() =>
                      api.sourceCall(sourceId, 'writeLink', {
                        sourceId: src,
                        destinationId: entityId,
                        action: 1,
                        author: user,
                      }),
                    )
                  }
                >
                  <Trash03 size={14} />
                </IconButton>
              </div>
            ))}
          </div>
        )}
      </section>
    </Modal>
  )
}

// ---------------------------------------------------------------------------

/** What a load state means, said once so the modal doesn't have to explain it. */
const STATE_NOTE: Record<LoadState, string> = {
  unloaded: 'not read yet',
  loading: 'reading',
  loaded: 'read',
  error: 'failed',
}

/**
 * What the *cache* holds for this entity, as against what the store does. The
 * two sections above read straight from the source; this one is the only place
 * the client's own picture is visible, which matters mostly for one thing: an
 * `events` script runs in the background, once a session, and until now failed
 * in complete silence.
 */
function Cached({ entityId }: { entityId: string }): React.JSX.Element {
  const cache = useAtomValue(entitiesAtom)
  const entry = cache[entityId]
  const script = entry?.entity.values.events

  return (
    <section className="space-y-2">
      <div className="flex items-center justify-between gap-2">
        <p className={SECTION}>In the cache</p>
        <Button variant="tertiary" size="sm" onClick={refreshDerived}>
          Recompute derived
        </Button>
      </div>
      {!entry ? (
        <p className="text-xs text-gray-400">
          Nothing here — the client has never had reason to read this entity.
        </p>
      ) : (
        <div className="overflow-hidden rounded-md bg-gray-50 font-mono text-xs">
          <Fact label="events" value={`${entry.events.length} (${STATE_NOTE[entry.loaded]})`} />
          <Fact
            label="derived"
            value={
              script == null
                ? 'no events script'
                : `${entry.derived.length} (${STATE_NOTE[entry.derivedState]})`
            }
          />
          {entry.error && <Fact label="read error" value={entry.error} bad />}
          {entry.derivedError && <Fact label="script error" value={entry.derivedError} bad />}
        </div>
      )}
      {entry?.derived.length ? (
        <pre className="max-h-48 overflow-auto rounded-md bg-gray-50 p-3 font-mono text-xs">
          {JSON.stringify(entry.derived, null, 2)}
        </pre>
      ) : null}
      {script != null && entry?.derivedState === 'loaded' && !entry.derived.length && (
        <p className="text-xs text-gray-400">
          The script ran and returned no events. Anything it logged is in the devtools console.
        </p>
      )}
    </section>
  )
}

function Fact({
  label,
  value,
  bad,
}: {
  label: string
  value: string
  bad?: boolean
}): React.JSX.Element {
  return (
    <div className="flex gap-2 px-3 py-1.5">
      <span className="w-24 shrink-0 text-gray-900">{label}</span>
      <span className={cn('break-all', bad ? 'text-error-600' : 'text-gray-500')}>{value}</span>
    </div>
  )
}

// ---------------------------------------------------------------------------

function WriteValue({ onWrite }: { onWrite: (key: string, value: unknown) => void }): React.JSX.Element {
  const [key, setKey] = useState('')
  const [value, setValue] = useState('')
  const [error, setError] = useState<string | null>(null)

  const submit = (): void => {
    if (!key.trim()) return
    let parsed: unknown
    try {
      parsed = JSON.parse(value)
    } catch {
      setError('Value must be valid JSON')
      return
    }
    setError(null)
    onWrite(key.trim(), parsed)
    setKey('')
    setValue('')
  }

  return (
    <div className="space-y-2">
      <div className="flex items-end gap-2">
        <Field label="Key" className="w-28">
          <Input value={key} onChange={(e) => setKey(e.target.value)} placeholder="text" />
        </Field>
        <Field label="Value (JSON)" className="flex-1">
          <Input mono value={value} onChange={(e) => setValue(e.target.value)} placeholder='"hello"' />
        </Field>
        <Button variant="secondary" onClick={submit} disabled={!key.trim() || !value.trim()}>
          Write
        </Button>
      </div>
      {error && <p className="text-xs text-error-600">{error}</p>}
    </div>
  )
}

function AddLink({ onAdd }: { onAdd: (dest: string) => void }): React.JSX.Element {
  const [dest, setDest] = useState('')

  const submit = (): void => {
    if (!dest.trim()) return
    onAdd(dest.trim())
    setDest('')
  }

  return (
    <div className="flex items-end gap-2">
      <Field label="Destination id" className="flex-1">
        <Input mono value={dest} onChange={(e) => setDest(e.target.value)} placeholder="entity id" />
      </Field>
      <Button variant="secondary" onClick={submit} disabled={!dest.trim()}>
        Add link
      </Button>
    </div>
  )
}
