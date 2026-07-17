import React, { useCallback, useEffect, useState } from 'react'
import { Trash03 } from '@untitledui/icons'
import type { QueryPage } from '../../../core/wrapper'
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
  const [error, setError] = useState<string | null>(null)

  const load = useCallback(async (): Promise<void> => {
    setError(null)
    try {
      const page = (await api.sourceCall(sourceId, 'query', {
        rootId: entityId,
        maxDepth: 1,
        limit: 500,
      })) as QueryPage
      const self = page.results.find((r) => r.entity.id === entityId)
      setValues(self?.entity.values ?? {})
      setLinks(self?.entity.outboundLinks ?? [])
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }, [sourceId, entityId])

  useEffect(() => {
    void load()
  }, [load])

  const write = async (fn: () => Promise<unknown>): Promise<void> => {
    setError(null)
    try {
      await fn()
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
    </Modal>
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
