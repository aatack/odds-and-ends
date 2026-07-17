import React, { useEffect, useState } from 'react'
import type { QueryPage } from '../../../core/wrapper'
import type { ToolMeta } from '../../../core/client'
import { Badge } from './ui/Badge'
import { Button } from './ui/Button'
import { Input } from './ui/Input'
import { Modal } from './ui/Modal'
import { Select } from './ui/Select'

const api = window.entityGraph

const SECTION = 'text-[11px] font-medium uppercase tracking-[0.09em] text-gray-500'

interface Props {
  /** Id of the open source (the handle `sourceCall`/`sourceTools` resolve by). */
  sourceId: string
  user: string
  onClose: () => void
}

/** Raw, low-level access to a source's tools — query + event writes + a tool list. */
export function DebugModal({ sourceId: connId, user, onClose }: Props): React.JSX.Element {
  return (
    <Modal title="Debug" onClose={onClose} wide>
      <div className="space-y-6">
        <RawQuery connId={connId} />
        <RawEvent connId={connId} user={user} />
        <ToolList connId={connId} />
      </div>
    </Modal>
  )
}

// ---------------------------------------------------------------------------

function RawQuery({ connId }: { connId: string }): React.JSX.Element {
  const [rootId, setRootId] = useState('@index')
  const [maxDepth, setMaxDepth] = useState('')
  const [limit, setLimit] = useState('100')
  const [page, setPage] = useState<QueryPage | null>(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const run = async (): Promise<void> => {
    if (!rootId.trim()) return
    setLoading(true)
    setError(null)
    try {
      const p = (await api.sourceCall(connId, 'query', {
        rootId: rootId.trim(),
        maxDepth: maxDepth ? parseInt(maxDepth, 10) : undefined,
        limit: limit ? parseInt(limit, 10) : 100,
      })) as QueryPage
      setPage(p)
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setLoading(false)
    }
  }

  return (
    <section className="space-y-2">
      <p className={SECTION}>Query</p>
      <div className="flex gap-2">
        <Input mono className="flex-1" value={rootId} onChange={(e) => setRootId(e.target.value)} placeholder="root id" />
        <Input className="w-24" type="number" min="1" value={maxDepth} onChange={(e) => setMaxDepth(e.target.value)} placeholder="depth" />
        <Input className="w-24" type="number" min="1" value={limit} onChange={(e) => setLimit(e.target.value)} placeholder="limit" />
        <Button variant="primary" onClick={run} disabled={loading || !rootId.trim()}>
          {loading ? '…' : 'Run'}
        </Button>
      </div>
      {error && <p className="text-[13px] text-error-600">{error}</p>}
      {page && (
        <div className="space-y-1">
          <p className="text-xs text-gray-500">
            {page.results.length} rows{page.continuationStack ? ' (more available — raise limit)' : ''}
          </p>
          <pre className="max-h-64 overflow-auto rounded-md bg-gray-50 p-3 font-mono text-xs">
            {JSON.stringify(page.results.map((r) => ({ id: r.entity.id, depth: r.depth, parentId: r.parentId, values: r.entity.values })), null, 2)}
          </pre>
        </div>
      )}
    </section>
  )
}

// ---------------------------------------------------------------------------

type Kind = 'value' | 'link'
const LINK_ACTIONS = [
  { value: 0, label: 'Add (0)' },
  { value: 1, label: 'Remove (1)' },
  { value: 2, label: 'Move forward (2)' },
  { value: 3, label: 'Move backward (3)' },
] as const

function RawEvent({ connId, user }: { connId: string; user: string }): React.JSX.Element {
  const [kind, setKind] = useState<Kind>('value')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [ok, setOk] = useState(false)

  const [entityId, setEntityId] = useState('')
  const [key, setKey] = useState('')
  const [value, setValue] = useState('')

  const [src, setSrc] = useState('')
  const [dest, setDest] = useState('')
  const [action, setAction] = useState<0 | 1 | 2 | 3>(0)

  const write = async (): Promise<void> => {
    setError(null)
    setOk(false)
    setBusy(true)
    try {
      if (kind === 'value') {
        let parsed: unknown
        try {
          parsed = JSON.parse(value)
        } catch {
          throw new Error('Value must be valid JSON')
        }
        await api.sourceCall(connId, 'writeValue', { entityId: entityId.trim(), key: key.trim(), value: parsed, author: user })
      } else {
        await api.sourceCall(connId, 'writeLink', { sourceId: src.trim(), destinationId: dest.trim(), action, author: user })
      }
      setOk(true)
      setTimeout(() => setOk(false), 2000)
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  const canSubmit = kind === 'value' ? entityId.trim() && key.trim() && value.trim() : src.trim() && dest.trim()

  return (
    <section className="space-y-2">
      <p className={SECTION}>Write event</p>
      <div className="inline-flex rounded-lg bg-gray-100 p-0.5">
        {(['value', 'link'] as Kind[]).map((k) => (
          <button
            key={k}
            onClick={() => setKind(k)}
            className={`rounded-md px-3 py-1 text-[13px] font-medium transition-colors focus:outline-none ${
              kind === k ? 'bg-white text-gray-900 shadow-xs' : 'text-gray-500 hover:text-gray-700'
            }`}
          >
            {k === 'value' ? 'Value' : 'Link'}
          </button>
        ))}
      </div>
      {kind === 'value' ? (
        <div className="space-y-2">
          <Input mono value={entityId} onChange={(e) => setEntityId(e.target.value)} placeholder="entity id" />
          <Input value={key} onChange={(e) => setKey(e.target.value)} placeholder="key (e.g. text)" />
          <Input mono value={value} onChange={(e) => setValue(e.target.value)} placeholder='value as JSON, e.g. "hi"' />
        </div>
      ) : (
        <div className="space-y-2">
          <Input mono value={src} onChange={(e) => setSrc(e.target.value)} placeholder="source id" />
          <Input mono value={dest} onChange={(e) => setDest(e.target.value)} placeholder="destination id" />
          <Select value={action} onChange={(e) => setAction(Number(e.target.value) as 0 | 1 | 2 | 3)}>
            {LINK_ACTIONS.map((a) => (
              <option key={a.value} value={a.value}>
                {a.label}
              </option>
            ))}
          </Select>
        </div>
      )}
      {error && <p className="text-[13px] text-error-600">{error}</p>}
      {ok && <p className="text-[13px] text-success-700">Written.</p>}
      <Button variant="primary" onClick={write} disabled={busy || !canSubmit}>
        {busy ? 'Writing…' : 'Write'}
      </Button>
    </section>
  )
}

// ---------------------------------------------------------------------------

function ToolList({ connId }: { connId: string }): React.JSX.Element {
  const [tools, setTools] = useState<ToolMeta[] | null>(null)
  const [error, setError] = useState<string | null>(null)

  useEffect(() => {
    api.sourceTools(connId).then(setTools).catch((e) => setError(e instanceof Error ? e.message : String(e)))
  }, [connId])

  return (
    <section className="space-y-2">
      <p className={SECTION}>Tools</p>
      {error && <p className="text-[13px] text-error-600">{error}</p>}
      {tools && (
        <div className="overflow-hidden rounded-md bg-gray-50 text-xs">
          {tools.map((t) => (
            <div key={t.id} className="flex gap-2 px-3 py-1.5">
              <span className="shrink-0 font-mono text-gray-900">{t.id}</span>
              <Badge color="gray">{t.safety}</Badge>
              <span className="truncate text-gray-500">{t.description}</span>
            </div>
          ))}
        </div>
      )}
    </section>
  )
}
