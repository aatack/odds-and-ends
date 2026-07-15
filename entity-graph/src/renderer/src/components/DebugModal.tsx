import React, { useEffect, useState } from 'react'
import type { QueryPage } from '../../../core/wrapper'
import type { ToolMeta } from '../../../core/client'

const api = window.entityGraph

interface Props {
  connId: string
  user: string
  onClose: () => void
}

/** Raw, low-level access to a source's tools — query + event writes + a tool list. */
export function DebugModal({ connId, user, onClose }: Props): React.JSX.Element {
  return (
    <div
      className="fixed inset-0 z-50 bg-black/40 flex items-start justify-center p-6 overflow-y-auto"
      onClick={onClose}
    >
      <div
        className="bg-white rounded-lg shadow-xl w-full max-w-2xl my-6"
        onClick={(e) => e.stopPropagation()}
      >
        <div className="flex items-center justify-between px-5 py-3 border-b border-gray-200">
          <p className="font-semibold text-gray-900">Debug</p>
          <button className="text-gray-400 hover:text-gray-700 text-text-lg leading-none" onClick={onClose}>✕</button>
        </div>
        <div className="p-5 space-y-6">
          <RawQuery connId={connId} />
          <RawEvent connId={connId} user={user} />
          <ToolList connId={connId} />
        </div>
      </div>
    </div>
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

  const run = async () => {
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
      <p className="section-title">Query</p>
      <div className="flex gap-2">
        <input className="input font-mono flex-1" value={rootId} onChange={(e) => setRootId(e.target.value)} placeholder="root id" />
        <input className="input w-24" type="number" min="1" value={maxDepth} onChange={(e) => setMaxDepth(e.target.value)} placeholder="depth" />
        <input className="input w-24" type="number" min="1" value={limit} onChange={(e) => setLimit(e.target.value)} placeholder="limit" />
        <button className="btn-primary" onClick={run} disabled={loading || !rootId.trim()}>{loading ? '…' : 'Run'}</button>
      </div>
      {error && <p className="text-text-sm text-error-500">{error}</p>}
      {page && (
        <div className="space-y-1">
          <p className="text-text-xs text-gray-500">
            {page.results.length} rows{page.continuationStack ? ' (more available — raise limit)' : ''}
          </p>
          <pre className="rounded-md bg-gray-50 border border-gray-200 p-3 text-text-xs font-mono overflow-x-auto max-h-64 overflow-y-auto">
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

  const write = async () => {
    setError(null)
    setOk(false)
    setBusy(true)
    try {
      if (kind === 'value') {
        let parsed: unknown
        try { parsed = JSON.parse(value) } catch { throw new Error('Value must be valid JSON') }
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
      <p className="section-title">Write event</p>
      <div className="flex gap-2">
        {(['value', 'link'] as Kind[]).map((k) => (
          <button key={k} onClick={() => setKind(k)} className={`btn text-text-sm ${kind === k ? 'btn-primary' : 'btn-secondary'}`}>
            {k === 'value' ? 'Value' : 'Link'}
          </button>
        ))}
      </div>
      {kind === 'value' ? (
        <div className="space-y-2">
          <input className="input font-mono" value={entityId} onChange={(e) => setEntityId(e.target.value)} placeholder="entity id" />
          <input className="input" value={key} onChange={(e) => setKey(e.target.value)} placeholder="key (e.g. text)" />
          <input className="input font-mono" value={value} onChange={(e) => setValue(e.target.value)} placeholder='value as JSON, e.g. "hi"' />
        </div>
      ) : (
        <div className="space-y-2">
          <input className="input font-mono" value={src} onChange={(e) => setSrc(e.target.value)} placeholder="source id" />
          <input className="input font-mono" value={dest} onChange={(e) => setDest(e.target.value)} placeholder="destination id" />
          <select className="input" value={action} onChange={(e) => setAction(Number(e.target.value) as 0 | 1 | 2 | 3)}>
            {LINK_ACTIONS.map((a) => <option key={a.value} value={a.value}>{a.label}</option>)}
          </select>
        </div>
      )}
      {error && <p className="text-text-sm text-error-500">{error}</p>}
      {ok && <p className="text-text-sm text-success-500">Written.</p>}
      <button className="btn-primary" onClick={write} disabled={busy || !canSubmit}>{busy ? 'Writing…' : 'Write'}</button>
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
      <p className="section-title">Tools</p>
      {error && <p className="text-text-sm text-error-500">{error}</p>}
      {tools && (
        <div className="rounded-md bg-gray-50 border border-gray-200 divide-y divide-gray-100 text-text-xs">
          {tools.map((t) => (
            <div key={t.id} className="px-3 py-1.5 flex gap-2">
              <span className="font-mono text-gray-900 shrink-0">{t.id}</span>
              <span className="badge badge-gray shrink-0">{t.safety}</span>
              <span className="text-gray-500 truncate">{t.description}</span>
            </div>
          ))}
        </div>
      )}
    </section>
  )
}
