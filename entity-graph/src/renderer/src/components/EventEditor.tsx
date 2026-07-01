import React, { useState } from 'react'
import type { AppEvent } from '../../../core/events'

interface Props {
  onWrite: (events: AppEvent[]) => Promise<void>
}

type Tab = 'value' | 'link'

const LINK_ACTIONS = [
  { value: 0, label: 'Add (0)' },
  { value: 1, label: 'Remove (1)' },
  { value: 2, label: 'Move forward (2)' },
  { value: 3, label: 'Move backward (3)' },
] as const

export function EventEditor({ onWrite }: Props) {
  const [tab, setTab] = useState<Tab>('value')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)
  const [success, setSuccess] = useState(false)

  // Value event fields
  const [vEntityId, setVEntityId] = useState('')
  const [vKey, setVKey]           = useState('')
  const [vValue, setVValue]       = useState('')

  // Link event fields
  const [lSource, setLSource]   = useState('')
  const [lDest, setLDest]       = useState('')
  const [lAction, setLAction]   = useState<0 | 1 | 2 | 3>(0)

  const handleWrite = async () => {
    setError(null); setSuccess(false)
    setBusy(true)
    try {
      const now = Date.now()
      let event: AppEvent
      if (tab === 'value') {
        let parsed: unknown
        try { parsed = JSON.parse(vValue) } catch { throw new Error('Value must be valid JSON') }
        event = { type: 'value', timestamp: now, author: '__ui__', entityId: vEntityId.trim(), key: vKey.trim(), value: parsed }
      } else {
        event = { type: 'link', timestamp: now, author: '__ui__', sourceId: lSource.trim(), destinationId: lDest.trim(), action: lAction }
      }
      await onWrite([event])
      setSuccess(true)
      setTimeout(() => setSuccess(false), 2000)
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  const canSubmit = tab === 'value'
    ? vEntityId.trim() && vKey.trim() && vValue.trim()
    : lSource.trim() && lDest.trim()

  return (
    <div className="card p-4 space-y-3">
      <p className="section-title">Write event</p>

      <div className="flex gap-2">
        {(['value', 'link'] as Tab[]).map((t) => (
          <button key={t} onClick={() => setTab(t)} className={`btn text-text-sm ${tab === t ? 'btn-primary' : 'btn-secondary'}`}>
            {t === 'value' ? 'Value event' : 'Link event'}
          </button>
        ))}
      </div>

      {tab === 'value' ? (
        <div className="space-y-3">
          <div>
            <label className="label">Entity ID</label>
            <input className="input font-mono" value={vEntityId} onChange={(e) => setVEntityId(e.target.value)} placeholder="entity-uuid" />
          </div>
          <div>
            <label className="label">Key</label>
            <input className="input" value={vKey} onChange={(e) => setVKey(e.target.value)} placeholder="title" />
          </div>
          <div>
            <label className="label">Value (JSON)</label>
            <input className="input font-mono" value={vValue} onChange={(e) => setVValue(e.target.value)} placeholder='"Hello world"' />
          </div>
        </div>
      ) : (
        <div className="space-y-3">
          <div>
            <label className="label">Source entity ID</label>
            <input className="input font-mono" value={lSource} onChange={(e) => setLSource(e.target.value)} placeholder="parent-uuid" />
          </div>
          <div>
            <label className="label">Destination entity ID</label>
            <input className="input font-mono" value={lDest} onChange={(e) => setLDest(e.target.value)} placeholder="child-uuid" />
          </div>
          <div>
            <label className="label">Action</label>
            <select
              className="input"
              value={lAction}
              onChange={(e) => setLAction(Number(e.target.value) as 0 | 1 | 2 | 3)}
            >
              {LINK_ACTIONS.map((a) => (
                <option key={a.value} value={a.value}>{a.label}</option>
              ))}
            </select>
          </div>
        </div>
      )}

      {error   && <p className="text-text-sm text-error-500">{error}</p>}
      {success && <p className="text-text-sm text-success-500">Written.</p>}

      <button className="btn-primary" onClick={handleWrite} disabled={busy || !canSubmit}>
        {busy ? 'Writing…' : 'Write'}
      </button>
    </div>
  )
}
