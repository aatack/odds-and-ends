import React, { useState } from 'react'
import type { Connection, NewConnection } from '../../../core/client'

interface Props {
  connections: Connection[]
  onOpen: (id: string) => Promise<void>
  onAdd: (cfg: NewConnection) => Promise<void>
  onUpdate: (id: string, patch: Partial<NewConnection>) => Promise<void>
  onRemove: (id: string) => Promise<void>
}

const EMPTY: NewConnection = { label: '', baseUrl: '', kind: 'admin', token: '', sourceId: '' }

export function ConnectionManager({ connections, onOpen, onAdd, onUpdate, onRemove }: Props): React.JSX.Element {
  const [form, setForm] = useState<NewConnection>(EMPTY)
  const [editingId, setEditingId] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const set = <K extends keyof NewConnection>(k: K, v: NewConnection[K]) =>
    setForm((f) => ({ ...f, [k]: v }))

  const reset = () => { setForm(EMPTY); setEditingId(null) }

  const valid =
    form.label.trim() &&
    form.baseUrl.trim() &&
    form.token.trim() &&
    (form.kind === 'admin' || form.sourceId?.trim())

  const submit = async () => {
    if (!valid) return
    setBusy(true)
    setError(null)
    try {
      const cfg: NewConnection = {
        label: form.label.trim(),
        baseUrl: form.baseUrl.trim().replace(/\/+$/, ''),
        kind: form.kind,
        token: form.token.trim(),
        ...(form.kind === 'source' ? { sourceId: form.sourceId?.trim() } : {}),
      }
      if (editingId) await onUpdate(editingId, cfg)
      else await onAdd(cfg)
      reset()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  const startEdit = (c: Connection) => {
    setEditingId(c.id)
    setForm({ label: c.label, baseUrl: c.baseUrl, kind: c.kind, token: c.token, sourceId: c.sourceId ?? '' })
  }

  return (
    <div className="space-y-4">
      <div className="card p-4 space-y-3">
        <p className="section-title">{editingId ? 'Edit connection' : 'Add connection'}</p>

        <div className="flex gap-2">
          {(['admin', 'source'] as const).map((k) => (
            <button
              key={k}
              onClick={() => set('kind', k)}
              className={`btn text-text-sm ${form.kind === k ? 'btn-primary' : 'btn-secondary'}`}
            >
              {k === 'admin' ? 'Configure sources (admin)' : 'Connect to a source'}
            </button>
          ))}
        </div>

        <div>
          <label className="label">Label</label>
          <input className="input" value={form.label} onChange={(e) => set('label', e.target.value)} placeholder="Local server" />
        </div>

        <div>
          <label className="label">Base URL</label>
          <input className="input font-mono" value={form.baseUrl} onChange={(e) => set('baseUrl', e.target.value)} placeholder="http://127.0.0.1:4000" />
        </div>

        {form.kind === 'source' && (
          <div>
            <label className="label">Source ID</label>
            <input className="input font-mono" value={form.sourceId ?? ''} onChange={(e) => set('sourceId', e.target.value)} placeholder="my-source" />
          </div>
        )}

        <div>
          <label className="label">{form.kind === 'admin' ? 'Admin token' : 'Source token'}</label>
          <input className="input font-mono" type="password" value={form.token} onChange={(e) => set('token', e.target.value)} placeholder="••••••••" />
        </div>

        {error && <p className="text-text-sm text-error-500">{error}</p>}

        <div className="flex gap-2">
          <button className="btn-primary" onClick={submit} disabled={busy || !valid}>
            {busy ? 'Saving…' : editingId ? 'Save' : 'Add connection'}
          </button>
          {editingId && (
            <button className="btn-secondary" onClick={reset} disabled={busy}>Cancel</button>
          )}
        </div>
      </div>

      {connections.length > 0 && (
        <div className="card divide-y divide-gray-100">
          {connections.map((c) => (
            <div key={c.id} className="flex items-center justify-between px-4 py-3 gap-3">
              <button className="min-w-0 text-left flex-1" onClick={() => onOpen(c.id)}>
                <p className="text-text-sm font-medium text-gray-900 truncate">{c.label}</p>
                <p className="text-text-xs text-gray-500 truncate font-mono">
                  {c.baseUrl}{c.kind === 'source' ? ` · ${c.sourceId}` : ''}
                </p>
              </button>
              <div className="flex items-center gap-2 shrink-0">
                <span className={`badge ${c.kind === 'admin' ? 'badge-blue' : 'badge-green'}`}>{c.kind}</span>
                <button className="btn-primary text-text-xs px-2 py-1" onClick={() => onOpen(c.id)}>Open</button>
                <button className="btn-secondary text-text-xs px-2 py-1" onClick={() => startEdit(c)}>Edit</button>
                <button className="btn-danger text-text-xs px-2 py-1" onClick={() => onRemove(c.id)}>Remove</button>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}
