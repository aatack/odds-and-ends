import React, { useState } from 'react'
import type { Connection, LocalServer, NewConnection } from '../../../core/client'

interface Props {
  connections: Connection[]
  localServers: LocalServer[]
  onOpen: (id: string) => Promise<void>
  onAdd: (cfg: NewConnection) => Promise<void>
  onUpdate: (id: string, patch: Partial<NewConnection>) => Promise<void>
  onRemove: (id: string) => Promise<void>
  onCreateLocal: (label: string) => Promise<void>
  onStartLocal: (id: string) => Promise<void>
  onStopLocal: (id: string) => Promise<void>
  onRemoveLocal: (id: string) => Promise<void>
}

const EMPTY: NewConnection = { label: '', baseUrl: '', kind: 'admin', token: '', sourceId: '' }

export function ConnectionManager({
  connections,
  localServers,
  onOpen,
  onAdd,
  onUpdate,
  onRemove,
  onCreateLocal,
  onStartLocal,
  onStopLocal,
  onRemoveLocal,
}: Props): React.JSX.Element {
  // Connections that belong to a managed local server are shown in that section,
  // not in the external-connections list.
  const external = connections.filter((c) => !c.localServerId)
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
    <div className="space-y-6">
      <LocalServers
        servers={localServers}
        onOpen={onOpen}
        onCreate={onCreateLocal}
        onStart={onStartLocal}
        onStop={onStopLocal}
        onRemove={onRemoveLocal}
      />

      <div className="space-y-4">
        <p className="section-title">External connection</p>
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

      {external.length > 0 && (
        <div className="card divide-y divide-gray-100">
          {external.map((c) => (
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
    </div>
  )
}

// ---------------------------------------------------------------------------
// Local servers — managed child processes, created and controlled from here
// ---------------------------------------------------------------------------

function LocalServers({
  servers,
  onOpen,
  onCreate,
  onStart,
  onStop,
  onRemove,
}: {
  servers: LocalServer[]
  onOpen: (id: string) => Promise<void>
  onCreate: (label: string) => Promise<void>
  onStart: (id: string) => Promise<void>
  onStop: (id: string) => Promise<void>
  onRemove: (id: string) => Promise<void>
}): React.JSX.Element {
  const [label, setLabel] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const create = async () => {
    const name = label.trim()
    if (!name) return
    setBusy(true)
    setError(null)
    try {
      await onCreate(name)
      setLabel('')
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <div className="space-y-3">
      <p className="section-title">Local servers</p>
      <div className="card p-4 space-y-3">
        <div className="flex gap-2">
          <input
            className="input flex-1"
            value={label}
            onChange={(e) => setLabel(e.target.value)}
            onKeyDown={(e) => e.key === 'Enter' && create()}
            placeholder="New local server label"
          />
          <button className="btn-primary" onClick={create} disabled={busy || !label.trim()}>
            {busy ? 'Starting…' : 'New local server'}
          </button>
        </div>
        {error && <p className="text-text-sm text-error-500">{error}</p>}
        <p className="text-text-xs text-gray-400">
          Runs the server on your machine as a managed process on its own port.
        </p>
      </div>

      {servers.length > 0 && (
        <div className="card divide-y divide-gray-100">
          {servers.map((s) => (
            <div key={s.id} className="flex items-center justify-between px-4 py-3 gap-3">
              <div className="min-w-0">
                <p className="text-text-sm font-medium text-gray-900 truncate flex items-center gap-2">
                  <span className={`inline-block w-2 h-2 rounded-full ${s.running ? 'bg-success-500' : 'bg-gray-300'}`} />
                  {s.label}
                </p>
                <p className="text-text-xs text-gray-500 truncate font-mono">{s.baseUrl}</p>
              </div>
              <div className="flex items-center gap-2 shrink-0">
                {s.running ? (
                  <>
                    <button
                      className="btn-primary text-text-xs px-2 py-1"
                      onClick={() => s.connectionId && onOpen(s.connectionId)}
                      disabled={!s.connectionId}
                    >
                      Open
                    </button>
                    <button className="btn-secondary text-text-xs px-2 py-1" onClick={() => onStop(s.id)}>Stop</button>
                  </>
                ) : (
                  <button className="btn-secondary text-text-xs px-2 py-1" onClick={() => onStart(s.id)}>Start</button>
                )}
                <button className="btn-danger text-text-xs px-2 py-1" onClick={() => onRemove(s.id)}>Remove</button>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}
