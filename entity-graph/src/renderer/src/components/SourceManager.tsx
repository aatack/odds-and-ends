import React, { useState } from 'react'

interface SourceConfig {
  id: string
  label: string
  type: 'sqlite' | 'http'
  path?: string
  url?: string
}

interface Props {
  sources: SourceConfig[]
  onAdd: (cfg: Omit<SourceConfig, 'id'>) => Promise<void>
  onRemove: (id: string) => Promise<void>
}

export function SourceManager({ sources, onAdd, onRemove }: Props) {
  const [type, setType]   = useState<'sqlite' | 'http'>('sqlite')
  const [label, setLabel] = useState('')
  const [path, setPath]   = useState('')
  const [url, setUrl]     = useState('')
  const [busy, setBusy]   = useState(false)

  const handleAdd = async () => {
    if (!label.trim()) return
    setBusy(true)
    try {
      const cfg: Omit<SourceConfig, 'id'> =
        type === 'sqlite'
          ? { label: label.trim(), type: 'sqlite', path: path.trim() }
          : { label: label.trim(), type: 'http',   url:  url.trim()  }
      await onAdd(cfg)
      setLabel(''); setPath(''); setUrl('')
    } finally {
      setBusy(false)
    }
  }

  return (
    <div className="space-y-4">
      <div className="card p-4 space-y-3">
        <p className="section-title">Add source</p>

        <div className="flex gap-2">
          {(['sqlite', 'http'] as const).map((t) => (
            <button
              key={t}
              onClick={() => setType(t)}
              className={`btn text-text-sm ${type === t ? 'btn-primary' : 'btn-secondary'}`}
            >
              {t === 'sqlite' ? 'SQLite file' : 'HTTP server'}
            </button>
          ))}
        </div>

        <div>
          <label className="label">Label</label>
          <input className="input" value={label} onChange={(e) => setLabel(e.target.value)} placeholder="My notes" />
        </div>

        {type === 'sqlite' ? (
          <div>
            <label className="label">File path</label>
            <input className="input" value={path} onChange={(e) => setPath(e.target.value)} placeholder="/path/to/notes.db" />
          </div>
        ) : (
          <div>
            <label className="label">Base URL</label>
            <input className="input" value={url} onChange={(e) => setUrl(e.target.value)} placeholder="http://localhost:4000" />
          </div>
        )}

        <button className="btn-primary" onClick={handleAdd} disabled={busy || !label.trim()}>
          {busy ? 'Adding…' : 'Add source'}
        </button>
      </div>

      {sources.length > 0 && (
        <div className="card divide-y divide-gray-100">
          {sources.map((s) => (
            <div key={s.id} className="flex items-center justify-between px-4 py-3 gap-3">
              <div className="min-w-0">
                <p className="text-text-sm font-medium text-gray-900 truncate">{s.label}</p>
                <p className="text-text-xs text-gray-500 truncate">{s.type === 'sqlite' ? s.path : s.url}</p>
              </div>
              <div className="flex items-center gap-2 shrink-0">
                <span className={`badge ${s.type === 'sqlite' ? 'badge-blue' : 'badge-green'}`}>{s.type}</span>
                <button className="btn-danger text-text-xs px-2 py-1" onClick={() => onRemove(s.id)}>
                  Remove
                </button>
              </div>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}
