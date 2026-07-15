import React, { useCallback, useEffect, useState } from 'react'
import type { Connection, NewConnection, SourceConfig, SourceRow, SourceType, TokenRow } from '../../../core/client'

const api = window.entityGraph

const SOURCE_TYPES: SourceType[] = ['sqlite', 'combined', 'frozen', 'filter', 'remote']
const SAFETIES = ['pure', 'safe-mutating', 'dangerous'] as const

interface Props {
  conn: Connection
  onConnectToSource: (cfg: NewConnection) => Promise<void>
}

export function AdminPanel({ conn, onConnectToSource }: Props): React.JSX.Element {
  const [sources, setSources] = useState<SourceRow[]>([])
  const [error, setError] = useState<string | null>(null)
  const [editing, setEditing] = useState<SourceRow | null>(null) // non-null → editing existing
  const [showForm, setShowForm] = useState(false)

  const refresh = useCallback(async () => {
    setError(null)
    try {
      setSources(await api.adminListSources(conn.id))
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }, [conn.id])

  useEffect(() => { void refresh() }, [refresh])

  const remove = async (id: string) => {
    try {
      await api.adminDeleteSource(conn.id, id)
      await refresh()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  const connect = async (s: SourceRow) => {
    try {
      const { token } = await api.adminIssueToken(conn.id, s.id, 'app')
      await onConnectToSource({
        label: s.label,
        baseUrl: conn.baseUrl,
        kind: 'source',
        token,
        sourceId: s.id,
      })
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  return (
    <div className="space-y-4">
      <div className="flex items-center justify-between">
        <p className="section-title">Sources</p>
        <button
          className="btn-primary text-text-sm"
          onClick={() => { setEditing(null); setShowForm(true) }}
        >
          New source
        </button>
      </div>

      {error && <p className="text-text-sm text-error-500">{error}</p>}

      {showForm && (
        <SourceForm
          connId={conn.id}
          existing={editing}
          onDone={async () => { setShowForm(false); setEditing(null); await refresh() }}
          onCancel={() => { setShowForm(false); setEditing(null) }}
        />
      )}

      {sources.length === 0 ? (
        <p className="text-text-sm text-gray-400">No sources yet.</p>
      ) : (
        <div className="space-y-2">
          {sources.map((s) => (
            <SourceCard
              key={s.id}
              connId={conn.id}
              source={s}
              onEdit={() => { setEditing(s); setShowForm(true) }}
              onRemove={() => remove(s.id)}
              onConnect={() => connect(s)}
            />
          ))}
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// Per-source card: metadata, token management, actions
// ---------------------------------------------------------------------------

function SourceCard({
  connId,
  source,
  onEdit,
  onRemove,
  onConnect,
}: {
  connId: string
  source: SourceRow
  onEdit: () => void
  onRemove: () => void
  onConnect: () => void
}): React.JSX.Element {
  const [tokens, setTokens] = useState<TokenRow[] | null>(null)
  const [issued, setIssued] = useState<string | null>(null)
  const [error, setError] = useState<string | null>(null)

  const loadTokens = async () => {
    setError(null)
    try {
      setTokens(await api.adminListTokens(connId, source.id))
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  const issue = async () => {
    try {
      const { token } = await api.adminIssueToken(connId, source.id, 'manual')
      setIssued(token)
      await loadTokens()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  const revoke = async (token: string) => {
    try {
      await api.adminRevokeToken(connId, token)
      await loadTokens()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  return (
    <div className="card p-3 space-y-2">
      <div className="flex items-start justify-between gap-2">
        <div className="min-w-0">
          <span className="font-medium text-text-sm text-gray-900">{source.label}</span>
          <span className="font-mono text-text-xs text-gray-400 ml-2">{source.id}</span>
        </div>
        <div className="flex items-center gap-2 shrink-0">
          <span className="badge badge-gray">{source.type}</span>
          <button className="btn-primary text-text-xs px-2 py-1" onClick={onConnect}>Connect</button>
          <button className="btn-secondary text-text-xs px-2 py-1" onClick={onEdit}>Edit</button>
          <button className="btn-danger text-text-xs px-2 py-1" onClick={onRemove}>Delete</button>
        </div>
      </div>

      <div className="text-text-xs font-mono text-gray-500 break-all">{JSON.stringify(source.config)}</div>

      <div className="flex items-center gap-2">
        <button className="text-primary-600 hover:text-primary-800 text-text-xs" onClick={loadTokens}>
          {tokens ? 'refresh tokens' : 'show tokens'}
        </button>
        <button className="text-primary-600 hover:text-primary-800 text-text-xs" onClick={issue}>issue token</button>
      </div>

      {error && <p className="text-text-xs text-error-500">{error}</p>}
      {issued && (
        <p className="text-text-xs font-mono bg-success-50 text-success-700 rounded px-2 py-1 break-all">
          new token: {issued}
        </p>
      )}

      {tokens && tokens.length > 0 && (
        <div className="rounded-md bg-gray-50 border border-gray-200 divide-y divide-gray-100 text-text-xs font-mono">
          {tokens.map((t) => (
            <div key={t.token} className="flex items-center justify-between px-3 py-1.5 gap-2">
              <span className={`truncate ${t.revoked ? 'line-through text-gray-400' : 'text-gray-900'}`}>
                {t.token.slice(0, 12)}… {t.label && `(${t.label})`}
              </span>
              {!t.revoked && (
                <button className="text-error-600 hover:text-error-800 shrink-0" onClick={() => revoke(t.token)}>
                  revoke
                </button>
              )}
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// Create / edit form — type-aware config fields for all five source types
// ---------------------------------------------------------------------------

function SourceForm({
  connId,
  existing,
  onDone,
  onCancel,
}: {
  connId: string
  existing: SourceRow | null
  onDone: () => void
  onCancel: () => void
}): React.JSX.Element {
  const [id, setId] = useState(existing?.id ?? '')
  const [label, setLabel] = useState(existing?.label ?? '')
  const [type, setType] = useState<SourceType>(existing?.config.type ?? 'sqlite')
  const [cfg, setCfg] = useState<Record<string, string>>(() => flattenConfig(existing?.config))
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const set = (k: string, v: string) => setCfg((c) => ({ ...c, [k]: v }))

  const submit = async () => {
    setBusy(true)
    setError(null)
    try {
      const config = buildConfig(type, cfg)
      if (existing) {
        await api.adminUpdateSource(connId, existing.id, { label: label.trim() || existing.id, config })
      } else {
        if (!id.trim()) throw new Error('id is required')
        await api.adminCreateSource(connId, { id: id.trim(), label: label.trim() || id.trim(), config })
      }
      onDone()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <div className="card p-4 space-y-3 border-primary-200">
      <p className="section-title">{existing ? `Edit ${existing.id}` : 'New source'}</p>

      {!existing && (
        <div>
          <label className="label">ID</label>
          <input className="input font-mono" value={id} onChange={(e) => setId(e.target.value)} placeholder="my-source" />
        </div>
      )}

      <div>
        <label className="label">Label</label>
        <input className="input" value={label} onChange={(e) => setLabel(e.target.value)} placeholder="My source" />
      </div>

      <div>
        <label className="label">Type</label>
        <select className="input" value={type} onChange={(e) => setType(e.target.value as SourceType)} disabled={!!existing}>
          {SOURCE_TYPES.map((t) => <option key={t} value={t}>{t}</option>)}
        </select>
      </div>

      <ConfigFields type={type} cfg={cfg} set={set} />

      {error && <p className="text-text-sm text-error-500">{error}</p>}

      <div className="flex gap-2">
        <button className="btn-primary" onClick={submit} disabled={busy}>{busy ? 'Saving…' : 'Save'}</button>
        <button className="btn-secondary" onClick={onCancel} disabled={busy}>Cancel</button>
      </div>
    </div>
  )
}

function ConfigFields({
  type,
  cfg,
  set,
}: {
  type: SourceType
  cfg: Record<string, string>
  set: (k: string, v: string) => void
}): React.JSX.Element {
  const field = (key: string, label: string, placeholder = '', mono = true) => (
    <div>
      <label className="label">{label}</label>
      <input
        className={`input ${mono ? 'font-mono' : ''}`}
        value={cfg[key] ?? ''}
        onChange={(e) => set(key, e.target.value)}
        placeholder={placeholder}
      />
    </div>
  )

  switch (type) {
    case 'sqlite':
      return <>{field('path', 'DB file path', '/path/to/store.db')}{field('defaultAuthor', 'Default author (optional)', 'anonymous', false)}</>
    case 'combined':
      return field('children', 'Child source ids (comma-separated)', 'a, b, c')
    case 'frozen':
      return <>{field('child', 'Child source id', 'source-id')}{field('beforeTs', 'Before timestamp (Unix ms)', '1700000000000')}</>
    case 'filter':
      return (
        <>
          {field('child', 'Child source id', 'source-id')}
          {field('allow', 'Allow tool ids (comma-separated, optional)', 'readEvents, query')}
          {field('deny', 'Deny tool ids (comma-separated, optional)', 'writeValue')}
          <div>
            <label className="label">Max safety (optional)</label>
            <select className="input" value={cfg.maxSafety ?? ''} onChange={(e) => set('maxSafety', e.target.value)}>
              <option value="">(none)</option>
              {SAFETIES.map((s) => <option key={s} value={s}>{s}</option>)}
            </select>
          </div>
        </>
      )
    case 'remote':
      return <>{field('url', 'Remote source URL', 'http://host:4000/other-source')}{field('token', 'Remote token (optional)', '')}</>
  }
}

// ---------------------------------------------------------------------------
// Config (de)serialisation between the flat form state and SourceConfig
// ---------------------------------------------------------------------------

function flattenConfig(c?: SourceConfig): Record<string, string> {
  if (!c) return {}
  switch (c.type) {
    case 'sqlite': return { path: c.path, defaultAuthor: c.defaultAuthor ?? '' }
    case 'combined': return { children: c.children.join(', ') }
    case 'frozen': return { child: c.child, beforeTs: String(c.beforeTs) }
    case 'filter': return {
      child: c.child,
      allow: (c.allow ?? []).join(', '),
      deny: (c.deny ?? []).join(', '),
      maxSafety: c.maxSafety ?? '',
    }
    case 'remote': return { url: c.url, token: c.token ?? '' }
  }
}

const csv = (s?: string): string[] =>
  (s ?? '').split(',').map((x) => x.trim()).filter(Boolean)

function buildConfig(type: SourceType, cfg: Record<string, string>): SourceConfig {
  switch (type) {
    case 'sqlite':
      if (!cfg.path?.trim()) throw new Error('path is required')
      return { type, path: cfg.path.trim(), ...(cfg.defaultAuthor?.trim() ? { defaultAuthor: cfg.defaultAuthor.trim() } : {}) }
    case 'combined':
      return { type, children: csv(cfg.children) }
    case 'frozen':
      if (!cfg.child?.trim()) throw new Error('child is required')
      return { type, child: cfg.child.trim(), beforeTs: Number(cfg.beforeTs || 0) }
    case 'filter': {
      if (!cfg.child?.trim()) throw new Error('child is required')
      const allow = csv(cfg.allow)
      const deny = csv(cfg.deny)
      return {
        type,
        child: cfg.child.trim(),
        ...(allow.length ? { allow } : {}),
        ...(deny.length ? { deny } : {}),
        ...(cfg.maxSafety ? { maxSafety: cfg.maxSafety as (typeof SAFETIES)[number] } : {}),
      }
    }
    case 'remote':
      if (!cfg.url?.trim()) throw new Error('url is required')
      return { type, url: cfg.url.trim(), ...(cfg.token?.trim() ? { token: cfg.token.trim() } : {}) }
  }
}
