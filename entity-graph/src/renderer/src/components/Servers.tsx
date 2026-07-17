import React, { useEffect, useState } from 'react'
import type { ActiveSource, ServerView, SourceType, TokenRow } from '../../../core/client'
import { useServers, type ServerActions, type ServerRowModel, type SourceItem } from '../views/useServers'
import { ConfigFields, SOURCE_TYPES, buildConfig, flattenConfig } from './sourceConfig'

interface Props {
  onOpenSource: (active: ActiveSource) => void
}

/**
 * The server/source configuration page: every server, with its sources nested
 * underneath. All logic comes from {@link useServers}; the components here only
 * render and open the editor modals.
 */
export function Servers({ onOpenSource }: Props): React.JSX.Element | null {
  const { rows, error, ready, actions } = useServers()
  const [serverEditor, setServerEditor] = useState<{ existing: ServerView | null } | null>(null)
  const [sourceEditor, setSourceEditor] = useState<{ server: ServerView; existing: SourceItem | null } | null>(null)

  if (!ready) return null

  const openSource = async (serverId: string, sourceId: string, label: string): Promise<void> => {
    onOpenSource(await actions.openSource(serverId, sourceId, label))
  }

  return (
    <div className="space-y-4">
      <div className="flex items-center justify-between">
        <p className="section-title">Servers</p>
        <button className="btn-primary text-text-sm" onClick={() => setServerEditor({ existing: null })}>
          <PlusIcon /> Add server
        </button>
      </div>

      {error && <p className="text-text-sm text-error-500">{error}</p>}

      {rows.length === 0 ? (
        <p className="text-text-sm text-gray-400">No servers yet.</p>
      ) : (
        <div className="space-y-3">
          {rows.map((row) => (
            <ServerRow
              key={row.server.id}
              row={row}
              onEdit={() => setServerEditor({ existing: row.server })}
              onRemove={() => actions.removeServer(row.server.id)}
              onStart={() => actions.startServer(row.server.id)}
              onStop={() => actions.stopServer(row.server.id)}
              onAddSource={() => setSourceEditor({ server: row.server, existing: null })}
              onEditSource={(s) => setSourceEditor({ server: row.server, existing: s })}
              onRemoveSource={(s) =>
                row.server.admin
                  ? actions.deleteAdminSource(row.server.id, s.sourceId)
                  : actions.deleteSourceConnection(s.connectionId!)
              }
              onOpenSource={(s) => openSource(row.server.id, s.sourceId, s.label)}
            />
          ))}
        </div>
      )}

      {serverEditor && (
        <ServerEditor
          existing={serverEditor.existing}
          actions={actions}
          onClose={() => setServerEditor(null)}
        />
      )}
      {sourceEditor && (
        <SourceEditor
          server={sourceEditor.server}
          existing={sourceEditor.existing}
          actions={actions}
          onClose={() => setSourceEditor(null)}
        />
      )}
    </div>
  )
}

// ---------------------------------------------------------------------------
// Server row — header + nested sources
// ---------------------------------------------------------------------------

function ServerRow({
  row,
  onEdit,
  onRemove,
  onStart,
  onStop,
  onAddSource,
  onEditSource,
  onRemoveSource,
  onOpenSource,
}: {
  row: ServerRowModel
  onEdit: () => void
  onRemove: () => void
  onStart: () => void
  onStop: () => void
  onAddSource: () => void
  onEditSource: (s: SourceItem) => void
  onRemoveSource: (s: SourceItem) => void
  onOpenSource: (s: SourceItem) => void
}): React.JSX.Element {
  const { server, sources, sourcesError, busy } = row

  return (
    <div className="card">
      <div className="flex items-center justify-between px-4 py-3 gap-3 border-b border-gray-100">
        <div className="min-w-0 flex items-center gap-2">
          {server.kind === 'local' && (
            <span className={`inline-block w-2 h-2 rounded-full ${server.running ? 'bg-success-500' : 'bg-gray-300'}`} />
          )}
          <div className="min-w-0">
            <p className="text-text-sm font-medium text-gray-900 truncate">{server.label}</p>
            <p className="text-text-xs text-gray-500 truncate font-mono">{server.baseUrl}</p>
          </div>
        </div>
        <div className="flex items-center gap-2 shrink-0">
          <span className={`badge ${server.kind === 'local' ? 'badge-green' : 'badge-gray'}`}>{server.kind}</span>
          {server.admin && <span className="badge badge-blue">admin</span>}
          {server.kind === 'local' && (
            <IconButton
              title={busy ? 'Working…' : server.running ? 'Stop' : 'Start'}
              onClick={busy ? undefined : server.running ? onStop : onStart}
            >
              {busy ? <Spinner /> : server.running ? <PauseIcon /> : <PlayIcon />}
            </IconButton>
          )}
          <IconButton title="Edit server" onClick={onEdit}><EditIcon /></IconButton>
          <IconButton title="Remove server" onClick={onRemove}><TrashIcon /></IconButton>
        </div>
      </div>

      <div className="px-4 py-2 space-y-1">
        {sourcesError && <p className="text-text-xs text-error-500">{sourcesError}</p>}
        {sources.length === 0 && !sourcesError && (
          <p className="text-text-xs text-gray-400 py-1">No sources yet.</p>
        )}
        {sources.map((s) => (
          <SourceRow
            key={s.key}
            source={s}
            fullUrl={`${server.baseUrl}/${s.sourceId}`}
            onOpen={() => onOpenSource(s)}
            onEdit={() => onEditSource(s)}
            onRemove={() => onRemoveSource(s)}
          />
        ))}
        <button className="text-primary-600 hover:text-primary-800 text-text-xs pt-1 inline-flex items-center gap-1" onClick={onAddSource}>
          <PlusIcon /> {server.admin ? 'Create source' : 'Connect to source'}
        </button>
      </div>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Source row
// ---------------------------------------------------------------------------

function SourceRow({
  source,
  fullUrl,
  onOpen,
  onEdit,
  onRemove,
}: {
  source: SourceItem
  fullUrl: string
  onOpen: () => void
  onEdit: () => void
  onRemove: () => void
}): React.JSX.Element {
  const [copied, setCopied] = useState(false)

  const copyLink = async (): Promise<void> => {
    await navigator.clipboard.writeText(fullUrl)
    setCopied(true)
    setTimeout(() => setCopied(false), 1200)
  }

  return (
    <div className="flex items-center justify-between gap-2 py-1.5 pl-2">
      <button className="min-w-0 text-left flex-1" onClick={onOpen}>
        <span className="text-text-sm text-gray-900 truncate">{source.label}</span>
      </button>
      <div className="flex items-center gap-2 shrink-0">
        {source.type && <span className="badge badge-gray">{source.type}</span>}
        <button className="btn-primary text-text-xs px-2 py-1" onClick={onOpen}>Open</button>
        <IconButton title={copied ? 'Copied!' : 'Copy source link'} onClick={copyLink}>
          {copied ? <CheckIcon /> : <LinkIcon />}
        </IconButton>
        <IconButton title="Edit source" onClick={onEdit}><EditIcon /></IconButton>
        <IconButton title="Remove source" onClick={onRemove}><TrashIcon /></IconButton>
      </div>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Server editor modal
// ---------------------------------------------------------------------------

function ServerEditor({
  existing,
  actions,
  onClose,
}: {
  existing: ServerView | null
  actions: ServerActions
  onClose: () => void
}): React.JSX.Element {
  const editing = !!existing
  const [local, setLocal] = useState(existing ? existing.kind === 'local' : true)
  const [label, setLabel] = useState(existing?.label ?? '')
  const [baseUrl, setBaseUrl] = useState(existing?.baseUrl ?? '')
  const [adminToken, setAdminToken] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const isLocal = editing ? existing.kind === 'local' : local
  const valid = label.trim() && (isLocal || baseUrl.trim())

  const submit = async (): Promise<void> => {
    if (!valid) return
    setBusy(true)
    setError(null)
    try {
      if (editing) {
        await actions.updateServer(existing.id, {
          label: label.trim(),
          ...(isLocal ? {} : { baseUrl: baseUrl.trim() }),
          ...(adminToken.trim() ? { adminToken: adminToken.trim() } : {}),
        })
      } else if (isLocal) {
        await actions.createLocalServer(label.trim())
      } else {
        await actions.addServer({
          label: label.trim(),
          baseUrl: baseUrl.trim(),
          ...(adminToken.trim() ? { adminToken: adminToken.trim() } : {}),
        })
      }
      onClose()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <Modal title={editing ? 'Edit server' : 'Add server'} onClose={onClose}>
      {!editing && (
        <div className="flex gap-2">
          {([true, false] as const).map((v) => (
            <button
              key={String(v)}
              onClick={() => setLocal(v)}
              className={`btn text-text-sm ${local === v ? 'btn-primary' : 'btn-secondary'}`}
            >
              {v ? 'Local' : 'External'}
            </button>
          ))}
        </div>
      )}

      <div>
        <label className="label">Label</label>
        <input className="input" value={label} onChange={(e) => setLabel(e.target.value)} placeholder="My server" autoFocus />
      </div>

      {!isLocal && (
        <div>
          <label className="label">Base URL</label>
          <input className="input font-mono" value={baseUrl} onChange={(e) => setBaseUrl(e.target.value)} placeholder="http://host:4000" />
        </div>
      )}

      {isLocal ? (
        !editing && (
          <p className="text-text-xs text-gray-400">
            Runs the server on your machine as a managed process on its own port, with admin access.
          </p>
        )
      ) : (
        <div>
          <label className="label">Admin token {editing ? '(leave blank to keep)' : '(optional — enables creating sources)'}</label>
          <input
            className="input font-mono"
            type="password"
            value={adminToken}
            onChange={(e) => setAdminToken(e.target.value)}
            placeholder="••••••••"
          />
        </div>
      )}

      {error && <p className="text-text-sm text-error-500">{error}</p>}

      <div className="flex gap-2">
        <button className="btn-primary" onClick={submit} disabled={busy || !valid}>
          {busy ? 'Saving…' : editing ? 'Save' : isLocal ? 'Create local server' : 'Add server'}
        </button>
        <button className="btn-secondary" onClick={onClose} disabled={busy}>Cancel</button>
      </div>
    </Modal>
  )
}

// ---------------------------------------------------------------------------
// Source editor modal — admin (create/edit + tokens) or connect-to-source
// ---------------------------------------------------------------------------

function SourceEditor({
  server,
  existing,
  actions,
  onClose,
}: {
  server: ServerView
  existing: SourceItem | null
  actions: ServerActions
  onClose: () => void
}): React.JSX.Element {
  const title = existing ? `Edit ${existing.label}` : server.admin ? 'Create source' : 'Connect to source'
  return (
    <Modal title={title} onClose={onClose}>
      {server.admin ? (
        <AdminSourceFields server={server} existing={existing} actions={actions} onClose={onClose} />
      ) : (
        <ConnectSourceFields server={server} existing={existing} actions={actions} onClose={onClose} />
      )}
    </Modal>
  )
}

function AdminSourceFields({
  server,
  existing,
  actions,
  onClose,
}: {
  server: ServerView
  existing: SourceItem | null
  actions: ServerActions
  onClose: () => void
}): React.JSX.Element {
  const [label, setLabel] = useState(existing?.label ?? '')
  const [type, setType] = useState<SourceType>(existing?.type ?? 'sqlite')
  const [cfg, setCfg] = useState<Record<string, string>>(() => flattenConfig(existing?.config))
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const set = (k: string, v: string): void => setCfg((c) => ({ ...c, [k]: v }))

  const submit = async (): Promise<void> => {
    setBusy(true)
    setError(null)
    try {
      const config = buildConfig(type, cfg)
      // The id is assigned server-side for new sources; the label is all the user sets.
      await actions.saveAdminSource(server.id, existing?.sourceId ?? null, {
        label: label.trim() || existing?.sourceId || 'Untitled source',
        config,
      })
      onClose()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <>
      <div>
        <label className="label">Label</label>
        <input className="input" value={label} onChange={(e) => setLabel(e.target.value)} placeholder="My source" autoFocus />
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
        <button className="btn-secondary" onClick={onClose} disabled={busy}>Cancel</button>
      </div>

      {existing && <TokenSection serverId={server.id} sourceId={existing.sourceId} actions={actions} />}
    </>
  )
}

function ConnectSourceFields({
  server,
  existing,
  actions,
  onClose,
}: {
  server: ServerView
  existing: SourceItem | null
  actions: ServerActions
  onClose: () => void
}): React.JSX.Element {
  const [sourceId, setSourceId] = useState(existing?.sourceId ?? '')
  const [label, setLabel] = useState(existing?.label ?? '')
  const [token, setToken] = useState('')
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const valid = sourceId.trim() && (existing ? true : token.trim())

  const submit = async (): Promise<void> => {
    if (!valid) return
    setBusy(true)
    setError(null)
    try {
      await actions.saveSourceConnection(existing?.connectionId ?? null, {
        serverId: server.id,
        sourceId: sourceId.trim(),
        label: label.trim() || sourceId.trim(),
        // Editing without a new token keeps the stored one (blank patch is ignored server-side).
        token: token.trim(),
      })
      onClose()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <>
      <div>
        <label className="label">Source ID</label>
        <input className="input font-mono" value={sourceId} onChange={(e) => setSourceId(e.target.value)} placeholder="my-source" autoFocus />
      </div>
      <div>
        <label className="label">Label</label>
        <input className="input" value={label} onChange={(e) => setLabel(e.target.value)} placeholder="My source" />
      </div>
      <div>
        <label className="label">Source token {existing ? '(leave blank to keep)' : ''}</label>
        <input className="input font-mono" type="password" value={token} onChange={(e) => setToken(e.target.value)} placeholder="••••••••" />
      </div>

      {error && <p className="text-text-sm text-error-500">{error}</p>}

      <div className="flex gap-2">
        <button className="btn-primary" onClick={submit} disabled={busy || !valid}>{busy ? 'Saving…' : 'Save'}</button>
        <button className="btn-secondary" onClick={onClose} disabled={busy}>Cancel</button>
      </div>
    </>
  )
}

function TokenSection({
  serverId,
  sourceId,
  actions,
}: {
  serverId: string
  sourceId: string
  actions: ServerActions
}): React.JSX.Element {
  const [tokens, setTokens] = useState<TokenRow[] | null>(null)
  const [issued, setIssued] = useState<string | null>(null)
  const [error, setError] = useState<string | null>(null)

  const load = async (): Promise<void> => {
    setError(null)
    try {
      setTokens(await actions.listTokens(serverId, sourceId))
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  useEffect(() => { void load() }, [serverId, sourceId]) // eslint-disable-line react-hooks/exhaustive-deps

  const issue = async (): Promise<void> => {
    setError(null)
    try {
      setIssued(await actions.issueToken(serverId, sourceId))
      await load()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  const revoke = async (token: string): Promise<void> => {
    setError(null)
    try {
      await actions.revokeToken(serverId, token)
      await load()
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }

  return (
    <section className="pt-2 border-t border-gray-100 space-y-2">
      <div className="flex items-center justify-between">
        <p className="section-title">Access tokens</p>
        <button className="text-primary-600 hover:text-primary-800 text-text-xs" onClick={issue}>issue token</button>
      </div>
      {error && <p className="text-text-xs text-error-500">{error}</p>}
      {issued && (
        <p className="text-text-xs font-mono bg-success-50 text-success-700 rounded px-2 py-1 break-all">
          new token: {issued}
        </p>
      )}
      {tokens && tokens.length > 0 ? (
        <div className="rounded-md bg-gray-50 border border-gray-200 divide-y divide-gray-100 text-text-xs font-mono">
          {tokens.map((t) => (
            <div key={t.token} className="flex items-center justify-between px-3 py-1.5 gap-2">
              <span className={`truncate ${t.revoked ? 'line-through text-gray-400' : 'text-gray-900'}`}>
                {t.token.slice(0, 12)}… {t.label && `(${t.label})`}
              </span>
              {!t.revoked && (
                <button className="text-error-600 hover:text-error-800 shrink-0" onClick={() => revoke(t.token)}>revoke</button>
              )}
            </div>
          ))}
        </div>
      ) : (
        tokens && <p className="text-text-xs text-gray-400">No tokens.</p>
      )}
    </section>
  )
}

// ---------------------------------------------------------------------------
// Shared primitives — modal, icon button, icons
// ---------------------------------------------------------------------------

function Modal({
  title,
  onClose,
  children,
}: {
  title: string
  onClose: () => void
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <div className="fixed inset-0 z-50 bg-black/40 flex items-start justify-center p-6 overflow-y-auto" onClick={onClose}>
      <div className="bg-white rounded-lg shadow-xl w-full max-w-md my-6" onClick={(e) => e.stopPropagation()}>
        <div className="flex items-center justify-between px-5 py-3 border-b border-gray-200">
          <p className="font-semibold text-gray-900">{title}</p>
          <button className="text-gray-400 hover:text-gray-700 text-text-lg leading-none" onClick={onClose}>✕</button>
        </div>
        <div className="p-5 space-y-3">{children}</div>
      </div>
    </div>
  )
}

function IconButton({
  title,
  onClick,
  children,
}: {
  title: string
  onClick?: () => void
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <button
      title={title}
      onClick={onClick}
      disabled={!onClick}
      className="p-1.5 rounded-md text-gray-500 hover:text-gray-900 hover:bg-gray-100 disabled:opacity-50 disabled:cursor-default transition-colors"
    >
      {children}
    </button>
  )
}

const ICON = 'w-4 h-4'

function PlayIcon(): React.JSX.Element {
  return <svg className={ICON} viewBox="0 0 20 20" fill="currentColor"><path d="M6 4l10 6-10 6V4z" /></svg>
}
function PauseIcon(): React.JSX.Element {
  return <svg className={ICON} viewBox="0 0 20 20" fill="currentColor"><path d="M6 4h3v12H6zM11 4h3v12h-3z" /></svg>
}
function Spinner(): React.JSX.Element {
  return (
    <svg className={`${ICON} animate-spin`} viewBox="0 0 24 24" fill="none">
      <circle className="opacity-25" cx="12" cy="12" r="10" stroke="currentColor" strokeWidth="4" />
      <path className="opacity-75" fill="currentColor" d="M4 12a8 8 0 018-8v4a4 4 0 00-4 4H4z" />
    </svg>
  )
}
function EditIcon(): React.JSX.Element {
  return (
    <svg className={ICON} viewBox="0 0 20 20" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M13.5 3.5l3 3L7 16l-4 1 1-4 9.5-9.5z" />
    </svg>
  )
}
function TrashIcon(): React.JSX.Element {
  return (
    <svg className={ICON} viewBox="0 0 20 20" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M4 6h12M8 6V4h4v2M6 6l1 10h6l1-10" />
    </svg>
  )
}
function PlusIcon(): React.JSX.Element {
  return <svg className="w-3.5 h-3.5" viewBox="0 0 20 20" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round"><path d="M10 4v12M4 10h12" /></svg>
}
function LinkIcon(): React.JSX.Element {
  return (
    <svg className={ICON} viewBox="0 0 20 20" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M8 12a3 3 0 004.2 0l2.3-2.3a3 3 0 00-4.2-4.2L11 6" />
      <path d="M12 8a3 3 0 00-4.2 0L5.5 10.3a3 3 0 004.2 4.2L11 14" />
    </svg>
  )
}
function CheckIcon(): React.JSX.Element {
  return (
    <svg className={ICON} viewBox="0 0 20 20" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M4 10l4 4 8-9" />
    </svg>
  )
}
