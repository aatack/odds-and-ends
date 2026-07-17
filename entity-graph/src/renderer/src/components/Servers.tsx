import React, { useEffect, useState } from 'react'
import {
  Check,
  Edit01,
  Link03,
  PauseCircle,
  PlayCircle,
  Plus,
  Loading02,
  Trash03,
} from '@untitledui/icons'
import type { CurrentSource, ServerView, SourceType, TokenRow } from '../../../core/client'
import { useServers, type ServerActions, type ServerRowModel, type SourceItem } from '../views/useServers'
import { ConfigFields, SOURCE_TYPES, buildConfig, flattenConfig } from './sourceConfig'
import { Badge } from './ui/Badge'
import { Button } from './ui/Button'
import { Field } from './ui/Field'
import { IconButton } from './ui/IconButton'
import { Input } from './ui/Input'
import { Modal } from './ui/Modal'
import { Select } from './ui/Select'

interface Props {
  /** The source currently shown in the editor, so its row reads as selected. */
  current: CurrentSource | null
  /** Choose which source the editor opens; ticking a source's box calls this. */
  onSelectSource: (source: CurrentSource) => void | Promise<void>
}

const SECTION = 'text-[11px] font-medium uppercase tracking-[0.09em] text-gray-500'

/**
 * The server/source configuration page: every server, with its sources nested
 * underneath. All logic comes from {@link useServers}; the components here only
 * render and open the editor modals. Ticking a source picks it as the editor's
 * current source via {@link Props.onSelectSource}.
 */
export function Servers({ current, onSelectSource }: Props): React.JSX.Element | null {
  const { rows, error, ready, actions } = useServers()
  const [serverEditor, setServerEditor] = useState<{ existing: ServerView | null } | null>(null)
  const [sourceEditor, setSourceEditor] = useState<{ server: ServerView; existing: SourceItem | null } | null>(null)

  if (!ready) return null

  return (
    <div className="space-y-4">
      <div className="flex items-center justify-between">
        <p className={SECTION}>Servers</p>
        <Button variant="primary" size="sm" onClick={() => setServerEditor({ existing: null })}>
          <Plus size={16} /> Add server
        </Button>
      </div>

      {error && <p className="text-[13px] text-error-600">{error}</p>}

      {rows.length === 0 ? (
        <p className="text-[13px] text-gray-400">No servers yet.</p>
      ) : (
        <div className="space-y-3">
          {rows.map((row) => (
            <ServerRow
              key={row.server.id}
              row={row}
              current={current}
              onSelectSource={onSelectSource}
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
  current,
  onSelectSource,
  onEdit,
  onRemove,
  onStart,
  onStop,
  onAddSource,
  onEditSource,
  onRemoveSource,
}: {
  row: ServerRowModel
  current: CurrentSource | null
  onSelectSource: (source: CurrentSource) => void | Promise<void>
  onEdit: () => void
  onRemove: () => void
  onStart: () => void
  onStop: () => void
  onAddSource: () => void
  onEditSource: (s: SourceItem) => void
  onRemoveSource: (s: SourceItem) => void
}): React.JSX.Element {
  const { server, sources, sourcesError, busy } = row

  return (
    <div className="rounded-lg bg-white shadow-xs">
      <div className="flex items-center justify-between gap-3 border-b border-gray-100 px-4 py-3">
        <div className="flex min-w-0 items-center gap-2">
          {server.kind === 'local' && (
            <span
              className={`inline-block size-2 shrink-0 rounded-full ${server.running ? 'bg-success-600' : 'bg-gray-300'}`}
            />
          )}
          <div className="min-w-0">
            <p className="truncate text-[13px] font-medium text-gray-900">{server.label}</p>
            <p className="truncate font-mono text-xs text-gray-500">{server.baseUrl}</p>
          </div>
        </div>
        <div className="flex shrink-0 items-center gap-2">
          <Badge color={server.kind === 'local' ? 'success' : 'gray'}>{server.kind}</Badge>
          {server.admin && <Badge color="brand">admin</Badge>}
          {server.kind === 'local' && (
            <IconButton
              title={busy ? 'Working…' : server.running ? 'Stop' : 'Start'}
              onClick={busy ? undefined : server.running ? onStop : onStart}
            >
              {busy ? (
                <Loading02 size={16} />
              ) : server.running ? (
                <PauseCircle size={16} />
              ) : (
                <PlayCircle size={16} />
              )}
            </IconButton>
          )}
          <IconButton title="Edit server" onClick={onEdit}>
            <Edit01 size={16} />
          </IconButton>
          <IconButton title="Remove server" onClick={onRemove}>
            <Trash03 size={16} />
          </IconButton>
        </div>
      </div>

      <div className="space-y-1 px-4 py-2">
        {sourcesError && <p className="text-xs text-error-600">{sourcesError}</p>}
        {sources.length === 0 && !sourcesError && (
          <p className="py-1 text-xs text-gray-400">No sources yet.</p>
        )}
        {sources.map((s) => (
          <SourceRow
            key={s.key}
            source={s}
            fullUrl={`${server.baseUrl}/${s.sourceId}`}
            selected={current?.serverId === server.id && current?.sourceId === s.sourceId}
            onSelect={() => onSelectSource({ serverId: server.id, sourceId: s.sourceId, label: s.label })}
            onEdit={() => onEditSource(s)}
            onRemove={() => onRemoveSource(s)}
          />
        ))}
        <button
          className="inline-flex items-center gap-1 pt-1 text-xs text-brand-600 hover:text-brand-700 focus:outline-none"
          onClick={onAddSource}
        >
          <Plus size={14} /> {server.admin ? 'Create source' : 'Connect to source'}
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
  selected,
  onSelect,
  onEdit,
  onRemove,
}: {
  source: SourceItem
  fullUrl: string
  /** True when this is the editor's current source. */
  selected: boolean
  /** Pick this source as the editor's current source. */
  onSelect: () => void
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
    <div className="flex items-center justify-between gap-2 rounded-md py-1.5 pl-2 hover:bg-gray-100/70">
      <button
        className="flex min-w-0 flex-1 items-center gap-2 text-left focus:outline-none"
        onClick={onSelect}
        title={selected ? 'Current source' : 'Set as current source'}
      >
        <Tickbox checked={selected} />
        <span
          className={`truncate text-[13px] ${selected ? 'font-medium text-brand-700' : 'text-gray-900'}`}
        >
          {source.label}
        </span>
      </button>
      <div className="flex shrink-0 items-center gap-1">
        {source.type && <Badge color="gray">{source.type}</Badge>}
        <IconButton title={copied ? 'Copied!' : 'Copy source link'} onClick={copyLink}>
          {copied ? <Check size={16} /> : <Link03 size={16} />}
        </IconButton>
        <IconButton title="Edit source" onClick={onEdit}>
          <Edit01 size={16} />
        </IconButton>
        <IconButton title="Remove source" onClick={onRemove}>
          <Trash03 size={16} />
        </IconButton>
      </div>
    </div>
  )
}

/** A small radio-style indicator marking the editor's current source. */
function Tickbox({ checked }: { checked: boolean }): React.JSX.Element {
  return (
    <span
      className={`flex size-4 shrink-0 items-center justify-center rounded-full border ${
        checked ? 'border-brand-600 bg-brand-600 text-white' : 'border-gray-300 text-transparent'
      }`}
    >
      <Check size={10} strokeWidth={3} />
    </span>
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
        <div className="inline-flex rounded-lg bg-gray-100 p-0.5">
          {([true, false] as const).map((v) => (
            <button
              key={String(v)}
              onClick={() => setLocal(v)}
              className={`rounded-md px-3 py-1 text-[13px] font-medium focus:outline-none ${
                local === v ? 'bg-white text-gray-900 shadow-xs' : 'text-gray-500 hover:text-gray-700'
              }`}
            >
              {v ? 'Local' : 'External'}
            </button>
          ))}
        </div>
      )}

      <Field label="Label">
        <Input value={label} onChange={(e) => setLabel(e.target.value)} placeholder="My server" autoFocus />
      </Field>

      {!isLocal && (
        <Field label="Base URL">
          <Input mono value={baseUrl} onChange={(e) => setBaseUrl(e.target.value)} placeholder="http://host:4000" />
        </Field>
      )}

      {isLocal ? (
        !editing && (
          <p className="text-xs text-gray-400">
            Runs the server on your machine as a managed process on its own port, with admin access.
          </p>
        )
      ) : (
        <Field label={`Admin token ${editing ? '(leave blank to keep)' : '(optional — enables creating sources)'}`}>
          <Input
            mono
            type="password"
            value={adminToken}
            onChange={(e) => setAdminToken(e.target.value)}
            placeholder="••••••••"
          />
        </Field>
      )}

      {error && <p className="text-[13px] text-error-600">{error}</p>}

      <div className="flex gap-2">
        <Button variant="primary" onClick={submit} disabled={busy || !valid}>
          {busy ? 'Saving…' : editing ? 'Save' : isLocal ? 'Create local server' : 'Add server'}
        </Button>
        <Button variant="secondary" onClick={onClose} disabled={busy}>
          Cancel
        </Button>
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
      <Field label="Label">
        <Input value={label} onChange={(e) => setLabel(e.target.value)} placeholder="My source" autoFocus />
      </Field>
      <Field label="Type">
        <Select value={type} onChange={(e) => setType(e.target.value as SourceType)} disabled={!!existing}>
          {SOURCE_TYPES.map((t) => (
            <option key={t} value={t}>
              {t}
            </option>
          ))}
        </Select>
      </Field>

      <ConfigFields type={type} cfg={cfg} set={set} />

      {error && <p className="text-[13px] text-error-600">{error}</p>}

      <div className="flex gap-2">
        <Button variant="primary" onClick={submit} disabled={busy}>
          {busy ? 'Saving…' : 'Save'}
        </Button>
        <Button variant="secondary" onClick={onClose} disabled={busy}>
          Cancel
        </Button>
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
      <Field label="Source ID">
        <Input mono value={sourceId} onChange={(e) => setSourceId(e.target.value)} placeholder="my-source" autoFocus />
      </Field>
      <Field label="Label">
        <Input value={label} onChange={(e) => setLabel(e.target.value)} placeholder="My source" />
      </Field>
      <Field label={`Source token ${existing ? '(leave blank to keep)' : ''}`}>
        <Input mono type="password" value={token} onChange={(e) => setToken(e.target.value)} placeholder="••••••••" />
      </Field>

      {error && <p className="text-[13px] text-error-600">{error}</p>}

      <div className="flex gap-2">
        <Button variant="primary" onClick={submit} disabled={busy || !valid}>
          {busy ? 'Saving…' : 'Save'}
        </Button>
        <Button variant="secondary" onClick={onClose} disabled={busy}>
          Cancel
        </Button>
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

  useEffect(() => {
    void load()
  }, [serverId, sourceId]) // eslint-disable-line react-hooks/exhaustive-deps

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
    <section className="space-y-2 border-t border-gray-100 pt-3">
      <div className="flex items-center justify-between">
        <p className={SECTION}>Access tokens</p>
        <button
          className="text-xs text-brand-600 hover:text-brand-700 focus:outline-none"
          onClick={issue}
        >
          issue token
        </button>
      </div>
      {error && <p className="text-xs text-error-600">{error}</p>}
      {issued && (
        <p className="break-all rounded-md bg-success-50 px-2 py-1 font-mono text-xs text-success-700">
          new token: {issued}
        </p>
      )}
      {tokens && tokens.length > 0 ? (
        <div className="overflow-hidden rounded-md bg-gray-50 font-mono text-xs">
          {tokens.map((t) => (
            <div key={t.token} className="flex items-center justify-between gap-2 px-3 py-1.5">
              <span className={`truncate ${t.revoked ? 'text-gray-400 line-through' : 'text-gray-900'}`}>
                {t.token.slice(0, 12)}… {t.label && `(${t.label})`}
              </span>
              {!t.revoked && (
                <button
                  className="shrink-0 text-error-600 hover:text-error-700 focus:outline-none"
                  onClick={() => revoke(t.token)}
                >
                  revoke
                </button>
              )}
            </div>
          ))}
        </div>
      ) : (
        tokens && <p className="text-xs text-gray-400">No tokens.</p>
      )}
    </section>
  )
}
