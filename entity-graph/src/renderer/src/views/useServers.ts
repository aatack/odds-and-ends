import { useCallback, useEffect, useMemo, useState } from 'react'
import type {
  NewServer,
  ServerView,
  SourceConfig,
  SourceConnection,
  SourceRow,
  SourceType,
  TokenRow,
} from '../../../core/client'

const graph = window.entityGraph

/** One source shown under a server — unifies admin sources and saved connections. */
export interface SourceItem {
  /** Stable React key. */
  key: string
  /** The source's id on the server. */
  sourceId: string
  label: string
  /** Admin sources only. */
  type?: SourceType
  config?: SourceConfig
  /** Non-admin servers only: the SourceConnection's own id (for edit / remove). */
  connectionId?: string
}

/** A server plus its resolved sources, ready to render. */
export interface ServerRowModel {
  server: ServerView
  sources: SourceItem[]
  /** Set when this server's sources failed to load (e.g. admin server offline). */
  sourcesError: string | null
  /** A start/stop request is in flight (swaps the play/pause icon for busy). */
  busy: boolean
}

export interface ServerActions {
  refresh: () => Promise<void>
  createLocalServer: (label: string) => Promise<void>
  addServer: (cfg: NewServer) => Promise<void>
  updateServer: (id: string, patch: Partial<NewServer>) => Promise<void>
  removeServer: (id: string) => Promise<void>
  startServer: (id: string) => Promise<void>
  stopServer: (id: string) => Promise<void>
  // Admin-server sources
  saveAdminSource: (
    serverId: string,
    existingId: string | null,
    body: { label: string; config: SourceConfig },
  ) => Promise<void>
  deleteAdminSource: (serverId: string, id: string) => Promise<void>
  // Non-admin source connections
  saveSourceConnection: (
    existingId: string | null,
    body: { serverId: string; sourceId: string; label: string; token: string },
  ) => Promise<void>
  deleteSourceConnection: (id: string) => Promise<void>
  // Tokens (admin sources)
  listTokens: (serverId: string, sourceId: string) => Promise<TokenRow[]>
  issueToken: (serverId: string, sourceId: string) => Promise<string>
  revokeToken: (serverId: string, token: string) => Promise<void>
}

/**
 * All logic for the server/source configuration tree. Holds the latent state
 * (servers, saved connections, per-admin-server live sources, in-flight starts)
 * and derives the render-ready row models. Transport lives entirely here so the
 * dumb components never touch the IPC surface.
 */
export function useServers(): {
  rows: ServerRowModel[]
  error: string | null
  ready: boolean
  actions: ServerActions
} {
  const [servers, setServers] = useState<ServerView[]>([])
  const [sourceConns, setSourceConns] = useState<SourceConnection[]>([])
  const [adminSources, setAdminSources] = useState<Record<string, SourceRow[]>>({})
  const [sourceErrors, setSourceErrors] = useState<Record<string, string>>({})
  const [busy, setBusy] = useState<Set<string>>(new Set())
  const [error, setError] = useState<string | null>(null)
  const [ready, setReady] = useState(false)

  const refresh = useCallback(async () => {
    setError(null)
    try {
      const [srv, conns] = await Promise.all([graph.listServers(), graph.listSourceConnections()])
      setServers(srv)
      setSourceConns(conns)

      // Load live sources for every admin server, best-effort per server.
      const sources: Record<string, SourceRow[]> = {}
      const errs: Record<string, string> = {}
      await Promise.all(
        srv
          .filter((s) => s.admin)
          .map(async (s) => {
            try {
              sources[s.id] = await graph.adminListSources(s.id)
            } catch (e) {
              errs[s.id] = e instanceof Error ? e.message : String(e)
            }
          }),
      )
      setAdminSources(sources)
      setSourceErrors(errs)
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    }
  }, [])

  useEffect(() => {
    void refresh().finally(() => setReady(true))
  }, [refresh])

  const withBusy = useCallback(
    async (id: string, fn: () => Promise<void>) => {
      setBusy((b) => new Set(b).add(id))
      try {
        await fn()
        await refresh()
      } finally {
        setBusy((b) => {
          const next = new Set(b)
          next.delete(id)
          return next
        })
      }
    },
    [refresh],
  )

  const actions = useMemo<ServerActions>(
    () => ({
      refresh,
      createLocalServer: async (label) => {
        await graph.createLocalServer(label)
        await refresh()
      },
      addServer: async (cfg) => {
        await graph.addServer(cfg)
        await refresh()
      },
      updateServer: async (id, patch) => {
        await graph.updateServer(id, patch)
        await refresh()
      },
      removeServer: async (id) => {
        await graph.removeServer(id)
        await refresh()
      },
      startServer: (id) => withBusy(id, () => graph.startServer(id)),
      stopServer: (id) => withBusy(id, () => graph.stopServer(id)),
      saveAdminSource: async (serverId, existingId, body) => {
        if (existingId) {
          await graph.adminUpdateSource(serverId, existingId, { label: body.label, config: body.config })
        } else {
          // No id — the main process assigns one.
          await graph.adminCreateSource(serverId, { label: body.label, config: body.config })
        }
        await refresh()
      },
      deleteAdminSource: async (serverId, id) => {
        await graph.adminDeleteSource(serverId, id)
        await refresh()
      },
      saveSourceConnection: async (existingId, body) => {
        if (existingId) {
          await graph.updateSourceConnection(existingId, body)
        } else {
          await graph.addSourceConnection(body)
        }
        await refresh()
      },
      deleteSourceConnection: async (id) => {
        await graph.removeSourceConnection(id)
        await refresh()
      },
      listTokens: (serverId, sourceId) => graph.adminListTokens(serverId, sourceId),
      issueToken: async (serverId, sourceId) => {
        const { token } = await graph.adminIssueToken(serverId, sourceId, 'manual')
        return token
      },
      revokeToken: async (serverId, token) => {
        await graph.adminRevokeToken(serverId, token)
      },
    }),
    [refresh, withBusy],
  )

  const rows = useMemo<ServerRowModel[]>(
    () =>
      servers.map((server) => {
        const sources: SourceItem[] = server.admin
          ? (adminSources[server.id] ?? []).map((s) => ({
              key: s.id,
              sourceId: s.id,
              label: s.label,
              type: s.type,
              config: s.config,
            }))
          : sourceConns
              .filter((c) => c.serverId === server.id)
              .map((c) => ({
                key: c.id,
                sourceId: c.sourceId,
                label: c.label,
                connectionId: c.id,
              }))
        return {
          server,
          sources,
          sourcesError: sourceErrors[server.id] ?? null,
          busy: busy.has(server.id),
        }
      }),
    [servers, sourceConns, adminSources, sourceErrors, busy],
  )

  return { rows, error, ready, actions }
}
