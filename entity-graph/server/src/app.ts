import Fastify, { type FastifyInstance, type FastifyReply, type FastifyRequest } from 'fastify'
import { ZodError } from 'zod'
import { toolMeta } from '../../src/core/source/index'
import { ConfigDb, RESERVED_IDS, type SourceConfig } from './config'
import { Registry, SourceNotFoundError } from './registry'
import { registerAdmin } from './admin'
import { registerDebug } from './debug'
import { registerMcp } from './mcp'
import { INTEGRATION_TOOLS, runIntegrationTool } from './integrations/index'

export interface AppOptions {
  db: ConfigDb
  registry: Registry
  /** If unset, admin endpoints are open (dev only) and a warning is logged. */
  adminToken?: string
}

interface SourceParams {
  sourceId: string
}

export function bearerToken(req: FastifyRequest): string | null {
  const h = req.headers['authorization']
  if (!h || Array.isArray(h)) return null
  const m = /^Bearer\s+(.+)$/i.exec(h)
  return m ? m[1] : null
}

function formatError(e: unknown): string {
  if (e instanceof ZodError) {
    return e.issues.map((i) => `${i.path.join('.') || '(root)'}: ${i.message}`).join('; ')
  }
  return e instanceof Error ? e.message : String(e)
}

export function buildApp(opts: AppOptions): FastifyInstance {
  const { db, registry } = opts
  const app = Fastify({ logger: false })
  const adminOpen = !opts.adminToken
  if (adminOpen) {
    // eslint-disable-next-line no-console
    console.warn('[entity-graph] ADMIN_TOKEN not set — admin endpoints are OPEN (dev mode).')
  }

  // ---- CORS, for the source-scoped API only ----
  //
  // A browser client served from anywhere but this server — the mobile app, which
  // is a page on the laptop's dev server or wherever it ends up hosted — makes
  // every call cross-origin, and a JSON POST with an `Authorization` header is
  // preflighted, so without this it cannot call a source at all.
  //
  // A wildcard origin is safe *here* because authentication is a bearer token the
  // client sends explicitly, never a cookie: a hostile page gains nothing it
  // didn't already have, since without the token the request is refused just the
  // same. Credentials are deliberately not allowed, which is what keeps that true.
  //
  // The admin surface is excluded, and that exclusion is the point rather than
  // tidiness: with ADMIN_TOKEN unset those endpoints are open (dev mode), and
  // opening them to any page the browser happens to be showing would hand it
  // source creation and deletion on a machine it can merely reach.
  const adminPaths = /^\/(admin|runTool|tools)(\/|$)/

  const corsAllowed = (url: string): boolean => !adminPaths.test(url.split('?')[0])

  app.addHook('onRequest', async (req, reply) => {
    if (corsAllowed(req.url)) {
      reply.header('access-control-allow-origin', '*')
      reply.header('vary', 'origin')
    }
  })

  // Preflight. A route of its own rather than a hook, since a bare OPTIONS request
  // matches no other route and would otherwise be answered by the 404 handler.
  app.options('/*', async (req, reply) => {
    if (!corsAllowed(req.url)) return reply.code(404).send({ error: 'not found' })
    return reply
      .header('access-control-allow-methods', 'GET, POST, PUT, DELETE, OPTIONS')
      .header(
        'access-control-allow-headers',
        (req.headers['access-control-request-headers'] as string | undefined) ??
          'authorization, content-type',
      )
      .header('access-control-max-age', '86400')
      .code(204)
      .send()
  })

  /** preHandler: gate admin endpoints. */
  async function requireAdmin(req: FastifyRequest, reply: FastifyReply): Promise<unknown> {
    if (adminOpen) return
    if (bearerToken(req) === opts.adminToken) return
    return reply.code(401).send({ error: 'admin authentication required' })
  }

  /** preHandler: the source must exist and the bearer token must be valid for it. */
  async function requireSourceAuth(req: FastifyRequest, reply: FastifyReply): Promise<unknown> {
    const { sourceId } = req.params as SourceParams
    if (!db.getSource(sourceId)) {
      return reply.code(404).send({ error: `source "${sourceId}" not found` })
    }
    const token = bearerToken(req)
    if (!token || !db.verifyToken(sourceId, token)) {
      return reply.code(401).send({ error: 'invalid or missing source token' })
    }
  }

  // ---- Source-scoped endpoints ----

  app.get<{ Params: SourceParams }>(
    '/:sourceId/tools',
    { preHandler: requireSourceAuth },
    async (req) => {
      const src = await registry.get(req.params.sourceId)
      return src.tools().map(toolMeta)
    }
  )

  app.post<{ Params: SourceParams; Body: { tool?: string; args?: unknown } }>(
    '/:sourceId/call',
    { preHandler: requireSourceAuth },
    async (req) => {
      const { tool, args } = req.body ?? {}
      if (!tool) return { status: 'error' as const, message: 'request body must include "tool"' }
      try {
        const src = await registry.get(req.params.sourceId)
        const result = await src.call(tool, args ?? {})
        return { status: 'success' as const, result }
      } catch (e) {
        return { status: 'error' as const, message: formatError(e) }
      }
    }
  )

  // ---- Integrations ----
  //
  // Not source-scoped: these are the server's own reach into GitHub, Slack and
  // Claude, so they answer to the admin token rather than to any one source's.
  // `/runTool` is the only door — nothing else invokes an integration — which is
  // what makes the tool list the whole of the surface.

  app.get('/tools', { preHandler: requireAdmin }, async () => INTEGRATION_TOOLS.map(toolMeta))

  app.post<{ Body: { tool?: string; args?: unknown } }>(
    '/runTool',
    { preHandler: requireAdmin },
    async (req) => {
      const { tool, args } = req.body ?? {}
      if (!tool) return { status: 'error' as const, message: 'request body must include "tool"' }
      try {
        return { status: 'success' as const, result: await runIntegrationTool(tool, args ?? {}) }
      } catch (e) {
        return { status: 'error' as const, message: formatError(e) }
      }
    }
  )

  // Static HTML consoles (no token — they prompt for one) and the MCP endpoint.
  registerAdmin(app)
  registerDebug(app)
  registerMcp(app, { db, registry })

  // ---- Admin: source CRUD ----

  app.get('/admin/sources', { preHandler: requireAdmin }, async () => db.listSources())

  app.post<{ Body: { id?: string; label?: string; config?: SourceConfig } }>(
    '/admin/sources',
    { preHandler: requireAdmin },
    async (req, reply) => {
      const { id, label, config } = req.body ?? {}
      if (!id || !config) return reply.code(400).send({ error: 'id and config are required' })
      if (RESERVED_IDS.has(id)) return reply.code(400).send({ error: `id "${id}" is reserved` })
      if (db.getSource(id)) return reply.code(409).send({ error: `source "${id}" already exists` })
      const row = db.createSource({ id, label: label ?? id, config })
      registry.invalidate()
      return reply.code(201).send(row)
    }
  )

  app.get<{ Params: { id: string } }>(
    '/admin/sources/:id',
    { preHandler: requireAdmin },
    async (req, reply) => {
      const row = db.getSource(req.params.id)
      return row ?? reply.code(404).send({ error: 'not found' })
    }
  )

  app.put<{ Params: { id: string }; Body: { label?: string; config?: SourceConfig } }>(
    '/admin/sources/:id',
    { preHandler: requireAdmin },
    async (req, reply) => {
      if (!db.getSource(req.params.id)) return reply.code(404).send({ error: 'not found' })
      const row = db.updateSource(req.params.id, req.body ?? {})
      registry.invalidate()
      return row
    }
  )

  app.delete<{ Params: { id: string } }>(
    '/admin/sources/:id',
    { preHandler: requireAdmin },
    async (req, reply) => {
      if (!db.getSource(req.params.id)) return reply.code(404).send({ error: 'not found' })
      const deps = db.dependents(req.params.id)
      if (deps.length) {
        return reply.code(409).send({ error: `referenced by: ${deps.join(', ')}` })
      }
      db.deleteSource(req.params.id)
      registry.invalidate()
      return { ok: true }
    }
  )

  // ---- Admin: token management ----

  app.post<{ Params: { id: string }; Body: { label?: string } }>(
    '/admin/sources/:id/tokens',
    { preHandler: requireAdmin },
    async (req, reply) => {
      if (!db.getSource(req.params.id)) return reply.code(404).send({ error: 'not found' })
      const token = db.issueToken(req.params.id, req.body?.label ?? '')
      return reply.code(201).send({ token, sourceId: req.params.id })
    }
  )

  app.get<{ Params: { id: string } }>(
    '/admin/sources/:id/tokens',
    { preHandler: requireAdmin },
    async (req) => db.listTokens(req.params.id)
  )

  app.delete<{ Params: { token: string } }>(
    '/admin/tokens/:token',
    { preHandler: requireAdmin },
    async (req) => {
      db.revokeToken(req.params.token)
      return { ok: true }
    }
  )

  // Friendly 404 for SourceNotFound surfacing from async handlers.
  app.setErrorHandler((err, _req, reply) => {
    if (err instanceof SourceNotFoundError) return reply.code(404).send({ error: err.message })
    return reply.code(500).send({ error: formatError(err) })
  })

  return app
}
