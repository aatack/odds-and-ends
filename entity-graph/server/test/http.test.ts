import { mkdtempSync, rmSync } from 'fs'
import { tmpdir } from 'os'
import { join } from 'path'
import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import type { FastifyInstance } from 'fastify'
import { buildApp } from '../src/app'
import { ConfigDb } from '../src/config'
import { Registry } from '../src/registry'
import type { SourceConfig } from '../src/config'

const ADMIN = 'admin-secret'
let dir: string
let db: ConfigDb
let app: FastifyInstance

const adminHeaders = { authorization: `Bearer ${ADMIN}`, 'content-type': 'application/json' }

async function createSource(id: string, config: SourceConfig) {
  return app.inject({
    method: 'POST',
    url: '/admin/sources',
    headers: adminHeaders,
    payload: { id, label: id, config },
  })
}

async function issueToken(id: string): Promise<string> {
  const r = await app.inject({
    method: 'POST',
    url: `/admin/sources/${id}/tokens`,
    headers: adminHeaders,
    payload: {},
  })
  return r.json().token
}

function srcHeaders(token: string) {
  return { authorization: `Bearer ${token}`, 'content-type': 'application/json' }
}

async function call(id: string, token: string, tool: string, args: unknown) {
  const r = await app.inject({
    method: 'POST',
    url: `/${id}/call`,
    headers: srcHeaders(token),
    payload: { tool, args },
  })
  return r.json()
}

beforeAll(async () => {
  dir = mkdtempSync(join(tmpdir(), 'eg-http-'))
  db = new ConfigDb(join(dir, 'config.db'))
  app = buildApp({ db, registry: new Registry(db), adminToken: ADMIN })
  await app.ready()
})

afterAll(async () => {
  await app.close()
  db.close()
  rmSync(dir, { recursive: true, force: true })
})

describe('admin auth', () => {
  it('rejects admin endpoints without the admin token', async () => {
    const r = await app.inject({ method: 'GET', url: '/admin/sources' })
    expect(r.statusCode).toBe(401)
  })
})

// A browser client — the mobile app — is served from another origin, so a JSON POST
// with an Authorization header is preflighted. The wildcard is safe because the token
// is sent explicitly rather than as a cookie; the admin surface is excluded because
// with ADMIN_TOKEN unset it is open, and no page should be handed that.
describe('cors, on the source API only', () => {
  it('answers a preflight for a source call', async () => {
    const r = await app.inject({
      method: 'OPTIONS',
      url: '/a/call',
      headers: { origin: 'http://phone.local', 'access-control-request-method': 'POST' },
    })
    expect(r.statusCode).toBe(204)
    expect(r.headers['access-control-allow-origin']).toBe('*')
  })

  it('echoes the requested headers, so authorization survives the preflight', async () => {
    const r = await app.inject({
      method: 'OPTIONS',
      url: '/a/call',
      headers: { 'access-control-request-headers': 'authorization, content-type' },
    })
    expect(r.headers['access-control-allow-headers']).toBe('authorization, content-type')
  })

  it('allows the origin on a source response', async () => {
    const r = await app.inject({ method: 'GET', url: '/a/tools' })
    // 401 without a token, but the header is there either way — a cross-origin
    // client has to be able to *see* the rejection.
    expect(r.headers['access-control-allow-origin']).toBe('*')
  })

  it('does not allow any origin on the admin surface', async () => {
    const admin = await app.inject({ method: 'GET', url: '/admin/sources', headers: adminHeaders })
    expect(admin.headers['access-control-allow-origin']).toBeUndefined()
    const preflight = await app.inject({ method: 'OPTIONS', url: '/admin/sources' })
    expect(preflight.statusCode).toBe(404)
    const integrations = await app.inject({ method: 'OPTIONS', url: '/runTool' })
    expect(integrations.statusCode).toBe(404)
  })
})

describe('sqlite source: crud, auth, round-trip', () => {
  let token: string

  it('creates a sqlite source and issues a token', async () => {
    const c = await createSource('a', {
      type: 'sqlite',
      path: join(dir, 'a.db'),
      defaultAuthor: 'alice',
    })
    expect(c.statusCode).toBe(201)
    token = await issueToken('a')
    expect(token).toMatch(/^[0-9a-f]{48}$/)
  })

  it('rejects unauthenticated tool listing', async () => {
    const r = await app.inject({ method: 'GET', url: '/a/tools' })
    expect(r.statusCode).toBe(401)
  })

  it('rejects a bad token', async () => {
    const r = await app.inject({
      method: 'GET',
      url: '/a/tools',
      headers: { authorization: 'Bearer nope' },
    })
    expect(r.statusCode).toBe(401)
  })

  it('lists the default tools with JSON Schema', async () => {
    const r = await app.inject({ method: 'GET', url: '/a/tools', headers: srcHeaders(token) })
    expect(r.statusCode).toBe(200)
    const tools = r.json()
    const ids = tools.map((t: any) => t.id)
    expect(ids).toEqual(
      expect.arrayContaining([
        'readEvents', 'writeValue', 'writeLink',
        'query', 'readEntities', 'createEntity', 'moveEntity',
        'httpRequest', 'runCommand',
      ])
    )
    const wv = tools.find((t: any) => t.id === 'writeValue')
    expect(wv.safety).toBe('safe-mutating')
    expect(wv.args.type).toBe('object')
    expect(wv.args.properties).toHaveProperty('entityId')
    // HTTP/CLI are exposed as dangerous permissions-backed tools.
    expect(tools.find((t: any) => t.id === 'httpRequest').safety).toBe('dangerous')
    expect(tools.find((t: any) => t.id === 'runCommand').safety).toBe('dangerous')
  })

  it('exposes entity-level tools built on the DB permissions', async () => {
    // createEntity writes the value + parent-link events and returns a new id.
    const created = await call('a', token, 'createEntity', {
      values: { text: 'root-child' },
      parentId: 'root',
    })
    expect(created.status).toBe('success')
    const childId = created.result as string
    expect(typeof childId).toBe('string')

    // readEntities rolls the child up into an entity with its values.
    const read = await call('a', token, 'readEntities', { entityIds: [childId] })
    expect(read.status).toBe('success')
    expect(read.result[childId].values.text).toBe('root-child')

    // query from the root reaches the child via the outbound link, and says
    // where each row sits by the path that got there.
    const q = await call('a', token, 'query', { path: 'root' })
    expect(q.status).toBe('success')
    expect(q.result.rows.map((r: any) => r.entity.id)).toContain(childId)
    expect(q.result.rows[0].path).toEqual(['root'])
    expect(q.result.rows.find((r: any) => r.entity.id === childId).path).toEqual(['root', childId])
    expect(q.result.continuation).toBeNull()
  })

  // What an agent reads the tree through: a walk it can resume, and filters that
  // narrow what comes back without narrowing what was looked at.
  it('pages a query by path, and filters the rows it returns', async () => {
    const linked = (from: string, to: string) =>
      call('a', token, 'writeLink', { sourceId: from, destinationId: to, action: 0 })
    await call('a', token, 'writeValue', { entityId: 'q-p', key: 'text', value: 'Parent' })
    await call('a', token, 'writeValue', { entityId: 'q-a', key: 'text', value: 'apple' })
    await call('a', token, 'writeValue', { entityId: 'q-b', key: 'text', value: 'pear' })
    await call('a', token, 'writeValue', { entityId: 'q-b', key: 'section', value: true })
    await call('a', token, 'writeValue', { entityId: 'q-c', key: 'text', value: 'apple pie' })
    await linked('q-p', 'q-a')
    await linked('q-p', 'q-b')
    await linked('q-b', 'q-c')

    const whole = await call('a', token, 'query', { path: 'q-p' })
    expect(whole.result.rows.map((r: any) => r.entity.id)).toEqual(['q-p', 'q-a', 'q-b', 'q-c'])

    // A limit stops the walk and says where to pick it up; passing that path
    // straight back gets the rest.
    const first = await call('a', token, 'query', { path: 'q-p', limit: 2 })
    expect(first.result.rows.map((r: any) => r.entity.id)).toEqual(['q-p', 'q-a'])
    expect(first.result.continuation).toEqual(['q-p', 'q-b'])
    const rest = await call('a', token, 'query', { path: first.result.continuation })
    expect(rest.result.rows.map((r: any) => r.entity.id)).toEqual(['q-b', 'q-c'])
    expect(rest.result.continuation).toBeNull()

    // Depth is counted from the first entity in the path.
    const shallow = await call('a', token, 'query', { path: 'q-p', maxDepth: 1 })
    expect(shallow.result.rows.map((r: any) => r.entity.id)).toEqual(['q-p', 'q-a', 'q-b'])

    // From *there*, and not from wherever a page happens to resume, so a capped
    // walk read in pages keeps the shape it started with: q-c sits two below q-p
    // and stays out of both pages, though it is a child of the resume point.
    const capped = await call('a', token, 'query', { path: 'q-p', maxDepth: 1, limit: 2 })
    expect(capped.result.continuation).toEqual(['q-p', 'q-b'])
    const cappedRest = await call('a', token, 'query', {
      path: capped.result.continuation,
      maxDepth: 1,
    })
    expect(cappedRest.result.rows.map((r: any) => r.entity.id)).toEqual(['q-b'])
    expect(cappedRest.result.continuation).toBeNull()

    // Find keeps a match's ancestors so the tree still reads; sections doesn't,
    // since the point of it is to see the sections and nothing else. Either way
    // the whole tree was walked, which is what `scanned` reports.
    const found = await call('a', token, 'query', { path: 'q-p', find: 'apple' })
    expect(found.result.rows.map((r: any) => r.entity.id)).toEqual(['q-p', 'q-a', 'q-b', 'q-c'])
    expect(found.result.scanned).toBe(4)
    const pies = await call('a', token, 'query', { path: 'q-p', find: 'pie' })
    expect(pies.result.rows.map((r: any) => r.entity.id)).toEqual(['q-p', 'q-b', 'q-c'])
    const sections = await call('a', token, 'query', { path: 'q-p', sections: true })
    expect(sections.result.rows.map((r: any) => r.entity.id)).toEqual(['q-p', 'q-b'])

    // Reversed, the same walk answers "what links to this?".
    const back = await call('a', token, 'query', { path: 'q-c', direction: 'in' })
    expect(back.result.rows.map((r: any) => r.entity.id)).toEqual(['q-c', 'q-b', 'q-p'])
  })

  it('reports not-implemented for stubbed IO permissions', async () => {
    const http = await call('a', token, 'httpRequest', { url: 'https://example.com' })
    expect(http.status).toBe('error')
    expect(http.message).toContain('not implemented')
    const cmd = await call('a', token, 'runCommand', { command: 'ls' })
    expect(cmd.status).toBe('error')
    expect(cmd.message).toContain('not implemented')
  })

  it('round-trips events through writeValue + readEvents', async () => {
    expect((await call('a', token, 'writeValue', { entityId: 'e1', key: 'title', value: 'hello', timestamp: 100 })).status).toBe('success')
    await call('a', token, 'writeValue', { entityId: 'e1', key: 'title', value: 'world', timestamp: 200 })
    const res = await call('a', token, 'readEvents', { entityIds: ['e1'] })
    expect(res.status).toBe('success')
    expect(res.result.map((e: any) => e.value)).toEqual(['hello', 'world'])
    expect(res.result.every((e: any) => e.author === 'alice')).toBe(true)
  })

  // What a client keeping its own cache reads through: the entities asked for
  // plus, by default, two layers of their children, so walking down the tree
  // costs one round trip rather than one per level.
  it('scans events with an overscan over the entities linked from them', async () => {
    const linked = (from: string, to: string) =>
      call('a', token, 'writeLink', { sourceId: from, destinationId: to, action: 0 })
    await call('a', token, 'writeValue', { entityId: 'sc-p', key: 'text', value: 'parent' })
    await call('a', token, 'writeValue', { entityId: 'sc-c1', key: 'text', value: 'one' })
    await call('a', token, 'writeValue', { entityId: 'sc-g', key: 'text', value: 'grandchild' })
    await linked('sc-p', 'sc-c1')
    await linked('sc-p', 'sc-c2')
    await linked('sc-c1', 'sc-g')

    const shallow = await call('a', token, 'scanEvents', { entityIds: ['sc-p'], depth: 1 })
    expect(shallow.status).toBe('success')
    expect(shallow.result.entityIds.sort()).toEqual(['sc-c1', 'sc-c2', 'sc-p'])
    // The p → c1 link is read from both ends; it comes back once all the same,
    // since a duplicated link *move* would be applied twice by a rollup.
    const links = shallow.result.events.filter((e: any) => e.type === 'link')
    expect(links).toHaveLength(3)
    expect(new Set(links.map((e: any) => `${e.sourceId}>${e.destinationId}`)).size).toBe(3)

    // One layer further reaches the grandchild; `depth: 0` reaches nothing.
    const deep = await call('a', token, 'scanEvents', { entityIds: ['sc-p'] })
    expect(deep.result.entityIds).toContain('sc-g')
    const alone = await call('a', token, 'scanEvents', { entityIds: ['sc-p'], depth: 0 })
    expect(alone.result.entityIds).toEqual(['sc-p'])

    // The overscan is clipped per layer, and a clipped entity is not reported as
    // covered — the client would otherwise think it had that entity's events.
    const clipped = await call('a', token, 'scanEvents', {
      entityIds: ['sc-p'],
      depth: 1,
      overscan: 1,
    })
    expect(clipped.result.entityIds).toHaveLength(2)
  })

  // Child order is the outline's order, and it comes out of the order the link
  // events were written. Two links written at one instant tie on timestamp, so
  // what breaks the tie has to be the store, not which entities were asked for.
  it('keeps child order whoever is read alongside the parent', async () => {
    const at = Date.now()
    for (const child of ['ord-a', 'ord-b']) {
      await call('a', token, 'writeLink', {
        sourceId: 'ord-p',
        destinationId: child,
        action: 0,
        timestamp: at,
      })
    }
    const alone = await call('a', token, 'readEntities', { entityIds: ['ord-p'] })
    expect(alone.result['ord-p'].outboundLinks).toEqual(['ord-a', 'ord-b'])
    // `ord-b` shares the second link, so reading it first is what used to drag
    // that link to the front of the parent's list.
    const together = await call('a', token, 'readEntities', { entityIds: ['ord-b', 'ord-p'] })
    expect(together.result['ord-p'].outboundLinks).toEqual(['ord-a', 'ord-b'])
  })

  it('errors (not crashes) on a required missing arg', async () => {
    const res = await call('a', token, 'writeValue', { key: 'k', value: 1 }) // no entityId
    expect(res.status).toBe('error')
    expect(res.message).toContain('entityId')
  })

  it('errors on an unknown tool', async () => {
    const res = await call('a', token, 'nope', {})
    expect(res.status).toBe('error')
  })
})

describe('filter / readonly wrapper', () => {
  let roToken: string
  it('exposes only reads and blocks writes', async () => {
    await createSource('a-ro', { type: 'filter', child: 'a', maxSafety: 'pure' })
    roToken = await issueToken('a-ro')
    const tools = (await app.inject({ method: 'GET', url: '/a-ro/tools', headers: srcHeaders(roToken) })).json()
    // Only the `pure` tools survive maxSafety: 'pure' — reading a resource
    // among them; writing one is mutating and goes with the rest.
    expect(tools.map((t: any) => t.id).sort()).toEqual([
      'query',
      'readEntities',
      'readEvents',
      'readResource',
      'scanEvents',
    ])

    const res = await call('a-ro', roToken, 'writeValue', { entityId: 'x', key: 'k', value: 1 })
    expect(res.status).toBe('error')
    // and the write did not happen
    const read = await call('a-ro', roToken, 'readEvents', { entityIds: ['x'] })
    expect(read.result).toEqual([])
  })
})

describe('frozen wrapper', () => {
  it('drops events at/after the cutoff', async () => {
    await createSource('a-frozen', { type: 'frozen', child: 'a', beforeTs: 150 })
    const token = await issueToken('a-frozen')
    const res = await call('a-frozen', token, 'readEvents', { entityIds: ['e1'] })
    expect(res.result.map((e: any) => e.value)).toEqual(['hello']) // only ts=100
  })
})

describe('combined wrapper', () => {
  let cToken: string
  it('unions events and routes writes to the first child', async () => {
    await createSource('b', { type: 'sqlite', path: join(dir, 'b.db'), defaultAuthor: 'bob' })
    const bToken = await issueToken('b')
    await call('b', bToken, 'writeValue', { entityId: 'e2', key: 'title', value: 'from-b', timestamp: 300 })

    await createSource('c', { type: 'combined', children: ['a', 'b'] })
    cToken = await issueToken('c')

    const all = await call('c', cToken, 'readEvents', {})
    // a has e1(x2); b has e2(x1) => at least 3, and includes both authors
    const authors = new Set(all.result.map((e: any) => e.author))
    expect(authors.has('alice')).toBe(true)
    expect(authors.has('bob')).toBe(true)

    await call('c', cToken, 'writeValue', { entityId: 'e3', key: 'k', value: 'via-c', timestamp: 400 })
    const inA = await call('a', await issueToken('a'), 'readEvents', { entityIds: ['e3'] })
    const inB = await call('b', bToken, 'readEvents', { entityIds: ['e3'] })
    expect(inA.result.length).toBe(1)
    expect(inB.result.length).toBe(0)
  })
})

describe('user-defined tools (@tools)', () => {
  let uToken: string
  const argSchema = {
    type: 'object',
    properties: { who: { type: 'string' } },
    required: ['who'],
  }

  it('splices tool-shaped children of @tools into the tool list', async () => {
    await createSource('u', { type: 'sqlite', path: join(dir, 'u.db') })
    uToken = await issueToken('u')

    // Seed a tool-shaped entity and link it under @tools. Its text is what it is
    // called, and the type is what says it is a definition at all.
    await call('u', uToken, 'writeValue', { entityId: 'greet', key: 'type', value: 'tool' })
    await call('u', uToken, 'writeValue', { entityId: 'greet', key: 'text', value: 'greet' })
    await call('u', uToken, 'writeValue', { entityId: 'greet', key: 'description', value: 'Greet someone' })
    await call('u', uToken, 'writeValue', { entityId: 'greet', key: 'arguments', value: argSchema })
    await call('u', uToken, 'writeLink', { sourceId: '@tools', destinationId: 'greet', action: 0 })

    // Force a registry rebuild so the source reloads its @tools tools.
    await app.inject({
      method: 'PUT',
      url: '/admin/sources/u',
      headers: adminHeaders,
      payload: { label: 'u' },
    })

    const tools = (await app.inject({ method: 'GET', url: '/u/tools', headers: srcHeaders(uToken) })).json()
    const greet = tools.find((t: any) => t.id === 'greet')
    expect(greet).toBeDefined()
    expect(greet.description).toBe('Greet someone')
    expect(greet.safety).toBe('dangerous')
    // The stored JSON Schema flows straight through to /tools.
    expect(greet.args).toEqual(argSchema)
  })

  it('publishes a declared argument list as a schema, not as a list', async () => {
    // The form a definition is actually written in. What MCP is handed has to be
    // a schema either way, so the conversion happens before it is published.
    await call('u', uToken, 'writeValue', { entityId: 'listed', key: 'type', value: 'tool' })
    await call('u', uToken, 'writeValue', { entityId: 'listed', key: 'text', value: 'listed' })
    await call('u', uToken, 'writeValue', { entityId: 'listed', key: 'description', value: 'Takes a list' })
    await call('u', uToken, 'writeValue', {
      entityId: 'listed',
      key: 'arguments',
      value: [
        { name: 'who', type: 'string', required: true },
        { name: 'loudly', type: 'boolean' },
      ],
    })
    await call('u', uToken, 'writeLink', { sourceId: '@tools', destinationId: 'listed', action: 0 })
    await app.inject({
      method: 'PUT',
      url: '/admin/sources/u',
      headers: adminHeaders,
      payload: { label: 'u' },
    })

    const tools = (await app.inject({ method: 'GET', url: '/u/tools', headers: srcHeaders(uToken) })).json()
    const listed = tools.find((t: any) => t.id === 'listed')
    expect(listed.args).toEqual({
      type: 'object',
      properties: { who: { type: 'string' }, loudly: { type: 'boolean' } },
      required: ['who'],
    })
  })

  it('throws not-implemented when a user-defined tool is called', async () => {
    const res = await call('u', uToken, 'greet', { who: 'world' })
    expect(res.status).toBe('error')
    expect(res.message).toContain('not yet executable')
  })
})

describe('source deletion guards', () => {
  it('refuses to delete a source that others reference', async () => {
    const r = await app.inject({
      method: 'DELETE',
      url: '/admin/sources/a',
      headers: { authorization: `Bearer ${ADMIN}` },
    })
    expect(r.statusCode).toBe(409) // a is referenced by a-ro, a-frozen, c
  })
})
