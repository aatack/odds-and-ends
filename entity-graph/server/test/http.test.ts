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

    // query from the root reaches the child via the outbound link.
    const q = await call('a', token, 'query', { rootId: 'root' })
    expect(q.status).toBe('success')
    expect(q.result.results.map((r: any) => r.entity.id)).toContain(childId)
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
    // Only the `pure` tools survive maxSafety: 'pure'.
    expect(tools.map((t: any) => t.id).sort()).toEqual(['query', 'readEntities', 'readEvents'])

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

    // Seed a tool-shaped entity and link it under @tools.
    await call('u', uToken, 'writeValue', { entityId: 'greet', key: 'name', value: 'greet' })
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
