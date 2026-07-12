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

  it('lists the three standard tools with JSON Schema', async () => {
    const r = await app.inject({ method: 'GET', url: '/a/tools', headers: srcHeaders(token) })
    expect(r.statusCode).toBe(200)
    const tools = r.json()
    expect(tools.map((t: any) => t.id).sort()).toEqual(['readEvents', 'writeValue', 'writeLink'].sort())
    const wv = tools.find((t: any) => t.id === 'writeValue')
    expect(wv.safety).toBe('safe-mutating')
    expect(wv.args.type).toBe('object')
    expect(wv.args.properties).toHaveProperty('entityId')
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
    expect(tools.map((t: any) => t.id)).toEqual(['readEvents'])

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
