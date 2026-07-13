import { mkdtempSync, rmSync } from 'fs'
import { tmpdir } from 'os'
import { join } from 'path'
import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import type { FastifyInstance } from 'fastify'
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { StreamableHTTPClientTransport } from '@modelcontextprotocol/sdk/client/streamableHttp.js'
import {
  CombinedSource,
  FrozenSource,
  RemoteSource,
  SqliteSource,
} from '../../src/core/source/index'
import { buildApp } from '../src/app'
import { ConfigDb } from '../src/config'
import { Registry } from '../src/registry'

const ADMIN = 'admin-secret'
const adminHeaders = { authorization: `Bearer ${ADMIN}`, 'content-type': 'application/json' }
let dir: string
let db: ConfigDb
let app: FastifyInstance
let port: number
let token: string

beforeAll(async () => {
  dir = mkdtempSync(join(tmpdir(), 'eg-int-'))
  db = new ConfigDb(join(dir, 'config.db'))
  app = buildApp({ db, registry: new Registry(db), adminToken: ADMIN })
  await app.listen({ port: 0, host: '127.0.0.1' })
  port = (app.server.address() as { port: number }).port

  await app.inject({
    method: 'POST',
    url: '/admin/sources',
    headers: adminHeaders,
    payload: { id: 'src', label: 'src', config: { type: 'sqlite', path: join(dir, 'src.db'), defaultAuthor: 'srv' } },
  })
  const t = await app.inject({ method: 'POST', url: '/admin/sources/src/tokens', headers: adminHeaders, payload: {} })
  token = t.json().token

  // seed two events at known timestamps
  const base = `http://127.0.0.1:${port}/src`
  for (const [value, ts] of [['old', 100], ['new', 200]] as const) {
    await fetch(`${base}/call`, {
      method: 'POST',
      headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' },
      body: JSON.stringify({ tool: 'writeValue', args: { entityId: 'e1', key: 'v', value, timestamp: ts } }),
    })
  }
})

afterAll(async () => {
  await app.close()
  db.close()
  rmSync(dir, { recursive: true, force: true })
})

describe('RemoteSource passthrough', () => {
  it('discovers remote tools and round-trips calls', async () => {
    const remote = new RemoteSource('r', 'R', `http://127.0.0.1:${port}/src`, token)
    await remote.refresh()
    expect(remote.tools().map((t) => t.id)).toEqual(
      expect.arrayContaining(['readEvents', 'writeLink', 'writeValue', 'query', 'createEntity'])
    )

    const events = (await remote.call('readEvents', { entityIds: ['e1'] })) as any[]
    expect(events.map((e) => e.value)).toEqual(['old', 'new'])
  })

  it('composes: Frozen over a Remote filters by timestamp', async () => {
    const remote = new RemoteSource('r', 'R', `http://127.0.0.1:${port}/src`, token)
    await remote.refresh()
    const frozen = new FrozenSource('rf', 'RF', remote, 150)
    const events = (await frozen.call('readEvents', { entityIds: ['e1'] })) as any[]
    expect(events.map((e) => e.value)).toEqual(['old'])
  })

  it('composes: Combined over [local, remote] unions events', async () => {
    const local = new SqliteSource('local', 'local', join(dir, 'local.db'), 'me')
    await local.call('writeValue', { entityId: 'e2', key: 'v', value: 'local-one', timestamp: 300 })
    const remote = new RemoteSource('r', 'R', `http://127.0.0.1:${port}/src`, token)
    await remote.refresh()
    const combo = new CombinedSource('cx', 'cx', [local, remote])
    const events = (await combo.call('readEvents', {})) as any[]
    const authors = new Set(events.map((e) => e.author))
    expect(authors.has('me')).toBe(true)
    expect(authors.has('srv')).toBe(true)
    local.close()
  })

  it('rejects a remote call with a bad token', async () => {
    const remote = new RemoteSource('r', 'R', `http://127.0.0.1:${port}/src`, 'wrong')
    await expect(remote.refresh()).rejects.toThrow()
  })
})

describe('MCP endpoint', () => {
  it('lists tools and calls one over Streamable HTTP', async () => {
    const transport = new StreamableHTTPClientTransport(
      new URL(`http://127.0.0.1:${port}/src/mcp`),
      { requestInit: { headers: { authorization: `Bearer ${token}` } } }
    )
    const client = new Client({ name: 'test', version: '0.0.0' })
    await client.connect(transport)

    const { tools } = await client.listTools()
    expect(tools.map((t) => t.name)).toEqual(
      expect.arrayContaining(['readEvents', 'writeLink', 'writeValue', 'query', 'createEntity'])
    )

    const res: any = await client.callTool({ name: 'readEvents', arguments: { entityIds: ['e1'] } })
    const events = JSON.parse(res.content[0].text)
    expect(events.map((e: any) => e.value)).toEqual(['old', 'new'])

    await client.close()
  })
})
