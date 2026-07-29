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

  it('round-trips a resource, bytes intact, through a remote source', async () => {
    const remote = new RemoteSource('r', 'R', `http://127.0.0.1:${port}/src`, token)
    await remote.refresh()
    // Deliberately not valid UTF-8: the bytes go over JSON as base64 and must
    // come back byte-for-byte, not through a string round trip.
    const bytes = Buffer.from([0x89, 0x50, 0x4e, 0x47, 0x00, 0xff, 0xfe])
    await remote.call('writeResource', {
      id: 'e1',
      mimeType: 'image/png',
      data: bytes.toString('base64'),
      name: 'shot.png',
      author: 'me',
      timestamp: 400,
    })

    const resource = (await remote.call('readResource', { id: 'e1' })) as {
      id: string
      mimeType: string
      name: string | null
      author: string
      timestamp: number
      data: string
    }
    expect(resource.mimeType).toBe('image/png')
    expect(resource.name).toBe('shot.png')
    expect(resource.author).toBe('me')
    expect(resource.timestamp).toBe(400)
    expect(Buffer.from(resource.data, 'base64').equals(bytes)).toBe(true)

    // Nothing stored under an id reads as null rather than an error.
    expect(await remote.call('readResource', { id: 'nothing-here' })).toBeNull()
  })

  it('rejects a remote call with a bad token', async () => {
    const remote = new RemoteSource('r', 'R', `http://127.0.0.1:${port}/src`, 'wrong')
    await expect(remote.refresh()).rejects.toThrow()
  })
})

// The MCP endpoint is not the source's tool list: it is five tools over the same
// store, so a model reads an outline and writes a note rather than designing its
// own reads out of raw events. These run over a real MCP client, so what is
// asserted is what an agent would actually see.
describe('MCP endpoint', () => {
  async function connect(): Promise<Client> {
    const transport = new StreamableHTTPClientTransport(
      new URL(`http://127.0.0.1:${port}/src/mcp`),
      { requestInit: { headers: { authorization: `Bearer ${token}` } } }
    )
    const client = new Client({ name: 'test', version: '0.0.0' })
    await client.connect(transport)
    return client
  }

  const said = (res: unknown): string => (res as { content: { text: string }[] }).content[0].text

  it('offers the six tools, and says how to use them', async () => {
    const client = await connect()
    const { tools } = await client.listTools()
    expect(tools.map((t) => t.name)).toEqual([
      'query',
      'get_details',
      'create',
      'set_value',
      'add_link',
      'remove_link',
    ])
    // Creating is the one call that isn't safe to repeat.
    const hints = Object.fromEntries(tools.map((t) => [t.name, t.annotations?.idempotentHint]))
    expect(hints).toMatchObject({ create: false, set_value: true, query: true })
    // The store's own tools are deliberately absent: raw events, resources and
    // undo are an API for a client, not for a model.
    expect(tools.map((t) => t.name)).not.toContain('readEvents')
    expect(client.getInstructions()).toContain('@index')
    await client.close()
  })

  it('says what type every argument takes, including the free-form one', async () => {
    // A property with no type is not "anything" to a client that builds its
    // call from the schema — it is a string, and a boolean written through it
    // arrives as `"true"`. `set_value` is the one that invites it, since its
    // value genuinely is any JSON; naming the types is what makes that true at
    // both ends.
    const client = await connect()
    const { tools } = await client.listTools()
    const untyped: string[] = []
    for (const tool of tools) {
      const props = (tool.inputSchema.properties ?? {}) as Record<string, object>
      for (const [name, schema] of Object.entries(props)) {
        if (!('type' in schema) && !('anyOf' in schema)) untyped.push(`${tool.name}.${name}`)
      }
    }
    expect(untyped).toEqual([])

    const value = (tools.find((t) => t.name === 'set_value')!.inputSchema.properties as any).value
    expect((value.anyOf as { type: string }[]).map((s) => s.type)).toEqual(
      expect.arrayContaining(['string', 'number', 'boolean', 'null', 'object', 'array'])
    )
    await client.close()
  })

  it('stores a value as the type it arrived as', async () => {
    // The rollup asks whether `open` *is* `false`, so a ticked task and the
    // string "false" are not the same thing.
    const client = await connect()
    for (const [key, value] of [
      ['open', false],
      ['section', true],
      ['count', 3],
      ['nested', { a: [1, null] }],
    ] as const) {
      await client.callTool({ name: 'set_value', arguments: { entityId: 'm-typed', key, value } })
    }
    const details = JSON.parse(
      said(await client.callTool({ name: 'get_details', arguments: { entityIds: ['m-typed'] } }))
    )
    expect(details['m-typed'].values).toEqual({
      open: false,
      section: true,
      count: 3,
      nested: { a: [1, null] },
    })

    // And `null` clears rather than storing the word.
    await client.callTool({
      name: 'set_value',
      arguments: { entityId: 'm-typed', key: 'count', value: null },
    })
    const cleared = JSON.parse(
      said(await client.callTool({ name: 'get_details', arguments: { entityIds: ['m-typed'] } }))
    )
    expect(cleared['m-typed'].values.count).toBeNull()
    await client.close()
  })

  it('writes an outline, and reads it back with the ids down the left', async () => {
    const client = await connect()
    // A root, a section under it, and a task under that — written the only way
    // the MCP offers: a value on a fresh id, then a link to put it somewhere.
    const notes: [string, Record<string, unknown>][] = [
      ['m-root', { text: 'Notes' }],
      ['m-sec', { text: 'Plans', section: true }],
      ['m-task', { text: 'Ship it', open: true }],
    ]
    for (const [id, values] of notes) {
      for (const [key, value] of Object.entries(values)) {
        await client.callTool({ name: 'set_value', arguments: { entityId: id, key, value } })
      }
    }
    for (const [parentId, childId] of [
      ['m-root', 'm-sec'],
      ['m-sec', 'm-task'],
    ]) {
      await client.callTool({ name: 'add_link', arguments: { parentId, childId } })
    }

    const page = said(await client.callTool({ name: 'query', arguments: { path: 'm-root' } }))
    expect(page.split('\n\n')[0].split('\n')).toEqual([
      'm-root  Notes',
      'm-sec   - ## Plans',
      'm-task    - [ ] Ship it',
    ])
    expect(page).toContain('3 rows shown, 3 entities visited')

    // Sections only: the task goes, the row asked about stays.
    const outline = said(
      await client.callTool({ name: 'query', arguments: { path: 'm-root', sections: true } })
    )
    expect(outline.split('\n\n')[0].split('\n')).toEqual(['m-root  Notes', 'm-sec   - ## Plans'])

    await client.close()
  })

  it('reads a bounded slice with the notes in it, not just their ids', async () => {
    // `maxDepth` stops the walk at a row rather than below it, and the walk does
    // not read what it will not descend through — so the row it stops at is the
    // one most easily handed back blank.
    const client = await connect()
    const page = said(
      await client.callTool({ name: 'query', arguments: { path: 'm-root', maxDepth: 1 } })
    )
    expect(page.split('\n\n')[0].split('\n')).toEqual(['m-root  Notes', 'm-sec   - ## Plans'])
    await client.close()
  })

  it('hands back the path to resume from when the limit cuts a walk short', async () => {
    const client = await connect()
    const first = said(
      await client.callTool({ name: 'query', arguments: { path: 'm-root', limit: 2 } })
    )
    expect(first).toContain('path: ["m-root","m-sec","m-task"]')
    // Continuing from that path reads the rest and nothing twice.
    const rest = said(
      await client.callTool({
        name: 'query',
        arguments: { path: ['m-root', 'm-sec', 'm-task'] },
      })
    )
    // Depth is still counted from the root of the walk, not from where it resumed.
    expect(rest.split('\n\n')[0]).toBe('m-task    - [ ] Ship it')
    expect(rest).toContain('that is everything under this path')
    await client.close()
  })

  it('rolls up an entity, including what links to it', async () => {
    const client = await connect()
    const details = JSON.parse(
      said(await client.callTool({ name: 'get_details', arguments: { entityIds: ['m-sec'] } }))
    )
    expect(details['m-sec'].values).toMatchObject({ text: 'Plans', section: true })
    expect(details['m-sec'].outboundLinks).toEqual(['m-task'])
    expect(details['m-sec'].inboundLinks).toEqual(['m-root'])

    await client.callTool({
      name: 'remove_link',
      arguments: { parentId: 'm-sec', childId: 'm-task' },
    })
    const after = JSON.parse(
      said(await client.callTool({ name: 'get_details', arguments: { entityIds: ['m-sec'] } }))
    )
    expect(after['m-sec'].outboundLinks).toEqual([])
    await client.close()
  })

  it('creates a note under a parent, minting the id itself', async () => {
    const client = await connect()
    const made = said(
      await client.callTool({
        name: 'create',
        arguments: { parentId: 'm-root', text: 'Fresh', open: true },
      })
    )
    const id = /Created (\S+) under/.exec(made)?.[1]
    // A uuid, not something the caller had to think of.
    expect(id).toMatch(/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/)

    const details = JSON.parse(
      said(await client.callTool({ name: 'get_details', arguments: { entityIds: [id!] } }))
    )
    // The flags that were given, and only those: `section` was not, so it is absent
    // rather than false.
    expect(details[id!].values).toEqual({ text: 'Fresh', open: true })
    // And it is in the outline already, not stranded waiting for a link.
    expect(details[id!].inboundLinks).toEqual(['m-root'])
    await client.close()
  })

  it('reports a bad call as an error rather than a crash', async () => {
    const client = await connect()
    const missing: any = await client.callTool({ name: 'set_colour', arguments: {} })
    expect(missing.isError).toBe(true)
    expect(said(missing)).toContain('No tool named "set_colour"')

    const invalid: any = await client.callTool({ name: 'query', arguments: {} })
    expect(invalid.isError).toBe(true)
    expect(said(invalid)).toContain('path')
    await client.close()
  })
})
