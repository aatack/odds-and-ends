import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import { f } from '../src/filter.js'
import { op } from '../src/ops.js'
import { Egdb } from '../src/store.js'
import type { WalkQuery } from '../src/walk.js'
import { computeWindow } from '../src/walk.js'
import { haveServer, startServer, treeOps, type Running } from './server.js'

const query: WalkQuery = { follow: { list: [['child', 'outbound', 'list'], ['child', 'outbound', 'task']] } }

describe.skipIf(!haveServer)('against egdb-server', () => {
  let server: Running
  let branch: string

  beforeAll(async () => {
    server = await startServer()
    branch = await server.client.createBranch('main')
    await server.client.write({ branch, author: 'test', ops: treeOps(4, 6) })
  })
  afterAll(() => server?.stop())

  /** Preload until the window has nothing missing; returns the rounds it took. */
  async function settle(egdb: Egdb, request: Parameters<typeof computeWindow>[1]) {
    for (let round = 0; round < 100; round++) {
      const w = computeWindow(egdb.view(), request)
      if (!w.missing.forward && !w.missing.backward) return { window: w, round }
      if (w.missing.forward) await egdb.preloadWalk(request.root, request.query, w.missing.forward, 'forward')
      if (w.missing.backward) await egdb.preloadWalk(request.root, request.query, w.missing.backward, 'backward')
    }
    throw new Error('did not settle')
  }

  it('gives the same rows as the server walk, forward and backward, in few round trips', async () => {
    const all = await server.client.walk(branch, { start: ['root'], follow: query.follow, options: { steps: 100_000 } })
    const order = all.paths.map((p) => p.join('/'))
    expect(order.length).toBeGreaterThan(300)

    const egdb = new Egdb({ client: server.client, branch, author: 'test' })
    const { window, round } = await settle(egdb, { root: ['root'], query, offset: 0, limit: 150, lookahead: 50 })
    expect(window.rows.map((r) => r.path.join('/'))).toEqual(order.slice(0, 150))
    expect(round).toBeLessThanOrEqual(3)

    // From an anchor deep in the tree, both ways, with a fresh cache.
    const at = 250
    const fresh = new Egdb({ client: server.client, branch, author: 'test' })
    const deep = await settle(fresh, {
      root: ['root'],
      anchor: all.paths[at]!,
      query,
      offset: -60,
      limit: 120,
      lookahead: 20,
    })
    expect(deep.window.rows.map((r) => r.path.join('/'))).toEqual(order.slice(at - 60, at + 60))
    expect(deep.window.first).toBe(-60)
  })

  it('proves an entity is in the walk exactly when the walk emits it', async () => {
    const all = await server.client.walk(branch, { start: ['root'], follow: query.follow, options: { steps: 100_000 } })
    const target = all.paths[all.paths.length - 1]!
    const item = target[target.length - 1]!
    const proved = await server.client.prove(branch, { start: ['root'], follow: query.follow, item })
    expect(proved.outcome).toMatchObject({ proved: { path: target } })
    expect((proved.outcome as { proved: { links: unknown[] } }).proved.links).toHaveLength(target.length - 1)
    const filtered = await server.client.prove(branch, {
      start: ['root'],
      follow: query.follow,
      item,
      include: f.eq('type', 'nothing'),
    })
    expect(filtered.outcome).toBe('no_path')
  })

  it('matches the server with collapsed rows and filters', async () => {
    const collapsed = [['root', 'root.0'], ['root', 'root.1', 'root.1.0']]
    const descend = f.ne('text', 'item root.3')
    const include = f.eq('type', 'task')
    const all = await server.client.walk(branch, {
      start: ['root'],
      follow: query.follow,
      descend,
      include,
      collapsed,
      options: { steps: 100_000 },
    })
    const egdb = new Egdb({ client: server.client, branch, author: 'test' })
    const { window } = await settle(egdb, {
      root: ['root'],
      query: { ...query, descend, include, collapsed },
      offset: 0,
      limit: 100_000,
      lookahead: 0,
    })
    expect(window.rows.map((r) => r.path.join('/'))).toEqual(all.paths.map((p) => p.join('/')))
  })

  it('shows a write at once, keeps it when the server confirms, and drops a refused one', async () => {
    const egdb = new Egdb({ client: server.client, branch, author: 'test' })
    const request = { root: ['root'], query, offset: 0, limit: 5, lookahead: 0 }
    await settle(egdb, request)
    const writing = egdb.write([
      op.set('fresh', 'type', 'task'),
      op.set('fresh', 'text', 'new'),
      op.link('root', 'fresh', 'child', { explicit: -1 }),
    ])
    expect(computeWindow(egdb.view(), request).rows[1]).toMatchObject({ id: 'fresh', status: 'ready' })
    await writing
    const after = computeWindow(egdb.view(), request)
    expect(after.rows[1]).toMatchObject({ id: 'fresh', values: { type: 'task', text: 'new' } })
    expect(egdb.store.get(egdb.cacheAtom).pending).toHaveLength(0)

    const bad = new Egdb({ client: server.client, branch: '00000000-0000-7000-8000-000000000000', author: 'x', onError: () => {} })
    await expect(bad.write(op.set('a', 'b', 1))).rejects.toMatchObject({ kind: 'unknown_branch' })
    expect(bad.store.get(bad.cacheAtom).pending).toHaveLength(0)
  })

  it('pages a find', async () => {
    const egdb = new Egdb({ client: server.client, branch, author: 'test' })
    const filter = f.eq('type', 'list')
    await egdb.findMore('lists', filter, { pageSize: 10 }, 25)
    const state = egdb.store.get(egdb.findsAtom).get('lists')!
    expect(state.ids.length).toBe(30)
    expect(state.done).toBe(false)
    expect(egdb.view().get(state.ids[0]!)?.values?.type).toBe('list')
  })

  it('reads the log, values and links together, and filters it', async () => {
    await server.client.write({ branch, author: 'logger', ops: [op.set('logged', 'text', 'one'), op.link('logged', 'other', 'ref')] })
    await server.client.write({ branch, author: 'logger', ops: [op.set('logged', 'text', null), op.unlink('logged', 'other', 'ref')] })
    const log = await server.client.log(branch, { authors: ['logger'] })
    expect(log.events).toHaveLength(4)
    expect(log.events[0]!.event.seq).toBeGreaterThanOrEqual(log.events[3]!.event.seq)
    const removed = await server.client.log(branch, { authors: ['logger'], removed: true })
    expect(removed.events).toHaveLength(2)
    const links = await server.client.log(branch, { authors: ['logger'], kind: 'links' })
    expect(links.events.every((e) => e.kind === 'link')).toBe(true)
    const after = await server.client.log(branch, { after: log.head })
    expect(after.events).toEqual([])
    expect(after.head).toBe(log.head)
  })

  it('follows the feed: writes from elsewhere land on what is loaded, with no reads', async () => {
    await server.client.write({ branch, author: 'test', ops: [op.set('followed', 'text', 'before'), op.set('kid', 'type', 'task'), op.set('kid2', 'type', 'task'), op.link('followed', 'kid', 'child')] })
    const egdb = new Egdb({ client: server.client, branch, author: 'test' })
    await egdb.load(['followed'], [['child', 'outbound']])
    const stop = egdb.follow({ wait: 2000 })
    try {
      // Wait until it has a cursor.
      for (let i = 0; i < 100 && egdb.store.get(egdb.headAtom) === null; i++) await new Promise((r) => setTimeout(r, 20))
      const values = server.client.values.bind(server.client)
      let reads = 0
      server.client.values = (...args) => {
        reads++
        return values(...args)
      }
      // Another writer: a value, a new link, and a value on an entity not loaded.
      await server.client.write({
        branch,
        author: 'elsewhere',
        ops: [op.set('followed', 'text', 'after'), op.link('followed', 'kid2', 'child'), op.set('unloaded', 'text', 'x')],
      })
      for (let i = 0; i < 100 && egdb.view().get('followed')?.values?.text !== 'after'; i++) await new Promise((r) => setTimeout(r, 20))
      const record = egdb.view().get('followed')!
      expect(record.values?.text).toBe('after')
      expect(record.links['child\u0000outbound']!.map((l) => l.destination)).toEqual(['kid', 'kid2'])
      expect(egdb.view().get('unloaded')).toBeUndefined()
      expect(reads).toBe(0)
      expect(egdb.writtenSince(0).has('followed')).toBe(true)
    } finally {
      stop()
      server.client.values = Object.getPrototypeOf(server.client).values
    }
  })
})

