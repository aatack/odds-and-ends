import { describe, expect, it } from 'vitest'
import { groupKey, viewOf, type CacheState, type EntityRecord } from '../src/cache.js'
import { f } from '../src/filter.js'
import { op } from '../src/ops.js'
import type { Path } from '../src/types.js'
import { computeWindow, countBefore, type WalkQuery } from '../src/walk.js'

const child = groupKey('child', 'outbound')
const query: WalkQuery = { follow: { list: [['child', 'outbound', 'list'], ['child', 'outbound', 'task']] } }

/** A cache from `{ id: [type, ...children] }`; every entity fully loaded. */
function cache(tree: Record<string, [string, ...string[]]>, skip: string[] = []): CacheState {
  const base = new Map<string, EntityRecord>()
  for (const [id, [type, ...kids]] of Object.entries(tree)) {
    if (skip.includes(id)) continue
    base.set(id, {
      id,
      values: { type, text: id.toUpperCase() },
      links: {
        [child]: kids.map((k, i) => ({ source: id, destination: k, link_type: 'child', sort: i, event: null })),
      },
    })
  }
  return { base, pending: [] }
}

// root -> a -> a1, a2 ; root -> b (task) ; root -> c -> c1 -> c2
const tree: Record<string, [string, ...string[]]> = {
  root: ['list', 'a', 'b', 'c'],
  a: ['list', 'a1', 'a2'],
  a1: ['task'],
  a2: ['task'],
  b: ['task'],
  c: ['list', 'c1'],
  c1: ['list', 'c2'],
  c2: ['task'],
}
const order = ['root', 'root/a', 'root/a/a1', 'root/a/a2', 'root/b', 'root/c', 'root/c/c1', 'root/c/c1/c2']
const paths = (w: { rows: { path: string[] }[] }) => w.rows.map((r) => r.path.join('/'))

describe('computeWindow', () => {
  it('reads the whole tree depth first from the root', () => {
    const w = computeWindow(viewOf(cache(tree)), { root: ['root'], query, offset: 0, limit: 100 })
    expect(paths(w)).toEqual(order)
    expect(w).toMatchObject({ first: 0, atStart: true, atEnd: true, missing: {} })
    expect(w.rows[1]).toMatchObject({ depth: 1, hasChildren: true, status: 'ready', childrenLoading: false })
  })

  it('counts rows from any anchor, both ways', () => {
    const view = viewOf(cache(tree))
    order.forEach((anchor, at) => {
      for (const offset of [-8, -3, -1, 0, 2]) {
        const w = computeWindow(view, { root: ['root'], anchor: anchor.split('/'), query, offset, limit: 4 })
        const from = Math.max(0, at + offset)
        const to = Math.max(from, Math.min(order.length, at + offset + 4))
        expect(paths(w), `anchor ${anchor} offset ${offset}`).toEqual(order.slice(from, to))
        if (to > from) expect(w.first).toBe(from - at)
        expect(w.atStart).toBe(at + offset <= 0)
        expect(w.atEnd).toBe(at + offset + 4 >= order.length)
      }
    })
  })

  it('folds collapsed rows and skips rows that include rejects', () => {
    const view = viewOf(cache(tree))
    const folded = computeWindow(view, { root: ['root'], query: { ...query, collapsed: [['root', 'a']] }, offset: 0, limit: 100 })
    expect(paths(folded)).toEqual(['root', 'root/a', 'root/b', 'root/c', 'root/c/c1', 'root/c/c1/c2'])
    expect(folded.rows[1]).toMatchObject({ collapsed: true, hasChildren: true })
    const tasks = computeWindow(view, { root: ['root'], query: { ...query, include: f.eq('type', 'task') }, offset: 0, limit: 100 })
    expect(paths(tasks)).toEqual(['root/a/a1', 'root/a/a2', 'root/b', 'root/c/c1/c2'])
    // Anchored on a row that is not included, row 0 is the next included one.
    const mid = computeWindow(view, {
      root: ['root'],
      anchor: ['root', 'c'],
      query: { ...query, include: f.eq('type', 'task') },
      offset: -1,
      limit: 2,
    })
    expect(paths(mid)).toEqual(['root/b', 'root/c/c1/c2'])
  })

  it('cuts the anchor short at a collapsed ancestor or a removed link', () => {
    const view = viewOf(cache(tree))
    const folded = computeWindow(view, {
      root: ['root'],
      anchor: ['root', 'c', 'c1', 'c2'],
      query: { ...query, collapsed: [['root', 'c']] },
      offset: 0,
      limit: 1,
    })
    expect(folded.anchor).toEqual(['root', 'c'])
    const gone = computeWindow(view, { root: ['root'], anchor: ['root', 'zzz'], query, offset: 0, limit: 1 })
    expect(gone.anchor).toEqual(['root'])
  })

  it('shows missing entities as loading rows and says where to preload', () => {
    const view = viewOf(cache(tree, ['a', 'c1']))
    const w = computeWindow(view, { root: ['root'], query, offset: 0, limit: 100 })
    expect(paths(w)).toEqual(['root', 'root/a', 'root/b', 'root/c', 'root/c/c1'])
    expect(w.rows[1]).toMatchObject({ status: 'loading', values: undefined })
    expect(w.missing.forward).toEqual(['root', 'a'])
    const back = computeWindow(view, { root: ['root'], anchor: ['root', 'b'], query, offset: -2, limit: 2 })
    expect(paths(back)).toEqual(['root', 'root/a'])
    expect(back.missing.backward).toEqual(['root', 'b'])
  })

  it('with a filter, waits for missing entities rather than showing them, and knows no end yet', () => {
    const view = viewOf(cache(tree, ['a', 'c1']))
    const w = computeWindow(view, { root: ['root'], query: { ...query, include: f.eq('type', 'task') }, offset: 0, limit: 100 })
    expect(paths(w)).toEqual(['root/b'])
    expect(w.atEnd).toBe(false)
    expect(w.missing.forward).toEqual(['root', 'a'])
    const plain = computeWindow(view, { root: ['root'], query, offset: 0, limit: 100 })
    expect(plain.atEnd).toBe(false)
  })

  it('counts the rows before an anchor', () => {
    const view = viewOf(cache(tree))
    expect(countBefore(view, { root: ['root'], anchor: ['root', 'c', 'c1'], query })).toEqual({ count: 6, complete: true })
    const tasks = { ...query, include: f.eq('type', 'task') }
    expect(countBefore(view, { root: ['root'], anchor: ['root', 'c'], query: tasks })).toEqual({ count: 3, complete: true })
    expect(countBefore(viewOf(cache(tree, ['a'])), { root: ['root'], anchor: ['root', 'b'], query }).complete).toBe(false)
  })

  it('applies pending writes at once', () => {
    const state = cache(tree)
    const pending = {
      id: 1,
      timestamp: 1e12,
      ops: [
        op.set('new', 'type', 'task'),
        op.link('root', 'new', 'child', { explicit: 0.5 }),
        op.set('b', 'text', 'edited'),
        op.archive('c2'),
        op.unlink('a', 'a1', 'child'),
      ],
    }
    const w = computeWindow(viewOf({ ...state, pending: [pending] }), { root: ['root'], query, offset: 0, limit: 100 })
    expect(paths(w)).toEqual(['root', 'root/a', 'root/a/a2', 'root/new', 'root/b', 'root/c', 'root/c/c1'])
    expect(w.rows.find((r) => r.id === 'b')?.values?.text).toBe('edited')
    expect(w.rows.find((r) => r.id === 'c1')?.hasChildren).toBe(false)
  })

  it('follows a link only to the child types of its rules, in link order per group', () => {
    const typed = { ...tree, root: ['list', 'a', 'n', 'b', 'c'] as [string, ...string[]], n: ['note'] as [string, ...string[]] }
    const view = viewOf(cache(typed))
    const lists: WalkQuery = { follow: { list: [['child', 'outbound', 'list']] } }
    expect(paths(computeWindow(view, { root: ['root'], query: lists, offset: 0, limit: 100 }))).toEqual([
      'root',
      'root/a',
      'root/c',
      'root/c/c1',
    ])
    // Two child types on one group keep the link order; the note is not followed.
    const both: WalkQuery = { follow: { list: [['child', 'outbound', 'task'], ['child', 'outbound', 'list']] } }
    expect(paths(computeWindow(view, { root: ['root'], query: both, offset: 0, limit: 100 }))).toEqual(order)
    // A child that is not loaded may be of a followed type: it shows, and loads.
    const partial = viewOf(cache(typed, ['n']))
    const w = computeWindow(partial, { root: ['root'], query: both, offset: 0, limit: 100 })
    expect(paths(w)).toContain('root/n')
    expect(w.missing.forward).toEqual(['root', 'n'])
  })

  it('never walks through an entity already on the path', () => {
    const cyclic = { ...tree, c1: ['list', 'c2', 'root', 'a'] as [string, ...string[]] }
    const w = computeWindow(viewOf(cache(cyclic)), { root: ['root'], query, offset: 0, limit: 100 })
    expect(paths(w)).toContain('root/c/c1/a/a1')
    expect(paths(w).some((p) => p.startsWith('root/c/c1/root'))).toBe(false)
  })

  it('treats a longer root as one subtree', () => {
    const root: Path = ['root', 'c']
    const w = computeWindow(viewOf(cache(tree)), { root, query, offset: 0, limit: 100 })
    expect(paths(w)).toEqual(['root/c', 'root/c/c1', 'root/c/c1/c2'])
    expect(w.rows[0]!.depth).toBe(0)
  })
})
