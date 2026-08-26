import { describe, expect, it } from 'vitest'
import {
  filterPaths,
  NO_TRAVERSAL,
  resolveQuery,
  runQuery,
  stepPath,
  type GetEntities,
  type LoadEntities,
  type QueryFilters,
} from '../../src/core/query'
import type { Entity } from '../../src/core/entity'

// The traversal is a pure function over a `getEntities`, which is the whole
// point of it: the same stepper runs off the frontend's cache and off a store.
// Here it runs off a literal.

const entity = (id: string, out: string[] = [], inbound: string[] = []): Entity => ({
  id,
  createdAt: 0,
  editedAt: 0,
  createdBy: '',
  editedBy: '',
  values: {},
  outboundLinks: out,
  inboundLinks: inbound,
})

/** A tree of `id → children`, as a `getEntities` that also records what it read. */
function graph(links: Record<string, string[]>): GetEntities {
  const inbound: Record<string, string[]> = {}
  for (const [id, children] of Object.entries(links)) {
    for (const child of children) (inbound[child] ??= []).push(id)
  }
  return (ids) =>
    Object.fromEntries(ids.map((id) => [id, entity(id, links[id] ?? [], inbound[id] ?? [])]))
}

// a ─ b ─ d
//   │   └ e
//   └ c
const TREE = graph({ a: ['b', 'c'], b: ['d', 'e'] })

const paths = (r: { paths: string[][] }): string[] => r.paths.map((p) => p.join('/'))

describe('stepPath', () => {
  it('reads depth-first, and stops when there is nowhere left to go', () => {
    expect(paths(resolveQuery(['a'], TREE, NO_TRAVERSAL, 100))).toEqual([
      'a',
      'a/b',
      'a/b/d',
      'a/b/e',
      'a/c',
    ])
    expect(stepPath(['a', 'c'], TREE, NO_TRAVERSAL)).toBeNull()
  })

  it('walks inbound links the other way round', () => {
    const t = { ...NO_TRAVERSAL, direction: 'in' as const }
    expect(paths(resolveQuery(['d'], TREE, t, 100))).toEqual(['d', 'd/b', 'd/b/a'])
  })

  it('does not walk below a folded entity, but carries on past it', () => {
    const t = { ...NO_TRAVERSAL, collapsed: ['b'] }
    expect(paths(resolveQuery(['a'], TREE, t, 100))).toEqual(['a', 'a/b', 'a/c'])
  })

  it('caps depth relative to the entity that set the cap', () => {
    expect(paths(resolveQuery(['a'], TREE, { ...NO_TRAVERSAL, maxDepth: { a: 1 } }, 100))).toEqual([
      'a',
      'a/b',
      'a/c',
    ])
    // A nearer ancestor overrides a more distant one — including by lifting it.
    const nearer = { ...NO_TRAVERSAL, maxDepth: { a: 1, b: null } }
    expect(paths(resolveQuery(['a'], TREE, nearer, 100))).toEqual([
      'a',
      'a/b',
      'a/b/d',
      'a/b/e',
      'a/c',
    ])
  })

  it('never lets an entity be its own ancestor', () => {
    // a → b → a: the cycle is cut, and b's other child is still reached.
    const cyclic = graph({ a: ['b'], b: ['a', 'c'] })
    expect(paths(resolveQuery(['a'], cyclic, NO_TRAVERSAL, 100))).toEqual(['a', 'a/b', 'a/b/c'])
    // The same entity in two branches is fine — it just can't be inside itself.
    const shared = graph({ a: ['b', 'c'], b: ['x'], c: ['x'] })
    expect(paths(resolveQuery(['a'], shared, NO_TRAVERSAL, 100))).toEqual([
      'a',
      'a/b',
      'a/b/x',
      'a/c',
      'a/c/x',
    ])
  })

  it('reports an unfinished traversal, and resumes from where it stopped', () => {
    const cut = resolveQuery(['a'], TREE, NO_TRAVERSAL, 3)
    expect(paths(cut)).toEqual(['a', 'a/b', 'a/b/d'])
    expect(cut.complete).toBe(false)
    const rest = resolveQuery(stepPath(['a', 'b', 'd'], TREE, NO_TRAVERSAL)!, TREE, NO_TRAVERSAL, 100)
    expect(paths(rest)).toEqual(['a/b/e', 'a/c'])
    expect(rest.complete).toBe(true)
  })

  it('treats an entity nothing is known about as childless rather than missing', () => {
    // Which is what makes it safe to run off a half-loaded cache: the row is
    // there, and grows children when its events arrive.
    expect(paths(resolveQuery(['nobody'], TREE, NO_TRAVERSAL, 100))).toEqual(['nobody'])
  })
})

/** The same graph, with values on it — which is what the filters read. */
const withValues =
  (get: GetEntities, values: Record<string, Record<string, unknown>>): GetEntities =>
  (ids) => {
    const base = get(ids)
    return Object.fromEntries(ids.map((id) => [id, { ...base[id], values: values[id] ?? {} }]))
  }

// a ─ b (section) ─ d
//   │              ├ e (section)
//   │              └ f
//   └ c
const OUTLINE = withValues(graph({ a: ['b', 'c'], b: ['d', 'e', 'f'] }), {
  a: { text: 'Alpha' },
  b: { text: 'Bravo', section: true },
  c: { text: 'Charlie' },
  d: { text: 'Delta' },
  e: { text: 'Echo', section: true },
  f: { text: 'Foxtrot' },
})

// a ─ b [ ] ─ d [x] ─ g [ ]
//   │        └ e [ ]
//   └ c [x] ─ h [ ]
const TASKS = withValues(graph({ a: ['b', 'c'], b: ['d', 'e'], c: ['h'], d: ['g'] }), {
  a: { text: 'Alpha' },
  b: { text: 'Bravo', open: true },
  c: { text: 'Charlie', open: false },
  d: { text: 'Delta', open: false },
  e: { text: 'Echo', open: true },
  g: { text: 'Golf', open: true },
  h: { text: 'Hotel', open: true },
})

const filtered = (start: string[], filters: QueryFilters, get: GetEntities = OUTLINE): string[] =>
  filterPaths(start, resolveQuery(start, get, NO_TRAVERSAL, 100, filters).paths, get, filters).map(
    (p) => p.join('/'),
  )

describe('filterPaths', () => {
  it('keeps a match and the rows above it, so the outline still reads', () => {
    expect(filtered(['a'], { find: 'echo' })).toEqual(['a', 'a/b', 'a/b/e'])
  })

  it('keeps the sections, and the row that was asked about', () => {
    expect(filtered(['a'], { sections: true })).toEqual(['a', 'a/b', 'a/b/e'])
  })

  it('keeps the open items, and the row that was asked about', () => {
    // `a` is neither open nor ticked, so it is walked through rather than kept
    // for being open — it is here because it is what was asked about.
    expect(filtered(['a'], { open: true }, TASKS)).toEqual(['a', 'a/b', 'a/b/e'])
  })

  it('stops at a ticked item, taking its subtree with it', () => {
    // `d` and `c` are ticked, so `g` and `h` are never reached however open they
    // are: what is under something finished is finished too. Without the filter
    // the walk reads all of them.
    expect(filtered(['a'], {}, TASKS)).toEqual([
      'a',
      'a/b',
      'a/b/d',
      'a/b/d/g',
      'a/b/e',
      'a/c',
      'a/c/h',
    ])
  })

  it('reads through a plain bullet to the tasks under it', () => {
    // `null` is not `false`: a note that is not a task at all stops nothing.
    const notes = withValues(graph({ a: ['b'], b: ['c'] }), {
      a: { text: 'Alpha' },
      b: { text: 'Bravo' },
      c: { text: 'Charlie', open: true },
    })
    expect(filtered(['a'], { open: true }, notes)).toEqual(['a', 'a/b/c'])
  })

  it('keeps only the resume path itself when a walk carries on mid-outline', () => {
    // The row a page starts at is kept because it is what was asked for — not
    // every row that happens to sit at the same depth, which is most of what a
    // page after the first contains.
    expect(filtered(['a', 'b', 'd'], {})).toEqual(['a/b/d', 'a/b/e', 'a/b/f', 'a/c'])
    expect(filtered(['a', 'b', 'd'], { sections: true })).toEqual(['a/b/d', 'a/b/e'])
  })
})

/** The same graph read asynchronously, keeping the batches it was asked for. */
function loader(get: GetEntities): { load: LoadEntities; batches: string[][] } {
  const batches: string[][] = []
  return {
    batches,
    load: async (ids) => {
      batches.push(ids)
      return get(ids)
    },
  }
}

const rows = (page: { rows: { path: string[] }[] }): string[] =>
  page.rows.map((r) => r.path.join('/'))

describe('runQuery', () => {
  it('reads a level at a time, and stops when the walk has everything', async () => {
    const { load, batches } = loader(OUTLINE)
    const page = await runQuery(['a'], load, NO_TRAVERSAL, 100)
    expect(rows(page)).toEqual(['a', 'a/b', 'a/b/d', 'a/b/e', 'a/b/f', 'a/c'])
    expect(batches).toEqual([['a'], ['b', 'c'], ['d', 'e', 'f']])
  })

  it('loads the rows the walk stops at, rather than returning them blank', async () => {
    // The walk never reads an entity it will not descend through, so the rows
    // at a depth cap — or under a fold — are reached but never asked for. They
    // are still rows, and a page is not finished until they have loaded.
    for (const t of [{ ...NO_TRAVERSAL, maxDepth: { a: 1 } }, { ...NO_TRAVERSAL, collapsed: ['b'] }]) {
      const { load } = loader(OUTLINE)
      const page = await runQuery(['a'], load, t, 100)
      expect(rows(page)).toEqual(['a', 'a/b', 'a/c'])
      expect(page.rows.map((r) => r.entity.values.text)).toEqual(['Alpha', 'Bravo', 'Charlie'])
    }
  })

  it('filters on text the walk itself never had to read', async () => {
    // Same thing seen from the filters: they read every row the walk visited,
    // which is more than the walk needed, so a find at the depth cap only works
    // if what it reads has loaded.
    const { load } = loader(OUTLINE)
    const t = { ...NO_TRAVERSAL, maxDepth: { a: 1 } }
    expect(rows(await runQuery(['a'], load, t, 100, { find: 'charlie' }))).toEqual(['a', 'a/c'])
    expect(rows(await runQuery(['a'], load, t, 100, { sections: true }))).toEqual(['a', 'a/b'])
  })

  it('settles on an id the store knows nothing about', async () => {
    const batches: string[][] = []
    const load: LoadEntities = async (ids) => {
      batches.push(ids)
      return {}
    }
    const page = await runQuery(['nobody'], load, NO_TRAVERSAL, 100)
    expect(page.rows).toHaveLength(1)
    expect(page.rows[0].entity.values).toEqual({})
    // Asked for once: an id that came back with nothing is an answer, not a miss.
    expect(batches).toEqual([['nobody']])
  })

  it('reports what the limit cut short, and resumes from there', async () => {
    const { load } = loader(OUTLINE)
    const cut = await runQuery(['a'], load, NO_TRAVERSAL, 3)
    expect(rows(cut)).toEqual(['a', 'a/b', 'a/b/d'])
    expect(cut.scanned).toBe(3)
    const rest = await runQuery(cut.continuation!, load, NO_TRAVERSAL, 100)
    expect(rows(rest)).toEqual(['a/b/e', 'a/b/f', 'a/c'])
    expect(rest.continuation).toBeNull()
  })
})
