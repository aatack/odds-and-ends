import { describe, expect, it } from 'vitest'
import { NO_TRAVERSAL, resolveQuery, stepPath, type GetEntities } from '../../src/core/query'
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
