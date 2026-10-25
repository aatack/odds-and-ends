import { describe, expect, it } from 'vitest'
import { f, matches } from '../src/filter.js'
import { localize } from '../src/find.js'
import { viewOf, type EntityRecord } from '../src/cache.js'

const v = { type: 'task', title: 'Write REPORT', score: 70, done: false, tags: ['a'] }

describe('matches', () => {
  it('follows the server rules', () => {
    expect(matches(f.eq('type', 'task'), 'x', v)).toBe(true)
    expect(matches(f.eq('missing', null), 'x', v)).toBe(true)
    expect(matches(f.ne('missing', 1), 'x', v)).toBe(true)
    expect(matches(f.gt('score', 50), 'x', v)).toBe(true)
    expect(matches(f.gt('title', 50), 'x', v)).toBe(false)
    expect(matches(f.lt('done', 1), 'x', v)).toBe(false)
    expect(matches(f.contains('title', 'report'), 'x', v)).toBe(false)
    expect(matches(f.containsIgnoreCase('title', 'report'), 'x', v)).toBe(true)
    expect(matches(f.prefix('title', 'Write'), 'x', v)).toBe(true)
    expect(matches(f.in('score', [1, 70]), 'x', v)).toBe(true)
    expect(matches(f.eq('tags', ['a']), 'x', v)).toBe(true)
    expect(matches(f.and(f.notNull('type'), f.not(f.isNull('score'))), 'x', v)).toBe(true)
    expect(matches(f.or(f.isNull('type'), f.lt('score', 0)), 'x', v)).toBe(false)
  })
})

describe('localize', () => {
  const record = (id: string, values: Record<string, string | number>): [string, EntityRecord] => [id, { id, values, links: {} }]
  const view = viewOf({
    base: new Map([
      record('a', { type: 'task', score: 1 }),
      record('b', { type: 'task', score: 5 }),
      record('c', { type: 'note', score: 3 }),
      record('d', { type: 'task', score: 3 }),
    ]),
    pending: [],
  })
  const filter = f.eq('type', 'task')

  it('drops written rows that no longer match and adds new matches in order', () => {
    expect(localize(view, { ids: ['a', 'b', 'c'], done: true, filter, written: new Set(['c', 'd']) })).toEqual(['a', 'b', 'd'])
    expect(
      localize(view, { ids: ['b', 'a'], done: true, filter, order: { value_desc: 'score' }, written: new Set(['d']) }),
    ).toEqual(['b', 'd', 'a'])
  })

  it('leaves a match past the last fetched row to the next page', () => {
    expect(localize(view, { ids: ['a', 'b'], done: false, filter, written: new Set(['d']) })).toEqual(['a', 'b'])
  })
})
