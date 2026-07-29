import { describe, expect, it } from 'vitest'
import { outlineMarkdown, subtreeMarkdown } from '../../src/core/markdown'
import type { TreeRow } from '../../src/core/tree'

// The format three callers share: the desktop's export, the phone's, and the
// answer `query` gives an agent. Rows are written out by hand here, since what is
// being tested is the shape of the text and not how the rows were reached.

const row = (id: string, depth: number, text: string, rest: Partial<TreeRow> = {}): TreeRow => ({
  id,
  depth,
  path: [id],
  parentId: null,
  text,
  hasChildren: false,
  collapsed: false,
  ...rest,
})

const lines = (md: string): string[] => md.split('\n')

describe('outlineMarkdown', () => {
  it('reads the top row as a title and everything below it as a bullet', () => {
    const md = outlineMarkdown([row('a', 0, 'Top'), row('b', 1, 'One'), row('c', 2, 'Two')])
    expect(lines(md)).toEqual(['Top', '- One', '  - Two'])
  })

  it('marks a section with a hash per level, and a task with a box', () => {
    const md = outlineMarkdown([
      row('a', 0, 'Top', { section: true }),
      row('b', 1, 'Head', { section: true }),
      row('c', 1, 'To do', { open: true }),
      row('d', 1, 'Done', { open: false }),
    ])
    expect(lines(md)).toEqual(['# Top', '- ## Head', '- [ ] To do', '- [x] Done'])
  })

  it('stops at six heading levels, markdown having no seventh', () => {
    const md = outlineMarkdown([row('a', 7, 'Deep', { section: true })])
    expect(md).toBe(`${'  '.repeat(6)}- ###### Deep`)
  })

  it('pads the id column so the indentation still reads as indentation', () => {
    const md = outlineMarkdown([row('@index', 0, 'Top'), row('x', 1, 'One')], { ids: true })
    expect(lines(md)).toEqual(['@index  Top', 'x       - One'])
  })

  it('keeps a multi-line text out of the id column', () => {
    // A line in that column that names no entity would read as an id; the rest of
    // a code block goes under its own bullet instead.
    const md = outlineMarkdown([row('a', 0, 'Top'), row('bb', 1, 'Code:\n```\nx = 1\n```')], {
      ids: true,
    })
    expect(lines(md)).toEqual(['a   Top', 'bb  - Code:', '      ```', '      x = 1', '      ```'])
  })
})

describe('subtreeMarkdown', () => {
  const rows = [
    row('a', 0, 'Top'),
    row('b', 1, 'Branch'),
    row('c', 2, 'Leaf'),
    row('d', 2, 'Folded', { collapsed: true, hasChildren: true }),
    row('e', 1, 'Sibling'),
  ]

  it('takes the row and its descendants, and rebases the depths on it', () => {
    expect(lines(subtreeMarkdown(rows, 1))).toEqual(['Branch', '- Leaf'])
  })

  it('leaves out a folded row, since what is under it is not on screen', () => {
    // But exports the folded row itself when that is what was asked for.
    expect(subtreeMarkdown(rows, 3)).toBe('Folded')
  })

  it('is empty when there is no such row', () => {
    expect(subtreeMarkdown(rows, 9)).toBe('')
  })
})
