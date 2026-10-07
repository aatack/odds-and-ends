import assert from 'node:assert/strict'
import { test } from 'node:test'
import { outline, splitText, type OutlineItem } from './outline.ts'

/** An outline as `text` and children, sections marked `#`, boxes `[ ]`/`[x]`. */
const shape = (items: OutlineItem[]): unknown[] =>
  items.map((item) => {
    const mark = item.section ? '# ' : item.open === true ? '[ ] ' : item.open === false ? '[x] ' : ''
    return item.children.length ? [mark + item.text, shape(item.children)] : mark + item.text
  })

test('outline: prose before any structure is the lead, all of it when there is none', () => {
  assert.deepEqual(outline('Just this.\n\nAnd this.'), { lead: 'Just this.\n\nAnd this.', items: [] })
})

test('outline: headings hold what follows, bullets nest by indent, blocks are one item each', () => {
  const { lead, items } = outline(
    [
      'Here is the plan.',
      '',
      '## Steps',
      '',
      '1. First',
      '   carried on',
      '   - nested',
      '     - deeper',
      '2. Second',
      '',
      '   A paragraph in the second.',
      '',
      '### Detail',
      '',
      '- [ ] to do',
      '- [x] done',
      '',
      '```ts',
      'const x = 1',
      '',
      'const y = 2',
      '```',
      '',
      '---',
      '',
      '## Notes',
      '',
      '| a | b |',
      '|-|-|',
      '| 1 | 2 |',
      '',
      'Last words.',
    ].join('\n'),
  )
  assert.equal(lead, 'Here is the plan.')
  assert.deepEqual(shape(items), [
    [
      '# Steps',
      [
        ['First carried on', [['nested', ['deeper']]]],
        ['Second', ['A paragraph in the second.']],
        ['# Detail', ['[ ] to do', '[x] done', '```ts\nconst x = 1\n\nconst y = 2\n```']],
      ],
    ],
    ['# Notes', ['| a | b |\n|-|-|\n| 1 | 2 |', 'Last words.']],
  ])
})

test('outline: a split with no lead takes the first item as its text, its children moved up', () => {
  const split = splitText('# Title\n\n- a\n- b\n\n# Next')!
  assert.equal(split.lead, 'Title')
  assert.deepEqual(shape(split.items), ['a', 'b', '# Next'])
  assert.equal(splitText('Only prose.'), null)
})
