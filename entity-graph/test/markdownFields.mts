// The custom inline forms, over the real markdown parser: `[@type:arg](text)`
// and `[@type](text)` are the one bit of entity text that stops being markdown,
// and the two shapes it arrives in — a parsed link, or literal text — are easy to
// get right one at a time and wrong together.
//
// What it checks: that both shapes are taken, that the `:arg` is optional, that a
// type nobody can draw is left alone, and that ordinary links and code survive.
//
//   npm test

import assert from 'node:assert/strict'
import { unified } from 'unified'
import remarkParse from 'remark-parse'
import type { Root, RootContent } from 'mdast'

const { markdownFieldsPlugin } = await import('../src/renderer/src/components/ui/markdownFields')

/** What the renderer would be handed: the fields in the tree, in order. */
function fieldsIn(text: string, types: readonly string[]): Record<string, string>[] {
  const tree = unified().use(remarkParse).parse(text) as Root
  markdownFieldsPlugin(types)()(tree)
  const found: Record<string, string>[] = []
  const walk = (node: RootContent | Root): void => {
    const props = (node as { data?: { hProperties?: Record<string, string> } }).data?.hProperties
    if (props) found.push(props)
    for (const child of (node as { children?: RootContent[] }).children ?? []) walk(child)
  }
  walk(tree)
  return found
}

/** The text left in the tree, which is what an untaken form falls back to. */
function textIn(text: string, types: readonly string[]): string {
  const tree = unified().use(remarkParse).parse(text) as Root
  markdownFieldsPlugin(types)()(tree)
  const parts: string[] = []
  const walk = (node: RootContent | Root): void => {
    if (node.type === 'text' || node.type === 'inlineCode') parts.push(node.value)
    for (const child of (node as { children?: RootContent[] }).children ?? []) walk(child)
  }
  walk(tree)
  return parts.join('')
}

const KNOWN = ['button', 'codeEditor', 'pill']

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

test('takes a form the parser turned into a link', () => {
  // No space in the parens, so markdown reads it as a link with an empty label.
  assert.deepEqual(fieldsIn('[@pill](changeset)', KNOWN), [
    { 'data-field-type': 'pill', 'data-field-arg': '', 'data-field-text': 'changeset' },
  ])
})

test('takes a form the parser left as text', () => {
  // A space in the parens is not a link destination, so it arrives as prose.
  assert.deepEqual(fieldsIn('[@pill](in progress)', KNOWN), [
    { 'data-field-type': 'pill', 'data-field-arg': '', 'data-field-text': 'in progress' },
  ])
})

test('keeps the arg when there is one', () => {
  assert.deepEqual(fieldsIn('[@button:entity.inspect](Look)', KNOWN), [
    { 'data-field-type': 'button', 'data-field-arg': 'entity.inspect', 'data-field-text': 'Look' },
  ])
  assert.deepEqual(fieldsIn('[@codeEditor:events](a hint)', KNOWN), [
    { 'data-field-type': 'codeEditor', 'data-field-arg': 'events', 'data-field-text': 'a hint' },
  ])
})

test('takes a form mid-sentence, and leaves the sentence', () => {
  assert.deepEqual(fieldsIn('this is a [@pill](note) in a line', KNOWN), [
    { 'data-field-type': 'pill', 'data-field-arg': '', 'data-field-text': 'note' },
  ])
  assert.equal(textIn('this is a [@pill](note) in a line', KNOWN), 'this is a  in a line')
})

test('leaves a type nobody can draw', () => {
  assert.deepEqual(fieldsIn('[@pill](changeset)', ['button']), [])
  assert.deepEqual(fieldsIn('[@nonesuch](x)', KNOWN), [])
  assert.deepEqual(fieldsIn('[@nonesuch:y](a phrase)', KNOWN), [])
  assert.equal(textIn('[@nonesuch:y](a phrase)', KNOWN), '[@nonesuch:y](a phrase)')
})

test('leaves ordinary links, and the syntax quoted in code', () => {
  assert.deepEqual(fieldsIn('[a link](https://example.com)', KNOWN), [])
  assert.deepEqual(fieldsIn('`[@pill](changeset)`', KNOWN), [])
  assert.equal(textIn('`[@pill](changeset)`', KNOWN), '[@pill](changeset)')
})

// The shape a type's actions are drawn in: `views/Editor` writes one of these on
// the end of the row's text and the field renderer does the rest, so the dotted
// id and the empty parens are load-bearing rather than incidental.
test('takes the buttons a type puts on a row', () => {
  assert.deepEqual(
    fieldsIn('Improvements [@button:changeset.checkout]() [@button:changeset.merge]()', KNOWN),
    [
      { 'data-field-type': 'button', 'data-field-arg': 'changeset.checkout', 'data-field-text': '' },
      { 'data-field-type': 'button', 'data-field-arg': 'changeset.merge', 'data-field-text': '' },
    ],
  )
  assert.equal(
    textIn('Improvements [@button:changeset.checkout]()', KNOWN),
    'Improvements ',
    'the row still reads as itself',
  )
})

let failed = 0
for (const [name, run] of tests) {
  try {
    run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.error(`fail  ${name}`)
    console.error(e instanceof Error ? `      ${e.message}` : e)
  }
}
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
process.exit(failed ? 1 : 0)
