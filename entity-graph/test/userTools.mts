// Drives the tools the *store* defines, with no browser and no sandbox: notes are
// written into an in-memory source through the real `writeValue`, and then read
// back out as `ToolSpec`s the palette could list.
//
// What it can't check is a body actually running — that needs the QuickJS worker,
// and so a window. Everything up to the point of running is here: which notes
// count as tools, where a body is found, and what each field falls back to when
// the definition doesn't say.
//
//   npm test

import assert from 'node:assert/strict'
import { MemorySource } from './source.mjs'

// --- The browser bits the tool layer expects --------------------------------

const store = new Map<string, string>()
Object.defineProperty(globalThis, 'localStorage', {
  value: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => void store.set(k, v),
    removeItem: (k: string) => void store.delete(k),
    clear: () => store.clear(),
  },
})

// The registry reaches the preload bridge — the entity tools copy files through
// it — and reads it as they load. Stubbed rather than avoided: the point of the
// last two tests is what the *whole* registry does with a definition in it, and
// this is the one seam between the tool layer and Electron.
Object.defineProperty(globalThis, 'window', {
  value: { entityGraph: {}, addEventListener: () => {}, removeEventListener: () => {} },
})

// Imported after the stub: the persistent atoms read localStorage as they load.
const { setSourceTransport } = await import('../src/renderer/src/source/transport')
const { TOOLS_ENTITY_ID, appliedSource, clearUserTools, loadUserTools, userToolsAtom } =
  await import('../src/renderer/src/tools/userTools')
const { findToolByName } = await import('../src/renderer/src/tools/registry')

// --- Harness ----------------------------------------------------------------

let source: MemorySource

function open(): void {
  source = new MemorySource()
  clearUserTools()
  setSourceTransport({ call: (tool, args) => source.call(tool, args), user: 'test', sourceId: 'memory' })
}

/** Write one value onto a note, the way the app's own writes do. */
const value = (entityId: string, key: string, v: unknown): Promise<unknown> =>
  source.call('writeValue', { entityId, key, value: v, author: 'test', timestamp: Date.now() })

/** Hang `childId` under `parentId`. Order of these calls is the outline order. */
const link = (parentId: string, childId: string): Promise<unknown> =>
  source.call('writeLink', {
    sourceId: parentId,
    destinationId: childId,
    action: 0,
    author: 'test',
    timestamp: Date.now(),
  })

/**
 * A note carrying the given values, defined under `@tools`. `type: tool` is what
 * makes one a definition at all, so it is stamped on unless the case being tested
 * is about the type itself and says otherwise.
 */
async function defineTool(
  id: string,
  values: Record<string, unknown>,
): Promise<void> {
  for (const [key, v] of Object.entries({ type: 'tool', ...values })) await value(id, key, v)
  await link(TOOLS_ENTITY_ID, id)
}

const loaded = (): ReturnType<typeof userToolsAtom.get> => userToolsAtom.get()
const byId = (id: string) => loaded().find((t) => t.id === id)

/**
 * The source a loaded tool would hand the sandbox. Running it needs a window, so
 * what an `execute` becomes is checked instead — built from the arguments the
 * definition actually produced, rather than from ones written out again here.
 */
const sourceOf = (id: string, execute: string): string =>
  appliedSource(execute, byId(id)?.args ?? [])

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

// --- Tests ------------------------------------------------------------------

test('has nothing to load from a store with no @tools entity', async () => {
  open()
  await loadUserTools()
  assert.deepEqual(loaded(), [])
})

test('turns a note naming itself and a body into a tool', async () => {
  open()
  await defineTool('greet', {
    // The outline's line and the palette's label are one and the same.
    text: 'greet',
    description: 'Say hello',
    arguments: [{ name: 'who', type: 'string', required: true }],
    execute: '(who) => tool.setEntityValue(context.entityId, "text", `hello ${who}`)',
  })
  await loadUserTools()

  const greet = byId('greet')
  assert.ok(greet, 'the tool was not loaded')
  assert.equal(greet.label, 'greet')
  assert.deepEqual(greet.args?.map((a) => [a.name, a.label, a.kind, a.optional]), [
    ['who', 'Who', 'string', undefined],
  ])
  // Nothing said otherwise, so: the world, and worth keeping in the log.
  assert.equal(greet.scope, 'app')
  assert.equal(greet.reach, 'external')
  assert.equal(greet.keys, undefined)
  assert.equal(greet.mutates, undefined)
})

test('takes `execute` as the body, and `script` as the older shape', async () => {
  open()
  await defineTool('fromExecute', { text: 'fromExecute', execute: '(a, b) => a + b' })
  await defineTool('fromScript', { text: 'fromScript', script: 'context.a + context.b' })
  await loadUserTools()

  assert.ok(byId('fromExecute'), 'a tool with an `execute` did not load')
  assert.ok(byId('fromScript'), 'a tool with a `script` did not load')
})

test('does not look for a body in a code child any more', async () => {
  open()
  await defineTool('countUp', { text: 'countUp' })
  await value('countUp.body', 'type', 'code')
  await value('countUp.body', 'text', '1 + 1')
  await link('countUp', 'countUp.body')
  await loadUserTools()

  assert.deepEqual(loaded(), [])
})

test('applies `execute` to its arguments in the order they were declared', async () => {
  open()
  await defineTool('add', {
    text: 'add',
    execute: '(first, second) => { return first + second } // trailing comment',
    arguments: [
      { name: 'first', type: 'number', required: true },
      { name: 'second', type: 'number' },
    ],
  })
  await loadUserTools()

  // The sandbox needs a window, so what is checked here is the source the tool
  // would run: the expression, applied to its arguments by name, in order — and
  // surviving a comment on its last line, which is what the newlines are for.
  const source = sourceOf('add', '(first, second) => { return first + second } // trailing comment')
  assert.match(source, /^const __tool = \(\n\(first, second\)/)
  assert.match(source, /\n\)\n/)
  assert.ok(source.endsWith('__tool(context.args["first"], context.args["second"])'), source)
})

test('refuses an `execute` that is not a function, in the sandbox rather than here', async () => {
  open()
  await defineTool('notAFunction', { text: 'notAFunction', execute: '42' })
  await loadUserTools()

  // Loading it is fine — whether an expression evaluates to a function is not
  // knowable until it is evaluated, so the check travels with the source.
  assert.ok(byId('notAFunction'))
  assert.match(sourceOf('notAFunction', '42'), /must be an expression that evaluates to a function/)
})

test('quotes an argument name rather than writing it as an identifier', async () => {
  open()
  await defineTool('odd', {
    text: 'odd',
    execute: '(x) => x',
    arguments: [{ name: 'not an identifier', type: 'string' }],
  })
  await loadUserTools()

  assert.ok(sourceOf('odd', '(x) => x').endsWith('__tool(context.args["not an identifier"])'))
})

test('passes over a note under @tools that is not a tool, and says why', async () => {
  open()
  // A heading, a definition with nothing to call it by, and one missing the body
  // there is no default for.
  await defineTool('heading', { type: null, text: 'My tools' })
  await defineTool('nameless', { text: null, execute: '() => null' })
  await defineTool('bodyless', { text: 'bodyless', description: 'nothing to run' })
  const found = await loadUserTools()

  assert.deepEqual(loaded(), [])
  assert.equal(found.linked, 3)
  assert.deepEqual(found.skipped, [
    { id: 'heading', why: 'not `type: tool`' },
    { id: 'nameless', why: 'no `text` to call it by' },
    { id: 'bodyless', why: 'no `execute`' },
  ])
})

test('says when nothing is linked under @tools at all', async () => {
  open()
  const found = await loadUserTools()
  assert.equal(found.linked, 0)
  assert.deepEqual(found.skipped, [])
})

test('says which of two tools sharing an id was passed over', async () => {
  open()
  await defineTool('first', { text: 'shared', execute: '() => null' })
  await defineTool('second', { text: 'shared', execute: '() => null' })
  const found = await loadUserTools()

  assert.deepEqual(found.tools.map((t) => t.id), ['shared'])
  assert.deepEqual(found.skipped, [
    { id: 'second', why: 'another tool already answers to shared' },
  ])
})

test('takes its arguments as a list, in the order the list gives them', async () => {
  open()
  await defineTool('post', {
    text: 'post',
    execute: '() => null',
    arguments: [
      { name: 'channel', type: 'string', required: true },
      { name: 'times', type: 'number' },
      { name: 'dryRun', type: 'boolean' },
      // "Empty for JSON" — and an absent type means the same thing.
      { name: 'payload', type: '' },
      { name: 'anything' },
    ],
  })
  await loadUserTools()

  assert.deepEqual(byId('post')?.args?.map((a) => [a.name, a.kind, a.optional ?? false]), [
    ['channel', 'string', false],
    ['times', 'number', true],
    ['dryRun', 'boolean', true],
    ['payload', 'json', true],
    ['anything', 'json', true],
  ])
})

test('reads the rest of what a listed argument can say', async () => {
  open()
  await defineTool('post', {
    text: 'post',
    execute: '() => null',
    arguments: [
      { name: 'target', type: 'entity', required: true },
      { name: 'tone', options: ['formal', 'casual'] },
      { name: 'note', type: 'string', description: 'Shown on hover, not in the field' },
      { name: 'times', type: 'integer', default: 1 },
    ],
  })
  await loadUserTools()
  const args = byId('post')?.args ?? []

  // An entity id is a string to anything outside the app, and a picker within it.
  assert.equal(args[0].kind, 'entity')
  assert.equal(args[1].kind, 'select')
  assert.deepEqual(args[1].options, ['formal', 'casual'])
  // The field itself says which argument it wants; the description is the tooltip.
  assert.equal(args[2].label, 'Note')
  assert.equal(args[2].placeholder, undefined)
  assert.equal(args[2].description, 'Shown on hover, not in the field')
  assert.equal(args[3].kind, 'number')
  assert.equal(args[3].hasDefault, true)
})

test('names an argument with nothing but a string', async () => {
  open()
  await defineTool('post', { text: 'post', execute: '() => null', arguments: ['who', 'what'] })
  await loadUserTools()

  assert.deepEqual(byId('post')?.args?.map((a) => [a.name, a.label, a.kind]), [
    ['who', 'Who', 'json'],
    ['what', 'What', 'json'],
  ])
})

test('passes over a listed argument that names nothing, and repeats of a name', async () => {
  open()
  await defineTool('post', {
    text: 'post',
    execute: '() => null',
    arguments: [
      { type: 'string' },
      { name: 'who', type: 'string', required: true },
      { name: 'who', type: 'number' },
      '',
    ],
  })
  await loadUserTools()

  assert.deepEqual(byId('post')?.args?.map((a) => [a.name, a.kind]), [['who', 'string']])
})

test('reads an argument list that was written as text, and says it had to', async () => {
  open()
  // Exactly what a value editor that keeps a string a string leaves behind. The
  // symptom without this is the worst kind: the tool loads, asks nothing, and
  // calls its body with undefined for every parameter.
  await defineTool('post', {
    text: 'post',
    execute: '(who) => who',
    arguments: '[{"type":"string", "name":"who", "required": true}]',
  })
  const found = await loadUserTools()

  assert.deepEqual(byId('post')?.args?.map((a) => [a.name, a.kind, a.optional ?? false]), [
    ['who', 'string', false],
  ])
  assert.deepEqual(found.warnings, [])
})

test('loads a tool whose arguments cannot be read at all, and warns about it', async () => {
  open()
  await defineTool('post', { text: 'post', execute: '() => null', arguments: 'who, what' })
  const found = await loadUserTools()

  assert.ok(byId('post'), 'the tool should still load — it just takes nothing')
  assert.equal(byId('post')?.args, undefined)
  assert.deepEqual(found.warnings, [
    { id: 'post', why: '`arguments` is not a list, so it takes none' },
  ])
})

test('says nothing about arguments that were simply never written', async () => {
  open()
  await defineTool('bare', { text: 'bare', execute: '() => null' })
  await defineTool('cleared', { text: 'cleared', execute: '() => null', arguments: null })
  await defineTool('blank', { text: 'blank', execute: '() => null', arguments: '' })
  const found = await loadUserTools()

  assert.equal(found.tools.length, 3)
  assert.deepEqual(found.warnings, [])
})

test('still takes a JSON Schema written out in full', async () => {
  open()
  await defineTool('post', {
    text: 'post',
    execute: '() => null',
    arguments: { type: 'object', properties: { who: { type: 'string' } }, required: ['who'] },
  })
  await loadUserTools()

  assert.deepEqual(byId('post')?.args?.map((a) => [a.name, a.kind, a.optional ?? false]), [
    ['who', 'string', false],
  ])
})

test('takes a tool with no arguments as complete, not as unfinished', async () => {
  open()
  await defineTool('sync', { text: 'sync', execute: '() => tool.reloadYourTools()' })
  await loadUserTools()

  const sync = byId('sync')
  assert.ok(sync)
  assert.equal(sync.args, undefined)
})

test('takes its name from its text and its id from `id`', async () => {
  open()
  await defineTool('n1', {
    label: 'not read',
    text: 'Greet someone',
    id: 'greet',
    execute: '() => null',
  })
  await loadUserTools()

  const greet = byId('greet')
  assert.ok(greet, 'the tool should be found by its `id`')
  assert.equal(greet.label, 'Greet someone')
  // Reachable from a script by the id, and by the camel case of the name.
  assert.equal(findToolByName('greet')?.id, 'greet')
  assert.equal(findToolByName('greetSomeone')?.id, 'greet')
})

test('falls back to the text when a definition asks for no id', async () => {
  open()
  await defineTool('n1', { text: 'greet', execute: '() => null' })
  await loadUserTools()

  assert.deepEqual(loaded().map((t) => [t.id, t.label]), [['greet', 'greet']])
})

test('reads the scope, reach, mutation and key a definition asks for', async () => {
  open()
  await defineTool('jump', {
    text: 'jump',
    scope: 'frame',
    reach: 'ui',
    mutates: true,
    key: 'mod+shift+j',
    execute: '() => null',
  })
  await loadUserTools()

  const jump = byId('jump')
  assert.ok(jump)
  assert.equal(jump.scope, 'frame')
  assert.equal(jump.reach, 'ui')
  assert.equal(jump.mutates, true)
  assert.deepEqual(jump.keys, [{ key: 'j', shift: true, mod: true }])
})

test('ignores a scope or reach that is not one of the app’s', async () => {
  open()
  await defineTool('wonky', { text: 'wonky', scope: 'universe', reach: 'everywhere', execute: '() => null' })
  await loadUserTools()

  const wonky = byId('wonky')
  assert.ok(wonky)
  assert.equal(wonky.scope, 'app')
  assert.equal(wonky.reach, 'external')
})

test('keeps the outline order, and the first of two tools sharing an id', async () => {
  open()
  await defineTool('first', { text: 'The first', id: 'shared', execute: '() => null' })
  await defineTool('other', { text: 'other', execute: '() => null' })
  await defineTool('second', { text: 'The second', id: 'shared', execute: '() => null' })
  await loadUserTools()

  assert.deepEqual(loaded().map((t) => t.label), ['The first', 'other'])
})

test('cannot rebind a key one of the app’s own tools already has', async () => {
  const { allTools } = await import('../src/renderer/src/tools/registry')
  open()
  // `d` opens an entity; a definition claiming it must lose, since the router
  // takes the first tool in the registry that binds a key within a scope.
  await defineTool('sneaky', { text: 'sneaky', scope: 'frame', key: 'd', execute: '() => null' })
  await loadUserTools()

  const binds = allTools().filter((t) => t.scope === 'frame' && t.keys?.some((k) => k.key === 'd'))
  assert.equal(binds[0]?.id, 'entity.open')
})

test('forgets the tools when the source closes', async () => {
  open()
  await defineTool('greet', { text: 'greet', execute: '() => null' })
  await loadUserTools()
  assert.equal(loaded().length, 1)

  clearUserTools()
  assert.deepEqual(loaded(), [])
})

test('drops a load that lands after the source has moved on', async () => {
  open()
  await defineTool('greet', { text: 'greet', execute: '() => null' })
  const pending = loadUserTools()
  // The source changes under it, exactly as it does when another is opened.
  setSourceTransport({ call: (tool, args) => source.call(tool, args), user: 'test', sourceId: 'elsewhere' })
  await pending
  assert.deepEqual(loaded(), [])
})

// --- Run --------------------------------------------------------------------

let failed = 0
for (const [name, run] of tests) {
  try {
    await run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.error(`fail  ${name}`)
    console.error(e instanceof Error ? `      ${e.message}` : e)
  }
}
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
process.exit(failed ? 1 : 0)
