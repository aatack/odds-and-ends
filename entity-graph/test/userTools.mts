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
const { TOOLS_ENTITY_ID, clearUserTools, loadUserTools, userToolsAtom } = await import(
  '../src/renderer/src/tools/userTools'
)
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

/** A tool-shaped note with a `script` body, defined under `@tools`. */
async function defineTool(
  id: string,
  values: Record<string, unknown>,
): Promise<void> {
  for (const [key, v] of Object.entries(values)) await value(id, key, v)
  await link(TOOLS_ENTITY_ID, id)
}

const loaded = (): ReturnType<typeof userToolsAtom.get> => userToolsAtom.get()
const byId = (id: string) => loaded().find((t) => t.id === id)

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
    text: 'Greet someone',
    name: 'greet',
    description: 'Say hello',
    arguments: { type: 'object', properties: { who: { type: 'string' } }, required: ['who'] },
    script: 'tool.toast(`hello ${context.who}`)',
  })
  await loadUserTools()

  const greet = byId('greet')
  assert.ok(greet, 'the tool was not loaded')
  // The note's own text is what the outline shows, so it is what the palette does.
  assert.equal(greet.label, 'Greet someone')
  assert.deepEqual(greet.args?.map((a) => [a.name, a.label, a.kind, a.optional]), [
    ['who', 'Who', 'string', undefined],
  ])
  // Nothing said otherwise, so: the world, and worth keeping in the log.
  assert.equal(greet.scope, 'app')
  assert.equal(greet.reach, 'external')
  assert.equal(greet.keys, undefined)
  assert.equal(greet.mutates, undefined)
})

test('finds a body in a code child when the note has no script of its own', async () => {
  open()
  await defineTool('countUp', { name: 'countUp' })
  await value('countUp.body', 'type', 'code')
  await value('countUp.body', 'text', '1 + 1')
  await link('countUp', 'countUp.body')
  await loadUserTools()

  assert.ok(byId('countUp'), 'a tool whose body is a code child did not load')
})

test('passes over a note under @tools that is not a tool', async () => {
  open()
  // A heading, and a tool missing the one thing there is no default for.
  await defineTool('heading', { text: 'My tools' })
  await defineTool('bodyless', { name: 'bodyless', description: 'nothing to run' })
  await loadUserTools()
  assert.deepEqual(loaded(), [])
})

test('takes a tool with no arguments as complete, not as unfinished', async () => {
  open()
  await defineTool('sync', { name: 'sync', script: 'tool.reloadYourTools()' })
  await loadUserTools()

  const sync = byId('sync')
  assert.ok(sync)
  assert.equal(sync.args, undefined)
})

test('reads the scope, reach, mutation and key a definition asks for', async () => {
  open()
  await defineTool('jump', {
    name: 'jump',
    label: 'Jump about',
    scope: 'frame',
    reach: 'ui',
    mutates: true,
    key: 'mod+shift+j',
    script: 'null',
  })
  await loadUserTools()

  const jump = byId('jump')
  assert.ok(jump)
  // `label` wins over the note's text, and there is none here anyway.
  assert.equal(jump.label, 'Jump about')
  assert.equal(jump.scope, 'frame')
  assert.equal(jump.reach, 'ui')
  assert.equal(jump.mutates, true)
  assert.deepEqual(jump.keys, [{ key: 'j', shift: true, mod: true }])
})

test('ignores a scope or reach that is not one of the app’s', async () => {
  open()
  await defineTool('wonky', { name: 'wonky', scope: 'universe', reach: 'everywhere', script: 'null' })
  await loadUserTools()

  const wonky = byId('wonky')
  assert.ok(wonky)
  assert.equal(wonky.scope, 'app')
  assert.equal(wonky.reach, 'external')
})

test('keeps the outline order, and the first of two tools sharing a name', async () => {
  open()
  await defineTool('first', { name: 'shared', label: 'The first', script: 'null' })
  await defineTool('other', { name: 'other', script: 'null' })
  await defineTool('second', { name: 'shared', label: 'The second', script: 'null' })
  await loadUserTools()

  assert.deepEqual(loaded().map((t) => t.label), ['The first', 'other'])
})

test('is reachable from a script by its name and by its label', async () => {
  open()
  await defineTool('greet', { name: 'greet', label: 'Greet someone', script: 'null' })
  await loadUserTools()

  assert.equal(findToolByName('greet')?.id, 'greet')
  assert.equal(findToolByName('greetSomeone')?.id, 'greet')
})

test('cannot rebind a key one of the app’s own tools already has', async () => {
  const { allTools } = await import('../src/renderer/src/tools/registry')
  open()
  // `d` opens an entity; a definition claiming it must lose, since the router
  // takes the first tool in the registry that binds a key within a scope.
  await defineTool('sneaky', { name: 'sneaky', scope: 'frame', key: 'd', script: 'null' })
  await loadUserTools()

  const binds = allTools().filter((t) => t.scope === 'frame' && t.keys?.some((k) => k.key === 'd'))
  assert.equal(binds[0]?.id, 'entity.open')
})

test('forgets the tools when the source closes', async () => {
  open()
  await defineTool('greet', { name: 'greet', script: 'null' })
  await loadUserTools()
  assert.equal(loaded().length, 1)

  clearUserTools()
  assert.deepEqual(loaded(), [])
})

test('drops a load that lands after the source has moved on', async () => {
  open()
  await defineTool('greet', { name: 'greet', script: 'null' })
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
