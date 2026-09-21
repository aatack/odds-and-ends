// Chats: a note you talk to a tool through, driven headlessly.
//
// Two things are worth asserting and neither is visible from the panel. The
// first is what the tool on the far end is actually called with — the chat's own
// values for what it declares, the message as `text`, and nothing it has never
// heard of, since a tool handed a stray key refuses the call outright. The
// second is who each message belongs to: the app writes both halves of the
// conversation, so without `owner` on the reply both sides would come back
// looking like the user's and the panel would put them on the same edge.
//
//   npm test

import assert from 'node:assert/strict'
import { MemorySource } from './source.mjs'

const store = new Map<string, string>()
Object.defineProperty(globalThis, 'localStorage', {
  value: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => void store.set(k, v),
    removeItem: (k: string) => void store.delete(k),
    clear: () => store.clear(),
  },
})
Object.defineProperty(globalThis, 'window', {
  value: { entityGraph: {}, addEventListener: () => {}, removeEventListener: () => {} },
})

const { setSourceTransport } = await import('../src/renderer/src/source/transport')
const { callToolByName } = await import('../src/renderer/src/tools/call')
const { userToolsAtom } = await import('../src/renderer/src/tools/userTools')
const { entitiesAtom } = await import('../src/core/cache')
const R = await import('../src/renderer/src/state/reducers')
const { defaultLayout, tabChats } = await import('../src/renderer/src/state/types')
const { focusOf } = await import('../src/renderer/src/state/store')
const type = await import('../src/renderer/src/tools/types')

// --- Harness ----------------------------------------------------------------

let source: MemorySource
/** Every argument object the chat's tool was called with, in order. */
let heard: Record<string, unknown>[]

function open(): void {
  source = new MemorySource()
  heard = []
  entitiesAtom.set({})
  userToolsAtom.set([])
  // The same name on both sides, as in the app: the store's default author and
  // the one the transport reports are set from the one `user` setting, which is
  // why an entity the app creates comes back authored by whoever is signed in.
  setSourceTransport({ call: (t, a) => source.call(t, a), user: 'test', sourceId: 'memory' })
}

const context = () => ({
  values: {},
  path: [],
  groupId: null,
  tabId: null,
  frameId: null,
  startedAt: Date.now(),
})

const value = (entityId: string, key: string, v: unknown): Promise<unknown> =>
  source.call('writeValue', { entityId, key, value: v, author: 'test', timestamp: Date.now() })

/**
 * A tool for the chat to talk to, put in the registry the way the user's own
 * tools arrive: declared from outside, trailing the built-ins. `reply` is what it
 * hands back, which is what the chat writes down as the answer.
 */
function correspondent(
  id: string,
  args: type.ArgSpec[],
  reply: (a: Record<string, unknown>) => unknown,
): void {
  userToolsAtom.set([
    {
      id,
      label: id,
      scope: 'app',
      reach: 'external',
      args,
      run: (a) => {
        heard.push(a)
        return { data: reply(a) }
      },
    },
  ])
}

/** A chat note carrying `values`, with nothing under it yet. */
async function chat(id: string, values: Record<string, unknown>): Promise<void> {
  for (const [key, v] of Object.entries({ type: 'chat', ...values })) await value(id, key, v)
}

/** The chat's children, rolled up, in the order they were said. */
async function messages(id: string): Promise<{ text: string; owner?: string; by: string }[]> {
  const chatEntity = (await source.call('readEntities', { entityIds: [id] })) as Record<
    string,
    { outboundLinks: string[] }
  >
  const ids = chatEntity[id].outboundLinks
  const entities = (await source.call('readEntities', { entityIds: ids })) as Record<
    string,
    { values: Record<string, unknown>; createdBy: string }
  >
  return ids.map((childId) => ({
    text: String(entities[childId].values.text ?? ''),
    owner: entities[childId].values.owner as string | undefined,
    by: entities[childId].createdBy,
  }))
}

const send = (chatId: string, text: string): Promise<unknown> =>
  callToolByName('chat.send', [{ chatId, text }], context())

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

// --- Sending ----------------------------------------------------------------

test('writes what was said, then what was answered', async () => {
  open()
  correspondent('echo', [{ name: 'text', label: 'Text' }], (a) => `You said: ${a.text}`)
  await chat('c', { text: 'Echo', chat: 'echo' })

  await send('c', 'hello')

  const said = await messages('c')
  assert.deepEqual(
    said.map((m) => m.text),
    ['hello', 'You said: hello'],
  )
  // Both notes are written by this window, so the author alone cannot tell them
  // apart — which is the whole reason the reply carries an owner.
  assert.equal(said[0].by, 'test')
  assert.equal(said[1].by, 'test')
  assert.equal(said[0].owner, undefined)
  assert.equal(said[1].owner, 'echo')
})

test("passes the chat's values for what the tool declares, and nothing else", async () => {
  open()
  correspondent(
    'ask',
    [
      { name: 'text', label: 'Text' },
      { name: 'repo', label: 'Repo' },
    ],
    () => 'fine',
  )
  // `secret` is declared by nobody; `text` is the chat's *title*, which is not
  // something anybody meant to send.
  await chat('c', { text: 'Work', chat: 'ask', repo: '~/repos/thing', secret: 'no' })

  await send('c', 'what changed?')

  assert.equal(heard.length, 1)
  assert.deepEqual(heard[0], { text: 'what changed?', repo: '~/repos/thing' })
})

test('a chat naming no tool refuses rather than writing anything', async () => {
  open()
  await chat('c', { text: 'Nobody' })
  await assert.rejects(() => send('c', 'anyone there?'), /no `chat` value/)
  assert.deepEqual(await messages('c'), [])
})

test('a chat naming a tool that does not exist refuses too', async () => {
  open()
  await chat('c', { text: 'Ghost', chat: 'nosuchtool' })
  await assert.rejects(() => send('c', 'hello'), /No tool called "nosuchtool"/)
  assert.deepEqual(await messages('c'), [])
})

test('an empty message is refused before anything is written', async () => {
  open()
  correspondent('echo', [{ name: 'text', label: 'Text' }], () => 'hi')
  await chat('c', { text: 'Echo', chat: 'echo' })
  await assert.rejects(() => send('c', '   '), /Nothing to send/)
  assert.deepEqual(await messages('c'), [])
})

test('a tool that answers with nothing leaves only what was said', async () => {
  open()
  correspondent('quiet', [{ name: 'text', label: 'Text' }], () => undefined)
  await chat('c', { text: 'Quiet', chat: 'quiet' })

  await send('c', 'hello?')

  // The message still went, so it is still in the conversation; a blank note
  // beneath it would read as a reply that failed to render.
  assert.deepEqual(
    (await messages('c')).map((m) => m.text),
    ['hello?'],
  )
})

test('a tool that answers with an object writes the JSON it is', async () => {
  open()
  correspondent('lookup', [{ name: 'text', label: 'Text' }], () => ({ found: 2 }))
  await chat('c', { text: 'Lookup', chat: 'lookup' })

  await send('c', 'anything?')

  const said = await messages('c')
  assert.match(said[1].text, /^```json\n/)
  assert.match(said[1].text, /"found": 2/)
})

// --- The tab's list ---------------------------------------------------------

test('a tab carries chats, opens the one just added, and lets it go', async () => {
  const layout = defaultLayout()
  const tabId = focusOf(layout).tabId!

  const added = R.addChat(layout, tabId, 'c')
  assert.deepEqual(tabChats(added.tabs[tabId]), ['c'])
  // Adding one is always in order to say something, so it opens with it.
  assert.equal(added.tabs[tabId].openChatId, 'c')

  // A second add of the same chat is an open, not a duplicate.
  const again = R.addChat(R.openChat(added, tabId, null), tabId, 'c')
  assert.deepEqual(tabChats(again.tabs[tabId]), ['c'])
  assert.equal(again.tabs[tabId].openChatId, 'c')

  const gone = R.removeChat(again, tabId, 'c')
  assert.deepEqual(tabChats(gone.tabs[tabId]), [])
  // The panel goes with it rather than being left pointing at nothing.
  assert.equal(gone.tabs[tabId].openChatId, null)
})

test('a tab persisted before chats existed has none, and takes one', async () => {
  const layout = defaultLayout()
  const tabId = focusOf(layout).tabId!
  // Exactly the shape an older layout blob rolls up as.
  delete (layout.tabs[tabId] as { chats?: string[] }).chats
  assert.deepEqual(tabChats(layout.tabs[tabId]), [])
  assert.deepEqual(tabChats(R.addChat(layout, tabId, 'c').tabs[tabId]), ['c'])
})

// --- Run --------------------------------------------------------------------

let failed = 0
for (const [name, run] of tests) {
  try {
    await run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.log(`FAIL  ${name}`)
    console.log(`      ${e instanceof Error ? e.message : String(e)}`)
  }
}
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
if (failed) process.exit(1)
