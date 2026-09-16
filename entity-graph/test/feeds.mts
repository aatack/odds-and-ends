// The feeds, as far as they can be driven without Slack or GitHub on the line.
//
// What is worth checking here is the one property everything else rests on:
// **reading the same stretch twice writes nothing the second time**. Every
// catch-up rule — winding the cursor back a minute before each request, moving
// it only after the entities are in, stepping a day at a time over a long gap —
// is safe only because of that, so it is the thing to assert rather than the
// rules themselves.
//
// The rest is the small readings a feed does of what a service handed it: which
// hit is a thread reply, what a permalink looks like, how a pile of check runs
// comes out as one word.
//
//   npm test

import assert from 'node:assert/strict'
import { EntityWriter, INBOX_ID } from '../src/main/events/writer'
import { messageId, permalinkFor, threadOf } from '../src/main/events/slack'
import { checksOf, commentId, nextPage, stateOf } from '../src/main/events/github'
import { feedSignature } from '../src/main/events/feeds'
import type { SourceNode } from '../src/core/client'
import { MemorySource } from './source.mjs'

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

/** One entity as the store rolls it up. */
async function entity(store: MemorySource, id: string) {
  const read = (await store.callTool('readEntities', { entityIds: [id] })) as Record<
    string,
    { values: Record<string, unknown>; inboundLinks: string[] }
  >
  return read[id]
}

const LINK = 'https://acme.slack.com/archives/C1/p1712345678000100'

/** The batch a feed would write for one message, twice over the same period. */
const message = (text: string) => [
  { id: 'C1', values: { text: '#general', 'slack/channel': 'C1', 'slack/kind': 'channel' } },
  {
    id: LINK,
    parentId: 'C1',
    values: { text, 'slack/ts': '1712345678.000100', 'slack/permalink': LINK },
  },
]

// --- The writer -------------------------------------------------------------

test('writes an entity, hangs it where it belongs, and files it in the inbox', async () => {
  const store = new MemorySource()
  await new EntityWriter(store, 'slack').write(message('hello'))

  const note = await entity(store, LINK)
  assert.equal(note.values.text, 'hello')
  assert.equal(note.values['slack/permalink'], LINK)
  assert.deepEqual(new Set(note.inboundLinks), new Set(['C1', INBOX_ID]))
  // The channel is made by the first message in it rather than by a listing.
  assert.equal((await entity(store, 'C1')).values.text, '#general')
})

test('writes nothing at all the second time over the same period', async () => {
  const store = new MemorySource()
  await new EntityWriter(store, 'slack').write(message('hello'))
  const after = store.events.length
  assert.ok(after > 0)

  const report = await new EntityWriter(store, 'slack').write(message('hello'))
  assert.equal(store.events.length, after)
  assert.deepEqual(report, { created: 0, touched: 0, events: 0 })
})

test('writes only what changed when a message is edited', async () => {
  const store = new MemorySource()
  const writer = new EntityWriter(store, 'slack')
  await writer.write(message('hello'))
  const before = store.events.length

  await writer.write(message('hello, edited'))
  // One value event, and nothing else: not the channel, not the links, not the
  // three values on the message that are the same as they were.
  assert.equal(store.events.length, before + 1)
  assert.equal((await entity(store, LINK)).values.text, 'hello, edited')
})

test('leaves a note alone once it has been filed somewhere', async () => {
  const store = new MemorySource()
  const writer = new EntityWriter(store, 'slack')
  await writer.write(message('hello'))
  // Read, and moved out of the inbox by hand.
  await store.callTool('writeLink', { sourceId: INBOX_ID, destinationId: LINK, action: 1 })

  await writer.write(message('hello'))
  const note = await entity(store, LINK)
  assert.ok(!note.inboundLinks.includes(INBOX_ID), 'it was dragged back into the inbox')
})

test('leaves a value it knows nothing about rather than blanking it', async () => {
  const store = new MemorySource()
  const writer = new EntityWriter(store, 'github')
  await writer.write([{ id: 'o/r#1', values: { text: 'A pull request', 'github/reason': 'mention' } }])
  // The sweep for pull requests nobody notified us about knows no reason, and
  // says so with `undefined` rather than with null.
  await writer.write([{ id: 'o/r#1', values: { text: 'A pull request', 'github/reason': undefined } }])
  assert.equal((await entity(store, 'o/r#1')).values['github/reason'], 'mention')
})

test('folds an entity named twice in one batch, later winning', async () => {
  const store = new MemorySource()
  await new EntityWriter(store, 'slack').write([
    { id: 'C1:1', parentId: 'C1', values: { text: 'first' } },
    { id: 'C1:1', values: { text: 'second' } },
  ])
  const note = await entity(store, 'C1:1')
  assert.equal(note.values.text, 'second')
  assert.deepEqual(new Set(note.inboundLinks), new Set(['C1', INBOX_ID]))
})

// --- What a service handed over ---------------------------------------------

test('reads a thread reply out of its permalink, since a hit has no thread_ts', async () => {
  assert.equal(
    threadOf('https://acme.slack.com/archives/C1/p1712345679000200?thread_ts=1712345678.000100'),
    '1712345678.000100',
  )
  assert.equal(threadOf('https://acme.slack.com/archives/C1/p1712345678000100'), null)
  assert.equal(threadOf(undefined), null)
})

test('builds a permalink rather than asking for one', async () => {
  assert.equal(permalinkFor('https://acme.slack.com/', 'C1', '1712345678.000100'), LINK)
  assert.equal(
    permalinkFor('https://acme.slack.com', 'C1', '1712345679.000200', '1712345678.000100'),
    'https://acme.slack.com/archives/C1/p1712345679000200?thread_ts=1712345678.000100&cid=C1',
  )
})

test('names a message by its permalink, thread or no thread', async () => {
  const reply = permalinkFor('https://acme.slack.com', 'C1', '1712345679.000200', '1712345678.000100')
  // Learning that a message is a reply must not move it to another entity, so
  // the `?thread_ts=…&cid=…` a reply's link carries is not part of its id.
  assert.equal(messageId(reply), 'https://acme.slack.com/archives/C1/p1712345679000200')
  assert.equal(messageId(LINK), LINK)
})

test('names a comment the way GitHub does, so the two kinds cannot collide', async () => {
  assert.equal(
    commentId('https://github.com/o/r/pull/1#issuecomment-2412345678', 'comment-1'),
    'issuecomment-2412345678',
  )
  assert.equal(
    commentId('https://github.com/o/r/pull/1#discussion_r2412345678', 'comment-1'),
    'discussion_r2412345678',
  )
  assert.equal(commentId(undefined, 'comment-1'), 'comment-1')
})

test('says what a pull request is doing in one word', async () => {
  assert.equal(stateOf({ state: 'closed', merged_at: '2026-01-01T00:00:00Z' }), 'merged')
  assert.equal(stateOf({ state: 'open', draft: true }), 'draft')
  assert.equal(stateOf({ state: 'open' }), 'open')
  assert.equal(stateOf({ state: 'closed' }), 'closed')
})

test('rolls check runs up, with "still going" outranking a failure', async () => {
  assert.equal(checksOf([]), null)
  assert.equal(checksOf([{ status: 'completed', conclusion: 'success' }]), 'passing')
  assert.equal(
    checksOf([
      { status: 'completed', conclusion: 'success' },
      { status: 'completed', conclusion: 'failure' },
    ]),
    'failing',
  )
  assert.equal(
    checksOf([
      { status: 'completed', conclusion: 'failure' },
      { status: 'in_progress', conclusion: null },
    ]),
    'running',
  )
})

test('follows GitHub to the next page, and knows when there isn\'t one', async () => {
  const link =
    '<https://api.github.com/notifications?page=2>; rel="next", ' +
    '<https://api.github.com/notifications?page=9>; rel="last"'
  assert.equal(
    nextPage(new Headers({ link })),
    'https://api.github.com/notifications?page=2',
  )
  assert.equal(nextPage(new Headers({ link: '<https://x>; rel="prev"' })), null)
  assert.equal(nextPage(new Headers()), null)
})

// --- Keeping a feed in step with the drawing --------------------------------

test('a cursor that moved is not a reason to restart the feed that moved it', async () => {
  const node = (cursor: string, userToken = 'xoxp-1'): SourceNode => ({
    id: 'n',
    label: 'Slack',
    x: 0,
    y: 0,
    paused: false,
    config: { kind: 'slackEvents', userToken, appToken: '', cursor, muted: '' },
  })
  assert.equal(feedSignature(node('1')), feedSignature(node('2')))
  assert.notEqual(feedSignature(node('1')), feedSignature(node('1', 'xoxp-2')))
})

// --- Runner -----------------------------------------------------------------

let failed = 0
for (const [name, run] of tests) {
  try {
    await run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.error(`FAIL  ${name}`)
    console.error(e)
  }
}
console.log(failed ? `${failed} failed` : `${tests.length} passed`)
if (failed) process.exit(1)
