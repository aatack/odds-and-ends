import assert from 'node:assert/strict'
import { mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { DatabaseSync } from 'node:sqlite'
import { test } from 'node:test'
import { Core } from './core.ts'
import { EntityCache } from './graph/cache.ts'
import { link, value, values } from './graph/events.ts'
import { focusOf, foreignOf, viewOf } from './present.ts'
import { fakeSlack, memoryCore } from './testing.ts'

const auth = {
  'auth.test': () => ({ user_id: 'UME', url: 'https://x.slack.com/' }),
  'users.conversations': () => ({ channels: [] }),
  'conversations.list': () => ({ channels: [] }),
  'users.list': () => ({ members: [] }),
  'search.messages': () => ({ messages: { matches: [], paging: { pages: 1 } } }),
}

const searchAnswer = (matches: unknown[]) => () => ({ messages: { matches, paging: { pages: 1 } } })
const permalink = (channel: string, ts: string, thread?: string) =>
  `https://x.slack.com/archives/${channel}/p${ts.replace('.', '')}${thread ? `?thread_ts=${thread}` : ''}`

test('both stores read as one: a later event wins, and mine win a tie', () => {
  const core = memoryCore()
  core.cache.write(values('slack:conv:C1', { type: 'slack.conversation', channel: 'C1', kind: 'channel', name: 'general' }, 0, 'slack'))
  assert.equal(core.focus('slack:conv:C1').entity!.data.title, '#general')
  // Renamed for me only: an owned event, later than the cached name.
  core.owned.write([value('slack:conv:C1', 'name', 'mine', 5, 'me')])
  assert.equal(core.focus('slack:conv:C1').entity!.data.title, '#mine')
  // The cache saying it again changes nothing; clearing it keeps what is mine.
  core.cache.write([value('slack:conv:C1', 'name', 'general', 0, 'slack')])
  assert.equal(core.focus('slack:conv:C1').entity!.data.title, '#mine')
  core.clearCache()
  assert.equal(core.item('slack:conv:C1')!.data.name, 'mine')
  assert.equal(core.item('slack:conv:C1')!.data.kind, undefined)
})

test('the cache keeps one event per value and link, and a list that lost an item drops its link', () => {
  const core = memoryCore()
  core.cache.write([link('p', 'a', 0, 'x'), link('p', 'b', 0, 'x'), value('a', 'n', 1, 0, 'x')])
  core.cache.write([value('a', 'n', 2, 0, 'x')])
  core.owned.write([link('p', 'mine', 1, 'me')])
  core.cache.write([link('p', 'b', 0, 'x'), link('p', 'c', 0, 'x')], { replaceLinksFrom: ['p'] })
  assert.deepEqual(core.cache.read(['a']).filter((e) => e.type === 'value').length, 1)
  assert.deepEqual(core.entity('p').outboundLinks, ['b', 'c', 'mine'])
})

test('a fetched link can be hidden for me only by an owned unlink after it', () => {
  const core = memoryCore()
  core.cache.write([link('github:pr:https://github.com/o/r/pull/1', 'x', 1000, 'github')])
  core.actions.unlink({ parent: 'github:pr:https://github.com/o/r/pull/1', child: 'x' })
  assert.deepEqual(core.entity('github:pr:https://github.com/o/r/pull/1').outboundLinks, [])
})

test('notes: Enter makes an untyped owned note under anything; e overrides text for me only; x moves; r links', () => {
  const core = memoryCore()
  core.cache.write([
    ...values('slack:conv:C1', { type: 'slack.conversation', channel: 'C1', kind: 'channel', name: 'general' }, 0, 'slack'),
    ...values('slack:msg:C1:1.0', { type: 'slack.message', channel: 'C1', ts: '1.0', text: 'from slack' }, 1000, 'slack'),
    link('slack:conv:C1', 'slack:msg:C1:1.0', 1000, 'slack'),
  ])
  const made = core.actions.create({ parent: 'slack:msg:C1:1.0', text: 'remember this' })
  const note = made.events.find((e) => e.type === 'link')!.type === 'link' ? (made.events.find((e) => e.type === 'link') as { destinationId: string }).destinationId : ''
  assert.equal(core.item(note)!.type, 'note')
  assert.equal(core.owned.read([note]).length, 2)
  assert.equal(core.cache.read([note]).length, 0)

  // A note under a message nests in the conversation's view (Slack rows start open), and folds away.
  const open = core.actions.view({ id: 'slack:conv:C1' })
  assert.deepEqual(open.rows.map((row) => [row.depth, row.entity.data.text]), [[0, '#general'], [1, 'from slack'], [2, 'remember this']])
  const shut = core.actions.view({ id: 'slack:conv:C1', folds: { 'slack:msg:C1:1.0': false } })
  assert.deepEqual(shut.rows.map((row) => [row.depth, row.entity.data.text]), [[0, '#general'], [1, 'from slack']])
  assert.equal(shut.rows[1].hasChildren, true)

  // e: my text wins over what Slack said, and a reload of Slack's doesn't undo it.
  core.actions.setText({ id: 'slack:conv:C1', text: 'my name for it' })
  core.actions.setText({ id: 'slack:msg:C1:1.0', text: 'edited for me' })
  core.cache.write([value('slack:conv:C1', 'name', 'general', 0, 'slack'), value('slack:msg:C1:1.0', 'text', 'from slack', 1000, 'slack')])
  assert.equal(core.focus('slack:conv:C1').entity!.data.text, 'my name for it')
  assert.equal(core.focus('slack:conv:C1').children[0].data.text, 'edited for me')

  // x: move the note to the conversation. r: link it back under the message too.
  core.actions.move({ child: note, from: 'slack:msg:C1:1.0', to: 'slack:conv:C1' })
  assert.deepEqual(core.entity('slack:msg:C1:1.0').outboundLinks, [])
  assert.ok(core.entity('slack:conv:C1').outboundLinks.includes(note))
  core.actions.link({ parent: 'slack:msg:C1:1.0', child: note })
  assert.deepEqual(core.entity('slack:msg:C1:1.0').outboundLinks, [note])
  // Backspace: out of one parent only.
  core.actions.unlink({ parent: 'slack:msg:C1:1.0', child: note })
  assert.deepEqual(core.entity('slack:msg:C1:1.0').outboundLinks, [])
  assert.ok(core.entity('slack:conv:C1').outboundLinks.includes(note))
})

test('slack: read-only refuses writes before they reach the network', async () => {
  const called: string[] = []
  const core = memoryCore({
    fetch: (async (input: string | URL | Request) => {
      called.push(String(input).split('/').pop()!)
      return new Response(JSON.stringify({ ok: true, user_id: 'UME', url: '', channels: [] }))
    }) as typeof fetch,
  })
  await core.slack.setToken('xoxp-1')
  core.cache.write([
    ...values('slack:conv:C1', { channel: 'C1', kind: 'channel' }, 0, 'slack'),
    ...values('slack:msg:C1:1.0', { channel: 'C1', ts: '1.0', text: 'hi' }, 1000, 'slack'),
    value('slack:conv:C1', 'lastMessageReceived', '1.0', 0, 'slack'),
    link('slack:conv:C1', 'slack:msg:C1:1.0', 1000, 'slack'),
  ])
  assert.equal(core.focus('slack:conv:C1').compose, null)
  const marked = await core.actions.markRead({ id: 'slack:conv:C1' })
  await core.actions.submit({ id: 'slack:conv:C1', text: 'oops' })
  assert.ok(!called.includes('chat.postMessage'))
  // Marked read here only: Slack is never asked.
  assert.ok(!called.includes('conversations.mark'))
  assert.equal(marked.error, null)
  assert.equal(core.item('slack:conv:C1')!.data.lastRead, '1.0')
  assert.match(core.focus('slack:conv:C1').error ?? '', /read-only/)
})

test('slack: messages carry markdown, and the author links to my DM with them', () => {
  const core = memoryCore()
  core.cache.write([
    ...values('slack:user:U1', { name: 'Sam', dm: 'slack:conv:D1' }, 0, 'slack'),
    ...values('slack:user:U2', { name: 'Priya' }, 0, 'slack'),
    ...values('slack:conv:C1', { channel: 'C1', kind: 'channel' }, 0, 'slack'),
    ...values('slack:msg:C1:1.0', { channel: 'C1', ts: '1.0', user: 'U1', text: '*hi* <@U2> &amp; co' }, 1000, 'slack'),
    link('slack:conv:C1', 'slack:msg:C1:1.0', 1000, 'slack'),
  ])
  const [message] = core.focus('slack:conv:C1').children
  assert.equal(message.data.markdown, '**hi** [Priya](mention:U2/) & co')
  assert.equal(message.data.authorTarget, 'slack:conv:D1')
})

test('slack: images download once with the token, then come from the cache', async () => {
  const seen: { url: string; auth: string | null }[] = []
  const core = memoryCore({
    fetch: (async (input: string | URL | Request, init?: RequestInit) => {
      const url = String(input)
      if (url.startsWith('https://files.slack.com/')) {
        seen.push({ url, auth: new Headers(init?.headers).get('authorization') })
        return new Response(new Uint8Array([1, 2, 3]), { headers: { 'content-type': 'image/png' } })
      }
      return new Response(JSON.stringify({ ok: true, user_id: 'UME', url: '', channels: [] }))
    }) as typeof fetch,
  })
  await core.slack.setToken('xoxp-1')
  const image = { id: 'F1', name: 'a.png', full: 'https://files.slack.com/a.png', thumb: 'https://files.slack.com/a_480.png' }
  core.cache.write(values('slack:msg:C1:1.0', { channel: 'C1', ts: '1.0', text: '', images: [image] }, 1000, 'slack'))
  core.cache.write(values('slack:conv:C1', { channel: 'C1' }, 0, 'slack'))
  core.cache.write([link('slack:conv:C1', 'slack:msg:C1:1.0', 1000, 'slack')])
  const presented = core.focus('slack:conv:C1').children[0].data.images as { thumb: string }[]
  const first = await core.actions.slackImage({ ref: presented[0].thumb })
  await core.actions.slackImage({ ref: presented[0].thumb })
  assert.deepEqual([...first.data], [1, 2, 3])
  assert.deepEqual(seen, [{ url: image.thumb, auth: 'Bearer xoxp-1' }])
})

test('slack: the token is never sent to a host outside Slack', async () => {
  const urls: string[] = []
  const core = memoryCore({
    fetch: (async (input: string | URL | Request) => {
      urls.push(String(input))
      return new Response(JSON.stringify({ ok: true, user_id: 'UME', url: '', channels: [] }))
    }) as typeof fetch,
  })
  await core.slack.setToken('xoxp-1')
  const image = { id: 'F1', name: 'a.png', full: 'https://evil.example/a.png', thumb: 'https://evil.example/a.png' }
  core.cache.write(values('slack:msg:C1:1.0', { channel: 'C1', ts: '1.0', text: '', images: [image] }, 1000, 'slack'))
  await assert.rejects(core.actions.slackImage({ ref: 'thumb/slack:msg:C1:1.0/F1' }), /refusing/)
  assert.ok(!urls.some((url) => url.includes('evil')))
})

test('the first database is imported once, read-only, and left as it was', () => {
  const dir = mkdtempSync(join(tmpdir(), 'detail-views-'))
  const legacy = join(dir, 'detail-views.sqlite')
  const old = new DatabaseSync(legacy)
  old.exec(`
    CREATE TABLE entities (id TEXT PRIMARY KEY, type TEXT, data TEXT, created_at INTEGER, updated_at INTEGER, expires_at INTEGER);
    CREATE TABLE links (parent TEXT, child TEXT, rank REAL, created_at INTEGER, expires_at INTEGER);
    CREATE TABLE settings (key TEXT PRIMARY KEY, value TEXT);
    INSERT INTO entities VALUES ('tasks', 'tasks.home', '{}', 1, 1, NULL);
    INSERT INTO entities VALUES ('t1', 'task', '{"text":"keep","done":true}', 10, 20, NULL);
    INSERT INTO entities VALUES ('slack:conv:C1', 'slack.conversation', '{}', 10, 20, 99);
    INSERT INTO links VALUES ('tasks', 't1', 1, 10, NULL);
    INSERT INTO settings VALUES ('slack.token', 'xoxp-old');
  `)
  old.close()
  const before = new DatabaseSync(legacy).prepare('SELECT count(*) AS n FROM entities').get()
  const options = { owned: join(dir, 'owned.sqlite'), cache: join(dir, 'cache.sqlite'), legacy }
  memoryCore(options)
  const core = new Core(options)
  assert.deepEqual(core.focus('tasks').children.map((child) => [child.data.text, child.data.done]), [['keep', true]])
  // The old token moved onto Slack's own entity, and out of the settings table.
  assert.equal(core.entity('slack').values['secret.token'], 'xoxp-old')
  assert.equal(core.settings.get('slack.token'), null)
  assert.equal(core.owned.read(['t1']).length, 4)
  assert.equal(core.item('slack:conv:C1')?.data.kind, undefined)
  assert.deepEqual(new DatabaseSync(legacy).prepare('SELECT count(*) AS n FROM entities').get(), before)
})

test('slack: no token asks for one, and a token is checked before it is kept', async () => {
  const core = memoryCore({ fetch: fakeSlack(auth) })
  await core.load({ id: 'slack', part: 'children' })
  assert.equal(core.focus('slack').compose, 'slack-token')
  await core.actions.submit({ id: 'slack', text: 'xoxp-1' })
  // Kept on Slack's own entity, as a secret: it never leaves the core.
  assert.equal(core.entity('slack').values['secret.token'], 'xoxp-1')
  assert.equal(core.entity('slack').values.self, 'UME')
  assert.ok(!core.actions.scan({ ids: ['slack'] }).events.some((e) => e.type === 'value' && e.key.startsWith('secret.')))
  assert.equal(core.item('slack')!.data['secret.token'], undefined)
  // And undo, of my own edits, can't take it back.
  assert.deepEqual(core.actions.undo().events, [])
  assert.equal(core.focus('slack').compose, null)
})

test('slack: the lists name everyone and every channel, and the first batch of history orders them', async () => {
  const calls: string[] = []
  const core = memoryCore({
    now: () => 1_700_000_100_000,
    fetch: fakeSlack(
      {
        ...auth,
        'users.conversations': () => ({
          channels: [{ id: 'C1', name: 'quiet' }, { id: 'C2', name: 'busy' }, { id: 'C3', name: 'silent' }, { id: 'D1', is_im: true, user: 'U1' }],
        }),
        'conversations.list': () => ({ channels: [{ id: 'C9', name: 'not-mine' }] }),
        'users.list': () => ({ members: [{ id: 'U1', name: 'ann', profile: { display_name: 'Ann' } }] }),
        'search.messages': searchAnswer([
          { ts: '1700000050.000000', user: 'U1', text: 'b', channel: { id: 'C2', name: 'busy' }, permalink: permalink('C2', '1700000050.000000') },
          { ts: '1700000030.000000', user: 'U1', text: 'd', channel: { id: 'D1', is_im: true }, permalink: permalink('D1', '1700000030.000000') },
          { ts: '1700000020.000000', user: 'U1', text: 'q', channel: { id: 'C1', name: 'quiet' }, permalink: permalink('C1', '1700000020.000000') },
        ]),
      },
      calls,
    ),
  })
  await core.slack.setToken('xoxp-1')
  await core.slack.poll()
  const order = () => core.focus('slack').children.map((child) => child.data.title)
  // Newest message first; one with nothing in the batch isn't listed.
  assert.deepEqual(order(), ['#busy', 'Ann', '#quiet'])
  // Loading the lists again moves nothing.
  await core.load({ id: 'slack', part: 'children', force: true })
  assert.deepEqual(order(), ['#busy', 'Ann', '#quiet'])
  assert.equal(core.item('slack:conv:C9')!.data.name, 'not-mine')
  assert.ok(!core.entity('slack').outboundLinks.includes('slack:conv:C9'))
  // The batch covered everything up to now, so the watch starts from now.
  assert.equal(core.item('slack')!.data['watch.at'], '1700000100.000000')
  assert.equal(core.item('slack')!.data['history.oldest'], '1700000020.000000')
  assert.equal(core.item('slack')!.data['history.complete'], true)
  // No conversation was loaded on its own, and no name needed a call.
  assert.ok(!calls.some((call) => ['conversations.history', 'conversations.info', 'users.info'].includes(call)))
})

test('slack: going further back, all of Slack by search, a conversation by its history', async () => {
  const queries: string[] = []
  const histories: (string | null)[] = []
  const core = memoryCore({
    now: () => 1_700_000_100_000,
    fetch: fakeSlack({
      ...auth,
      'search.messages': (params) => {
        queries.push(params.get('query')!)
        return {
          messages: {
            matches: [
              // After the cursor: already cached, skipped.
              { ts: '1700000060.000000', user: 'U1', text: 'new', channel: { id: 'C1' }, permalink: permalink('C1', '1700000060.000000') },
              { ts: '1699990000.000000', user: 'U1', text: 'old', channel: { id: 'C1' }, permalink: permalink('C1', '1699990000.000000') },
            ],
            paging: { pages: 1 },
          },
        }
      },
      'conversations.history': (params) => {
        histories.push(params.get('latest'))
        return {
          messages: [{ ts: '1699900000.000000', user: 'U1', text: 'older', reactions: [{ name: 'tada', count: 1, users: ['UME'] }], edited: { ts: '1699950000.000000' } }],
          has_more: false,
        }
      },
    }),
  })
  await core.slack.setToken('xoxp-1')
  core.cache.write([...values('slack:conv:C1', { type: 'slack.conversation', channel: 'C1', kind: 'channel' }, 0, 'slack'), link('slack', 'slack:conv:C1', 0, 'slack')])
  core.cache.write(values('slack', { 'watch.at': '1700000060.000000', 'history.oldest': '1700000050.000000' }, 0, 'slack'))
  await core.actions.older({ id: 'slack' })
  assert.match(queries[0], /^before:2023-11-1\d$/)
  assert.deepEqual(core.focus('slack:conv:C1').children.map((child) => child.data.text), ['old'])
  assert.equal(core.item('slack')!.data['history.oldest'], '1699990000.000000')

  // A conversation starts from the global cursor, since everything after it is cached.
  await core.actions.older({ id: 'slack:conv:C1' })
  assert.deepEqual(histories, ['1699990000.000000'])
  const events = core.cache.read(['slack:msg:C1:1699900000.000000'])
  const at = (key: string) => events.find((e) => e.type === 'value' && e.key === key)!.timestamp
  assert.equal(at('text'), 1699950000000)
  assert.equal(at('reactions'), 0)
  assert.equal(core.item('slack:conv:C1')!.data['history.oldest'], '1699900000.000000')
  assert.equal(core.item('slack:conv:C1')!.data['history.complete'], true)
  await core.actions.older({ id: 'slack:conv:C1' })
  assert.deepEqual(histories, ['1699990000.000000', '1699900000.000000'])
  assert.deepEqual(core.focus('slack:conv:C1').children.map((child) => child.data.text), ['older', 'old'])
})

test('the frontend cache loads only the lists on its own; history waits to be asked for', async () => {
  const calls: string[] = []
  const core = memoryCore({
    fetch: fakeSlack(
      {
        ...auth,
        'users.conversations': () => ({ channels: [{ id: 'C1', name: 'general' }] }),
        'users.list': () => ({ members: [{ id: 'U1', name: 'sam' }] }),
        'search.messages': searchAnswer([
          { ts: '1700000001.000100', user: 'U1', text: 'hello', channel: { id: 'C1', name: 'general' }, permalink: permalink('C1', '1700000001.000100') },
        ]),
      },
      calls,
    ),
  })
  const cache = new EntityCache({
    scan: async (ids) => core.actions.scan({ ids }),
    load: (request) => core.actions.load(request),
    foreign: foreignOf,
  })
  core.onChange((changed) => cache.invalidate(changed))
  const settle = async (id: string) => {
    for (let i = 0; i < 10; i++) {
      focusOf(id, cache.source())
      await cache.idle()
      await new Promise((resolve) => setTimeout(resolve, 160))
    }
    return focusOf(id, cache.source())
  }
  await core.actions.submit({ id: 'slack', text: 'xoxp-1' })
  await core.slack.poll()
  assert.deepEqual((await settle('slack')).children.map((child) => child.data.title), ['#general'])
  const conversation = await settle('slack:conv:C1')
  assert.deepEqual(conversation.children.map((child) => [child.data.author, child.data.text]), [['sam', 'hello']])
  assert.equal(conversation.older, true)
  const count = (method: string) => calls.filter((call) => call === method).length
  assert.equal(count('users.conversations'), 1)
  assert.equal(count('conversations.history'), 0)
  assert.equal(count('users.info'), 0)
})

test('slack: the watch brings in new messages by search, under their conversation or thread', async () => {
  let time = 1_700_000_000_000
  let matches: unknown[] = []
  const core = memoryCore({
    now: () => time,
    fetch: fakeSlack({ ...auth, 'search.messages': () => ({ messages: { matches, paging: { pages: 1 } } }) }),
  })
  await core.slack.setToken('xoxp-1')
  core.cache.write([
    ...values('slack:conv:C1', { type: 'slack.conversation', channel: 'C1', kind: 'channel', name: 'general' }, 0, 'slack'),
    link('slack', 'slack:conv:C1', 0, 'slack'),
    ...values('slack:conv:C2', { type: 'slack.conversation', channel: 'C2', kind: 'channel', name: 'older' }, 0, 'slack'),
    link('slack', 'slack:conv:C2', 0, 'slack'),
    link('slack:conv:C2', 'slack:msg:C2:1699999000.000000', 1699999000000, 'slack'),
  ])
  // The first look is the first batch back: nothing there, so the watch starts now.
  await core.slack.poll()
  assert.equal(core.item('slack')!.data['watch.at'], '1700000000.000000')

  time += 30_000
  const channel = { id: 'C1', name: 'general' }
  matches = [
    { ts: '1700000020.000100', user: 'U2', text: 'reply', channel, permalink: permalink('C1', '1700000020.000100', '1700000010.000100') },
    { ts: '1700000010.000100', user: 'U2', text: 'hello', channel, permalink: permalink('C1', '1700000010.000100', '1700000010.000100') },
    { ts: '1700000015.000100', user: 'U3', text: 'hi Ann', channel: { id: 'D9', is_im: true, name: 'U3' }, permalink: permalink('D9', '1700000015.000100') },
    { ts: '1700000016.000100', user: 'U3', text: 'elsewhere', channel: { id: 'C7', name: 'not-mine' }, permalink: permalink('C7', '1700000016.000100') },
  ]
  await core.slack.poll()
  // The status says what the last look found.
  assert.deepEqual([core.item('slack:watch')!.data.found, core.focus('slack').entity!.data.found], [4, 4])
  assert.equal(core.focus('slack').entity!.data.polledAt, time)
  await core.slack.poll()
  assert.equal(core.focus('slack').entity!.data.found, 0)
  const general = core.focus('slack:conv:C1')
  // The reply is in its thread, not its conversation.
  assert.deepEqual(general.children.map((child) => child.data.text), ['hello'])
  assert.equal(general.children[0].data.replyCount, 1)
  assert.deepEqual(core.focus('slack:msg:C1:1700000010.000100').children.map((child) => child.data.text), ['reply'])
  // Only conversations with something new are listed, by their newest message: a reply counts.
  assert.deepEqual(core.focus('slack').children.map((child) => child.id), ['slack:conv:C1', 'slack:conv:D9'])
  assert.ok(!core.entity('slack').outboundLinks.includes('slack:conv:C7'))
  assert.equal(core.item('slack')!.data['watch.at'], '1700000020.000100')
})

test('slack: a reply moves its thread to the end; the workspace shows, at every level, only what came after m', async () => {
  let time = 1_700_000_000_000
  let matches: unknown[] = []
  const core = memoryCore({
    now: () => time,
    fetch: fakeSlack({ ...auth, 'search.messages': () => ({ messages: { matches, paging: { pages: 1 } } }) }),
  })
  await core.slack.setToken('xoxp-1')
  core.cache.write([
    ...['C1', 'C2', 'C3'].flatMap((channel) => [
      ...values(`slack:conv:${channel}`, { type: 'slack.conversation', channel, kind: 'channel', name: channel }, 0, 'slack'),
      link('slack', `slack:conv:${channel}`, 0, 'slack'),
    ]),
  ])
  await core.slack.poll()
  const at = (channel: string, ts: string, user: string, thread?: string) => ({
    ts, user, text: ts, channel: { id: channel, name: channel }, permalink: permalink(channel, ts, thread),
  })
  const poll = async (more: unknown[]) => {
    time += 30_000
    matches = [...more, ...matches]
    await core.slack.poll()
  }
  await poll([
    at('C1', '1700000010.000000', 'U2'),
    at('C1', '1700000012.000000', 'U2'),
    at('C2', '1700000015.000000', 'U2'),
    // Only my own message in C3: nothing there for me to read.
    at('C3', '1700000025.000000', 'UME'),
  ])
  const shown = () =>
    viewOf('slack', core.source()).rows.slice(1).map((row) => `${'  '.repeat(row.depth - 1)}${row.entity.id.split(':').pop()}`)
  assert.deepEqual(shown(), ['C2', '  1700000015.000000', 'C1', '  1700000010.000000', '  1700000012.000000'])

  // A reply nests in its thread, moves the thread to the end of its conversation, and brings the conversation up.
  await poll([at('C1', '1700000030.000000', 'U3', '1700000010.000000')])
  const thread = 'slack:msg:C1:1700000010.000000'
  assert.deepEqual(core.focus('slack:conv:C1').children.map((child) => child.id), ['slack:msg:C1:1700000012.000000', thread])
  assert.deepEqual(core.focus(thread).children.map((child) => child.data.ts), ['1700000030.000000'])
  assert.equal(core.item(thread)!.data.lastMessageReceived, '1700000030.000000')
  assert.deepEqual(shown(), ['C1', '  1700000012.000000', '  1700000010.000000', '    1700000030.000000', 'C2', '  1700000015.000000'])

  // m on the conversation hides it and everything in it.
  await core.actions.markRead({ id: 'slack:conv:C1' })
  assert.equal(core.item('slack:conv:C1')!.data.lastRead, '1700000030.000000')
  assert.deepEqual(shown(), ['C2', '  1700000015.000000'])
  // Another reply: the conversation is back, with only the thread, and in it only the new reply.
  await poll([at('C1', '1700000040.000000', 'U3', '1700000010.000000')])
  assert.deepEqual(shown(), ['C1', '  1700000010.000000', '    1700000040.000000', 'C2', '  1700000015.000000'])
  // m on the thread: read, so it goes; the conversation stays until it is marked too.
  await core.actions.markRead({ id: thread })
  assert.deepEqual(shown(), ['C1', 'C2', '  1700000015.000000'])
  // Its own view shows everything.
  assert.equal(core.focus('slack:conv:C1').children.length, 2)

  // A thread I put under a note of mine still brings that note up when a reply comes.
  core.owned.write([
    value('n1', 'text', 'follow up', time, 'me'),
    link('slack:conv:C2', 'n1', time, 'me'),
    link('n1', thread, time, 'me'),
    link('slack:conv:C1', thread, time, 'me', 1),
  ])
  await core.actions.markRead({ id: 'slack:conv:C2' })
  await poll([at('C1', '1700000050.000000', 'U3', '1700000010.000000')])
  assert.equal(core.item('n1')!.data.lastMessageReceived, '1700000050.000000')
  // And it stays where I put it.
  assert.ok(!core.entity('slack:conv:C1').outboundLinks.includes(thread))
  assert.deepEqual(shown(), ['C2', '  n1', '    1700000010.000000', '      1700000050.000000', 'C1'])
})

test('slack: a reply to an old thread brings it into its conversation, and each header knows where its history starts', async () => {
  const core = memoryCore({
    now: () => 1_700_000_100_000,
    fetch: fakeSlack({
      ...auth,
      'users.conversations': () => ({ channels: [{ id: 'C1', name: 'general' }] }),
      'search.messages': searchAnswer([
        { ts: '1700000050.000000', user: 'U1', text: 'reply', channel: { id: 'C1' }, permalink: permalink('C1', '1700000050.000000', '1690000000.000000') },
        { ts: '1700000040.000000', user: 'U1', text: 'top', channel: { id: 'C1' }, permalink: permalink('C1', '1700000040.000000') },
      ]),
      'conversations.replies': () => ({ messages: [{ ts: '1690000000.000000', user: 'U1', text: 'old question', reply_count: 1, latest_reply: '1700000050.000000' }, { ts: '1700000050.000000', user: 'U1', text: 'reply', thread_ts: '1690000000.000000' }] }),
    }),
  })
  await core.slack.setToken('xoxp-1')
  await core.slack.poll()
  const thread = 'slack:msg:C1:1690000000.000000'
  const listed = () => core.focus('slack').children.map((child) => child.id)
  // A reply on a message older than anything cached brings its thread into the conversation, at the end.
  assert.deepEqual(listed(), ['slack:conv:C1'])
  await core.load({ id: 'slack', part: 'children', force: true })
  assert.deepEqual(listed(), ['slack:conv:C1'])
  assert.deepEqual(core.focus('slack:conv:C1').children.map((child) => child.id), ['slack:msg:C1:1700000040.000000', thread])

  const home = core.focus('slack')
  assert.equal(home.entity!.data.from, '1700000040.000000')
  // Search had nothing older than these two, so there is no further back to go.
  assert.equal(home.entity!.data.complete, true)
  assert.equal(home.older, false)
  // The conversation starts where the workspace does.
  assert.equal(core.focus('slack:conv:C1').entity!.data.from, '1700000040.000000')
  // A thread loads whole once, and then has nothing older.
  assert.equal(core.focus(thread).older, true)
  await core.actions.older({ id: thread })
  assert.equal(core.focus(thread).older, false)
  assert.equal(core.focus(thread).entity!.data.text, 'old question')
})

test('slack: no message is linked under the workspace, and unlinking a chat sticks', async () => {
  const calls: string[] = []
  let matches: unknown[] = []
  let time = 1_700_000_000_000
  const core = memoryCore({
    now: () => time,
    fetch: fakeSlack(
      {
        ...auth,
        'users.conversations': () => ({ channels: [{ id: 'C1', name: 'mine' }, { id: 'C2', name: 'noisy' }] }),
        'search.messages': () => ({ messages: { matches, paging: { pages: 1 } } }),
      },
      calls,
    ),
  })
  await core.slack.setToken('xoxp-1')
  calls.length = 0
  // The lists load before the first search, even when nothing loaded them yet.
  core.clearCache()
  await core.slack.poll()
  assert.ok(calls.indexOf('users.conversations') < calls.indexOf('search.messages'))

  const reply = (channel: string, ts: string, thread: string) => ({ ts, user: 'U1', text: 'r', channel: { id: channel }, permalink: permalink(channel, ts, thread) })
  time += 30_000
  matches = [reply('C1', '1700000010.000000', '1690000000.000000'), reply('C2', '1700000011.000000', '1690000001.000000'), reply('C9', '1700000012.000000', '1690000002.000000')]
  await core.slack.poll()
  const listed = () => core.entity('slack').outboundLinks
  assert.ok(!listed().some((id) => id.startsWith('slack:msg:')))
  assert.deepEqual(core.focus('slack').children.map((child) => child.id), ['slack:conv:C2', 'slack:conv:C1'])

  // Unlinking a conversation removes it, and it stays gone.
  const outcome = core.actions.unlink({ parent: 'slack', child: 'slack:conv:C2' })
  assert.equal(outcome.events.length, 1)
  time += 30_000
  matches = [reply('C2', '1700000040.000000', '1690000001.000000'), reply('C2', '1700000041.000000', '1690000003.000000')]
  await core.slack.poll()
  await core.load({ id: 'slack', part: 'children', force: true })
  assert.ok(!listed().some((id) => id.includes(':C2')))
  assert.ok(listed().includes('slack:conv:C1'))
})

test('slack: removing a chat is a plain unlink; thread links from before are pruned', async () => {
  let matches: unknown[] = []
  let time = 1_700_000_000_000
  const core = memoryCore({
    now: () => time,
    fetch: fakeSlack({
      ...auth,
      'users.conversations': () => ({ channels: [{ id: 'C1', name: 'mine' }] }),
      'search.messages': () => ({ messages: { matches, paging: { pages: 1 } } }),
    }),
  })
  await core.slack.setToken('xoxp-1')
  await core.slack.poll()
  // Thread links from before threads read flat, under a channel I'm in and one I'm not.
  core.cache.write([
    link('slack', 'slack:msg:C9:1690000000.000000', 1690000000000, 'slack'),
    link('slack', 'slack:msg:C1:1690000001.000000', 1690000001000, 'slack'),
  ])
  time += 30_000
  await core.slack.poll()
  assert.ok(!core.entity('slack').outboundLinks.some((id) => id.startsWith('slack:msg:')))

  // A message knows its chat, and unlinking that chat removes it.
  matches = [{ ts: '1700000040.000000', user: 'U1', text: 'r', channel: { id: 'C1' }, permalink: permalink('C1', '1700000040.000000', '1690000005.000000') }]
  time += 30_000
  await core.slack.poll()
  const message = core.focus('slack:conv:C1').children.find((child) => child.type === 'slack.message')!
  assert.equal(message.data.conversation, 'slack:conv:C1')
  core.actions.unlink({ parent: 'slack', child: String(message.data.conversation) })
  assert.deepEqual(core.entity('slack').outboundLinks, [])
})

test('split: a note becomes its first part, the rest notes under it, all at one time; one undo takes it back', () => {
  const core = memoryCore()
  // Written a while ago, so undo (which takes edits within 100 ms together) leaves it.
  const note = 'note'
  const then = Date.now() - 1000
  core.actions.redo({ events: [value(note, 'text', 'Plan\n\n- one\n  - detail\n- two', then, 'me'), link('tasks', note, then, 'me')] })
  const { events } = core.actions.split({ id: note })
  assert.equal(new Set(events.map((event) => event.timestamp)).size, 1)
  assert.equal(core.item(note)!.data.text, 'Plan')
  const texts = (id: string): unknown[] =>
    core.entity(id).outboundLinks.map((child) => {
      const text = String(core.entity(child).values.text)
      return core.entity(child).outboundLinks.length ? [text, texts(child)] : text
    })
  assert.deepEqual(texts(note), [['one', ['detail']], 'two'])
  // A plain note's parts are plain notes.
  assert.equal(core.entity(core.entity(note).outboundLinks[0]).values.type, undefined)
  assert.equal(core.actions.split({ id: note }).error, 'Nothing to split')

  core.actions.undo()
  assert.equal(core.item(note)!.data.text, 'Plan\n\n- one\n  - detail\n- two')
  assert.deepEqual(core.entity(note).outboundLinks, [])

  // A typed item's parts keep its type.
  const typed = 'typed'
  core.actions.redo({ events: [...values(typed, { type: 'claude.response', text: 'Done.\n\n- one' }, then, 'claude'), link('tasks', typed, then, 'me')] })
  core.actions.split({ id: typed })
  assert.equal(core.item(core.entity(typed).outboundLinks[0])!.type, 'claude.response')
})
