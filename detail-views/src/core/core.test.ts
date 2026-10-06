import assert from 'node:assert/strict'
import { mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { DatabaseSync } from 'node:sqlite'
import { test } from 'node:test'
import { Core } from './core.ts'
import { EntityCache } from './graph/cache.ts'
import { link, value, values } from './graph/events.ts'
import { focusOf, foreignOf } from './present.ts'
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

test('tasks: composing adds an owned child, undone first', async () => {
  const core = memoryCore()
  const first = await core.actions.submit({ id: 'tasks', text: 'first' })
  await core.actions.submit({ id: 'tasks', text: 'second' })
  assert.equal(first.events.length, 4)
  const id = core.focus('tasks').children[0].id
  core.actions.toggle({ id })
  const focus = core.focus('tasks')
  assert.deepEqual(focus.children.map((child) => child.data.text), ['second', 'first'])
  assert.equal(focus.compose, 'task')
  assert.ok(core.owned.read([id]).length > 0)
  assert.equal(core.cache.read([id]).length, 0)
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
    link('slack:conv:C1', 'slack:msg:C1:1.0', 1000, 'slack'),
  ])
  assert.equal(core.focus('slack:conv:C1').compose, null)
  await core.actions.submit({ id: 'slack:conv:C1', text: 'oops' })
  const marked = await core.actions.markRead({ id: 'slack:conv:C1' })
  assert.ok(!called.includes('chat.postMessage'))
  assert.ok(!called.includes('conversations.mark'))
  assert.match(marked.error ?? '', /read-only/)
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
  assert.equal(core.settings.get('slack.token'), 'xoxp-old')
  assert.equal(core.owned.read(['t1']).length, 4)
  assert.equal(core.item('slack:conv:C1')?.data.kind, undefined)
  assert.deepEqual(new DatabaseSync(legacy).prepare('SELECT count(*) AS n FROM entities').get(), before)
})

test('slack: no token asks for one, and a token is checked before it is kept', async () => {
  const core = memoryCore({ fetch: fakeSlack(auth) })
  await core.load({ id: 'slack', part: 'children' })
  assert.equal(core.focus('slack').compose, 'slack-token')
  await core.actions.submit({ id: 'slack', text: 'xoxp-1' })
  assert.equal(core.settings.get('slack.token'), 'xoxp-1')
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
  // Newest message first; one with nothing in the batch sits at the bottom.
  assert.deepEqual(order(), ['#busy', 'Ann', '#quiet', '#silent'])
  // Loading the lists again moves nothing.
  await core.load({ id: 'slack', part: 'children', force: true })
  assert.deepEqual(order(), ['#busy', 'Ann', '#quiet', '#silent'])
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
  await core.slack.poll()
  const general = core.focus('slack:conv:C1')
  assert.deepEqual(general.children.map((child) => child.data.text), ['hello'])
  assert.equal(general.children[0].data.replyCount, 1)
  assert.deepEqual(core.focus('slack:msg:C1:1700000010.000100').children.map((child) => child.data.text), ['reply'])
  // The thread is listed too, by its newest reply, ahead of the conversations.
  assert.deepEqual(core.focus('slack').children.map((child) => child.id).slice(0, 4), [
    'slack:msg:C1:1700000010.000100',
    'slack:conv:D9',
    'slack:conv:C1',
    'slack:conv:C2',
  ])
  assert.ok(!core.entity('slack').outboundLinks.includes('slack:conv:C7'))
  assert.equal(core.item('slack')!.data['watch.at'], '1700000020.000100')
})

test('slack: threads stay in the workspace through a list reload, and each header knows where its history starts', async () => {
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
  // A reply on a message older than anything cached still lists its thread, first.
  assert.deepEqual(listed(), [thread, 'slack:conv:C1'])
  await core.load({ id: 'slack', part: 'children', force: true })
  assert.deepEqual(listed(), [thread, 'slack:conv:C1'])

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
