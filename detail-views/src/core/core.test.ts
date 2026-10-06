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

const auth = { 'auth.test': () => ({ user_id: 'UME', url: 'https://x.slack.com/' }), 'users.conversations': () => ({ channels: [] }) }

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

test('slack: no token asks for one, and a token is checked before it is kept', async () => {
  const core = memoryCore({ fetch: fakeSlack(auth) })
  await core.load({ id: 'slack', part: 'children' })
  assert.equal(core.focus('slack').compose, 'slack-token')
  await core.actions.submit({ id: 'slack', text: 'xoxp-1' })
  assert.equal(core.settings.get('slack.token'), 'xoxp-1')
  assert.equal(core.focus('slack').compose, null)
})

test('slack: conversations list unread first, then by recency', async () => {
  const core = memoryCore({
    fetch: fakeSlack({
      ...auth,
      'users.conversations': () => ({
        channels: [
          { id: 'C1', name: 'quiet' },
          { id: 'C2', name: 'busy' },
          { id: 'D1', is_im: true, user: 'U1' },
        ],
      }),
      'conversations.info': (params) => {
        const channel = params.get('channel')
        if (channel === 'C2') return { channel: { id: 'C2', name: 'busy', last_read: '10.0', unread_count_display: 3 } }
        if (channel === 'D1') return { channel: { id: 'D1', is_im: true, user: 'U1', last_read: '50.0', unread_count_display: 0 } }
        return { channel: { id: 'C1', name: 'quiet', last_read: '20.0' } }
      },
      'conversations.history': () => ({ messages: [] }),
      'users.info': () => ({ user: { name: 'ann', profile: { display_name: 'Ann' } } }),
    }),
  })
  await core.slack.setToken('xoxp-1')
  for (const id of ['slack:conv:C1', 'slack:conv:C2', 'slack:conv:D1', 'slack:user:U1']) await core.load({ id, part: 'self' })
  assert.deepEqual(core.focus('slack').children.map((child) => child.data.title), ['#busy', 'Ann', '#quiet'])
})

test('slack: messages are written when they were sent, reactions at 0, and marked loaded', async () => {
  const calls: string[] = []
  const core = memoryCore({
    fetch: fakeSlack(
      {
        ...auth,
        'conversations.history': () => ({
          messages: [
            { ts: '1700000002.000200', user: 'U1', text: 'second', reactions: [{ name: 'tada', count: 1, users: ['UME'] }] },
            { ts: '1700000001.000100', user: 'U1', text: 'first', edited: { ts: '1700000005.000000' } },
          ],
        }),
      },
      calls,
    ),
  })
  await core.slack.setToken('xoxp-1')
  await core.load({ id: 'slack:conv:C1', part: 'children' })
  const events = core.cache.read(['slack:msg:C1:1700000002.000200'])
  const at = (key: string) => events.find((e) => e.type === 'value' && e.key === key)!.timestamp
  assert.equal(at('text'), 1700000002000)
  assert.equal(at('reactions'), 0)
  assert.equal(core.cache.read(['slack:msg:C1:1700000001.000100']).find((e) => e.type === 'value' && e.key === 'text')!.timestamp, 1700000005000)
  assert.deepEqual(
    core.focus('slack:conv:C1').children.map((child) => child.data.text),
    ['first', 'second'],
  )
  // Loaded once; asked again while fresh, nothing is fetched.
  const before = calls.length
  await core.load({ id: 'slack:conv:C1', part: 'children' })
  await core.load({ id: 'slack:msg:C1:1700000001.000100', part: 'self' })
  assert.equal(calls.length, before)
  await core.load({ id: 'slack:conv:C1', part: 'children', force: true })
  assert.equal(calls.length, before + 1)
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
  core.cache.write([...values('slack:conv:C1', { channel: 'C1', kind: 'channel', latestTs: '1.0' }, 0, 'slack')])
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

test('the frontend cache loads from other services on its own, once', async () => {
  const calls: string[] = []
  const core = memoryCore({
    fetch: fakeSlack(
      {
        ...auth,
        'users.conversations': () => ({ channels: [{ id: 'C1', name: 'general' }] }),
        'conversations.info': () => ({ channel: { id: 'C1', name: 'general', last_read: '1.0', unread_count_display: 2 } }),
        'conversations.history': () => ({ messages: [{ ts: '1700000001.000100', user: 'U1', text: 'hello' }] }),
        'users.info': () => ({ user: { name: 'sam' } }),
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
  // A screen reads, waits, and reads again: here, until nothing changes.
  const settle = async (id: string) => {
    for (let i = 0; i < 20; i++) {
      focusOf(id, cache.source())
      await cache.idle()
      await new Promise((resolve) => setTimeout(resolve, 160))
    }
    return focusOf(id, cache.source())
  }
  await core.actions.submit({ id: 'slack', text: 'xoxp-1' })
  const home = await settle('slack')
  assert.deepEqual(home.children.map((child) => [child.data.title, child.data.unread]), [['#general', 2]])
  const conversation = await settle('slack:conv:C1')
  assert.deepEqual(conversation.children.map((child) => [child.data.author, child.data.text]), [['sam', 'hello']])
  // Each part was loaded once, however often it was read.
  const count = (method: string) => calls.filter((call) => call === method).length
  assert.equal(count('conversations.history'), 1)
  assert.equal(count('conversations.info'), 1)
  assert.equal(count('users.info'), 1)
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

test('slack: the watch brings in new messages by search, without loading any conversation', async () => {
  let time = 1_700_000_000_000
  const calls: string[] = []
  let matches: unknown[] = []
  const core = memoryCore({
    now: () => time,
    fetch: fakeSlack(
      { ...auth, 'search.messages': () => ({ messages: { matches, paging: { pages: 1 } } }) },
      calls,
    ),
  })
  await core.slack.setToken('xoxp-1')
  // A conversation whose count and history have loaded once.
  core.cache.write([
    ...values('slack:conv:C1', { type: 'slack.conversation', channel: 'C1', kind: 'channel', name: 'general', lastRead: '1700000000.000000', unread: 0 }, 0, 'slack'),
    link('slack', 'slack:conv:C1', 0, 'slack'),
  ])
  // The first look only notes where the watch starts.
  await core.slack.poll()
  assert.equal(core.item('slack')!.data['watch.at'], '1700000000.000000')

  time += 30_000
  const channel = { id: 'C1', name: 'general' }
  matches = [
    { ts: '1700000020.000100', user: 'U2', text: 'reply', channel, permalink: 'https://x.slack.com/archives/C1/p1700000020000100?thread_ts=1700000010.000100' },
    { ts: '1700000010.000100', user: 'U2', text: 'hello', channel, permalink: 'https://x.slack.com/archives/C1/p1700000010000100?thread_ts=1700000010.000100' },
    { ts: '1700000015.000100', user: 'U3', text: 'hi Ann', channel: { id: 'D9', is_im: true, name: 'U3' }, permalink: 'https://x.slack.com/archives/D9/p1700000015000100' },
  ]
  await core.slack.poll()
  await core.slack.poll()
  const general = core.focus('slack:conv:C1')
  assert.deepEqual(general.children.map((child) => child.data.text), ['hello'])
  assert.equal(general.entity!.data.unread, 1)
  assert.equal(general.entity!.data.latestTs, '1700000010.000100')
  assert.equal(general.children[0].data.replyCount, 1)
  assert.deepEqual(core.focus('slack:msg:C1:1700000010.000100').children.map((child) => child.data.text), ['reply'])
  // A DM nobody had loaded yet joins the list.
  assert.ok(core.entity('slack').outboundLinks.includes('slack:conv:D9'))
  assert.equal(core.item('slack:conv:D9')!.data.kind, 'im')
  assert.equal(core.item('slack')!.data['watch.at'], '1700000020.000100')
  assert.ok(!calls.includes('conversations.history') && !calls.includes('conversations.info'))
})

test('slack: conversations and threads load once, and only again when I refresh them', async () => {
  const calls: string[] = []
  let time = 1_700_000_000_000
  const core = memoryCore({
    now: () => time,
    fetch: fakeSlack({ ...auth, 'conversations.history': () => ({ messages: [] }) }, calls),
  })
  await core.slack.setToken('xoxp-1')
  await core.load({ id: 'slack:conv:C1', part: 'children' })
  time += 30 * 24 * 60 * 60_000
  await core.load({ id: 'slack:conv:C1', part: 'children' })
  assert.equal(calls.filter((call) => call === 'conversations.history').length, 1)
  await core.refresh('slack:conv:C1')
  assert.equal(calls.filter((call) => call === 'conversations.history').length, 2)
})
