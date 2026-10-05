import assert from 'node:assert/strict'
import { test } from 'node:test'
import { Core } from './core.ts'

function clock(start = 1_000_000) {
  let time = start
  return { now: () => time, advance: (ms: number) => (time += ms) }
}

/** Answers Slack methods from a table; anything missing fails the test. */
function fakeSlack(answers: Record<string, (params: URLSearchParams) => unknown>): typeof fetch {
  return (async (input: string | URL | Request, init?: RequestInit) => {
    const method = String(input).split('/').pop()!
    const answer = answers[method]
    if (!answer) throw new Error(`unexpected slack call ${method}`)
    return new Response(JSON.stringify({ ok: true, ...(answer(init!.body as URLSearchParams) as object) }))
  }) as typeof fetch
}

test('cached data is swept once it expires; owned data stays', () => {
  const time = clock()
  const core = new Core({ path: ':memory:', now: time.now })
  core.store.put('mine', 'task', { text: 'keep' })
  core.store.put('slack:conv:C1', 'slack.conversation', {}, { ttl: 1000 })
  core.store.link('slack', 'slack:conv:C1', { ttl: 1000 })
  time.advance(2000)
  assert.equal(core.store.sweep(), 1)
  assert.ok(core.store.get('mine'))
  assert.equal(core.store.get('slack:conv:C1'), null)
  assert.deepEqual(core.store.children('slack'), [])
})

test('an owned link keeps a cached entity alive past its expiry', () => {
  const time = clock()
  const core = new Core({ path: ':memory:', now: time.now })
  core.store.put('slack:msg:C1:1.0', 'slack.message', { text: 'hi' }, { ttl: 1000 })
  core.store.put('note', 'task', { text: 'reply to this' })
  core.store.link('note', 'slack:msg:C1:1.0')
  time.advance(2000)
  core.store.sweep()
  assert.ok(core.store.get('slack:msg:C1:1.0'))
  assert.equal(core.store.children('note').length, 1)
})

test('a cache write never makes owned data expire', () => {
  const core = new Core({ path: ':memory:' })
  core.store.put('x', 'task', { text: 'a' })
  core.store.put('x', 'task', { text: 'b' }, { ttl: 10 })
  assert.equal(core.store.get('x')!.expiresAt, null)
  assert.equal(core.store.get('x')!.data.text, 'b')
})

test('refreshing cached children keeps owned links under the same parent', () => {
  const core = new Core({ path: ':memory:' })
  core.store.put('a', 'slack.message', {}, { ttl: 1000 })
  core.store.put('b', 'slack.message', {}, { ttl: 1000 })
  core.store.put('mine', 'task', {})
  core.store.link('p', 'mine')
  core.store.setCachedChildren('p', [{ id: 'a', rank: 1 }], { ttl: 1000 })
  core.store.setCachedChildren('p', [{ id: 'b', rank: 1 }], { ttl: 1000 })
  assert.deepEqual(
    core.store.children('p').map((child) => child.id).sort(),
    ['b', 'mine'],
  )
})

test('tasks: composing adds an owned child, undone first', async () => {
  const core = new Core({ path: ':memory:' })
  await core.actions.submit({ id: 'tasks', text: 'first' })
  await core.actions.submit({ id: 'tasks', text: 'second' })
  const first = core.focus('tasks').children[0]
  core.actions.toggle({ id: first.id })
  const focus = core.focus('tasks')
  assert.deepEqual(focus.children.map((child) => child.data.text), ['second', 'first'])
  assert.equal(focus.compose, 'task')
  assert.equal(focus.children[0].expiresAt, null)
})

test('slack: no token asks for one, and a token is checked before it is kept', async () => {
  const core = new Core({
    path: ':memory:',
    fetch: fakeSlack({
      'auth.test': () => ({ user_id: 'UME', url: 'https://x.slack.com/' }),
      'users.conversations': () => ({ channels: [] }),
    }),
  })
  assert.equal(core.focus('slack').compose, 'slack-token')
  await core.actions.submit({ id: 'slack', text: 'xoxp-1' })
  assert.equal(core.store.getSetting('slack.token'), 'xoxp-1')
  assert.equal(core.focus('slack').compose, null)
})

test('slack: conversations list unread first, then by recency', async () => {
  const core = new Core({
    path: ':memory:',
    fetch: fakeSlack({
      'auth.test': () => ({ user_id: 'UME', url: 'https://x.slack.com/' }),
      'users.conversations': () => ({
        channels: [
          { id: 'C1', name: 'quiet' },
          { id: 'C2', name: 'busy' },
          { id: 'D1', is_im: true, user: 'U1' },
        ],
      }),
      'conversations.info': (params) => {
        const channel = params.get('channel')
        if (channel === 'C2') return { channel: { last_read: '10.0', unread_count_display: 3 } }
        if (channel === 'D1') return { channel: { last_read: '50.0', unread_count_display: 0 } }
        return { channel: { last_read: '20.0' } }
      },
      'conversations.history': () => ({ messages: [] }),
      'users.info': () => ({ user: { name: 'ann', profile: { display_name: 'Ann' } } }),
    }),
  })
  await core.slack.setToken('xoxp-1')
  // Counting runs behind the refresh; let it finish.
  for (let i = 0; i < 50 && core.focus('slack').children.some((c) => c.data.unread === undefined); i += 1) {
    await new Promise((resolve) => setTimeout(resolve, 5))
  }
  const titles = core.focus('slack').children.map((child) => child.data.title)
  assert.deepEqual(titles, ['#busy', 'Ann', '#quiet'])
})

test('slack: read-only refuses writes before they reach the network', async () => {
  const called: string[] = []
  const core = new Core({
    path: ':memory:',
    fetch: (async (input: string | URL | Request) => {
      called.push(String(input).split('/').pop()!)
      return new Response(JSON.stringify({ ok: true, user_id: 'UME', url: '', channels: [] }))
    }) as typeof fetch,
  })
  await core.slack.setToken('xoxp-1')
  core.store.put('slack:conv:C1', 'slack.conversation', { channel: 'C1', kind: 'channel', latestTs: '1.0' }, { ttl: 1000 })
  assert.equal(core.focus('slack:conv:C1').compose, null)
  await core.actions.submit({ id: 'slack:conv:C1', text: 'oops' })
  await core.actions.markRead({ id: 'slack:conv:C1' })
  assert.ok(!called.includes('chat.postMessage'))
  assert.ok(!called.includes('conversations.mark'))
  assert.match(core.focus('slack:conv:C1').error ?? '', /read-only/)
})
