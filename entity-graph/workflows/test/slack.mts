// Slack, against a stand-in for its Web API: messages and channels pulled into a task, read through the cache;
// the actions; mrkdwn as markdown; links in notes and pasted.

import assert from 'node:assert/strict'
import { Admin } from '../src/engine/admin.ts'
import { Engine } from '../src/engine/engine.ts'
import { parseSlackLink, slackWait } from '../src/shared/slack.ts'

const slackCalls: { method: string; body: Record<string, unknown> }[] = []
let thread: { ts: string; user: string; text: string }[] = []
const PEOPLE: Record<string, string> = { UME: 'Alex', USAM: 'Sam' }
// #eng: a thread at 1700000100, with two replies, among other messages.
const eng: { ts: string; user: string; text: string; thread_ts?: string; reply_count?: number }[] = [
  { ts: '1700000000.000100', user: 'USAM', text: 'Morning' },
  { ts: '1700000100.000200', user: 'USAM', text: '*Deploy* is out, <@UME>: see <https://ci.example/1|the run> &amp; <#C1>', thread_ts: '1700000100.000200', reply_count: 2 },
  { ts: '1700000150.000300', user: 'UME', text: 'Thanks!', thread_ts: '1700000100.000200' },
  { ts: '1700000160.000400', user: 'USAM', text: '_np_', thread_ts: '1700000100.000200' },
  { ts: '1700000200.000500', user: 'UME', text: 'Lunch?' },
]
const fakeFetch = (async (url: string, init: { headers: Record<string, string>; body?: string }) => {
  const u = new URL(url)
  const method = u.pathname.replace('/api/', '')
  const body = init.body ? JSON.parse(init.body) : Object.fromEntries(u.searchParams)
  slackCalls.push({ method, body })
  const ok = (r: Record<string, unknown>) => ({ status: 200, json: async () => ({ ok: true, ...r }) })
  if (method === 'auth.test')
    return init.headers.Authorization === 'Bearer xoxp-good-1234'
      ? ok({ team: 'Acme', user: 'alex', user_id: 'UME', url: 'https://acme.slack.com/' })
      : { status: 200, json: async () => ({ ok: false, error: 'invalid_auth' }) }
  if (method === 'chat.postMessage') {
    thread = [{ ts: '100.1', user: 'UME', text: String(body.text) }]
    return ok({ channel: body.channel, ts: '100.1' })
  }
  if (method === 'conversations.replies' && body.channel === 'C9')
    return ok({ messages: eng.filter((m) => m.ts === body.ts || m.thread_ts === body.ts) })
  if (method === 'conversations.replies') return ok({ messages: thread })
  if (method === 'conversations.history') {
    const top = eng.filter((m) => !m.thread_ts || m.thread_ts === m.ts)
    const within = top.filter((m) => (!body.oldest || Number(m.ts) > Number(body.oldest) || (body.inclusive && m.ts === body.oldest)) &&
      (!body.latest || Number(m.ts) < Number(body.latest) || (body.inclusive && m.ts === body.latest)))
    return ok({ messages: within.reverse().slice(0, Number(body.limit ?? 100)) })
  }
  if (method === 'users.info') return ok({ user: { name: String(body.user).toLowerCase(), profile: { display_name: PEOPLE[String(body.user)] } } })
  if (method === 'conversations.info') return ok({ channel: { id: body.channel, name: body.channel === 'C9' ? 'eng' : 'general' } })
  if (method === 'users.conversations') return ok({ channels: [{ id: 'C1', name: 'general' }, { id: 'C9', name: 'eng' }] })
  if (method === 'search.messages')
    return ok({ messages: { matches: eng.map((m) => ({ ...m, channel: { id: 'C9', name: 'eng' } })), paging: { pages: 1 } } })
  throw new Error(`unexpected Slack ${method}`)
}) as unknown as typeof fetch

let clock = 1_800_000_000_000
const engine = new Engine({ now: () => clock, fetch: fakeFetch, dataDir: '/tmp' })
await engine.signIn('alex')
const admin = new Admin(engine, { operator: () => ({ url: '', error: null }) } as never, fakeFetch)
const ctx = { engine, author: 'bot' as const, actor: 'bot', itemId: null, runId: null }

// A workspace, as added on the Integrations page: Slack says which, and its address.
await admin.addSlack('xoxp-good-1234')
assert.equal(engine.settings.slackWorkspaces()[0].url, 'https://acme.slack.com/')

// Links.
const link = 'https://acme.slack.com/archives/C9/p1700000100000200'
assert.deepEqual(parseSlackLink(link), { host: 'acme.slack.com', channel: 'C9', ts: '1700000100.000200' })
assert.equal(parseSlackLink(`https://acme.slack.com/archives/C9/p1700000150000300?thread_ts=1700000100.000200&cid=C9`)?.thread_ts, '1700000100.000200')

// A link in a note is pulled in after it; a link alone is just the message.
const task = (await engine.create('Slack'))!
await engine.send(task, 'note', `What happened here? ${link}`)
await engine.send(task, 'note', link)
assert.deepEqual(engine.children(task).map((e) => e.type), ['note', 'slack_message', 'slack_message'])
const msg = engine.children(task).find((e) => e.type === 'slack_message')!

// Its view: the thread, as markdown, with names for ids; read through the cache.
const replies = () => slackCalls.filter((c) => c.method === 'conversations.replies' && c.body.channel === 'C9').length
const view = await admin.slackView(msg.id)
assert.equal(view.channelName, '#eng')
assert.equal(view.focus, '1700000100.000200')
assert.deepEqual(view.messages.map((m) => m.author), ['Sam', 'Alex', 'Sam'])
assert.equal(view.messages[0].text, '**Deploy** is out, @Alex: see [the run](https://ci.example/1) & #general')
const before = replies()
await admin.slackView(msg.id)
assert.equal(replies(), before, 'the second look is the cache')
clock += 6 * 60_000
await admin.slackView(msg.id)
assert.equal(replies(), before + 1, 'messages are kept five minutes')
assert.ok((await admin.integrations()).slackCache.entries > 0)
await admin.clearSlackCache()
assert.equal((await admin.integrations()).slackCache.entries, 0)

// A channel over a range: by #name; its threads open in place.
const [chan] = await engine.addEvents(task, [{ type: 'slack_channel', params: { channel: '#eng', oldest: '1700000000.000000', latest: '1700000300.000000' } }])
const chView = await admin.slackView(chan)
assert.deepEqual(chView.messages.map((m) => m.ts), ['1700000000.000100', '1700000100.000200', '1700000200.000500'], 'replies are left out')
assert.deepEqual((await admin.slackView(chan, '1700000100.000200')).messages.map((m) => m.text), ['Thanks!', '*np*'])
const [nowhere] = await engine.addEvents(task, [{ type: 'slack_channel', params: { channel: '#nowhere', oldest: '1700000000' } }])
await assert.rejects(admin.slackView(nowhere), /No channel #nowhere/)

// The actions.
const around = (await engine.callAction('slack_messages_around', { link, before: 1, after: 1 }, ctx)) as { text: string }[]
assert.deepEqual(around.map((m) => m.text.slice(0, 6)), ['Mornin', '**Depl', 'Lunch?'])
const inThread = (await engine.callAction('slack_messages_around', { channel: 'C9', ts: '1700000150.000300', thread_ts: '1700000100.000200', before: 0, after: 5 }, ctx)) as { text: string }[]
assert.deepEqual(inThread.map((m) => m.text), ['Thanks!', '*np*'])
const ranged = (await engine.callAction('slack_messages', { after: '1700000140', before: '1700000170' }, ctx)) as { author: string }[]
assert.deepEqual(ranged.map((m) => m.author), ['Alex', 'Sam'])
assert.deepEqual(((await engine.callAction('slack_channels', { query: '#eng' }, ctx)) as { id: string }[]).map((c) => c.id), ['C9'])
await assert.rejects(engine.callAction('slack_post', { channel: 'C9', text: 'x' }, ctx), /unsafe/, 'posting is a person\'s, through an approval')

// The chat input's "Wait for Slack": a script that owns the task, checks with backoff, and hands it back on news.
{
  await admin.clearSlackCache()
  const waiting = (await engine.create('Waiting on Sam'))!
  const [w] = await engine.addEvents(waiting, [{ type: 'script', params: { description: 'Wait for Slack', wake_at: '+1m', owns: [waiting], source: slackWait(link, 'alex') } }])
  await engine.assign(waiting, 'alex', false)
  clock += 60_000
  await engine.checkDue()
  await engine.idle()
  assert.equal(engine.event(w)!.lastError, null)
  assert.ok(!engine.event(w)!.resolved, 'nothing new yet')
  assert.equal(engine.event(w)!.params.wake_at, clock + 5 * 60_000, 'then five minutes')
  // Something new in the thread.
  eng.push({ ts: '1700000170.000600', user: 'USAM', text: 'Any news?', thread_ts: '1700000100.000200' })
  await admin.clearSlackCache()
  clock += 5 * 60_000
  await engine.checkDue()
  await engine.idle()
  assert.ok(engine.event(w)!.resolved, String(engine.event(w)!.data?.error ?? ''))
  assert.deepEqual(engine.get(waiting)!.assignees, ['alex'])
  assert.match(String(engine.children(waiting).filter((e) => e.type === 'note').at(-1)!.params.text), /New on Slack, in #eng:\n\n\*\*Sam\*\*: Any news\?/)
}

console.log('slack: ok')
