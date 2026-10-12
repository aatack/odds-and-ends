import assert from 'node:assert/strict'
import { existsSync, mkdtempSync, readFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { test } from 'node:test'
import { memoryCore } from '../../testing.ts'

const settle = () => new Promise((resolve) => setTimeout(resolve, 20))

/** claude, answering each prompt with what it was sent; every call recorded. */
function fakeClaude(calls: { args: string[]; cwd: string }[]) {
  return async (command: string, args: string[], cwd: string) => {
    if (!command.endsWith('claude')) throw new Error(`unexpected ${command}`)
    calls.push({ args, cwd })
    return JSON.stringify({ result: `re: ${args[1]}`, is_error: false })
  }
}

/** A topic's chat: each child's type and text, in order. */
function chat(core: ReturnType<typeof memoryCore>, topic: string): [string, string][] {
  return core.entity(topic).outboundLinks.map((id) => [core.item(id)!.type, String(core.item(id)!.data.text)])
}

test('topics: a topic is named and listed; the box only adds messages; a dispatch sends every one since the last, at once', async () => {
  const calls: { args: string[]; cwd: string }[] = []
  const core = memoryCore({ run: fakeClaude(calls) })
  const made = await core.actions.topicCreate({ name: '  Billing bug ' })
  assert.equal(made.error, null)
  const [topic] = core.entity('topics').outboundLinks
  assert.equal(core.item(topic)!.type, 'topic')
  assert.equal(core.item(topic)!.data.text, 'Billing bug')
  assert.equal((await core.actions.topicCreate({ name: ' ' })).error, 'A topic needs a name')
  assert.equal(core.focus('topics').compose, null)
  assert.equal(core.focus(topic).compose, 'topic')

  await core.actions.submit({ id: topic, text: 'it double-charges' })
  await core.actions.submit({ id: topic, text: 'look at invoices.ts' })
  await settle()
  assert.equal(calls.length, 0)
  assert.deepEqual(chat(core, topic), [
    ['note', 'it double-charges'],
    ['note', 'look at invoices.ts'],
  ])

  const sent = await core.actions.topicDispatch({ topic })
  assert.equal(sent.error, null)
  // A placeholder answer, running, straight away.
  assert.ok(sent.events.some((event) => event.type === 'value' && event.key === 'running' && event.value === true))
  await settle()
  assert.deepEqual(calls.map((call) => call.args[1]), ['it double-charges\n\nlook at invoices.ts'])
  // No directory: a new temporary one, kept, the conversation started there.
  const cwd = String(core.item(topic)!.data.cwd)
  assert.ok(existsSync(cwd))
  assert.equal(calls[0].cwd, cwd)
  assert.deepEqual(calls[0].args.slice(calls[0].args.indexOf('--session-id'), calls[0].args.indexOf('--session-id') + 2), ['--session-id', topic])
  assert.equal(core.item(topic)!.data.started, true)
  assert.deepEqual(chat(core, topic).at(-1), ['claude.response', 're: it double-charges\n\nlook at invoices.ts'])

  assert.equal((await core.actions.topicDispatch({ topic })).error, 'Nothing new to send')
  await core.actions.submit({ id: topic, text: 'and refunds?' })
  await core.actions.topicDispatch({ topic })
  await settle()
  assert.deepEqual(calls.map((call) => call.args[1]), ['it double-charges\n\nlook at invoices.ts', 'and refunds?'])
  assert.equal(calls[1].cwd, cwd)
  assert.ok(calls[1].args.includes('--resume'))
  assert.deepEqual(chat(core, topic).map(([type]) => type), ['note', 'note', 'claude.response', 'note', 'claude.response'])
})

test('topics: a dispatch runs in the topic’s own directory when it has one; the list is newest first', async () => {
  const calls: { args: string[]; cwd: string }[] = []
  let now = 1000
  const core = memoryCore({ run: fakeClaude(calls), now: () => (now += 10) })
  await core.actions.topicCreate({ name: 'old' })
  await core.actions.topicCreate({ name: 'new' })
  const [old] = core.entity('topics').outboundLinks
  assert.deepEqual(core.focus('topics').children.map((topic) => topic.data.text), ['new', 'old'])
  core.actions.setValue({ id: old, key: 'cwd', value: '/tmp' })
  await core.actions.submit({ id: old, text: 'hi' })
  await core.actions.topicDispatch({ topic: old })
  await settle()
  assert.equal(calls[0].cwd, '/tmp')
  assert.deepEqual(core.focus('topics').children.map((topic) => topic.data.text), ['old', 'new'])
})

test('topics: no second dispatch while Claude is answering; what is written meanwhile waits for the next', async () => {
  let answer: (() => void) | null = null
  const calls: string[] = []
  const run = (command: string, args: string[]) => {
    if (!command.endsWith('claude')) throw new Error(`unexpected ${command}`)
    calls.push(args[1])
    return new Promise<string>((resolve) => (answer = () => resolve(JSON.stringify({ result: 'done', is_error: false }))))
  }
  const core = memoryCore({ run })
  await core.actions.topicCreate({ name: 'Slow' })
  const [topic] = core.entity('topics').outboundLinks
  await core.actions.submit({ id: topic, text: 'first' })
  assert.equal((await core.actions.topicDispatch({ topic })).error, null)
  await settle()
  await core.actions.submit({ id: topic, text: 'second' })
  assert.equal((await core.actions.topicDispatch({ topic })).error, 'Claude is still answering')
  assert.deepEqual(chat(core, topic).map(([type]) => type), ['note', 'claude.response', 'note'])
  answer!()
  await settle()
  assert.equal((await core.actions.topicDispatch({ topic })).error, null)
  await settle()
  assert.deepEqual(calls, ['first', 'second'])
})

test('topics: a paste is what it is: a PR, Slack message or branch link is that item; other text a message; a file kept, as an item; all go with the next dispatch', async () => {
  const calls: { args: string[]; cwd: string }[] = []
  const dataDir = mkdtempSync(join(tmpdir(), 'topics-data-'))
  const core = memoryCore({ run: fakeClaude(calls), dataDir })
  await core.actions.topicCreate({ name: 'Pasting' })
  const [topic] = core.entity('topics').outboundLinks
  const paste = async (text: string) => assert.equal((await core.actions.topicPaste({ topic, text })).error, null)
  await paste(' https://github.com/me/app/pull/12/files ')
  await paste('https://acme.slack.com/archives/C0123ABC/p1712345678000100?thread_ts=1712345600.000200&cid=C0123ABC')
  await paste('https://github.com/me/app/tree/claude/fix-it')
  await paste('https://example.com/docs')
  await paste('just some text')
  const attached = await core.actions.topicAttach({ topic, file: { name: '../notes.txt', mime: 'text/plain', data: Buffer.from('hello').toString('base64') } })
  assert.equal(attached.error, null)

  const children = core.entity(topic).outboundLinks
  assert.deepEqual(children.slice(0, 3), ['github:pr:https://github.com/me/app/pull/12', 'slack:msg:C0123ABC:1712345678.000100', 'github:branch:me/app:claude/fix-it'])
  assert.deepEqual(children.slice(0, 3).map((id) => core.item(id)?.type), ['github.pr', 'slack.message', 'github.branch'])
  assert.deepEqual(children.slice(3, 5).map((id) => [core.item(id)!.type, core.item(id)!.data.text]), [
    ['note', 'https://example.com/docs'],
    ['note', 'just some text'],
  ])
  const file = core.item(children[5])!
  assert.equal(file.type, 'file')
  assert.equal(file.data.text, 'notes.txt')
  assert.equal(file.data.size, 5)
  assert.ok(String(file.data.path).startsWith(dataDir))
  assert.equal(readFileSync(String(file.data.path), 'utf8'), 'hello')
  // Presented, as the views see it: all from its id.
  const branch = core.focus('github:branch:me/app:claude/fix-it').entity!
  assert.equal(branch.data.text, 'claude/fix-it')
  assert.equal(branch.data.url, 'https://github.com/me/app/tree/claude/fix-it')

  await core.actions.topicDispatch({ topic })
  await settle()
  assert.equal(
    calls[0].args[1],
    [
      '[Item: github:pr:https://github.com/me/app/pull/12]',
      '[Item: slack:msg:C0123ABC:1712345678.000100]',
      '[Item: github:branch:me/app:claude/fix-it]',
      'https://example.com/docs',
      'just some text',
      `[File: ${String(file.data.path)}]`,
    ].join('\n\n'),
  )
})
