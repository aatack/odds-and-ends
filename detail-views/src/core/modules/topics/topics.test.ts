import assert from 'node:assert/strict'
import { existsSync, mkdirSync, mkdtempSync, readFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { test } from 'node:test'
import { memoryCore } from '../../testing.ts'
import { commitPrompt } from './topics.ts'

const settle = () => new Promise((resolve) => setTimeout(resolve, 20))

/** Waits (up to 2 s) until `done` holds: for chains of git and claude calls longer than a settle. */
async function until(done: () => boolean): Promise<void> {
  for (let waited = 0; !done() && waited < 2000; waited += 10) await new Promise((resolve) => setTimeout(resolve, 10))
}

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

/** claude, and a git answering from `state`; every call recorded. */
function fakeRepo(state: { dirty: string[]; branch: string; top?: string; listed?: string[]; unpushed?: string; unmerged?: string }, calls: { command: string; args: string[]; cwd: string }[]) {
  return async (command: string, args: string[], cwd: string) => {
    calls.push({ command, args, cwd })
    if (command.endsWith('claude')) return JSON.stringify({ result: 'ok', is_error: false })
    const verb = args.join(' ')
    if (verb === 'status --porcelain') return state.dirty.shift() ?? ''
    if (verb === 'rev-parse --abbrev-ref HEAD') return `${state.branch}\n`
    if (verb === 'rev-parse --show-toplevel') return `${state.top ?? cwd}\n`
    if (verb === 'symbolic-ref --short refs/remotes/origin/HEAD') return 'origin/main\n'
    if (verb === 'remote get-url origin') return 'git@github.com:me/app.git\n'
    if (verb === 'worktree list --porcelain') return (state.listed ?? []).map((path) => `worktree ${path}\nHEAD abc\n`).join('\n')
    if (verb === 'log --oneline HEAD --not --remotes') return state.unpushed ?? ''
    if (verb === 'log --oneline origin/main..HEAD') return state.unmerged ?? ''
    if (args[0] === 'worktree' && args[1] === 'remove') return ''
    throw new Error(`unexpected ${command} ${verb}`)
  }
}

test('topics: an answer that leaves changes uncommitted is followed once by a prompt to commit them; the branch checked out goes into the chat, once', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const state = { dirty: [' M app.ts\n', ''], branch: 'feature/x' }
  const core = memoryCore({ run: fakeRepo(state, calls) })
  await core.actions.topicCreate({ name: 'Work' })
  const [topic] = core.entity('topics').outboundLinks
  await core.actions.submit({ id: topic, text: 'change it' })
  await core.actions.topicDispatch({ topic })
  await until(() => core.entity(topic).outboundLinks.includes('github:branch:me/app:feature/x'))
  const prompts = calls.filter((call) => call.command.endsWith('claude')).map((call) => call.args[1])
  assert.deepEqual(prompts, ['change it', commitPrompt])
  assert.deepEqual(chat(core, topic), [
    ['note', 'change it'],
    ['claude.response', 'ok'],
    ['note', commitPrompt],
    ['claude.response', 'ok'],
    ['github.branch', 'undefined'],
  ])
  assert.equal(core.entity(topic).outboundLinks.at(-1), 'github:branch:me/app:feature/x')
  assert.equal(core.item(topic)!.data.branch, 'feature/x')
  assert.equal(core.item(topic)!.data.github, 'me/app')

  // Clean now: no follow-up; the branch is there already.
  await core.actions.submit({ id: topic, text: 'more' })
  await core.actions.topicDispatch({ topic })
  // Over once the default branch has been asked for (after the status and the answer).
  const asked = () => calls.filter((call) => call.args[0] === 'symbolic-ref').length
  const before = asked()
  await until(() => asked() > before)
  await settle()
  assert.deepEqual(chat(core, topic).slice(5).map(([type]) => type), ['note', 'claude.response'])

  // The default branch is not added.
  state.branch = 'main'
  const other = (await core.actions.topicCreate({ name: 'Main' })).events.find((event) => event.type === 'link')!
  const id = other.type === 'link' ? other.destinationId : ''
  await core.actions.submit({ id, text: 'hi' })
  const seen = asked()
  await core.actions.topicDispatch({ topic: id })
  await until(() => asked() > seen)
  await settle()
  assert.deepEqual(chat(core, id).map(([type]) => type), ['note', 'claude.response'])
})

test('topics: resolving takes a topic out of the list, and removes its worktree only when it is provably one, warning first of work that would be lost', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const dataDir = mkdtempSync(join(tmpdir(), 'topics-data-'))
  const tree = join(dataDir, 'worktrees', 'app-1234')
  mkdirSync(tree, { recursive: true })
  const state = { dirty: [] as string[], branch: 'claude/x', listed: [tree], unpushed: 'abc one\n' }
  const core = memoryCore({ run: fakeRepo(state, calls), dataDir })
  const make = async (name: string, fields: Record<string, unknown>) => {
    const made = (await core.actions.topicCreate({ name })).events.find((event) => event.type === 'link')!
    const id = made.type === 'link' ? made.destinationId : ''
    for (const [key, v] of Object.entries(fields)) core.actions.setValue({ id, key, value: v })
    return id
  }
  const removes = () => calls.filter((call) => call.args[0] === 'worktree' && call.args[1] === 'remove').map((call) => call.args)

  // A plain directory: out of the list, the directory untouched.
  const plain = await make('Plain', { cwd: dataDir })
  assert.deepEqual((await core.actions.topicResolveCheck({ topic: plain })).warnings, [])
  assert.equal((await core.actions.topicResolve({ topic: plain, force: false })).error, null)
  assert.ok(!core.entity('topics').outboundLinks.includes(plain))
  assert.ok(core.item(plain)!.data.resolved)
  assert.deepEqual(removes(), [])

  // A worktree outside the app's own worktrees: not provably one, so left alone.
  const elsewhere = await make('Elsewhere', { cwd: dataDir, worktree: dataDir, repo: '/repo' })
  assert.equal((await core.actions.topicResolve({ topic: elsewhere, force: true })).error, null)
  assert.deepEqual(removes(), [])

  // A worktree the repo doesn't list: left alone too.
  const unlisted = await make('Unlisted', { cwd: tree, worktree: tree, repo: '/repo' })
  state.listed = []
  assert.equal((await core.actions.topicResolve({ topic: unlisted, force: true })).error, null)
  assert.deepEqual(removes(), [])

  // A proven worktree with an unpushed commit: a warning, refused, then forced.
  state.listed = [tree]
  const proven = await make('Proven', { cwd: tree, worktree: tree, repo: '/repo' })
  state.dirty = [' M a.ts\n']
  assert.deepEqual((await core.actions.topicResolveCheck({ topic: proven })).warnings, [`Uncommitted changes in ${tree}`, '1 commit not pushed'])
  state.dirty = [' M a.ts\n']
  assert.match(String((await core.actions.topicResolve({ topic: proven, force: false })).error), /not pushed/)
  assert.ok(core.entity('topics').outboundLinks.includes(proven))
  assert.deepEqual(removes(), [])
  assert.equal((await core.actions.topicResolve({ topic: proven, force: true })).error, null)
  assert.deepEqual(removes(), [['worktree', 'remove', '--force', tree]])
  assert.equal(calls.find((call) => call.args[1] === 'remove')!.cwd, '/repo')
  assert.ok(!core.entity('topics').outboundLinks.includes(proven))

  // Nothing to lose: removed without being forced.
  const clean = await make('Clean', { cwd: tree, worktree: tree, repo: '/repo' })
  state.unpushed = ''
  assert.deepEqual((await core.actions.topicResolveCheck({ topic: clean })).warnings, [])
  assert.equal((await core.actions.topicResolve({ topic: clean, force: false })).error, null)
  assert.deepEqual(removes().at(-1), ['worktree', 'remove', tree])
})
