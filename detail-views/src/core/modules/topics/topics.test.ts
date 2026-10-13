import assert from 'node:assert/strict'
import { existsSync, mkdirSync, mkdtempSync, readFileSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { test } from 'node:test'
import { Core } from '../../core.ts'
import { link, values } from '../../graph/events.ts'
import { fakeSlack, memoryCore } from '../../testing.ts'
import { interrupted } from '../claude/claude.ts'
import { commitPrompt } from './topics.ts'
import { sendTargets, waitCandidates, waiting, waitsOf } from './view.ts'

const settle = () => new Promise((resolve) => setTimeout(resolve, 20))

/** Waits (up to 10 s) until `done` holds: for chains of git and claude calls longer than a settle. */
async function until(done: () => boolean): Promise<void> {
  for (let waited = 0; !done() && waited < 10_000; waited += 10) await new Promise((resolve) => setTimeout(resolve, 10))
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
function fakeRepo(state: { dirty: string[]; branch: string; remote?: string; worktree?: boolean; top?: string; listed?: string[]; unpushed?: string; unmerged?: string }, calls: { command: string; args: string[]; cwd: string }[]) {
  return async (command: string, args: string[], cwd: string) => {
    calls.push({ command, args, cwd })
    if (command.endsWith('claude')) return JSON.stringify({ result: 'ok', is_error: false })
    const verb = args.join(' ')
    if (verb === 'status --porcelain') return state.dirty.shift() ?? ''
    if (verb === 'rev-parse --abbrev-ref HEAD') return `${state.branch}\n`
    // A worktree's git directory is its own; a plain checkout's is the repo's.
    if (verb === 'rev-parse --absolute-git-dir') return state.worktree ? '/repo/.git/worktrees/x\n' : '/repo/.git\n'
    if (verb === 'rev-parse --path-format=absolute --git-common-dir') return '/repo/.git\n'
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

test('topics: an answer that leaves changes uncommitted is followed once by a prompt to commit them; in a worktree, the branch\'s PR goes into the chat and the topic\'s context, once', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const state = { dirty: [' M app.ts\n', ''], branch: 'feature/x', worktree: true }
  const asked: string[][] = []
  const prs: Record<string, string> = { 'feature/x': 'https://github.com/me/app/pull/7' }
  const gh = async (args: string[]) => {
    asked.push(args)
    const branch = args.find((arg) => arg.startsWith('branch='))!.slice('branch='.length)
    return JSON.stringify({ data: { repository: { pullRequests: { nodes: prs[branch] ? [{ url: prs[branch] }] : [] } } } })
  }
  const core = memoryCore({ run: fakeRepo(state, calls), gh })
  const pr = 'github:pr:https://github.com/me/app/pull/7'
  await core.actions.topicCreate({ name: 'Work' })
  const [topic] = core.entity('topics').outboundLinks
  await core.actions.submit({ id: topic, text: 'change it' })
  await core.actions.topicDispatch({ topic })
  await until(() => Boolean(core.item(topic)!.data.context))
  // The PR is the last write, a moment before the dispatch is over (and the next one allowed).
  await settle()
  const prompts = calls.filter((call) => call.command.endsWith('claude')).map((call) => call.args[1])
  assert.deepEqual(prompts, ['change it', commitPrompt])
  assert.deepEqual(core.entity(topic).outboundLinks.slice(2, 4).map((id) => core.item(id)!.data.text), [commitPrompt, 'ok'])
  assert.equal(core.entity(topic).outboundLinks.at(-1), pr)
  // Read-only: a query, its variables as strings.
  assert.ok(asked[0].includes('-f') && !asked[0].includes('-F') && !/mutation/.test(asked[0].join(' ')))
  assert.ok(asked[0].includes('owner=me') && asked[0].includes('name=app') && asked[0].includes('branch=feature/x'))
  // In one context with the topic.
  const context = String(core.item(topic)!.data.context)
  assert.equal(core.item(pr)!.data.context, context)
  assert.deepEqual(core.entity(context).outboundLinks.sort(), [topic, pr].sort())

  // Clean now: no follow-up; the PR is there already, not added again.
  const looked = () => asked.length
  await core.actions.submit({ id: topic, text: 'more' })
  assert.equal((await core.actions.topicDispatch({ topic })).error, null)
  const before = looked()
  await until(() => looked() > before)
  await settle()
  assert.equal(core.entity(topic).outboundLinks.filter((id) => id === pr).length, 1)
  assert.deepEqual(core.entity(topic).outboundLinks.slice(5).map((id) => core.item(id)!.type), ['note', 'claude.response'])

  // A branch with no PR (only here, or not pushed), and a checkout that isn't a worktree: nothing.
  const fresh = async (name: string) => {
    const made = (await core.actions.topicCreate({ name })).events.find((event) => event.type === 'link')!
    return made.type === 'link' ? made.destinationId : ''
  }
  state.branch = 'local-only'
  const local = await fresh('Local')
  await core.actions.submit({ id: local, text: 'hi' })
  const seen = looked()
  await core.actions.topicDispatch({ topic: local })
  await until(() => looked() > seen)
  await settle()
  assert.deepEqual(core.entity(local).outboundLinks.map((id) => core.item(id)!.type), ['note', 'claude.response'])
  assert.equal(core.item(local)!.data.context, undefined)

  state.branch = 'feature/x'
  state.worktree = false
  const plain = await fresh('Plain')
  await core.actions.submit({ id: plain, text: 'hi' })
  await core.actions.topicDispatch({ topic: plain })
  await until(() => calls.filter((call) => call.args.includes('--git-common-dir')).length >= 4)
  await settle()
  assert.equal(looked(), seen + 1)
  assert.deepEqual(core.entity(plain).outboundLinks.map((id) => core.item(id)!.type), ['note', 'claude.response'])
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
  assert.deepEqual((await core.actions.topicResolveCheck({ topic: proven })).warnings, [`Uncommitted changes in ${tree}`, `1 commit not pushed in ${tree}`])
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

test('topics: after a restart, an answer cut off is waited for while its claude runs, then read from its transcript; without one it says it was interrupted', async () => {
  const dir = mkdtempSync(join(tmpdir(), 'recover-'))
  const owned = join(dir, 'owned.sqlite')
  const home = process.env.CLAUDE_CONFIG_DIR
  process.env.CLAUDE_CONFIG_DIR = join(dir, 'claude-home')
  try {
    // The app stops while Claude answers: its claude never comes back to it.
    const before = new Core({ owned, cache: ':memory:', run: () => new Promise<string>(() => {}) })
    await before.actions.topicCreate({ name: 'Answered' })
    await before.actions.topicCreate({ name: 'Lost' })
    const named = (name: string) => before.entity('topics').outboundLinks.find((id) => before.item(id)!.data.text === name)!
    const [answered, lost] = [named('Answered'), named('Lost')]
    for (const topic of [answered, lost]) {
      await before.actions.submit({ id: topic, text: 'go' })
      assert.equal((await before.actions.topicDispatch({ topic })).error, null)
    }
    const responseOf = (core: Core, topic: string) => core.entity(topic).outboundLinks.find((id) => core.item(id)!.type === 'claude.response')!

    // Claude Code wrote the answer to its transcript, in the topic's directory's project folder.
    const cwd = String(before.item(answered)!.data.cwd)
    const project = join(process.env.CLAUDE_CONFIG_DIR, 'projects', cwd.replace(/[^a-zA-Z0-9]/g, '-'))
    mkdirSync(project, { recursive: true })
    const at = (ms: number) => new Date(Date.now() + ms).toISOString()
    const entries = [
      { type: 'user', timestamp: at(1), message: { content: 'go' } },
      { type: 'assistant', timestamp: at(2), message: { id: 'm1', stop_reason: 'tool_use', content: [{ type: 'tool_use' }] } },
      { type: 'user', timestamp: at(3), message: { content: [{ type: 'tool_result' }] } },
      { type: 'assistant', timestamp: at(4), message: { id: 'm2', stop_reason: 'end_turn', content: [{ type: 'text', text: 'All ' }] } },
      { type: 'assistant', timestamp: at(4), message: { id: 'm2', stop_reason: 'end_turn', content: [{ type: 'text', text: 'done.' }] } },
      { type: 'cost-state' },
    ]
    writeFileSync(join(project, `${answered}.jsonl`), entries.map((entry) => JSON.stringify(entry)).join('\n'))

    // Started again: the first answer's claude is still running for a moment.
    let looks = 0
    const after = new Core({
      owned,
      cache: ':memory:',
      run: async (command) => {
        if (command !== 'ps') throw new Error(`unexpected ${command}`)
        looks += 1
        return looks <= 2 ? `/usr/bin/claude -p go --resume ${answered}\n` : 'bash\n'
      },
    })
    await after.recoverAnswers(1)
    assert.ok(looks > 2)
    const done = after.item(responseOf(after, answered))!.data
    assert.equal(done.text, 'All done.')
    assert.equal(done.running, false)
    assert.equal(done.error, null)
    assert.equal(after.item(answered)!.data.runningSince, null)
    const cut = after.item(responseOf(after, lost))!.data
    assert.equal(cut.running, false)
    assert.equal(cut.error, interrupted)
    // Neither topic is answering now: it can be sent to again.
    await after.actions.submit({ id: lost, text: 'again' })
    assert.equal((await after.actions.topicDispatch({ topic: lost })).error, null)
  } finally {
    if (home === undefined) delete process.env.CLAUDE_CONFIG_DIR
    else process.env.CLAUDE_CONFIG_DIR = home
  }
})

test('topics: a path is an item under the topic; each dispatch runs in the newest one, the conversation moving with it; Resolve removes every proven worktree among them', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const dataDir = mkdtempSync(join(tmpdir(), 'topics-data-'))
  const first = mkdtempSync(join(tmpdir(), 'path-a-'))
  const second = mkdtempSync(join(tmpdir(), 'path-b-'))
  const state = { dirty: [] as string[], branch: 'main', listed: [] as string[] }
  const run = fakeRepo(state, calls)
  const core = memoryCore({
    dataDir,
    run: async (command, args, cwd) => {
      // A worktree is made on disk, as git would.
      if (args[0] === 'worktree' && args[1] === 'add') {
        mkdirSync(args[5], { recursive: true })
        state.listed.push(args[5])
        return ''
      }
      if (args[0] === 'fetch') return ''
      if (args[1] === '--verify') throw new Error('no such branch')
      return run(command, args, cwd)
    },
  })
  // Made with its first path.
  const made = await core.actions.topicCreate({ name: 'Paths', cwd: first, worktree: false })
  assert.equal(made.error, null)
  const [topic] = core.entity('topics').outboundLinks
  const pathItems = () => core.entity(topic).outboundLinks.filter((id) => core.item(id)?.type === 'path')
  assert.equal(pathItems().length, 1)
  assert.equal(core.item(pathItems()[0])!.data.text, first)
  assert.deepEqual(core.item('claude')!.data.cwds, [first])
  // A path alone is nothing to send.
  assert.equal((await core.actions.topicDispatch({ topic })).error, 'Nothing new to send')

  await core.actions.submit({ id: topic, text: 'one' })
  await core.actions.topicDispatch({ topic })
  await until(() => calls.some((call) => call.command.endsWith('claude')))
  await settle()
  assert.equal(calls.find((call) => call.command.endsWith('claude'))!.cwd, first)

  // Claude Code kept the conversation's transcript where it ran (in a config directory of the test's own).
  const config = mkdtempSync(join(tmpdir(), 'claude-config-'))
  const previous = process.env.CLAUDE_CONFIG_DIR
  process.env.CLAUDE_CONFIG_DIR = config
  mkdirSync(join(config, 'projects', 'somewhere'), { recursive: true })
  writeFileSync(join(config, 'projects', 'somewhere', `${topic}.jsonl`), '{}\n')

  // A second path, then a worktree of it: the newest wins.
  assert.equal((await core.actions.topicAddPath({ topic, cwd: second, worktree: false })).error, null)
  assert.equal((await core.actions.topicAddPath({ topic, cwd: second, worktree: true })).error, null)
  const tree = core.item(pathItems()[2])!.data
  assert.ok(String(tree.cwd).startsWith(join(dataDir, 'worktrees')))
  assert.equal(tree.origin, second)
  await core.actions.submit({ id: topic, text: 'two' })
  await core.actions.topicDispatch({ topic })
  await until(() => calls.filter((call) => call.command.endsWith('claude')).length === 2)
  await settle()
  const claude = calls.filter((call) => call.command.endsWith('claude'))
  assert.equal(claude[1].cwd, tree.cwd)
  assert.ok(claude[1].args.includes('--resume'))
  // The transcript went with it, to where Claude Code looks from there.
  assert.ok(existsSync(join(config, 'projects', String(tree.cwd).replace(/[^a-zA-Z0-9]/g, '-'), `${topic}.jsonl`)))
  if (previous === undefined) delete process.env.CLAUDE_CONFIG_DIR
  else process.env.CLAUDE_CONFIG_DIR = previous
  assert.equal(core.item(topic)!.data.cwd, tree.cwd)
  assert.equal(core.item(topic)!.data.worktree, tree.cwd)

  // No such directory: refused, nothing added.
  assert.match(String((await core.actions.topicAddPath({ topic, cwd: join(first, 'nope'), worktree: false })).error), /No such directory/)
  assert.equal(pathItems().length, 3)

  // Resolve removes the worktree (proven), and leaves the plain directories alone.
  assert.equal((await core.actions.topicResolve({ topic, force: false })).error, null)
  const removed = calls.filter((call) => call.args[0] === 'worktree' && call.args[1] === 'remove').map((call) => call.args.at(-1))
  assert.deepEqual(removed, [tree.cwd])
  assert.equal(core.item(pathItems()[2])!.data.worktree, null)
})

test('topics: Check out puts the worktree\'s commit in its repo, Master the default branch, Push the branch to GitHub; each refuses what could lose or misplace work', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const dataDir = mkdtempSync(join(tmpdir(), 'topics-data-'))
  const state = { dirty: [] as string[], branch: 'claude/x', remote: 'git@github.com:me/app.git' }
  const base = fakeRepo(state, calls)
  const core = memoryCore({
    dataDir,
    gh: async () => JSON.stringify({ data: { repository: { pullRequests: { nodes: [] } } } }),
    run: async (command, args, cwd) => {
      const verb = args.join(' ')
      if (verb === 'rev-parse HEAD') return 'abc123\n'
      if (verb === 'remote get-url origin') {
        calls.push({ command, args, cwd })
        return `${state.remote}\n`
      }
      if (args[0] === 'checkout' || args[0] === 'push') {
        calls.push({ command, args, cwd })
        return ''
      }
      return base(command, args, cwd)
    },
  })
  const made = (await core.actions.topicCreate({ name: 'Git' })).events.find((event) => event.type === 'link')!
  const topic = made.type === 'link' ? made.destinationId : ''
  const wrote = (verb: string) => calls.filter((call) => call.args[0] === verb).map((call) => [call.cwd, ...call.args])

  // No path yet: nothing to act on.
  assert.equal((await core.actions.topicPush({ topic })).error, 'No path: choose one first')

  // A plain directory: no worktree to check out; Master in the repo it is in.
  const plain = mkdtempSync(join(tmpdir(), 'plain-'))
  core.actions.setValue({ id: topic, key: 'cwd', value: plain })
  assert.equal((await core.actions.topicCheckOut({ topic })).error, 'No worktree to check out')
  assert.equal((await core.actions.topicCheckOutDefault({ topic })).error, null)
  assert.deepEqual(wrote('checkout'), [[plain, 'checkout', 'main']])

  // A worktree path: its commit, detached, in its repo; Master there too; refused over uncommitted changes.
  const tree = join(dataDir, 'worktrees', 'app-1')
  mkdirSync(tree, { recursive: true })
  for (const [key, v] of Object.entries({ type: 'path', text: tree, cwd: tree, worktree: tree, repo: '/repo', origin: '/repo' })) core.actions.setValue({ id: 'p1', key, value: v })
  core.actions.link({ parent: topic, child: 'p1' })
  assert.equal((await core.actions.topicCheckOut({ topic })).error, null)
  assert.deepEqual(wrote('checkout').at(-1), ['/repo', 'checkout', '--detach', 'abc123'])
  state.dirty = [' M x.ts\n']
  assert.match(String((await core.actions.topicCheckOutDefault({ topic })).error), /Uncommitted changes in \/repo/)
  assert.equal(wrote('checkout').length, 2)

  // Push: from the worktree, the branch to origin, made if new.
  assert.equal((await core.actions.topicPush({ topic })).error, null)
  assert.deepEqual(wrote('push'), [[tree, 'push', '-u', 'origin', 'claude/x']])
  // Never the default branch, a detached HEAD, or a remote not on GitHub.
  state.branch = 'main'
  assert.match(String((await core.actions.topicPush({ topic })).error), /Won't push main/)
  state.branch = 'HEAD'
  assert.match(String((await core.actions.topicPush({ topic })).error), /Not on a branch/)
  state.branch = 'claude/x'
  state.remote = 'git@gitlab.com:me/app.git'
  assert.equal((await core.actions.topicPush({ topic })).error, 'origin is not on GitHub')
  assert.equal(wrote('push').length, 1)
})

/** Slack's answers for a token to be kept, and nothing in the lists. */
const slackAuth = {
  'auth.test': () => ({ user_id: 'UME', url: 'https://x.slack.com/' }),
  'users.conversations': () => ({ channels: [] }),
  'conversations.list': () => ({ channels: [] }),
  'users.list': () => ({ members: [] }),
}

/** A Slack message in the cache, linked where the watch would put it. */
function cacheMessage(core: Core, channel: string, ts: string, user: string, thread?: string): string {
  const id = `slack:msg:${channel}:${ts}`
  const at = Number(ts) * 1000
  core.cache.write([
    ...values(id, { type: 'slack.message', channel, ts, user, text: `said at ${ts}`, threadTs: thread ?? null }, at, 'slack'),
    link(thread ? `slack:msg:${channel}:${thread}` : `slack:conv:${channel}`, id, at, 'slack'),
  ])
  return id
}

test('topics: the box sends to Slack only where a Slack item in the chat offers, and only through the one way in', async () => {
  const posted: URLSearchParams[] = []
  const core = memoryCore({
    now: () => 250_000,
    fetch: fakeSlack({
      ...slackAuth,
      'chat.postMessage': (params) => {
        posted.push(params)
        return { ts: '260.000000', message: { ts: '260.000000', user: 'UME', text: params.get('text'), thread_ts: params.get('thread_ts') ?? undefined } }
      },
    }),
  })
  await core.slack.setToken('xoxp-1')
  await core.actions.topicCreate({ name: 'Ask about the outage' })
  const [topic] = core.entity('topics').outboundLinks
  const message = cacheMessage(core, 'C1', '100.000000', 'UOTHER')
  const reply = cacheMessage(core, 'C1', '120.000000', 'UOTHER', '100.000000')
  await core.actions.topicPaste({ topic, text: 'https://x.slack.com/archives/C1/p100000000' })
  assert.deepEqual(core.entity(topic).outboundLinks, [message])
  // A message in a conversation: a reply in its thread, or a message to the conversation.
  assert.deepEqual(sendTargets(topic, core.lens()), [
    { on: message, channel: 'C1', threadTs: '100.000000' },
    { on: 'slack:conv:C1', channel: 'C1' },
  ])
  // A reply in a thread: only its thread, once.
  await core.actions.link({ parent: topic, child: reply })
  assert.equal(sendTargets(topic, core.lens()).length, 2)

  // Not an action: the phone, the MCP server and Claude reach only `actions`.
  assert.ok(!Object.keys(core.actions).some((name) => /slack.*send|send.*slack/i.test(name)))
  // Anything else that writes to Slack is still refused.
  assert.match(String((await core.actions.submit({ id: 'slack:conv:C1', text: 'hi' })).error), /read-only/)
  assert.equal(posted.length, 0)

  assert.equal((await core.topicSlackSend({ topic, on: 'slack:conv:C9', text: 'hi' })).error, 'Nothing in this topic to send that to')
  const sent = await core.topicSlackSend({ topic, on: message, text: 'Any news?' })
  assert.equal(sent.error, null)
  assert.equal(posted.length, 1)
  assert.equal(posted[0].get('channel'), 'C1')
  assert.equal(posted[0].get('thread_ts'), '100.000000')
  // Into the chat, and into the thread as the watch would put it.
  assert.equal(core.entity(topic).outboundLinks.at(-1), 'slack:msg:C1:260.000000')
  assert.ok(core.entity(message).outboundLinks.includes('slack:msg:C1:260.000000'))
  assert.equal(core.item('slack:msg:C1:260.000000')?.data.text, 'Any news?')
})

test('topics: a wait puts a topic down until someone else says something where it waits', async () => {
  const core = memoryCore({ now: () => 250_000, fetch: fakeSlack(slackAuth) })
  await core.slack.setToken('xoxp-1')
  await core.actions.topicCreate({ name: 'Waiting on ops' })
  const [topic] = core.entity('topics').outboundLinks
  const message = cacheMessage(core, 'C1', '100.000000', 'UOTHER')
  await core.actions.link({ parent: topic, child: message })
  assert.equal(waiting(topic, core.lens()), false)

  assert.match(String((await core.actions.topicWait({ topic, on: ['https://example.com'] })).error), /Only a Slack/)
  await core.actions.topicWait({ topic, on: [message, 'slack:conv:C1'] })
  assert.deepEqual(waitsOf(core.item(topic)!.data), [
    { slack: message, since: 250_000 },
    { slack: 'slack:conv:C1', since: 250_000 },
  ])
  assert.equal(waiting(topic, core.lens()), true)

  // Before the wait, mine, and a thread's reply in the conversation's wait: none of them count.
  cacheMessage(core, 'C1', '200.000000', 'UOTHER', '100.000000')
  cacheMessage(core, 'C1', '260.000000', 'UME', '100.000000')
  cacheMessage(core, 'C1', '270.000000', 'UOTHER', '150.000000')
  assert.equal(waiting(topic, core.lens()), true)

  const reply = cacheMessage(core, 'C1', '280.000000', 'UOTHER', '100.000000')
  assert.equal(waiting(topic, core.lens()), false)
  const [thread, channel] = waitsOf(core.item(topic)!.data)
  assert.deepEqual(waitCandidates(topic, thread, core.lens()), [reply])
  assert.deepEqual(waitCandidates(topic, channel, core.lens()), [])

  // Passed over, it waits again; added to the topic, it is no longer offered.
  await core.actions.topicDismiss({ topic, on: message, message: reply })
  assert.equal(waiting(topic, core.lens()), true)
  const later = cacheMessage(core, 'C1', '290.000000', 'UOTHER')
  assert.equal(waiting(topic, core.lens()), false)
  await core.actions.link({ parent: topic, child: later })
  assert.equal(waiting(topic, core.lens()), true)

  await core.actions.topicUnwait({ topic, on: message })
  await core.actions.topicUnwait({ topic, on: 'slack:conv:C1' })
  assert.equal(core.item(topic)!.data.wait ?? null, null)
  assert.equal(waiting(topic, core.lens()), false)
})

test('topics: a topic made from another goes into its chat; a topic can wait on another until it is resolved', async () => {
  const core = memoryCore({ now: () => 250_000 })
  await core.actions.topicCreate({ name: 'Release' })
  const [release] = core.entity('topics').outboundLinks
  const made = await core.actions.topicCreate({ name: 'Fix the build', parent: release })
  assert.equal(made.error, null)
  const fix = core.entity('topics').outboundLinks.find((id) => id !== release)!
  // In the list, and in the other topic's chat.
  assert.deepEqual(core.entity(release).outboundLinks, [fix])
  assert.match(String((await core.actions.topicCreate({ name: 'x', parent: 'nope' })).error), /Not a topic/)

  // Waits on it: down until it is resolved.
  assert.match(String((await core.actions.topicWait({ topic: release, on: [release] })).error), /can be waited on/)
  assert.equal((await core.actions.topicWait({ topic: release, on: [fix] })).error, null)
  assert.deepEqual(waitsOf(core.item(release)!.data), [{ topic: fix, since: 250_000 }])
  assert.equal(waiting(release, core.lens()), true)
  assert.equal((await core.actions.topicResolve({ topic: fix, force: false })).error, null)
  assert.equal(waiting(release, core.lens()), false)
  // Nothing to add from a topic's wait; × takes it away.
  assert.deepEqual(waitCandidates(release, waitsOf(core.item(release)!.data)[0], core.lens()), [])
  await core.actions.topicUnwait({ topic: release, on: fix })
  assert.equal(core.item(release)!.data.wait ?? null, null)
})
