import assert from 'node:assert/strict'
import { mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { test } from 'node:test'
import { memoryCore } from '../../testing.ts'

/** git and claude, answering from a script; every call recorded. */
function fakeRun(calls: { command: string; args: string[]; cwd: string }[]) {
  return async (command: string, args: string[], cwd: string) => {
    calls.push({ command, args, cwd })
    if (command === 'git' && args[0] === 'rev-parse' && args[1] === '--show-toplevel') return '/home/me/repos/app\n'
    if (command === 'git' && args[0] === 'worktree') return ''
    if (command === 'git' && args[0] === 'rev-parse') return 'claude/fix-it-abc\n'
    if (command === 'git' && args[0] === 'remote') return 'git@github.com:o/app.git\n'
    if (command.endsWith('claude')) return JSON.stringify({ result: `answer to ${args[1]}`, is_error: false, total_cost_usd: 0.01 })
    throw new Error(`unexpected ${command} ${args.join(' ')}`)
  }
}

const settle = () => new Promise((resolve) => setTimeout(resolve, 20))

test('claude: a session in a new worktree, prompted twice, links its PR; each answer under its prompt', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const gh = async () => JSON.stringify({ data: { repository: { pullRequests: { nodes: [{ url: 'https://github.com/o/app/pull/9' }] } } } })
  const core = memoryCore({ run: fakeRun(calls), gh, dataDir: '/data' })
  core.actions.create({ parent: 'tasks', text: 'fix the thing' })
  const note = core.entity('tasks').outboundLinks[0]

  const src = mkdtempSync(join(tmpdir(), 'repo-src-'))
  const made = await core.actions.claudeCreate({ name: 'Fix it!', cwd: src, worktree: true, attachTo: note })
  assert.equal(made.error, null)
  const session = core.entity(note).values.claudeSessionId as string
  const data = core.item(session)!.data
  assert.equal(core.item(session)!.type, 'claude.session')
  assert.match(String(data.branch), /^claude\/fix-it-[0-9a-f]{8}$/)
  assert.match(String(data.worktree), /^\/data\/worktrees\/app-[0-9a-f]{8}$/)
  assert.equal(data.cwd, data.worktree)
  assert.deepEqual(calls[1].args, ['worktree', 'add', '-b', data.branch, data.worktree])
  assert.ok(core.entity(note).outboundLinks.includes(session))
  assert.ok(core.entity('claude').outboundLinks.includes(session))
  // The directory is remembered for next time.
  assert.deepEqual(core.item('claude')!.data.cwds, [src])

  // k: the prompt goes under the item it was asked from and under the session; the answer under the prompt.
  const first = core.actions.claudePrompt({ session, parent: note, text: 'hello' })
  const prompt = first.events.find((e) => e.type === 'value' && e.key === 'type' && e.value === 'claude.prompt')!
  const promptId = prompt.type === 'value' ? prompt.entityId : ''
  assert.ok(core.entity(note).outboundLinks.includes(promptId))
  assert.ok(core.entity(session).outboundLinks.includes(promptId))
  await settle()
  const [response] = core.entity(promptId).outboundLinks
  assert.equal(core.item(response)!.data.text, 'answer to hello')
  assert.equal(core.item(response)!.data.running, false)
  const claudeCall = calls.find((call) => call.command.endsWith('claude'))!
  assert.equal(claudeCall.cwd, data.worktree)
  // Opus 5.5, in auto mode.
  assert.deepEqual(claudeCall.args.slice(4, 8), ['--model', 'claude-opus-5-5', '--permission-mode', 'auto'])
  assert.deepEqual(claudeCall.args.slice(-2), ['--session-id', session])

  // The second prompt resumes the same session.
  core.actions.claudePrompt({ session, parent: note, text: 'again' })
  await settle()
  assert.deepEqual(calls.filter((call) => call.command.endsWith('claude'))[1].args.slice(-2), ['--resume', session])

  // The worktree's branch has a PR: it is linked under the session, once.
  assert.equal(core.entity(session).outboundLinks.filter((id) => id === 'github:pr:https://github.com/o/app/pull/9').length, 1)
  // Claude's writes aren't mine to undo.
  const undone = core.actions.undo().events
  assert.ok(!undone.some((e) => e.author === 'claude'))
})

test('claude: no directory is a new temporary one; a plain directory is used as it is', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const core = memoryCore({ run: fakeRun(calls) })
  await core.actions.claudeCreate({ name: 'scratch', cwd: '', worktree: false, attachTo: 'claude' })
  const [session] = core.entity('claude').outboundLinks
  assert.match(String(core.item(session)!.data.cwd), /claude-/)
  assert.equal(core.item(session)!.data.worktree, null)
  assert.equal(core.item('claude')!.data.claudeSessionId, undefined)
  await core.actions.claudeCreate({ name: 'here', cwd: '/tmp', worktree: false, attachTo: 'claude' })
  const here = core.entity('claude').outboundLinks.find((id) => core.item(id)!.data.text === 'here')!
  assert.equal(core.item(here)!.data.cwd, '/tmp')
  // A directory that isn't there is refused, saying which; ~ is the home directory.
  const missing = await core.actions.claudeCreate({ name: 'typo', cwd: '~/repos/no-such-thing', worktree: false, attachTo: 'claude' })
  assert.match(missing.error ?? '', /^No such directory: \/.+\/repos\/no-such-thing$/)
  assert.equal(calls.length, 0)
})

test('claude: a failure is said in place of the answer', async () => {
  const run = async () => {
    throw new Error('claude: not logged in')
  }
  const core = memoryCore({ run })
  await core.actions.claudeCreate({ name: 'x', cwd: '/tmp', worktree: false, attachTo: 'claude' })
  const [session] = core.entity('claude').outboundLinks
  const made = core.actions.claudePrompt({ session, parent: session, text: 'hello' })
  await settle()
  const prompt = made.events.find((e) => e.type === 'value' && e.key === 'type' && e.value === 'claude.prompt')!
  const [response] = core.entity(prompt.type === 'value' ? prompt.entityId : '').outboundLinks
  assert.deepEqual([core.item(response)!.data.text, core.item(response)!.data.error, core.item(response)!.data.running], [
    'claude: not logged in',
    'claude: not logged in',
    false,
  ])
})

test('claude: running a program says where it failed and why, and tells a missing directory from a missing program', async () => {
  const { runCommand } = await import('../../core.ts')
  await assert.rejects(runCommand('ls', [], '/no/such/dir'), /ls failed in \/no\/such\/dir: the directory doesn't exist: \/no\/such\/dir/)
  await assert.rejects(runCommand('no-such-program-x', [], '/tmp'), /no-such-program-x failed in \/tmp: no-such-program-x isn't installed/)
  await assert.rejects(runCommand('sh', ['-c', 'echo oops >&2; exit 3'], '/tmp'), /sh failed in \/tmp: exit code 3\n\noops/)
})
