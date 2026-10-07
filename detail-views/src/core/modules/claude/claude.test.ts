import assert from 'node:assert/strict'
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
    if (command === 'claude') return JSON.stringify({ result: `answer to ${args[1]}`, is_error: false, total_cost_usd: 0.01 })
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

  const made = await core.actions.claudeCreate({ name: 'Fix it!', cwd: '/home/me/repos/app/src', worktree: true, attachTo: note })
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
  assert.deepEqual(core.item('claude')!.data.cwds, ['/home/me/repos/app/src'])

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
  const claudeCall = calls.find((call) => call.command === 'claude')!
  assert.equal(claudeCall.cwd, data.worktree)
  assert.deepEqual(claudeCall.args.slice(-2), ['--session-id', session])

  // The second prompt resumes the same session.
  core.actions.claudePrompt({ session, parent: note, text: 'again' })
  await settle()
  assert.deepEqual(calls.filter((call) => call.command === 'claude')[1].args.slice(-2), ['--resume', session])

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
  await core.actions.claudeCreate({ name: 'here', cwd: '/somewhere', worktree: false, attachTo: 'claude' })
  const here = core.entity('claude').outboundLinks.find((id) => core.item(id)!.data.text === 'here')!
  assert.equal(core.item(here)!.data.cwd, '/somewhere')
  assert.equal(calls.length, 0)
})
