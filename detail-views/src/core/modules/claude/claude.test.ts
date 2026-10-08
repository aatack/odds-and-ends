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
    if (command.endsWith('claude')) return JSON.stringify({ result: `answer to ${args[1].split('\n').pop()}`, is_error: false, total_cost_usd: 0.01 })
    throw new Error(`unexpected ${command} ${args.join(' ')}`)
  }
}

const settle = () => new Promise((resolve) => setTimeout(resolve, 20))

/** The session's chat: each child's type and text, in order. */
function chat(core: ReturnType<typeof memoryCore>, session: string): [string, string][] {
  return core.entity(session).outboundLinks.map((id) => [core.item(id)!.type, String(core.item(id)!.data.text)])
}

test('claude: a session in a new worktree, under the Claude root only; each prompt and its answer go under it, in order', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const core = memoryCore({ run: fakeRun(calls), dataDir: '/data' })
  const src = mkdtempSync(join(tmpdir(), 'repo-src-'))
  const made = await core.actions.claudeCreate({ name: 'Fix it!', cwd: src, worktree: true })
  assert.equal(made.error, null)
  const [session] = core.entity('claude').outboundLinks
  const data = core.item(session)!.data
  assert.equal(core.item(session)!.type, 'claude.session')
  assert.match(String(data.branch), /^claude\/fix-it-[0-9a-f]{8}$/)
  assert.match(String(data.worktree), /^\/data\/worktrees\/app-[0-9a-f]{8}$/)
  assert.equal(data.cwd, data.worktree)
  assert.deepEqual(calls[1].args, ['worktree', 'add', '-b', data.branch, data.worktree])
  // Linked nowhere but the Claude root.
  assert.deepEqual(made.events.filter((e) => e.type === 'link').map((e) => e.type === 'link' && e.sourceId), ['claude'])
  // The directory is remembered for next time.
  assert.deepEqual(core.item('claude')!.data.cwds, [src])

  // A prompt and its response both go under the session, the response after it, running until the answer comes.
  core.actions.claudePrompt({ session, text: 'hello' })
  assert.deepEqual(chat(core, session), [['claude.prompt', 'hello'], ['claude.response', '']])
  await settle()
  assert.deepEqual(chat(core, session), [['claude.prompt', 'hello'], ['claude.response', 'answer to hello']])
  assert.equal(core.item(core.entity(session).outboundLinks[1])!.data.running, false)
  const claudeCall = calls.find((call) => call.command.endsWith('claude'))!
  assert.equal(claudeCall.cwd, data.worktree)
  // The prompt is sent as I wrote it; Opus 5.5, in auto mode.
  assert.equal(claudeCall.args[1], 'hello')
  assert.deepEqual(claudeCall.args.slice(4, 8), ['--model', 'claude-opus-5-5', '--permission-mode', 'auto'])
  assert.deepEqual(claudeCall.args.slice(8, 10), ['--session-id', session])
  // No MCP server in a headless core: no flags for one.
  assert.ok(!claudeCall.args.includes('--mcp-config'))

  // The second prompt (through the box under the chat) resumes the same session.
  await core.submit(session, 'again')
  await settle()
  assert.deepEqual(calls.filter((call) => call.command.endsWith('claude'))[1].args.slice(8, 10), ['--resume', session])
  assert.deepEqual(chat(core, session), [
    ['claude.prompt', 'hello'],
    ['claude.response', 'answer to hello'],
    ['claude.prompt', 'again'],
    ['claude.response', 'answer to again'],
  ])
  // Claude's writes aren't mine to undo.
  const undone = core.actions.undo().events
  assert.ok(!undone.some((e) => e.author === 'claude'))
})

test('claude: no directory is a new temporary one; a plain directory is used as it is', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const core = memoryCore({ run: fakeRun(calls) })
  await core.actions.claudeCreate({ name: 'scratch', cwd: '', worktree: false })
  const [session] = core.entity('claude').outboundLinks
  assert.match(String(core.item(session)!.data.cwd), /claude-/)
  assert.equal(core.item(session)!.data.worktree, null)
  await core.actions.claudeCreate({ name: 'here', cwd: '/tmp', worktree: false })
  const here = core.entity('claude').outboundLinks.find((id) => core.item(id)!.data.text === 'here')!
  assert.equal(core.item(here)!.data.cwd, '/tmp')
  // A directory that isn't there is refused, saying which; ~ is the home directory.
  const missing = await core.actions.claudeCreate({ name: 'typo', cwd: '~/repos/no-such-thing', worktree: false })
  assert.match(missing.error ?? '', /^No such directory: \/.+\/repos\/no-such-thing$/)
  assert.equal(calls.length, 0)
})

test('claude: a failure is said in place of the answer', async () => {
  const run = async () => {
    throw new Error('claude: not logged in')
  }
  const core = memoryCore({ run })
  await core.actions.claudeCreate({ name: 'x', cwd: '/tmp', worktree: false })
  const [session] = core.entity('claude').outboundLinks
  core.actions.claudePrompt({ session, text: 'hello' })
  await settle()
  const [, response] = core.entity(session).outboundLinks
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

test('claude: an answer is kept whole in its response', async () => {
  const answer = 'Done.\n\n## Changes\n\n- one\n  - detail\n- two'
  const run = async (command: string) => (command.endsWith('claude') ? JSON.stringify({ result: answer, is_error: false }) : '')
  const core = memoryCore({ run, dataDir: '/data' })
  await core.actions.claudeCreate({ name: 'S', cwd: '', worktree: false })
  const [session] = core.entity('claude').outboundLinks
  core.actions.claudePrompt({ session, text: 'go' })
  await settle()
  const [, response] = core.entity(session).outboundLinks
  assert.equal(core.item(response)!.data.text, answer)
  assert.deepEqual(core.entity(response).outboundLinks, [])
})

test('claude: every session is given the app MCP server, its tools allowed', async () => {
  const calls: { command: string; args: string[]; cwd: string }[] = []
  const core = memoryCore({ run: fakeRun(calls), dataDir: '/data' })
  core.setMcp({ url: 'http://127.0.0.1:1/mcp', token: 't0k' })
  await core.actions.claudeCreate({ name: 'x', cwd: '', worktree: false })
  const [session] = core.entity('claude').outboundLinks
  core.actions.claudePrompt({ session, text: 'hi' })
  await settle()
  const args = calls.find((call) => call.command.endsWith('claude'))!.args
  assert.deepEqual(args.slice(-4, -1), ['--allowedTools', 'mcp__detail-views', '--mcp-config'])
  assert.deepEqual(JSON.parse(args[args.length - 1]), {
    mcpServers: { 'detail-views': { type: 'http', url: 'http://127.0.0.1:1/mcp', headers: { Authorization: 'Bearer t0k' } } },
  })
})
