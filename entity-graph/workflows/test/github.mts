// GitHub, through a stand-in for `gh`: the integration's shape, its reads, its writes kept to people, its setup.

import assert from 'node:assert/strict'
import { Admin } from '../src/engine/admin.ts'
import { Engine } from '../src/engine/engine.ts'

const calls: string[][] = []
const exec = async (cmd: string, args: string[]) => {
  if (cmd === 'claude') return '2.1.309 (Claude Code)\n'
  assert.equal(cmd, 'gh')
  calls.push(args)
  if (args[0] === 'api' && args[1] === 'user') return 'alex\n'
  if (args[0] === 'pr' && args[1] === 'list') return JSON.stringify([{ number: 7, title: 'Fix it' }])
  if (args[0] === 'pr' && args[1] === 'comment') return 'https://github.com/o/r/pull/7#issuecomment-1\n'
  if (args[0] === 'api') return JSON.stringify({ path: args.at(-1) })
  return '{}'
}
const engine = new Engine({ exec, now: () => 1_800_000_000_000, dataDir: '/tmp' })
await engine.signIn('alex')
const admin = new Admin(engine, { operator: () => ({ url: '', error: null }) } as never)
const bot = { engine, author: 'bot' as const, actor: 'bot', itemId: null, runId: null }
const person = { engine, author: 'human' as const, actor: 'alex', itemId: null, runId: null }

// The page's view of it: setup, read, write, triggers; its status from gh.
const view = (await admin.integrations()).registered.find((i) => i.id === 'github')!
assert.equal(view.status!.detail, 'Signed in as alex')
assert.ok(view.read.some((t) => t.name === 'github_pr_view') && view.write.some((t) => t.name === 'github_pr_merge'))
assert.ok(!view.read.some((t) => view.write.some((w) => w.name === t.name)))
assert.deepEqual(view.setup.map((f) => [f.key, f.set]), [['default_repo', false]])

// A read: with no repo named and no default, it says so; with a default, it uses it.
await assert.rejects(engine.callAction('github_pr_list', {}, bot), /Name the repo/)
admin.setIntegrationValue('github', 'default_repo', 'o/r')
assert.equal((await admin.integrations()).registered[0].setup[0].value, 'o/r')
assert.deepEqual(await engine.callAction('github_pr_list', { search: 'author:@me' }, bot), [{ number: 7, title: 'Fix it' }])
assert.deepEqual(calls.at(-1)!.slice(0, 4), ['pr', 'list', '-R', 'o/r'])
assert.ok(calls.at(-1)!.includes('author:@me'))
// The API read is GET only, whatever the path says.
await engine.callAction('github_api_get', { path: '/repos/o/r/pulls' }, bot)
assert.deepEqual(calls.at(-1), ['api', '--method', 'GET', 'repos/o/r/pulls'])

// A write: never a bot's; a person's, yes.
await assert.rejects(engine.callAction('github_pr_comment', { number: 7, body: 'hi' }, bot), /unsafe/)
assert.match(String(await engine.callAction('github_pr_comment', { number: 7, body: 'hi' }, person)), /issuecomment/)
assert.deepEqual(calls.at(-1), ['pr', 'comment', '7', '-R', 'o/r', '--body', 'hi'])
admin.setIntegrationValue('github', 'default_repo', null)
assert.equal((await admin.integrations()).registered[0].setup[0].set, false)

console.log('github: ok')
