// The changeset tools, driven headlessly against an in-memory source with the
// server's integrations stubbed out.
//
// What is worth asserting here is the *order* of things, since almost every bug
// this composition can have is one: a system prompt built on the second turn, a
// session id written before there is a session, a reply lost because the push
// failed, a pull request raised twice.
//
//   npm test

import assert from 'node:assert/strict'
import { MemorySource } from './source.mjs'

const store = new Map<string, string>()
Object.defineProperty(globalThis, 'localStorage', {
  value: {
    getItem: (k: string) => store.get(k) ?? null,
    setItem: (k: string, v: string) => void store.set(k, v),
    removeItem: (k: string) => void store.delete(k),
    clear: () => store.clear(),
  },
})

/** Every integration call made, in order, and what each was given. */
let integrationCalls: { tool: string; args: any }[] = []
/** What each integration answers with, by tool id. A function may throw. */
let replies: Record<string, (args: any) => unknown> = {}

Object.defineProperty(globalThis, 'window', {
  value: {
    entityGraph: {
      integrationTools: async () => [],
      runIntegrationTool: async (_server: string, tool: string, args: any) => {
        integrationCalls.push({ tool, args })
        const reply = replies[tool]
        if (!reply) throw new Error(`Nothing stubbed for ${tool}`)
        return reply(args)
      },
    },
  },
})

const { setSourceTransport } = await import('../src/renderer/src/source/transport')
const { setIntegrationServer } = await import('../src/renderer/src/tools/integrationTools')
const { callToolByName } = await import('../src/renderer/src/tools/call')
const { readEntities } = await import('../src/renderer/src/source/entity')
const { entitiesAtom } = await import('../src/core/cache')

// --- Harness ----------------------------------------------------------------

let source: MemorySource

/** The stubs a run gets when the test doesn't care what an integration said. */
const DEFAULTS: Record<string, (args: any) => unknown> = {
  'git.createWorktree': () => ({ path: '/tmp/worktrees/aB3xY9', branch: 'aB3xY9', id: 'aB3xY9' }),
  'claude.runPrompt': () => ({ result: 'Done.', session_id: 'whatever' }),
  'git.commitAll': () => ({ committed: false, branch: 'aB3xY9', commit: null }),
  'git.push': () => ({ branch: 'aB3xY9', output: '' }),
  'github.pullRequestForBranch': () => ({ branch: 'aB3xY9', pullRequest: null }),
  'github.createPullRequest': () => ({ url: 'https://github.com/a/b/pull/7', number: 7 }),
}

function open(): void {
  source = new MemorySource()
  entitiesAtom.set({})
  integrationCalls = []
  replies = { ...DEFAULTS }
  setSourceTransport({ call: (t, a) => source.call(t, a), user: 'test', sourceId: 'memory' })
  setIntegrationServer('server')
}

const context = (values: Record<string, unknown> = {}) => ({
  values,
  path: [],
  groupId: null,
  tabId: null,
  frameId: null,
  startedAt: Date.now(),
})

const create = (args: unknown[], values: Record<string, unknown> = {}): Promise<any> =>
  callToolByName('newChangeset', args, context(values))

const ask = (args: unknown[], values: Record<string, unknown> = {}): Promise<any> =>
  callToolByName('promptChangeset', args, context(values))

/** Every call made to one integration. */
const callsTo = (tool: string): any[] =>
  integrationCalls.filter((c) => c.tool === tool).map((c) => c.args)

const only = (tool: string): any => {
  const all = callsTo(tool)
  assert.equal(all.length, 1, `expected exactly one ${tool}, got ${all.length}`)
  return all[0]
}

const valuesOf = async (id: string): Promise<Record<string, unknown>> =>
  (await readEntities([id]))[id]?.values ?? {}

const childrenOf = async (id: string): Promise<string[]> =>
  (await readEntities([id]))[id]?.outboundLinks ?? []

const textOf = async (id: string): Promise<string> => String((await valuesOf(id)).text ?? '')

/**
 * A child of `id` reading like this. Notes are found by what they say rather than
 * by position, since the root a changeset was started from has the changeset
 * itself as a child too, and which came first is not the point of any test here.
 */
async function noteUnder(id: string, matching: RegExp): Promise<string> {
  for (const child of await childrenOf(id)) {
    if (matching.test(await textOf(child))) return child
  }
  throw new Error(`no child of ${id} matching ${matching}`)
}

/** The tree of rules a session's system prompt is built out of. */
const RULES = 'c2765f0c-6428-4347-b9d6-cba4744ea0a6'

const tests: [string, () => Promise<void>][] = []
const test = (name: string, run: () => Promise<void>): void => void tests.push([name, run])

// --- Creating ---------------------------------------------------------------

test('branches off origin/master, and records both ids on the changeset', async () => {
  open()
  const { changesetId, branch } = await create(['Fix the login flow', '~/repos/app', 'notes'])

  assert.deepEqual(only('git.createWorktree'), { path: '~/repos/app', from: 'origin/master' })

  const values = await valuesOf(changesetId)
  assert.equal(values.type, 'changeset')
  assert.equal(values.text, 'Fix the login flow')
  assert.equal(values.repo, '~/repos/app')
  // The worktree's id is git's and the changeset's is the store's. Decoupled, so
  // both have to be written down or one of them is lost.
  assert.equal(values.worktree, '/tmp/worktrees/aB3xY9')
  assert.equal(values.branch, 'aB3xY9')
  assert.equal(branch, 'aB3xY9')
  assert.notEqual(changesetId, 'aB3xY9')
  assert.equal(values.rootId, 'notes')
})

test('takes a base other than master from the context, without asking for one', async () => {
  open()
  await create(['Something', '~/repos/app', 'notes'], { base: 'origin/develop' })
  assert.equal(only('git.createWorktree').from, 'origin/develop')
})

test('hangs the changeset off @changesets and links it to the notes both ways', async () => {
  open()
  const { changesetId } = await create(['Fix it', '~/repos/app', 'notes'])

  assert.ok((await childrenOf('@changesets')).includes(changesetId))
  // Both directions: the changeset shows under the notes, and the notes under the
  // changeset. The query's cycle guard is what stops that being a loop.
  assert.ok((await childrenOf(changesetId)).includes('notes'))
  assert.ok((await childrenOf('notes')).includes(changesetId))
})

test('writes changesetId on the root entity rather than on the changeset', async () => {
  open()
  const { changesetId } = await create(['Fix it', '~/repos/app', 'notes'])
  // On the root, because that is what folds into the context of everything
  // underneath it — which is how `k` finds the changeset from anywhere in the
  // notes without anything having to be selected.
  assert.equal((await valuesOf('notes')).changesetId, changesetId)
  assert.equal((await valuesOf(changesetId)).changesetId, undefined)
})

test('starts a changeset with no notes behind it', async () => {
  open()
  const { changesetId } = await create(['Just poking about', '~/repos/app', ''])
  assert.equal((await valuesOf(changesetId)).rootId, undefined)
  // The prompt has nowhere else to hang, so it hangs off the changeset — and with
  // no notes to point at, the name is all there is to open with.
  assert.equal(await textOf((await childrenOf(changesetId))[0]), 'Get started on: Just poking about')
})

test('refuses a changeset with no name and one with nowhere to work', async () => {
  open()
  await assert.rejects(create(['', '~/repos/app', 'notes']), /needs a name/)
  await assert.rejects(create(['Fix it', '', 'notes']), /repository/)
})

// --- The session and its system prompt --------------------------------------

test('sends a system prompt naming the notes and the rules, once', async () => {
  open()
  source.values({ [RULES]: { text: 'Agent instructions', section: true } })
  source.tree({ [RULES]: ['rule'] })
  source.values({ rule: { text: 'Commit in reasonable chunks as you go' } })

  const { changesetId } = await create(['Fix it', '~/repos/app', 'notes'])
  const first = only('claude.runPrompt')
  assert.match(first.systemPrompt, /`notes`/)
  assert.match(first.systemPrompt, new RegExp(changesetId))
  // Read out of the store on the turn that starts the session, so editing the
  // tree changes what the next one is told.
  assert.match(first.systemPrompt, /Commit in reasonable chunks/)

  await ask(['And now the other bit'], { changesetId })
  const second = callsTo('claude.runPrompt')[1]
  // A resumed conversation already has the system prompt it began with.
  assert.equal(second.systemPrompt, undefined)
})

test('names the session after the changeset, and writes it down on first use', async () => {
  open()
  const { changesetId } = await create(['Fix it', '~/repos/app', 'notes'])
  const sessionId = (await valuesOf(changesetId)).sessionId
  assert.equal(sessionId, `changeset-${changesetId}`)
  assert.equal(only('claude.runPrompt').sessionId, sessionId)
})

test('says nothing to a session until the changeset has actually been prompted', async () => {
  open()
  // The worktree exists and the entity is written before the prompt goes out, so
  // a creation that fails at the session leaves a changeset with no claim to a
  // conversation that was never started.
  replies['claude.runPrompt'] = () => {
    throw new Error('claude is not installed')
  }
  await assert.rejects(create(['Fix it', '~/repos/app', 'notes']), /not installed/)
  const [changesetId] = await childrenOf('@changesets')
  assert.equal((await valuesOf(changesetId)).sessionId, undefined)
})

// --- The conversation as notes ----------------------------------------------

test('writes the prompt under the row it was asked from, and the answer under it', async () => {
  open()
  replies['claude.runPrompt'] = () => ({ result: 'Renamed the thing.' })
  const { changesetId } = await create(['Fix it', '~/repos/app', 'notes'])
  await ask(['Rename the thing'], { changesetId, entityId: 'somewhere' })

  const [noteId] = await childrenOf('somewhere')
  assert.equal(await textOf(noteId), 'Rename the thing')
  const [replyId] = await childrenOf(noteId)
  assert.equal(await textOf(replyId), '*Claude:* Renamed the thing.')
})

test('finds the changeset in the context, without being handed one', async () => {
  open()
  const { changesetId } = await create(['Fix it', '~/repos/app', 'notes'])
  // What `k` has to work with anywhere inside a changeset's notes: a folded
  // `changesetId` and whatever row the cursor happens to be on.
  await ask(['Carry on'], { changesetId, entityId: 'deep-note' })
  assert.equal(callsTo('claude.runPrompt').length, 2)
})

test('refuses to guess when there is no changeset in reach', async () => {
  open()
  await assert.rejects(ask(['Carry on'], { entityId: 'somewhere' }), /No changeset here/)
})

test('refuses an entity that is not a changeset', async () => {
  open()
  source.values({ ordinary: { text: 'just a note' } })
  await assert.rejects(ask(['Carry on'], { changesetId: 'ordinary' }), /isn't a changeset/)
})

// --- Publishing --------------------------------------------------------------

test('commits, pushes and raises a pull request, then records it', async () => {
  open()
  const { changesetId } = await create(['Fix the login flow', '~/repos/app', 'notes'])

  // The sweep runs with the prompt as its message: whatever the session left
  // behind belongs to the thing it was asked to do.
  assert.equal(only('git.commitAll').message, 'Implement the notes under `notes`.')
  assert.equal(only('git.push').path, '/tmp/worktrees/aB3xY9')
  assert.equal(only('github.pullRequestForBranch').branch, 'aB3xY9')

  const raised = only('github.createPullRequest')
  assert.equal(raised.title, 'Fix the login flow')
  assert.match(raised.body, new RegExp(`Changeset \`${changesetId}\``))
  assert.equal((await valuesOf(changesetId)).pullRequest, 'https://github.com/a/b/pull/7')
})

test('describes the pull request with the notes it was started from', async () => {
  open()
  source.values({ notes: { text: 'Login flow', section: true } })
  source.tree({ notes: ['detail'] })
  source.values({ detail: { text: 'The button does nothing on Safari' } })

  await create(['Fix the login flow', '~/repos/app', 'notes'])
  const { body } = only('github.createPullRequest')
  assert.match(body, /Login flow/)
  assert.match(body, /does nothing on Safari/)
})

test('leaves an existing pull request alone', async () => {
  open()
  replies['github.pullRequestForBranch'] = () => ({
    branch: 'aB3xY9',
    pullRequest: { url: 'https://github.com/a/b/pull/3', number: 3 },
  })
  const { changesetId, published } = await create(['Fix it', '~/repos/app', 'notes'])
  assert.equal(callsTo('github.createPullRequest').length, 0)
  assert.equal(published.raised, false)
  assert.equal((await valuesOf(changesetId)).pullRequest, 'https://github.com/a/b/pull/3')
})

test('keeps the answer when the publishing fails', async () => {
  open()
  replies['claude.runPrompt'] = () => ({ result: 'All done.' })
  replies['git.push'] = () => {
    throw new Error('no configured push destination')
  }
  // Not a rejection: the session ran, and both halves of the conversation are
  // written down. A repository with no remote would otherwise fail every turn
  // after doing all of the work.
  const { changesetId, published } = await create(['Fix it', '~/repos/app', 'notes'])
  assert.match(String(published.error), /no configured push destination/)

  const noteId = await noteUnder('notes', /Implement the notes/)
  assert.equal(await textOf((await childrenOf(noteId))[0]), '*Claude:* All done.')
  assert.equal((await valuesOf(changesetId)).pullRequest, undefined)
})

// --- Running -----------------------------------------------------------------

let failures = 0
for (const [name, run] of tests) {
  try {
    await run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failures++
    console.log(`FAIL  ${name}`)
    console.log(`      ${e instanceof Error ? e.message : String(e)}`)
  }
}
console.log(`\n${tests.length - failures} passed${failures ? `, ${failures} failed` : ''}`)
if (failures) process.exit(1)
