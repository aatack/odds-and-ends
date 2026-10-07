import assert from 'node:assert/strict'
import { test } from 'node:test'
import { memoryCore } from '../../testing.ts'
import { badge, blockers, prName } from './view.ts'

const url = 'https://github.com/o/r/pull/7'

function fakeGh(calls: string[][], options: { author?: string; approvers?: string[] } = {}) {
  return async (args: string[]) => {
    calls.push(args)
    if (args[0] === 'pr') return ''
    const query = args[3]
    if (query.includes('search(')) {
      return JSON.stringify({
        data: {
          viewer: { login: 'me' },
          search: {
            nodes: [
              { url, number: 7, title: 'Fix it', state: 'OPEN', isDraft: false, updatedAt: '2026-10-01T00:00:00Z', reviewDecision: 'APPROVED',
                mergeable: 'MERGEABLE', author: { login: 'me' }, latestReviews: { nodes: [{ author: { login: 'ann' }, state: 'APPROVED' }] },
                repository: { nameWithOwner: 'o/r' }, commits: { nodes: [{ commit: { statusCheckRollup: { state: 'FAILURE' } } }] } },
            ],
          },
        },
      })
    }
    return JSON.stringify({
      data: {
        viewer: { login: 'me' },
        resource: {
          url, number: 7, title: 'Fix it', body: '<!-- bot -->Does the thing', state: 'OPEN', isDraft: false, mergeable: 'MERGEABLE',
          reviewDecision: 'APPROVED', latestReviews: { nodes: (options.approvers ?? ['ann']).map((login) => ({ author: { login }, state: 'APPROVED' })) }, additions: 3, deletions: 1, changedFiles: 1, headRefName: 'fix', baseRefName: 'main',
          createdAt: '2026-10-01T00:00:00Z', updatedAt: '2026-10-02T00:00:00Z', author: { login: options.author ?? 'me' },
          repository: { nameWithOwner: 'o/r', viewerDefaultMergeMethod: 'SQUASH' }, autoMergeRequest: null,
          comments: { nodes: [{ id: 'C1', url: `${url}#c1`, author: { login: 'bot' }, body: '<!-- only bookkeeping -->', createdAt: '2026-10-01T01:00:00Z' }] },
          reviews: { nodes: [
            { id: 'R1', url: `${url}#r1`, author: { login: 'ann' }, body: '', state: 'COMMENTED', createdAt: '2026-10-01T02:00:00Z', comments: { nodes: [] } },
            { id: 'R2', url: `${url}#r2`, author: { login: 'ann' }, body: 'LGTM', state: 'APPROVED', createdAt: '2026-10-01T03:00:00Z',
              comments: { nodes: [{ id: 'X', url: '', path: 'a.ts', line: 4, originalLine: 4, diffHunk: '@@ -1 +1 @@\n-a\n+b', body: 'nice', createdAt: '2026-10-01T03:00:00Z' }] } },
          ] },
          commits: { nodes: [{ commit: { statusCheckRollup: { state: 'FAILURE', contexts: { nodes: [
            { __typename: 'CheckRun', name: 'build', status: 'COMPLETED', conclusion: 'FAILURE', detailsUrl: 'https://ci/1', startedAt: null, completedAt: null, checkSuite: null },
            { __typename: 'CheckRun', name: 'lint', status: 'COMPLETED', conclusion: 'SUCCESS', detailsUrl: null, startedAt: null, completedAt: null, checkSuite: null },
            { __typename: 'CheckRun', name: 'e2e', status: 'COMPLETED', conclusion: 'SKIPPED', detailsUrl: null, startedAt: null, completedAt: null, checkSuite: null },
            { __typename: 'StatusContext', context: 'deploy', state: 'PENDING', targetUrl: null, createdAt: '2026-10-01T00:00:00Z' },
          ] } } } }] },
        },
      },
    })
  }
}

test('github: my open PRs, keyed by URL', async () => {
  const core = memoryCore({ gh: fakeGh([]) })
  await core.refresh('github')
  const [pr] = core.focus('github').children
  assert.equal(pr.id, `github:pr:${url}`)
  assert.equal(pr.data.checks, 'failing')
  assert.equal(pr.data.label, 'Fix it')
})

test('github: comments are written when they were made, and can be hidden for me only', async () => {
  const core = memoryCore({ gh: fakeGh([]) })
  const id = `github:pr:${url}`
  await core.refresh(id)
  const review = `${id}#item:R2`
  const linked = core.cache.read([id]).find((e) => e.type === 'link' && e.destinationId === review)!
  assert.equal(linked.timestamp, Date.parse('2026-10-01T03:00:00Z'))
  assert.equal(core.cache.read([id]).find((e) => e.type === 'value' && e.key === 'title')!.timestamp, 0)
  core.actions.unlink({ parent: id, child: review })
  await core.refresh(id)
  assert.ok(!core.focus(id).children.some((child) => child.id === review))
})

test('github: a PR shows checks that need me, then the discussion in order', async () => {
  const core = memoryCore({ gh: fakeGh([]) })
  const id = `github:pr:${url}`
  // Seen only as a link: becomes an item when looked at.
  assert.ok(core.focus(id).entity)
  await core.refresh(id)
  const focus = core.focus(id)
  assert.deepEqual(focus.entity!.data.counts, { failing: 1, pending: 1, skipped: 1, passing: 1 })
  assert.deepEqual(
    focus.children.map((child) => [child.type, child.data.name ?? child.data.kind]),
    [
      ['github.check', 'build'],
      ['github.check', 'deploy'],
      ['github.item', 'description'],
      ['github.item', 'review'],
    ],
  )
  const review = focus.children[3].data
  assert.equal(review.verdict, 'approved')
  assert.match(String(review.markdown), /^LGTM\n\n`a\.ts:4`\n```diff\n@@ -1 \+1 @@\n-a\n\+b\n```\nnice$/)
  assert.equal(focus.children[2].data.text, 'Does the thing')
})

test('github: only queries, never mutations', async () => {
  const calls: string[][] = []
  const core = memoryCore({ gh: fakeGh(calls) })
  await core.refresh('github')
  assert.ok(calls.every((args) => args[0] === 'api' && args[1] === 'graphql' && !/^\s*mutation/.test(args[3].slice('query='.length))))
})

async function loaded(author: string, approvers?: string[]) {
  const calls: string[][] = []
  const core = memoryCore({ gh: fakeGh(calls, { author, approvers }) })
  const id = `github:pr:${url}`
  core.focus(id)
  await core.refresh(id)
  calls.length = 0
  return { core, id, calls }
}

test("github: approving someone else's PR leaves an approving review", async () => {
  const { core, id, calls } = await loaded('ann')
  assert.deepEqual(core.focus(id).actions.map((action) => action.id), ['approve'])
  await core.actions.perform({ id, action: 'approve', text: 'looks good' })
  assert.deepEqual(calls[0], ['pr', 'review', url, '--approve', '--body', 'looks good'])
})

test('github: approving my own PR is kept locally and turns on auto-merge', async () => {
  const { core, id, calls } = await loaded('me')
  assert.deepEqual(core.focus(id).actions.map((action) => action.id), ['approve', 'close'])
  await core.actions.perform({ id, action: 'approve', text: '' })
  assert.deepEqual(calls[0], ['pr', 'merge', url, '--auto', '--squash'])
  const focus = core.focus(id)
  assert.equal(focus.entity!.data.locallyApproved, true)
  assert.ok(focus.children.every((child) => child.type !== 'github.localApproval'))
  // Owned: survives the cache being cleared.
  core.clearCache()
  assert.equal(core.item(id)!.data.title, undefined)
  assert.equal(core.focus(id).entity!.data.locallyApproved, true)
})

test('github: closing my own PR deletes the branch, with an optional comment', async () => {
  const { core, id, calls } = await loaded('me')
  await core.actions.perform({ id, action: 'close', text: 'superseded by #8' })
  assert.deepEqual(calls[0], ['pr', 'close', url, '--delete-branch', '--comment', 'superseded by #8'])
})

test("github: no closing someone else's PR", async () => {
  const { core, id, calls } = await loaded('ann')
  await core.actions.perform({ id, action: 'close', text: '' })
  assert.equal(calls.filter((args) => args[0] === 'pr').length, 0)
  assert.match(core.focus(id).error ?? '', /not available/)
})

test('github: PR names drop the number and conventional prefix', () => {
  assert.equal(prName('feat: add decide() backed by OpenRouter'), 'add decide() backed by OpenRouter')
  assert.equal(prName('fix(frontend): bump prosemirror-view'), 'bump prosemirror-view')
  assert.equal(prName('#5779 chore!: drop node 18'), 'drop node 18')
  assert.equal(prName('feat/fix(ui): both'), 'both')
  assert.equal(prName('Next steps'), 'Next steps')
  assert.equal(prName('Note: capitalised words stay'), 'Note: capitalised words stay')
})

test('github: what stands in the way, most pressing first, and whose move it is', () => {
  const first = (data: Record<string, unknown>, local = false) => {
    const [found] = blockers(data, local)
    return found && `${found.label} (${found.turn})`
  }
  const all = (data: Record<string, unknown>, local = false) => blockers(data, local).map((blocker) => blocker.id)
  // Not loaded yet.
  assert.deepEqual(blockers({}, false), [])
  // Merged or closed: nothing more, whatever else is true.
  assert.equal(first({ mine: true, state: 'MERGED', checks: 'failing' }), 'Merged (none)')
  assert.equal(first({ mine: false, state: 'CLOSED' }), 'Closed (none)')
  // Mine: my own review here comes first, then others', then CI, conflicts, base, running CI.
  assert.deepEqual(all({ mine: true, state: 'OPEN', checks: 'failing', conflicts: true, mergeState: 'BEHIND' }), [
    'self-review',
    'awaiting',
    'ci',
    'conflicts',
    'behind',
  ])
  assert.equal(first({ mine: true, state: 'OPEN', draft: true }), 'Draft: mark it ready (me)')
  assert.equal(first({ mine: true, state: 'OPEN', reviewers: ['ann'] }, true), 'Awaiting review: ann (them)')
  assert.equal(first({ mine: true, state: 'OPEN', review: 'CHANGES_REQUESTED', unresolved: 2 }, true), 'Changes requested (me)')
  assert.equal(first({ mine: true, state: 'OPEN', approvedByOthers: true, review: 'APPROVED', checks: 'pending' }, true), 'CI running (none)')
  assert.equal(first({ mine: true, state: 'OPEN', approvedByOthers: true, review: 'APPROVED', checks: 'passing' }, true), 'Ready: merge it (me)')
  assert.equal(first({ mine: true, state: 'OPEN', approvedByOthers: true, review: 'APPROVED', autoMerge: true }, true), 'Will auto-merge (none)')
  // Theirs: my review first (asked for, or not), then what's on them.
  assert.equal(first({ mine: false, state: 'OPEN', reviewRequestedOfMe: true }), 'Review requested of you (me)')
  assert.equal(first({ mine: false, state: 'OPEN' }), 'Not reviewed by you (me)')
  assert.equal(first({ mine: false, state: 'OPEN', changesRequestedByMe: true, review: 'CHANGES_REQUESTED' }), 'You requested changes (them)')
  assert.equal(first({ mine: false, state: 'OPEN', approvedByMe: true, review: 'REVIEW_REQUIRED', reviewers: ['bo'] }), 'Awaiting review: bo (them)')
  assert.equal(first({ mine: false, state: 'OPEN', approvedByMe: true, review: 'APPROVED', checks: 'failing' }), 'CI failing (them)')
  assert.equal(first({ mine: false, state: 'OPEN', approvedByMe: true, review: 'APPROVED' }), 'Approved by you (none)')
  // The badge is the first blocker.
  assert.deepEqual(badge({ mine: true, state: 'OPEN', approvedByOthers: true, review: 'APPROVED', checks: 'failing' }, true), {
    shape: 'cross',
    tone: 'red',
    reason: 'CI failing',
  })
})

test('github: a PR seen only as a link is an item, and carries its badge once loaded', async () => {
  const core = memoryCore({ gh: fakeGh([]) })
  const id = `github:pr:${url}`
  const first = core.focus(id).entity!
  assert.equal(first.data.badge, null)
  assert.equal(first.data.label, url)
  await core.load({ id, part: 'self' })
  const loaded = core.focus(id).entity!
  // Mine, not yet approved here: that comes first; the failing check after.
  assert.deepEqual(loaded.data.badge, { shape: 'dot', tone: 'yellow', reason: 'Review it' })
  assert.deepEqual((loaded.data.blockers as { id: string }[]).map((blocker) => blocker.id), ['self-review', 'ci'])
})

test('github: an approve already done is shown but cannot be done again', async () => {
  const theirs = await loaded('ann', ['ann', 'me'])
  const [approve] = theirs.core.focus(theirs.id).actions
  assert.equal(approve.disabled, 'Already approved by me')
  const outcome = await theirs.core.actions.perform({ id: theirs.id, action: 'approve', text: '' })
  assert.equal(outcome.error, 'Already approved by me')
  assert.equal(theirs.calls.filter((args) => args[0] === 'pr').length, 0)

  // Mine: approved on GitHub (the fake's review decision) and set to
  // auto-merge, approving again is off, with no approval of my own.
  const approvedThere = await loaded('me')
  approvedThere.core.cache.write([{ type: 'value', entityId: approvedThere.id, key: 'autoMerge', value: true, timestamp: 0, author: 'github' }])
  assert.equal(approvedThere.core.focus(approvedThere.id).actions.find((action) => action.id === 'approve')!.disabled, 'Approved, and auto-merge is on')

  // Not approved on GitHub: my approval here counts, once auto-merge is on.
  const mine = await loaded('me')
  await mine.core.actions.perform({ id: mine.id, action: 'approve', text: '' })
  assert.equal(mine.core.focus(mine.id).actions.find((action) => action.id === 'approve')!.disabled, undefined)
  mine.core.cache.write([
    { type: 'value', entityId: mine.id, key: 'review', value: 'REVIEW_REQUIRED', timestamp: 0, author: 'github' },
    { type: 'value', entityId: mine.id, key: 'autoMerge', value: true, timestamp: 0, author: 'github' },
  ])
  assert.equal(mine.core.focus(mine.id).actions.find((action) => action.id === 'approve')!.disabled, 'Approved, and auto-merge is on')
})
