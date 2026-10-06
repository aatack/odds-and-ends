import assert from 'node:assert/strict'
import { test } from 'node:test'
import { Core } from '../../core.ts'

const url = 'https://github.com/o/r/pull/7'

function fakeGh(calls: string[][]) {
  return async (args: string[]) => {
    calls.push(args)
    const query = args[3]
    if (query.includes('search(')) {
      return JSON.stringify({
        data: {
          search: {
            nodes: [
              { url, number: 7, title: 'Fix it', isDraft: false, updatedAt: '2026-10-01T00:00:00Z', reviewDecision: 'APPROVED',
                repository: { nameWithOwner: 'o/r' }, commits: { nodes: [{ commit: { statusCheckRollup: { state: 'FAILURE' } } }] } },
            ],
          },
        },
      })
    }
    return JSON.stringify({
      data: {
        resource: {
          url, number: 7, title: 'Fix it', body: '<!-- bot -->Does the thing', state: 'OPEN', isDraft: false, mergeable: 'MERGEABLE',
          reviewDecision: 'APPROVED', additions: 3, deletions: 1, changedFiles: 1, headRefName: 'fix', baseRefName: 'main',
          createdAt: '2026-10-01T00:00:00Z', updatedAt: '2026-10-02T00:00:00Z', author: { login: 'me' }, repository: { nameWithOwner: 'o/r' },
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
  const core = new Core({ path: ':memory:', gh: fakeGh([]) })
  await core.refresh('github')
  const [pr] = core.focus('github').children
  assert.equal(pr.id, `github:pr:${url}`)
  assert.equal(pr.data.checks, 'failing')
  assert.equal(pr.data.label, 'o/r#7 Fix it')
})

test('github: a PR shows checks that need me, then the discussion in order', async () => {
  const core = new Core({ path: ':memory:', gh: fakeGh([]) })
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
  const core = new Core({ path: ':memory:', gh: fakeGh(calls) })
  await core.refresh('github')
  assert.ok(calls.every((args) => args[0] === 'api' && args[1] === 'graphql' && !/^\s*mutation/.test(args[3].slice('query='.length))))
})
