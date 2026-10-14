import { Core, type CoreOptions } from './core.ts'

/** Helpers for tests: a core with nothing on disk, and a Slack that answers from a table. */

export function memoryCore(options: Partial<CoreOptions> = {}): Core {
  return new Core({ owned: ':memory:', cache: ':memory:', ...options })
}

/** Answers Slack methods from a table; anything missing fails the test. */
export function fakeSlack(answers: Record<string, (params: URLSearchParams) => unknown>, calls: string[] = []): typeof fetch {
  return (async (input: string | URL | Request, init?: RequestInit) => {
    const method = String(input).split('/').pop()!
    calls.push(method)
    const answer = answers[method]
    if (!answer) throw new Error(`unexpected slack call ${method}`)
    return new Response(JSON.stringify({ ok: true, ...(answer(init!.body as URLSearchParams) as object) }))
  }) as typeof fetch
}

/**
 * gh, answering GitHub's queries with one PR (`url`, by `options.author`,
 * approved by `options.approvers`: ann by default); every call recorded.
 */
export function fakeGh(calls: string[][], options: { author?: string; approvers?: string[] } = {}, url = 'https://github.com/o/r/pull/7') {
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
