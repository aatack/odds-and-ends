// GitHub, through the gh CLI and whatever account it is logged in as.

import { z } from 'zod'
import type { Action, Exec, ReadTool, Source, WaitKind } from './integrations'

const pr = { repo: z.string().describe('owner/name'), number: z.number().int() }

const prArgs = (p: Record<string, unknown>) => [String(p.number), '-R', String(p.repo)]

const view = async (exec: Exec, p: Record<string, unknown>, fields: string) =>
  JSON.parse(await exec('gh', ['pr', 'view', ...prArgs(p), '--json', fields]))

/** Long output is cut, so one diff can't fill an agent's context. */
const cap = (s: string, n = 60_000) => (s.length > n ? `${s.slice(0, n)}\n… (${s.length - n} more characters)` : s)

export const githubReads: ReadTool[] = [
  {
    name: 'github_pr_view',
    description: 'A pull request: title, body, author, state, review decision, checks, reviews, comments and files.',
    input: pr,
    run: async (a, exec) =>
      cap(
        JSON.stringify(
          await view(exec, a, 'title,body,author,state,isDraft,reviewDecision,statusCheckRollup,reviews,comments,files,headRefName,baseRefName,url'),
          null,
          1,
        ),
      ),
  },
  {
    name: 'github_pr_diff',
    description: "A pull request's diff.",
    input: pr,
    run: async (a, exec) => cap(await exec('gh', ['pr', 'diff', ...prArgs(a)])),
  },
  {
    name: 'github_search_prs',
    description: 'Search pull requests with GitHub search syntax, e.g. "is:open author:@me" or "review-requested:@me".',
    input: { query: z.string() },
    run: async (a, exec) =>
      cap(
        await exec('gh', ['search', 'prs', ...String(a.query).split(/\s+/).filter(Boolean), '--json', 'number,title,url,repository,state,updatedAt', '--limit', '30']),
      ),
  },
  {
    name: 'github_api_get',
    description: 'A GET on the GitHub REST API, e.g. "repos/owner/name/pulls/12/comments". Read-only.',
    input: { path: z.string() },
    run: async (a, exec) => cap(await exec('gh', ['api', '--method', 'GET', String(a.path)])),
  },
]

const prFields = [
  { name: 'repo', label: 'Repository', kind: 'text' as const },
  { name: 'number', label: 'PR number', kind: 'text' as const },
]

export const githubActions: Action[] = [
  {
    id: 'github.pr.comment',
    label: 'Comment on a pull request',
    description: 'Post a comment on a pull request.',
    fields: [...prFields, { name: 'body', label: 'Comment', kind: 'multiline' }],
    run: async (i, exec) => exec('gh', ['pr', 'comment', i.number, '-R', i.repo, '--body', i.body]),
  },
  {
    id: 'github.pr.review',
    label: 'Review a pull request',
    description: 'Submit a review: approve, comment, or request changes.',
    fields: [
      ...prFields,
      { name: 'event', label: 'Review', kind: 'select', options: ['approve', 'comment', 'request-changes'] },
      { name: 'body', label: 'Body', kind: 'multiline' },
    ],
    run: async (i, exec) =>
      exec('gh', ['pr', 'review', i.number, '-R', i.repo, `--${i.event}`, ...(i.body ? ['--body', i.body] : [])]),
  },
  {
    id: 'github.pr.merge',
    label: 'Merge a pull request',
    description: 'Merge a pull request.',
    fields: [...prFields, { name: 'method', label: 'Method', kind: 'select', options: ['squash', 'merge', 'rebase'] }],
    run: async (i, exec) => exec('gh', ['pr', 'merge', i.number, '-R', i.repo, `--${i.method}`]),
  },
]

type Check = { __typename?: string; status?: string; conclusion?: string; state?: string; name?: string; context?: string }

const activityCount = async (exec: Exec, p: Record<string, unknown>) => {
  const v = await view(exec, p, 'comments,reviews')
  return (v.comments?.length ?? 0) + (v.reviews?.length ?? 0)
}

export const githubWaits: WaitKind[] = [
  {
    id: 'github.pr.checks',
    description: "A pull request's checks to finish, whatever their result.",
    input: pr,
    describe: (p) => `checks on ${p.repo}#${p.number}`,
    check: async (p, exec) => {
      const checks: Check[] = (await view(exec, p, 'statusCheckRollup')).statusCheckRollup ?? []
      const pending = checks.filter((c) =>
        c.__typename === 'StatusContext' ? ['PENDING', 'EXPECTED'].includes(c.state ?? '') : c.status !== 'COMPLETED',
      )
      if (pending.length) return null
      const failed = checks.filter((c) =>
        c.__typename === 'StatusContext' ? c.state !== 'SUCCESS' : !['SUCCESS', 'NEUTRAL', 'SKIPPED'].includes(c.conclusion ?? ''),
      )
      return failed.length
        ? `Checks finished; ${failed.length} of ${checks.length} failed: ${failed.map((c) => c.name ?? c.context).join(', ')}`
        : `Checks finished; all ${checks.length} passed.`
    },
  },
  {
    id: 'github.pr.activity',
    description: 'A new comment or review on a pull request.',
    input: pr,
    describe: (p) => `a comment or review on ${p.repo}#${p.number}`,
    start: async (p, exec) => ({ ...p, baseline: await activityCount(exec, p) }),
    check: async (p, exec) => {
      const n = await activityCount(exec, p)
      return n > Number(p.baseline ?? 0) ? `${n - Number(p.baseline ?? 0)} new comment(s) or review(s).` : null
    },
  },
  {
    id: 'github.pr.closed',
    description: 'A pull request to be merged or closed.',
    input: pr,
    describe: (p) => `${p.repo}#${p.number} to be merged or closed`,
    check: async (p, exec) => {
      const { state } = await view(exec, p, 'state')
      return state === 'OPEN' ? null : `The pull request is ${String(state).toLowerCase()}.`
    },
  },
]

export const githubSources: Source[] = [
  {
    id: 'github.review-requests',
    everyMinutes: 10,
    poll: async (exec) => {
      const prs: { number: number; title: string; url: string; repository: { nameWithOwner: string } }[] = JSON.parse(
        await exec('gh', ['search', 'prs', '--review-requested=@me', '--state=open', '--json', 'number,title,url,repository']),
      )
      return prs.map((p) => ({
        ref: `github:review:${p.url}`,
        title: `Review ${p.repository.nameWithOwner}#${p.number}: ${p.title}`,
        url: p.url,
        state: `Your review is requested on ${p.repository.nameWithOwner}#${p.number}.`,
      }))
    },
  },
]
