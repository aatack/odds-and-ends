// GitHub, through the gh CLI and whatever account it is logged in as.

import { z } from 'zod'
import type { Action, Exec, Source } from './integrations'

const pr = { repo: z.string().describe('owner/name'), number: z.coerce.number().int().describe('The pull request number.') }

const prArgs = (p: Record<string, unknown>) => [String(p.number), '-R', String(p.repo)]

const view = async (exec: Exec, p: Record<string, unknown>, fields: string) =>
  JSON.parse(await exec('gh', ['pr', 'view', ...prArgs(p), '--json', fields]))

/** Long output is cut, so one diff can't fill a bot's context. */
export const cap = (s: string, n = 60_000) => (s.length > n ? `${s.slice(0, n)}\n… (${s.length - n} more characters)` : s)

export const githubActions: Action[] = [
  {
    name: 'github_pr_view',
    unsafe: false,
    description: 'A pull request: title, body, author, state, review decision, checks, reviews, comments and files.',
    input: pr,
    run: (a, { engine }) =>
      view(engine.exec, a, 'title,body,author,state,isDraft,reviewDecision,statusCheckRollup,reviews,comments,files,headRefName,baseRefName,url'),
  },
  {
    name: 'github_pr_diff',
    unsafe: false,
    description: "A pull request's diff.",
    input: pr,
    run: async (a, { engine }) => cap(await engine.exec('gh', ['pr', 'diff', ...prArgs(a)])),
  },
  {
    name: 'github_search_prs',
    unsafe: false,
    description: 'Search pull requests with GitHub search syntax, e.g. "is:open author:@me" or "review-requested:@me".',
    input: { query: z.string() },
    run: async (a, { engine }) =>
      JSON.parse(
        await engine.exec('gh', [
          'search', 'prs', ...String(a.query).split(/\s+/).filter(Boolean),
          '--json', 'number,title,url,repository,state,updatedAt', '--limit', '30',
        ]),
      ),
  },
  {
    name: 'github_api_get',
    unsafe: false,
    description: 'A GET on the GitHub REST API, e.g. "repos/owner/name/pulls/12/comments". Read-only.',
    input: { path: z.string() },
    run: async (a, { engine }) => {
      const out = await engine.exec('gh', ['api', '--method', 'GET', String(a.path)])
      try {
        return JSON.parse(out)
      } catch {
        return cap(out)
      }
    },
  },
  {
    name: 'github_pr_comment',
    unsafe: true,
    description: 'Post a comment on a pull request.',
    input: { ...pr, body: z.string().describe('The comment, in markdown.') },
    run: async (a, { engine }) => ({ url: (await engine.exec('gh', ['pr', 'comment', ...prArgs(a), '--body', String(a.body)])).trim() }),
  },
  {
    name: 'github_pr_review',
    unsafe: true,
    description: 'Submit a review on a pull request: approve, comment, or request changes.',
    input: { ...pr, event: z.enum(['approve', 'comment', 'request-changes']), body: z.string().optional().describe('The review, in markdown.') },
    run: async (a, { engine }) => ({
      output: (await engine.exec('gh', ['pr', 'review', ...prArgs(a), `--${a.event}`, ...(a.body ? ['--body', String(a.body)] : [])])).trim(),
    }),
  },
  {
    name: 'github_pr_merge',
    unsafe: true,
    description: 'Merge a pull request.',
    input: { ...pr, method: z.enum(['squash', 'merge', 'rebase']) },
    run: async (a, { engine }) => ({ output: (await engine.exec('gh', ['pr', 'merge', ...prArgs(a), `--${a.method}`])).trim() }),
  },
]

export const githubEventParams = {
  repo: z.string().optional().describe('owner/name. With number, the pull request; or give query instead.'),
  number: z.coerce.number().int().optional().describe('The pull request number.'),
  query: z.string().optional().describe("Words from the pull request's title, to find it with, instead of repo and number."),
  until: z.enum(['checks', 'activity', 'merged']).describe(
    'checks: every check has finished. activity: a new comment or review. merged: merged or closed.',
  ),
  timeout_minutes: z.number().optional().describe('Resolve as timed_out after this long.'),
}

type Check = { __typename?: string; status?: string; conclusion?: string; state?: string; name?: string; context?: string }

const activity = async (exec: Exec, p: Record<string, unknown>) => {
  const v = await view(exec, p, 'comments,reviews')
  return (v.comments?.length ?? 0) + (v.reviews?.length ?? 0)
}

/**
 * Finds the pull request a query names, and takes a baseline to compare later
 * checks with. Resolves at once if there is no such pull request.
 */
export async function startPr(
  p: Record<string, unknown>,
  exec: Exec,
): Promise<{ params?: Record<string, unknown>; resolve?: Record<string, unknown> } | void> {
  let params = p
  if (!p.repo || !p.number) {
    if (!p.query) return { resolve: { outcome: 'not_found', detail: 'Give repo and number, or a query.' } }
    const found: { number: number; title: string; url: string; repository: { nameWithOwner: string } }[] = JSON.parse(
      await exec('gh', ['search', 'prs', String(p.query), '--json', 'number,title,url,repository', '--limit', '1', '--sort', 'updated']),
    )
    if (!found.length) return { resolve: { outcome: 'not_found', detail: `No pull request matches “${p.query}”.` } }
    params = { ...p, repo: found[0].repository.nameWithOwner, number: found[0].number, title: found[0].title, url: found[0].url }
  }
  if (params.until === 'activity') params = { ...params, baseline: await activity(exec, params) }
  return { params }
}

/** The data once what the event waits for has happened; null until then. */
export async function checkPr(p: Record<string, unknown>, exec: Exec): Promise<Record<string, unknown> | null> {
  if (p.until === 'checks') {
    const checks: Check[] = (await view(exec, p, 'statusCheckRollup')).statusCheckRollup ?? []
    const pending = checks.filter((c) =>
      c.__typename === 'StatusContext' ? ['PENDING', 'EXPECTED'].includes(c.state ?? '') : c.status !== 'COMPLETED',
    )
    if (pending.length) return null
    const failed = checks
      .filter((c) =>
        c.__typename === 'StatusContext' ? c.state !== 'SUCCESS' : !['SUCCESS', 'NEUTRAL', 'SKIPPED'].includes(c.conclusion ?? ''),
      )
      .map((c) => c.name ?? c.context ?? '?')
    return failed.length
      ? { outcome: 'failed', failed, detail: `${p.repo}#${p.number}: ${failed.length} of ${checks.length} checks failed: ${failed.join(', ')}` }
      : { outcome: 'passed', failed, detail: `${p.repo}#${p.number}: all ${checks.length} checks passed.` }
  }
  if (p.until === 'activity') {
    const n = await activity(exec, p)
    const added = n - Number(p.baseline ?? 0)
    return added > 0 ? { outcome: 'activity', detail: `${p.repo}#${p.number}: ${added} new comment(s) or review(s).` } : null
  }
  const { state } = await view(exec, p, 'state')
  return state === 'OPEN' ? null : { outcome: String(state).toLowerCase(), detail: `${p.repo}#${p.number} is ${String(state).toLowerCase()}.` }
}

export const githubSources: Source[] = [
  {
    id: 'github.review-requests',
    everyMinutes: 10,
    poll: async (engine) => {
      const prs: { number: number; title: string; url: string; repository: { nameWithOwner: string } }[] = JSON.parse(
        await engine.exec('gh', ['search', 'prs', '--review-requested=@me', '--state=open', '--json', 'number,title,url,repository']),
      )
      return prs.map((p) => ({
        type: 'item' as const,
        params: {
          title: `Review ${p.repository.nameWithOwner}#${p.number}: ${p.title}`,
          key: `github:review:${p.url}`,
          events: [{ type: 'message', params: { text: `Your review is requested on ${p.repository.nameWithOwner}#${p.number}: ${p.url}` } }],
        },
      }))
    },
  },
]
