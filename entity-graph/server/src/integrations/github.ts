import { z } from 'zod'
import type { ToolDef } from '../../../src/core/source/index'
import { directory, json, ok, said } from './exec'

// GitHub, through the `gh` CLI. It already knows how to authenticate, how to
// page, and how to turn a URL into a repo and a number, so nothing here talks to
// the REST API directly. Authentication is whatever `gh` is configured with —
// `gh auth login`, or a `GH_TOKEN` in `server/.env`.

const GH = 'gh'

/** Fields worth having for a pull request in a list: enough to decide on it. */
const LIST_FIELDS = 'number,title,url,state,isDraft,author,createdAt,updatedAt,labels'

/** Fields for a single pull request, where the point is to read the thing. */
const DETAIL_FIELDS = [
  'number,title,url,state,isDraft,author,createdAt,updatedAt,labels,body',
  'baseRefName,headRefName,additions,deletions,changedFiles',
  'reviewDecision,mergeable,mergeStateStatus,latestReviews,reviewRequests,comments',
].join(',')

const PR_URL = /^https?:\/\/[^/\s]+\/[^/\s]+\/[^/\s]+\/pull\/\d+/
const PR_SHORT = /^([\w.-]+\/[\w.-]+)[#/](\d+)$/

/**
 * The `gh` arguments naming one pull request. A URL is passed through; the
 * `owner/repo#123` shorthand becomes a number and a `--repo`. A bare number is
 * refused: `gh` would resolve it against the working directory, which for this
 * server is meaningless.
 */
export function pullRequestArgs(reference: string): string[] {
  const ref = reference.trim()
  if (PR_URL.test(ref)) return [ref]
  const short = PR_SHORT.exec(ref)
  if (short) return [short[2], '--repo', short[1]]
  throw new Error(
    `"${reference}" doesn't name a pull request — give its URL, or owner/repo#123`,
  )
}

/**
 * Who `gh` is signed in as. Asked once and remembered: "not mine" is a filter on
 * every repo listing, and it is the same answer every time.
 */
let login: Promise<string> | null = null

function currentLogin(): Promise<string> {
  login ??= json<{ login: string }>(GH, ['api', 'user'])
    .then((user) => user.login)
    .catch((e) => {
      login = null
      throw e
    })
  return login
}

interface PullRequest {
  number: number
  title: string
  url: string
  author?: { login?: string } | null
  repository?: { nameWithOwner?: string } | null
}

/** A pull request reference, phrased the same way in every tool that takes one. */
const pullRequest = z
  .string()
  .min(1)
  .describe('Pull request URL, or owner/repo#123')

/**
 * The two tools that are told *where* rather than which repository. Everything
 * else here names a repo outright, because this server has no working directory
 * worth resolving one against — but a worktree does, and it is precisely the
 * thing that knows its own remote and its own branch. So a caller holding a
 * checkout doesn't have to work out the `owner/repo` that `gh` can read off it.
 */
const checkout = z
  .string()
  .min(1)
  .describe('The checkout to act from — a worktree path; the repo is read off its remote')

/** The number at the end of a pull request URL, which is the only part `gh` prints. */
const numberIn = (url: string): number | null => {
  const match = /\/pull\/(\d+)/.exec(url)
  return match ? Number(match[1]) : null
}

export const GITHUB_TOOLS: ToolDef[] = [
  {
    id: 'github.listMyPullRequests',
    name: 'List my pull requests',
    description:
      'Open pull requests you authored, across every repository you can see, most recently updated first.',
    safety: 'dangerous',
    args: z.object({
      state: z.enum(['open', 'closed']).default('open').describe('Which pull requests to list'),
      limit: z.number().int().min(1).max(100).default(30).describe('How many to fetch'),
    }),
    handler: async ({ state, limit }) => {
      const pullRequests = await json<PullRequest[]>(GH, [
        'search',
        'prs',
        '--author=@me',
        `--state=${state}`,
        '--sort=updated',
        '--order=desc',
        `--limit=${limit}`,
        '--json',
        `${LIST_FIELDS},repository,commentsCount`,
      ])
      return { count: pullRequests.length, pullRequests }
    },
  },

  {
    id: 'github.getPullRequest',
    name: 'Get pull request',
    description:
      'Everything about one pull request: its description, review state, mergeability, diff size and comments.',
    safety: 'dangerous',
    args: z.object({ pullRequest }),
    handler: async (args) =>
      json(GH, ['pr', 'view', ...pullRequestArgs(args.pullRequest), '--json', DETAIL_FIELDS]),
  },

  {
    id: 'github.approvePullRequest',
    name: 'Approve pull request',
    description: 'Submit an approving review, optionally with a comment.',
    safety: 'dangerous',
    args: z.object({
      pullRequest,
      comment: z.string().optional().describe('Body of the review, if any'),
    }),
    handler: async (args) => {
      const result = await ok(GH, [
        'pr',
        'review',
        ...pullRequestArgs(args.pullRequest),
        '--approve',
        ...(args.comment ? ['--body', args.comment] : []),
      ])
      return { approved: true, output: said(result) }
    },
  },

  {
    id: 'github.markPullRequestReady',
    name: 'Mark pull request ready for review',
    description: 'Take a pull request out of draft.',
    safety: 'dangerous',
    args: z.object({ pullRequest }),
    handler: async (args) => {
      const result = await ok(GH, ['pr', 'ready', ...pullRequestArgs(args.pullRequest)])
      return { ready: true, output: said(result) }
    },
  },

  {
    id: 'github.mergePullRequest',
    name: 'Merge pull request',
    description:
      'Merge a pull request. `auto` queues it to merge once its requirements are met rather than merging now.',
    safety: 'dangerous',
    args: z.object({
      pullRequest,
      method: z.enum(['squash', 'merge', 'rebase']).default('squash').describe('How to merge'),
      auto: z.boolean().default(false).describe('Merge automatically once checks pass'),
      deleteBranch: z.boolean().default(false).describe('Delete the branch afterwards'),
    }),
    handler: async (args) => {
      const result = await ok(GH, [
        'pr',
        'merge',
        ...pullRequestArgs(args.pullRequest),
        `--${args.method}`,
        ...(args.auto ? ['--auto'] : []),
        ...(args.deleteBranch ? ['--delete-branch'] : []),
      ])
      return { merged: !args.auto, queued: args.auto, output: said(result) }
    },
  },

  {
    id: 'github.closePullRequest',
    name: 'Close pull request',
    description: 'Close a pull request without merging it, optionally leaving a parting comment.',
    safety: 'dangerous',
    args: z.object({
      pullRequest,
      comment: z.string().optional().describe('Comment to leave when closing'),
      deleteBranch: z.boolean().default(false).describe('Delete the branch afterwards'),
    }),
    handler: async (args) => {
      const result = await ok(GH, [
        'pr',
        'close',
        ...pullRequestArgs(args.pullRequest),
        ...(args.comment ? ['--comment', args.comment] : []),
        ...(args.deleteBranch ? ['--delete-branch'] : []),
      ])
      return { closed: true, output: said(result) }
    },
  },

  {
    id: 'github.pullRequestForBranch',
    name: 'Pull request for a branch',
    description: [
      'The open pull request raised from `branch`, or `null` when there isn’t one.',
      '',
      'This is the "has this been raised already?" question, answerable without a',
      'URL in hand — which is what something that just pushed a branch has. Nothing',
      'found is an ordinary answer, not an error.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: checkout,
      branch: z.string().min(1).describe('The head branch the pull request would come from'),
    }),
    handler: async ({ path, branch }) => {
      const cwd = directory(path)
      const found = await json<PullRequest[]>(
        GH,
        ['pr', 'list', '--head', branch, '--state', 'open', '--limit', '1', '--json', LIST_FIELDS],
        { cwd },
      )
      return { branch, pullRequest: found[0] ?? null }
    },
  },

  {
    id: 'github.createPullRequest',
    name: 'Create pull request',
    description: [
      'Open a pull request from the branch checked out at `path`.',
      '',
      '`gh` reads the repository off the checkout’s remote and the head branch off',
      'its `HEAD`, so neither has to be named: a worktree already knows both. The',
      'branch has to be pushed first — `git.push` does that.',
      '',
      'The body goes in over standard input, so a description as long as an exported',
      'set of notes never reaches an argument vector.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: checkout,
      title: z.string().min(1).describe('The pull request’s title'),
      body: z.string().default('').describe('The description, as markdown'),
      base: z
        .string()
        .optional()
        .describe('Branch to merge into. Omit for the repository’s default'),
      draft: z.boolean().default(false).describe('Open it as a draft'),
    }),
    handler: async ({ path, title, body, base, draft }) => {
      const cwd = directory(path)
      const result = await ok(
        GH,
        [
          'pr',
          'create',
          '--title',
          title,
          // `-` is stdin. `--body ''` would also work for an empty description, but
          // one path through is one thing to be wrong about.
          '--body-file',
          '-',
          ...(base ? ['--base', base] : []),
          ...(draft ? ['--draft'] : []),
        ],
        { cwd, stdin: body },
      )
      // All `gh` prints is the URL, so the number is read back off it rather than
      // asked for in a second call.
      const url = said(result).split('\n').find((line) => line.includes('/pull/'))?.trim() ?? ''
      if (!url) throw new Error(`\`${GH}\` created something but didn't say where: ${said(result)}`)
      return { url, number: numberIn(url), created: true }
    },
  },

  {
    id: 'github.listRepoPullRequests',
    name: 'List a repo’s pull requests',
    description:
      "Open pull requests on one repository, yours left out — the queue of what's waiting on you. Paged with `offset`.",
    safety: 'dangerous',
    args: z.object({
      repo: z.string().min(1).describe('owner/repo'),
      offset: z.number().int().min(0).default(0).describe('How many to skip'),
      limit: z.number().int().min(1).max(100).default(20).describe('How many to return'),
      includeMine: z.boolean().default(false).describe('Keep your own pull requests in'),
    }),
    handler: async ({ repo, offset, limit, includeMine }) => {
      const me = includeMine ? null : await currentLogin()
      // `gh` pages by "how many from the top", so the window is taken here: ask
      // for one past the end of it, and the extra is what says there's more.
      const wanted = offset + limit
      const search = includeMine ? 'sort:updated-desc' : `sort:updated-desc -author:${me}`
      const all = await json<PullRequest[]>(GH, [
        'pr',
        'list',
        '--repo',
        repo,
        '--state',
        'open',
        // Leading `sort:` rather than the bare `-author:` the query means, so
        // that `gh` can't mistake the value for a flag of its own.
        '--search',
        search,
        `--limit=${wanted + 1}`,
        '--json',
        LIST_FIELDS,
      ])
      // The search qualifier does the filtering server-side; this is the check
      // that it did, since a silent failure there would quietly show your own.
      const theirs = me ? all.filter((pr) => pr.author?.login !== me) : all
      return {
        repo,
        offset,
        limit,
        hasMore: theirs.length > wanted,
        pullRequests: theirs.slice(offset, wanted),
      }
    },
  },
]
