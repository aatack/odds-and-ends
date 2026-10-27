// GitHub, through the `gh` CLI, as whoever it is signed in as: an Integration (integrations.ts) in the shape every
// integration takes. Its reads are safe; its writes (comment, create, merge) are unsafe, so only a person runs them.

import { z } from 'zod'
import type { Engine } from './engine'
import type { Action, Integration } from './integrations'

const repo = z.string().optional().describe('owner/name. Left out, the default repo from its setup.')
const number = z.number().int().describe('The pull request or issue number.')

/** The repo a call is for: the one given, else the default set up, else a clear error. */
function repoOf(engine: Engine, given: unknown): string {
  const r = (given ? String(given) : engine.settings.integrationValue('github', 'default_repo')) ?? ''
  if (!/^[\w.-]+\/[\w.-]+$/.test(r)) throw new Error('Name the repo (owner/name), or set a default repo on the Integrations page.')
  return r
}

/** Runs gh, and reads its JSON. */
async function gh(engine: Engine, args: string[]): Promise<unknown> {
  const out = await engine.exec('gh', args)
  try {
    return JSON.parse(out)
  } catch {
    return out.trim()
  }
}

const PR_FIELDS = 'number,title,author,state,isDraft,headRefName,baseRefName,url,updatedAt'
const ISSUE_FIELDS = 'number,title,author,state,labels,url,updatedAt'

const read: Action[] = [
  {
    name: 'github_pr_list',
    unsafe: false,
    description: "A repo's pull requests: open ones by default. Narrow with `search` (GitHub's syntax, e.g. \"author:@me review:required\").",
    input: { repo, state: z.enum(['open', 'closed', 'merged', 'all']).default('open'), search: z.string().optional(), limit: z.number().int().default(30) },
    run: async (a, { engine }) =>
      gh(engine, ['pr', 'list', '-R', repoOf(engine, a.repo), '--state', String(a.state), '--limit', String(a.limit), '--json', PR_FIELDS, ...(a.search ? ['--search', String(a.search)] : [])]),
  },
  {
    name: 'github_pr_view',
    unsafe: false,
    description: 'One pull request: its body, state, whether it can merge, its review decision, its checks and its comments.',
    input: { repo, number },
    run: async (a, { engine }) =>
      gh(engine, ['pr', 'view', String(a.number), '-R', repoOf(engine, a.repo), '--json', `${PR_FIELDS},body,mergeable,mergeStateStatus,reviewDecision,statusCheckRollup,comments`]),
  },
  {
    name: 'github_pr_checks',
    unsafe: false,
    description: "A pull request's CI checks: each one's name, state and link.",
    input: { repo, number },
    run: async (a, { engine }) => gh(engine, ['pr', 'checks', String(a.number), '-R', repoOf(engine, a.repo), '--json', 'name,state,bucket,link']),
  },
  {
    name: 'github_pr_diff',
    unsafe: false,
    description: "A pull request's diff, cut at 100k characters.",
    input: { repo, number },
    run: async (a, { engine }) => String(await engine.exec('gh', ['pr', 'diff', String(a.number), '-R', repoOf(engine, a.repo)])).slice(0, 100_000),
  },
  {
    name: 'github_issue_list',
    unsafe: false,
    description: "A repo's issues: open ones by default. Narrow with `search`.",
    input: { repo, state: z.enum(['open', 'closed', 'all']).default('open'), search: z.string().optional(), limit: z.number().int().default(30) },
    run: async (a, { engine }) =>
      gh(engine, ['issue', 'list', '-R', repoOf(engine, a.repo), '--state', String(a.state), '--limit', String(a.limit), '--json', ISSUE_FIELDS, ...(a.search ? ['--search', String(a.search)] : [])]),
  },
  {
    name: 'github_issue_view',
    unsafe: false,
    description: 'One issue: its body, labels and comments.',
    input: { repo, number },
    run: async (a, { engine }) => gh(engine, ['issue', 'view', String(a.number), '-R', repoOf(engine, a.repo), '--json', `${ISSUE_FIELDS},body,comments`]),
  },
  {
    name: 'github_api_get',
    unsafe: false,
    description: 'Any read of the REST API: a GET of `path` (e.g. "repos/o/r/actions/runs?per_page=5"). Only GET: writes go through the write tools.',
    input: { path: z.string().describe('The path, without the host.') },
    run: async (a, { engine }) => gh(engine, ['api', '--method', 'GET', String(a.path).replace(/^\/+/, '')]),
  },
]

const write: Action[] = [
  {
    name: 'github_pr_comment',
    unsafe: true,
    description: 'Comment on a pull request, as whoever gh is signed in as.',
    input: { repo, number, body: z.string() },
    run: async (a, { engine }) => gh(engine, ['pr', 'comment', String(a.number), '-R', repoOf(engine, a.repo), '--body', String(a.body)]),
  },
  {
    name: 'github_pr_merge',
    unsafe: true,
    description: 'Merge a pull request (squash by default), deleting its branch.',
    input: { repo, number, method: z.enum(['squash', 'merge', 'rebase']).default('squash') },
    run: async (a, { engine }) => gh(engine, ['pr', 'merge', String(a.number), '-R', repoOf(engine, a.repo), `--${a.method}`, '--delete-branch']),
  },
  {
    name: 'github_issue_comment',
    unsafe: true,
    description: 'Comment on an issue.',
    input: { repo, number, body: z.string() },
    run: async (a, { engine }) => gh(engine, ['issue', 'comment', String(a.number), '-R', repoOf(engine, a.repo), '--body', String(a.body)]),
  },
  {
    name: 'github_issue_create',
    unsafe: true,
    description: 'Open an issue. Answers its link.',
    input: { repo, title: z.string(), body: z.string().default('') },
    run: async (a, { engine }) => gh(engine, ['issue', 'create', '-R', repoOf(engine, a.repo), '--title', String(a.title), '--body', String(a.body)]),
  },
]

export const github: Integration = {
  id: 'github',
  name: 'GitHub',
  description: 'Pull requests, issues and the REST API, through the `gh` CLI on this machine.',
  auth: { kind: 'cli', instructions: 'Signed in through `gh` itself, as whoever it is logged in as: run `gh auth login` in a terminal to change that.' },
  setup: [
    {
      key: 'default_repo',
      label: 'Default repo',
      instructions: 'The repo a tool uses when none is named, as `owner/name` (e.g. `theengineeringco/branch-demo`). Optional.',
    },
  ],
  tools: [...read, ...write],
  triggers: [],
  status: async (engine) => `Signed in as ${String(await engine.exec('gh', ['api', 'user', '--jq', '.login'])).trim().split('\n')[0]}`,
}
