import { randomUUID } from 'node:crypto'
import { link, value, values, type AppEvent } from '../../graph/events.ts'
import { loadedKey } from '../../types.ts'
import type { Entity, ItemType, LoadPart } from '../../types.ts'
import type { Module, ModuleContext } from '../module.ts'
import { me } from '../tasks/tasks.ts'
import { githubIds as ids, githubView, localApproval, outcomeOrder, prUrl, type CheckOutcome } from './view.ts'

export { badge, prName } from './view.ts'

const author = 'github'

/**
 * The only writes the app makes to GitHub, each started by me and confirmed.
 * Anything else is refused before gh runs.
 */
function checkWrite(args: string[]): void {
  const [noun, verb] = args
  const allowed =
    noun === 'pr' &&
    ((verb === 'review' && args.includes('--approve')) ||
      (verb === 'merge' && args.includes('--auto')) ||
      (verb === 'close' && args.includes('--delete-branch')))
  if (!allowed) throw new Error(`refusing gh ${args.slice(0, 2).join(' ')}`)
}

/** Reads: queries, never mutations. */
async function query<T>(gh: ModuleContext['gh'], text: string, variables: Record<string, string> = {}): Promise<T> {
  if (/^\s*mutation\b/.test(text)) throw new Error('GitHub is read-only')
  const args = ['api', 'graphql', '-f', `query=${text}`]
  for (const [key, value] of Object.entries(variables)) args.push('-F', `${key}=${value}`)
  const parsed = JSON.parse(await gh(args)) as { data?: T; errors?: { message: string }[] }
  if (parsed.errors?.length) throw new Error(parsed.errors.map((error) => error.message).join('; '))
  return parsed.data!
}

const listQuery = `query { viewer { login }
  search(query: "is:pr is:open author:@me archived:false sort:updated-desc", type: ISSUE, first: 50) {
    nodes { ... on PullRequest {
      url number title state isDraft updatedAt reviewDecision mergeable
      mergeStateStatus
      reviewRequests(first: 20) { nodes { requestedReviewer { ... on User { login } ... on Team { name } } } }
      reviewThreads(first: 100) { nodes { isResolved } }
      author { login } repository { nameWithOwner }
      latestReviews(first: 30) { nodes { author { login } state } }
      commits(last: 1) { nodes { commit { statusCheckRollup { state } } } }
    } }
  }
}`

const prQuery = `query($url: URI!) { viewer { login } resource(url: $url) { ... on PullRequest {
  url number title body state isDraft mergeable reviewDecision additions deletions changedFiles
      mergeStateStatus
      reviewRequests(first: 20) { nodes { requestedReviewer { ... on User { login } ... on Team { name } } } }
      reviewThreads(first: 100) { nodes { isResolved } }
  latestReviews(first: 30) { nodes { author { login } state } }
  headRefName baseRefName createdAt updatedAt
  author { login } repository { nameWithOwner viewerDefaultMergeMethod }
  autoMergeRequest { enabledAt }
  comments(first: 100) { nodes { id url author { login } body createdAt } }
  reviews(first: 50) { nodes { id url author { login } body state createdAt
    comments(first: 50) { nodes { id url path line originalLine diffHunk body createdAt } } } }
  commits(last: 1) { nodes { commit { statusCheckRollup { state contexts(first: 100) { nodes { __typename
    ... on CheckRun { name status conclusion detailsUrl startedAt completedAt
      checkSuite { workflowRun { workflow { name } } } }
    ... on StatusContext { context state targetUrl createdAt } } } } } } }
} } }`

type Login = { login: string } | null

interface RawListPr {
  state: string
  author: Login
  mergeable: string
  latestReviews: { nodes: { author: Login; state: string }[] }
  url: string
  number: number
  title: string
  isDraft: boolean
  updatedAt: string
  reviewDecision: string | null
  mergeStateStatus?: string
  reviewRequests?: { nodes: { requestedReviewer: { login?: string; name?: string } | null }[] }
  reviewThreads?: { nodes: { isResolved: boolean }[] }
  repository: { nameWithOwner: string }
  commits: { nodes: { commit: { statusCheckRollup: { state: string } | null } }[] }
}

interface RawPr extends RawListPr {
  body: string
  additions: number
  deletions: number
  changedFiles: number
  headRefName: string
  baseRefName: string
  createdAt: string
  author: Login
  repository: { nameWithOwner: string; viewerDefaultMergeMethod?: string }
  autoMergeRequest: { enabledAt: string } | null
  comments: { nodes: { id: string; url: string; author: Login; body: string; createdAt: string }[] }
  reviews: {
    nodes: {
      id: string
      url: string
      author: Login
      body: string
      state: string
      createdAt: string
      comments: {
        nodes: {
          id: string
          url: string
          path: string
          line: number | null
          originalLine: number | null
          diffHunk: string
          body: string
          createdAt: string
        }[]
      }
    }[]
  }
  commits: {
    nodes: {
      commit: {
        statusCheckRollup: {
          state: string
          contexts: { nodes: RawContext[] }
        } | null
      }
    }[]
  }
}

type RawContext =
  | {
      __typename: 'CheckRun'
      name: string
      status: string
      conclusion: string | null
      detailsUrl: string | null
      startedAt: string | null
      completedAt: string | null
      checkSuite: { workflowRun: { workflow: { name: string } } | null } | null
    }
  | { __typename: 'StatusContext'; context: string; state: string; targetUrl: string | null; createdAt: string }

function outcome(context: RawContext): CheckOutcome {
  if (context.__typename === 'StatusContext') {
    return context.state === 'SUCCESS' ? 'passing' : context.state === 'PENDING' || context.state === 'EXPECTED' ? 'pending' : 'failing'
  }
  if (context.status !== 'COMPLETED') return 'pending'
  switch (context.conclusion) {
    case 'SUCCESS':
      return 'passing'
    case 'SKIPPED':
    case 'NEUTRAL':
      return 'skipped'
    default:
      return 'failing'
  }
}

function rollup(state: string | undefined | null): CheckOutcome | null {
  if (!state) return null
  return state === 'SUCCESS' ? 'passing' : state === 'PENDING' || state === 'EXPECTED' ? 'pending' : 'failing'
}

function seconds(iso: string | null | undefined): string | undefined {
  return iso ? String(Date.parse(iso) / 1000) : undefined
}

/** When something on GitHub happened, as an event's timestamp. */
function millis(iso: string | null | undefined): number {
  return iso ? Date.parse(iso) : 0
}

/** HTML comments are bot bookkeeping; nothing to read. */
function clean(body: string): string {
  return body.replace(/<!--[\s\S]*?-->/g, '').trim()
}

/** The last few lines of a hunk, which is where the comment points. */
function hunkTail(hunk: string): string {
  return hunk.split('\n').slice(-6).join('\n')
}

const reviewWords: Record<string, string> = {
  APPROVED: 'approved',
  CHANGES_REQUESTED: 'requested changes',
  COMMENTED: 'reviewed',
  DISMISSED: 'review dismissed',
}

/**
 * My pull requests. Each is an entity keyed by its URL, so a link to one
 * anywhere in the app is the same item.
 */
export class GitHub implements Module {
  readonly view = githubView

  private readonly context: ModuleContext
  /** One query per PR at a time, whichever of its parts asked. */
  private readonly loading = new Map<string, Promise<void>>()

  constructor(context: ModuleContext) {
    this.context = context
  }

  async perform(entity: Entity, action: string, text: string): Promise<AppEvent[]> {
    const url = prUrl(entity.id)
    const events: AppEvent[] = []
    if (action === 'approve' && !entity.data.mine) {
      await this.write(['pr', 'review', url, '--approve', ...(text ? ['--body', text] : [])])
    } else if (action === 'approve') {
      if (!localApproval(this.context.lens, entity.id)) {
        const id = randomUUID()
        const now = this.context.now()
        events.push(...values(id, { type: 'github.localApproval', at: now, note: text || undefined }, now, me), link(entity.id, id, now, me))
        this.context.owned.write(events)
      }
      const method = String(entity.data.mergeMethod ?? 'SQUASH').toLowerCase()
      await this.write(['pr', 'merge', url, '--auto', `--${method === 'merge' ? 'merge' : method}`])
    } else if (action === 'close') {
      await this.write(['pr', 'close', url, '--delete-branch', ...(text ? ['--comment', text] : [])])
    }
    return events
  }

  private async write(args: string[]): Promise<void> {
    checkWrite(args)
    await this.context.gh(args)
  }

  async load(id: string, _part: LoadPart, type: ItemType): Promise<void> {
    if (type === 'github.home') await this.loadList()
    // A PR is one query whichever part was asked for, so it marks both.
    else if (type === 'github.pr') {
      const url = prUrl(id)
      let running = this.loading.get(url)
      if (!running) {
        running = this.loadPr(url).finally(() => this.loading.delete(url))
        this.loading.set(url, running)
      }
      await running
    }
  }

  private async loadList(): Promise<void> {
    const data = await query<{ search: { nodes: RawListPr[] }; viewer: { login: string } }>(this.context.gh, listQuery)
    const prs = data.search.nodes.filter((node) => node.url)
    const now = this.context.now()
    this.context.cache.write(
      [
        ...prs.flatMap((pr) => [
          ...this.summary(pr, data.viewer.login),
          value(ids.pr(pr.url), loadedKey('self'), now, 0, author),
          link(ids.root, ids.pr(pr.url), 0, author),
        ]),
      ],
      { replaceLinksFrom: [ids.root] },
    )
  }

  /**
   * What a PR is like at a glance; enough to work out its badge. None of it
   * has a date of its own, so all of it sits at 0, under anything of mine.
   */
  private summary(pr: RawListPr, viewer: string): AppEvent[] {
    const author_ = pr.author?.login
    const approvers = pr.latestReviews.nodes
      .filter((review) => review.state === 'APPROVED')
      .map((review) => review.author?.login)
    const requested = (pr.reviewRequests?.nodes ?? [])
      .map((request) => request.requestedReviewer?.login ?? request.requestedReviewer?.name)
      .filter((who): who is string => Boolean(who))
    const changesRequestedByMe = pr.latestReviews.nodes.some((review) => review.state === 'CHANGES_REQUESTED' && review.author?.login === viewer)
    return values(
      ids.pr(pr.url),
      {
        type: 'github.pr',
        state: pr.state,
        mine: author_ === viewer,
        conflicts: pr.mergeable === 'CONFLICTING',
        approvedByMe: approvers.includes(viewer),
        approvedByOthers: approvers.some((login) => login && login !== viewer && login !== author_),
        url: prUrl(ids.pr(pr.url)),
        number: pr.number,
        title: pr.title,
        repo: pr.repository.nameWithOwner,
        draft: pr.isDraft,
        review: pr.reviewDecision,
        checks: rollup(pr.commits.nodes[0]?.commit.statusCheckRollup?.state),
        updatedAt: pr.updatedAt,
        // What GitHub says stands between it and merging: BEHIND, BLOCKED, CLEAN, DIRTY, DRAFT, UNSTABLE…
        mergeState: pr.mergeStateStatus ?? null,
        // Who has been asked to review and hasn't yet; whether that includes me.
        reviewers: requested.filter((who) => who !== viewer),
        reviewRequestedOfMe: requested.includes(viewer),
        changesRequestedByMe,
        unresolved: (pr.reviewThreads?.nodes ?? []).filter((thread) => !thread.isResolved).length,
      },
      0,
      author,
    )
  }

  /**
   * Everything about a PR. The description, comments and reviews are written
   * at the time they were made, as are their links from the PR, so I can hide
   * one by unlinking it later; checks and the PR's state have no date, so 0.
   */
  private async loadPr(url: string): Promise<void> {
    const { resource: pr, viewer } = await query<{ resource: RawPr | null; viewer: { login: string } }>(this.context.gh, prQuery, { url })
    if (!pr) throw new Error('not a pull request, or not visible to gh')
    const id = ids.pr(url)
    const now = this.context.now()
    const contexts = pr.commits.nodes[0]?.commit.statusCheckRollup?.contexts.nodes ?? []
    const checks = contexts.map((context) => ({
      name:
        context.__typename === 'CheckRun'
          ? [context.checkSuite?.workflowRun?.workflow.name, context.name].filter(Boolean).join(' / ')
          : context.context,
      outcome: outcome(context),
      url: (context.__typename === 'CheckRun' ? context.detailsUrl : context.targetUrl) ?? null,
      startedAt: seconds(context.__typename === 'CheckRun' ? context.startedAt : context.createdAt) ?? null,
      completedAt: seconds(context.__typename === 'CheckRun' ? context.completedAt : undefined) ?? null,
    }))
    const counts = Object.fromEntries(outcomeOrder.map((kind) => [kind, checks.filter((check) => check.outcome === kind).length]))

    const items: { key: string; at: number; data: Record<string, unknown> }[] = []
    const body = clean(pr.body)
    items.push({
      key: 'description',
      at: millis(pr.createdAt),
      data: { kind: 'description', author: pr.author?.login ?? 'ghost', ts: seconds(pr.createdAt), text: body || '_No description._', url },
    })
    for (const comment of pr.comments.nodes) {
      const text = clean(comment.body)
      if (!text) continue
      items.push({
        key: comment.id,
        at: millis(comment.createdAt),
        data: { kind: 'comment', author: comment.author?.login ?? 'ghost', ts: seconds(comment.createdAt), text, url: comment.url },
      })
    }
    for (const review of pr.reviews.nodes) {
      const inline = review.comments.nodes.map((comment) => {
        const line = comment.line ?? comment.originalLine
        const where = `\`${comment.path}${line ? `:${line}` : ''}\``
        return `${where}\n\`\`\`diff\n${hunkTail(comment.diffHunk)}\n\`\`\`\n${clean(comment.body)}`
      })
      const text = [clean(review.body), ...inline].filter(Boolean).join('\n\n')
      // A bare "commented" review with nothing in it is noise.
      if (!text && review.state === 'COMMENTED') continue
      items.push({
        key: review.id,
        at: millis(review.createdAt),
        data: {
          kind: 'review',
          author: review.author?.login ?? 'ghost',
          ts: seconds(review.createdAt),
          verdict: reviewWords[review.state] ?? review.state.toLowerCase(),
          state: review.state,
          text,
          url: review.url,
        },
      })
    }

    const events: AppEvent[] = [
      ...this.summary(pr, viewer.login),
      ...values(
        id,
        {
          state: pr.state,
          mergeable: pr.mergeable,
          additions: pr.additions,
          deletions: pr.deletions,
          files: pr.changedFiles,
          head: pr.headRefName,
          base: pr.baseRefName,
          counts,
          autoMerge: Boolean(pr.autoMergeRequest),
          mergeMethod: pr.repository.viewerDefaultMergeMethod ?? null,
        },
        0,
        author,
      ),
      value(id, loadedKey('self'), now, 0, author),
      value(id, loadedKey('children'), now, 0, author),
    ]
    // Only checks that need me get a row; passing and skipped ones are counted.
    for (const check of checks.filter((candidate) => candidate.outcome === 'failing' || candidate.outcome === 'pending')) {
      const checkId = ids.check(id, check.name)
      events.push(...values(checkId, { type: 'github.check', ...check }, 0, author), link(id, checkId, 0, author))
    }
    for (const item of items) {
      const itemId = ids.item(id, item.key)
      events.push(value(itemId, 'type', 'github.item', 0, author), ...values(itemId, item.data, item.at, author), link(id, itemId, item.at, author))
    }
    this.context.cache.write(events, { replaceLinksFrom: [id] })
  }
}
