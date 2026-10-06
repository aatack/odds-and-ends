import { randomUUID } from 'node:crypto'
import type { Store } from '../../store.ts'
import { prEntityId } from '../../types.ts'
import type { Action, Entity } from '../../types.ts'
import type { Module, ModuleContext } from '../module.ts'

const day = 24 * 60 * 60 * 1000
const ttl = day

const ids = {
  root: 'github',
  pr: (url: string) => prEntityId(url)!,
  check: (pr: string, name: string) => `${pr}#check:${name}`,
  item: (pr: string, id: string) => `${pr}#item:${id}`,
}

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

const listQuery = `query {
  search(query: "is:pr is:open author:@me archived:false sort:updated-desc", type: ISSUE, first: 50) {
    nodes { ... on PullRequest {
      url number title isDraft updatedAt reviewDecision
      repository { nameWithOwner }
      commits(last: 1) { nodes { commit { statusCheckRollup { state } } } }
    } }
  }
}`

const prQuery = `query($url: URI!) { viewer { login } resource(url: $url) { ... on PullRequest {
  url number title body state isDraft mergeable reviewDecision additions deletions changedFiles
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
  url: string
  number: number
  title: string
  isDraft: boolean
  updatedAt: string
  reviewDecision: string | null
  repository: { nameWithOwner: string }
  commits: { nodes: { commit: { statusCheckRollup: { state: string } | null } }[] }
}

interface RawPr extends RawListPr {
  body: string
  state: string
  mergeable: string
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

/** One word for where a check is: what the view colours. */
export type CheckOutcome = 'failing' | 'pending' | 'passing' | 'skipped'

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

const outcomeOrder: CheckOutcome[] = ['failing', 'pending', 'skipped', 'passing']

/**
 * My pull requests. Each is an entity keyed by its URL, so a link to one
 * anywhere in the app is the same item.
 */
export class GitHub implements Module {
  readonly id = 'github'
  readonly name = 'GitHub'
  readonly root = { id: ids.root, type: 'github.home' }

  private readonly store: Store
  private readonly gh: ModuleContext['gh']

  constructor(context: ModuleContext) {
    this.store = context.store
    this.gh = context.gh
  }

  owns(entity: Entity): boolean {
    return entity.type.startsWith('github.')
  }

  /** My own approval of my own PR: owned, so it lasts past the cache. */
  private localApproval(prId: string): Entity | null {
    return this.store.children(prId).find((child) => child.type === 'github.localApproval') ?? null
  }

  actions(entity: Entity): Action[] {
    if (entity.type !== 'github.pr' || entity.data.state !== 'OPEN') return []
    const actions: Action[] = []
    if (!entity.data.mine) {
      actions.push({ id: 'approve', label: 'Approve', prompt: 'Approve: comment (optional), Enter to approve' })
    } else {
      if (!this.localApproval(entity.id) || !entity.data.autoMerge) {
        actions.push({
          id: 'approve',
          label: 'Approve',
          prompt: 'Approve locally and turn on auto-merge: Enter to confirm',
        })
      }
      actions.push({
        id: 'close',
        label: 'Close',
        prompt: 'Close and delete the branch: comment (optional), Enter to close',
      })
    }
    return actions
  }

  async perform(entity: Entity, action: string, text: string): Promise<void> {
    const url = String(entity.data.url)
    if (action === 'approve' && !entity.data.mine) {
      await this.write(['pr', 'review', url, '--approve', ...(text ? ['--body', text] : [])])
    } else if (action === 'approve') {
      if (!this.localApproval(entity.id)) {
        const id = randomUUID()
        this.store.transaction(() => {
          this.store.put(id, 'github.localApproval', { at: Date.now(), ...(text ? { note: text } : {}) })
          this.store.link(entity.id, id)
        })
      }
      const method = String(entity.data.mergeMethod ?? 'SQUASH').toLowerCase()
      await this.write(['pr', 'merge', url, '--auto', `--${method === 'merge' ? 'merge' : method}`])
    } else if (action === 'close') {
      await this.write(['pr', 'close', url, '--delete-branch', ...(text ? ['--comment', text] : [])])
    }
  }

  private async write(args: string[]): Promise<void> {
    checkWrite(args)
    await this.gh(args)
  }

  /** A PR seen only as a link becomes an item the first time it is looked at. */
  materialise(id: string): Entity | null {
    const url = id.startsWith('github:pr:') ? id.slice('github:pr:'.length) : null
    if (!url || prEntityId(url) !== id) return null
    return this.store.put(id, 'github.pr', { url }, { ttl })
  }

  staleAfter(id: string): number {
    return id === ids.root ? 2 * 60_000 : 60_000
  }

  async refresh(id: string): Promise<void> {
    const entity = this.store.get(id)
    if (entity?.type === 'github.home') await this.refreshList()
    else if (entity?.type === 'github.pr') await this.refreshPr(String(entity.data.url))
  }

  order(entity: Entity, children: Entity[]): Entity[] {
    if (entity.type !== 'github.pr') return children
    children = children.filter((child) => child.type !== 'github.localApproval')
    const checks = children.filter((child) => child.type === 'github.check')
    const discussion = children.filter((child) => child.type !== 'github.check')
    checks.sort(
      (a, b) =>
        outcomeOrder.indexOf(a.data.outcome as CheckOutcome) - outcomeOrder.indexOf(b.data.outcome as CheckOutcome) ||
        String(a.data.name).localeCompare(String(b.data.name)),
    )
    return [...checks, ...discussion]
  }

  present(entity: Entity): Entity {
    if (entity.type === 'github.pr') {
      const data = entity.data
      // `label` names the PR where nothing else does, as in a peek's bar.
      return { ...entity, data: { ...data, locallyApproved: Boolean(this.localApproval(entity.id)), label: data.title ? `${String(data.repo)}#${String(data.number)} ${String(data.title)}` : String(data.url) } }
    }
    if (entity.type === 'github.item') {
      // Shaped like a Slack message, so the same views draw it.
      return { ...entity, data: { ...entity.data, authorKey: entity.data.author, markdown: entity.data.text } }
    }
    return entity
  }

  private async refreshList(): Promise<void> {
    const data = await query<{ search: { nodes: RawListPr[] } }>(this.gh, listQuery)
    const prs = data.search.nodes.filter((node) => node.url)
    this.store.transaction(() => {
      for (const pr of prs) {
        const id = ids.pr(pr.url)
        const previous = this.store.get(id)?.data ?? {}
        this.store.put(id, 'github.pr', { ...previous, ...this.summary(pr) }, { ttl })
      }
      this.store.setCachedChildren(
        ids.root,
        prs.map((pr, index) => ({ id: ids.pr(pr.url), rank: index })),
        { ttl },
      )
    })
  }

  private summary(pr: RawListPr) {
    return {
      url: prEntityId(pr.url)!.slice('github:pr:'.length),
      number: pr.number,
      title: pr.title,
      repo: pr.repository.nameWithOwner,
      draft: pr.isDraft,
      review: pr.reviewDecision,
      checks: rollup(pr.commits.nodes[0]?.commit.statusCheckRollup?.state),
      updatedAt: pr.updatedAt,
    }
  }

  private async refreshPr(url: string): Promise<void> {
    const { resource: pr, viewer } = await query<{ resource: RawPr | null; viewer: { login: string } }>(
      this.gh,
      prQuery,
      { url },
    )
    if (!pr) throw new Error('not a pull request, or not visible to gh')
    const id = ids.pr(url)
    const contexts = pr.commits.nodes[0]?.commit.statusCheckRollup?.contexts.nodes ?? []
    const checks = contexts.map((context) => ({
      name:
        context.__typename === 'CheckRun'
          ? [context.checkSuite?.workflowRun?.workflow.name, context.name].filter(Boolean).join(' / ')
          : context.context,
      outcome: outcome(context),
      url: (context.__typename === 'CheckRun' ? context.detailsUrl : context.targetUrl) ?? undefined,
      startedAt: seconds(context.__typename === 'CheckRun' ? context.startedAt : context.createdAt),
      completedAt: seconds(context.__typename === 'CheckRun' ? context.completedAt : undefined),
    }))
    const counts = Object.fromEntries(outcomeOrder.map((kind) => [kind, checks.filter((check) => check.outcome === kind).length]))

    const items: { key: string; data: Record<string, unknown> }[] = []
    const body = clean(pr.body)
    items.push({
      key: 'description',
      data: { kind: 'description', author: pr.author?.login ?? 'ghost', ts: seconds(pr.createdAt), text: body || '_No description._', url },
    })
    for (const comment of pr.comments.nodes) {
      const text = clean(comment.body)
      if (!text) continue
      items.push({
        key: comment.id,
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
    items.sort((a, b) => Number(a.data.ts) - Number(b.data.ts))

    this.store.transaction(() => {
      const previous = this.store.get(id)?.data ?? {}
      this.store.put(
        id,
        'github.pr',
        {
          ...previous,
          ...this.summary(pr),
          state: pr.state,
          mergeable: pr.mergeable,
          additions: pr.additions,
          deletions: pr.deletions,
          files: pr.changedFiles,
          head: pr.headRefName,
          base: pr.baseRefName,
          counts,
          mine: pr.author?.login === viewer.login,
          autoMerge: Boolean(pr.autoMergeRequest),
          mergeMethod: pr.repository.viewerDefaultMergeMethod,
        },
        { ttl },
      )
      const children: { id: string; rank: number }[] = []
      // Only checks that need me get a row; passing and skipped ones are counted.
      for (const check of checks.filter((candidate) => candidate.outcome === 'failing' || candidate.outcome === 'pending')) {
        const checkId = ids.check(id, check.name)
        this.store.put(checkId, 'github.check', check, { ttl })
        children.push({ id: checkId, rank: 0 })
      }
      items.forEach((item, index) => {
        const itemId = ids.item(id, item.key)
        this.store.put(itemId, 'github.item', item.data, { ttl })
        children.push({ id: itemId, rank: index + 1 })
      })
      this.store.setCachedChildren(id, children, { ttl })
    })
  }
}
