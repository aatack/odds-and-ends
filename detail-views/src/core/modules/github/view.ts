import type { Lens, ModuleView } from '../../present.ts'
import { prEntityId } from '../../types.ts'
import type { Action, Badge, Entity, ItemType } from '../../types.ts'

const minute = 60_000

export const githubIds = {
  root: 'github',
  pr: (url: string) => prEntityId(url)!,
  check: (pr: string, name: string) => `${pr}#check:${name}`,
  item: (pr: string, id: string) => `${pr}#item:${id}`,
}

/** The URL a PR's id is made from. */
export const prUrl = (id: string): string => id.slice('github:pr:'.length)

/** One word for where a check is: what the view colours. */
export type CheckOutcome = 'failing' | 'pending' | 'passing' | 'skipped'

export const outcomeOrder: CheckOutcome[] = ['failing', 'pending', 'skipped', 'passing']

/**
 * A PR's name as shown: its title without a leading `#1234` or a
 * conventional-commit prefix (`feat: `, `fix(ui): `, `chore!: `).
 */
export function prName(title: string): string {
  const name = title
    .replace(/^#\d+\s*/, '')
    .replace(/^[a-z]+(?:\/[a-z]+)*(?:\([^)]*\))?!?:\s+/, '')
    .trim()
  return name || title
}

/** Whose move it is: mine to make, someone else's, or nobody's (wait, or done). */
export type Turn = 'me' | 'them' | 'none'

/**
 * One thing between a PR and being merged, from my side: what it is and
 * whose move it is. `tone` colours it: red for broken, yellow for waiting,
 * green for clear, purple for merged, gray for closed.
 */
export interface Blocker {
  id: string
  label: string
  turn: Turn
  tone: 'red' | 'yellow' | 'green' | 'purple' | 'gray'
}

/**
 * Whether a PR of mine counts as approved: GitHub's review decision says so,
 * or I approved it here (GitHub won't let me approve my own). One rule, for
 * the blocker and for whether Approve is offered.
 */
export function approvedMine(data: Record<string, unknown>, locallyApproved: boolean): boolean {
  return data.review === 'APPROVED' || locallyApproved
}

/**
 * What stands between a PR and being merged, most pressing first, as my
 * workflow goes. Views show only the first: what I need to know (and
 * usually do) to move it on. Empty until enough is known.
 *
 * Mine: approve it myself (here: GitHub won't let me approve my own); then
 * CI failing or merge conflicts, or changes asked for, to fix; then someone
 * else's review; then CI running, while it merges itself (auto-merge, which
 * approving turns on), or for me to turn that on.
 *
 * Someone else's: blocked on me until I approve it (unless I asked for
 * changes: then on them); after that, only waiting for it to be merged.
 *
 * What doesn't change what I do (open threads, behind its base) isn't shown.
 */
export function blockers(data: Record<string, unknown>, locallyApproved: boolean): Blocker[] {
  if (data.mine === undefined) return []
  if (data.state === 'MERGED') return [{ id: 'merged', label: 'Merged', turn: 'none', tone: 'purple' }]
  if (data.state === 'CLOSED') return [{ id: 'closed', label: 'Closed', turn: 'none', tone: 'gray' }]
  const out: Blocker[] = []
  const add = (id: string, label: string, turn: Turn, tone: Blocker['tone']) => out.push({ id, label, turn, tone })

  if (!data.mine) {
    // Changes I asked for are theirs to make, until they ask me again.
    if (data.changesRequestedByMe && !data.approvedByMe && !data.reviewRequestedOfMe) add('changes', 'You requested changes', 'them', 'yellow')
    else if (!data.approvedByMe) add('review', data.reviewRequestedOfMe ? 'Review requested' : 'Review it', 'me', 'yellow')
    else add('approved', 'Approved', 'them', 'green')
    return out
  }

  if (!approvedMine(data, locallyApproved)) add('self-review', 'Approve it', 'me', 'yellow')
  if (data.checks === 'failing') add('ci', 'CI failing', 'me', 'red')
  if (data.conflicts || data.mergeState === 'DIRTY') add('conflicts', 'Conflicts', 'me', 'red')
  if (data.review === 'CHANGES_REQUESTED') add('changes', 'Changes requested', 'me', 'red')
  else if (data.review !== 'APPROVED' && !data.approvedByOthers) {
    const others = Array.isArray(data.reviewers) ? (data.reviewers as string[]) : []
    add('awaiting', others.length ? `Awaiting ${others.join(', ')}` : 'Awaiting review', 'them', 'yellow')
  }
  if (data.checks === 'pending') add('pending', 'CI running', 'none', 'gray')
  if (!data.autoMerge) add('merge', 'Merge it', 'me', 'green')
  else if (!out.length) add('auto', 'Auto-merging', 'none', 'green')
  return out
}

/** What there is to fix on a PR of mine, as its blockers say: CI failing, conflicts, changes asked for. */
export function toFix(data: Record<string, unknown>): string[] {
  const reasons = blockers(data, true)
  return reasons.filter((blocker) => ['ci', 'conflicts', 'changes'].includes(blocker.id)).map((blocker) => blocker.id)
}

/**
 * The Claude sessions in an item's context, which can work on it: task
 * topics and Claude module sessions, by id.
 */
export function sessionsInContext(entity: Entity, lens: Lens): string[] {
  const context = entity.data.context
  if (typeof context !== 'string' || !context) return []
  return lens.children(context).filter((id) => {
    const item = lens.read(id)
    return (item?.type === 'topic' && item.data.display !== 'thoughts') || item?.type === 'claude.session'
  })
}

/** The mark beside a PR's name: its first blocker, as a shape and a colour. */
export function badge(data: Record<string, unknown>, locallyApproved: boolean): Badge | null {
  const [first] = blockers(data, locallyApproved)
  if (!first) return null
  const shape = first.id === 'ci' ? 'cross' : first.tone === 'green' ? 'tick' : 'dot'
  return { shape, tone: first.tone, reason: first.label }
}

/**
 * Repos whose PRs each get a preview deployment, by the PR's number: the
 * frontend to try a branch in. Only these; anywhere else a PR has none.
 */
const previews: Partial<Record<string, (number: number) => string>> = {
  'theengineeringco/branch-demo': (number) => `https://pr-${number}.preview.theeng.co/demo`,
}

/** A PR as one line of markdown, to paste: `[#12](url) Name - [preview](url)`, the preview only where it has one. */
export function prLine(data: Record<string, unknown>): string {
  const name = typeof data.name === 'string' ? data.name : prName(String(data.title ?? ''))
  const preview = previewOf(data)
  return `[#${String(data.number)}](${String(data.url)}) ${name}${preview ? ` - [preview](${preview})` : ''}`
}

/** A PR's preview frontend, if its repo has them. */
export function previewOf(data: Record<string, unknown>): string | null {
  const preview = previews[String(data.repo ?? '')]
  const number = Number(data.number)
  return preview && number && data.state === 'OPEN' ? preview(number) : null
}

/** My own approval of my own PR: an owned child, so it outlives the cache. */
export function localApproval(lens: Lens, prId: string): Entity | null {
  for (const child of lens.children(prId)) {
    const entity = lens.read(child)
    if (entity?.type === 'github.localApproval') return entity
  }
  return null
}

/** A branch on GitHub, as an item: its repo (`owner/name`) and its name, in its id. */
export const branchEntityId = (repo: string, branch: string): string => `github:branch:${repo}:${branch}`

/** The repo and branch in a branch's id. */
export function parseBranchId(id: string): { repo: string; branch: string } | null {
  const match = /^github:branch:([^/:]+\/[^/:]+):(.+)$/.exec(id)
  return match ? { repo: match[1], branch: match[2] } : null
}

/** The branch item for a link to a branch on GitHub (`…/tree/<branch>`); null for anything else. */
export function branchOfUrl(url: string): string | null {
  const match = /^https:\/\/github\.com\/([^/]+)\/([^/]+)\/tree\/([^?#]+?)\/?(?:[?#].*)?$/.exec(url)
  return match ? branchEntityId(`${match[1]}/${match[2]}`, decodeURIComponent(match[3])) : null
}

export const githubView: ModuleView = {
  id: 'github',
  name: 'GitHub',
  root: githubIds.root,

  typeOf(id): ItemType | null {
    if (id === githubIds.root) return 'github.home'
    if (parseBranchId(id)) return 'github.branch'
    if (!id.startsWith('github:pr:')) return null
    if (id.includes('#check:')) return 'github.check'
    if (id.includes('#item:')) return 'github.item'
    return prEntityId(prUrl(id)) === id ? 'github.pr' : null
  },

  owns: (type) => type.startsWith('github.'),

  foreign(_id, type) {
    if (type === 'github.home') return { children: 2 * minute }
    if (type === 'github.pr') return { self: 10 * minute, children: minute }
    return null
  },

  actions(entity, lens): Action[] {
    if (entity.type !== 'github.pr' || entity.data.state !== 'OPEN') return []
    const actions: Action[] = []
    if (!entity.data.mine) {
      actions.push({
        id: 'approve',
        label: 'Approve',
        prompt: 'Approve: comment (optional), Enter to approve',
        disabled: entity.data.approvedByMe ? 'Already approved by me' : undefined,
      })
      return actions
    }
    // GitHub won't let me approve my own PR, so "approved" is GitHub's review
    // decision, or my own approval here. Approving (asking nothing) marks a
    // draft ready for review, turns on auto-merge and copies its line to pass
    // on; once it is approved and set to merge, it is Bump: the line again.
    const approved = approvedMine(entity.data, Boolean(localApproval(lens, entity.id)))
    if (approved && entity.data.autoMerge) actions.push({ id: 'bump', label: 'Bump', prompt: null, copy: prLine(entity.data) })
    else actions.push({ id: 'approve', label: approved ? 'Auto-merge' : 'Approve', prompt: null, copy: prLine(entity.data) })
    // CI failing, conflicts or changes asked for: the Claude sessions in its context are asked to fix them.
    const fixes = toFix(entity.data)
    const sessions = sessionsInContext(entity, lens)
    if (fixes.length) {
      actions.push({
        id: 'fix',
        label: 'Fix',
        prompt: null,
        disabled: sessions.length ? undefined : 'No Claude session in its context to fix it',
      })
    }
    actions.push({ id: 'close', label: 'Close', prompt: 'Close and delete the branch: comment (optional), Enter to close' })
    return actions
  },

  order(entity, children) {
    if (entity.type !== 'github.pr') return children
    children = children.filter((child) => child.type !== 'github.localApproval')
    const checks = children.filter((child) => child.type === 'github.check')
    const discussion = children.filter((child) => child.type !== 'github.check')
    checks.sort(
      (a, b) =>
        outcomeOrder.indexOf(a.data.outcome as CheckOutcome) - outcomeOrder.indexOf(b.data.outcome as CheckOutcome) ||
        String(a.data.name).localeCompare(String(b.data.name)),
    )
    discussion.sort((a, b) => Number(a.data.ts ?? 0) - Number(b.data.ts ?? 0))
    return [...checks, ...discussion]
  },

  // `text` is what an item is called, everywhere: a PR's name, a check's. My
  // edit (written now) wins over GitHub's (written at 0), for me only.
  present(entity, lens) {
    const own = typeof entity.data.text === 'string' && entity.data.text ? entity.data.text : undefined
    if (entity.type === 'github.home') return { ...entity, data: { ...entity.data, text: own ?? 'GitHub' } }
    if (entity.type === 'github.check') return { ...entity, data: { ...entity.data, text: own ?? entity.data.name, name: own ?? entity.data.name } }
    if (entity.type === 'github.pr') {
      const data: Record<string, unknown> = { url: prUrl(entity.id), ...entity.data }
      // `label` names the PR where nothing else does, as in a peek's bar.
      const locallyApproved = Boolean(localApproval(lens, entity.id))
      const name = own ?? (data.title ? prName(String(data.title)) : undefined)
      const blocking = blockers(data, locallyApproved)
      return {
        ...entity,
        data: { ...data, preview: previewOf(data), locallyApproved, blockers: blocking, badge: badge(data, locallyApproved), name, text: name ?? String(data.url), label: name ?? String(data.url) },
      }
    }
    if (entity.type === 'github.branch') {
      // All it is is in its id: nothing is loaded for it.
      const { repo, branch } = parseBranchId(entity.id)!
      return { ...entity, data: { ...entity.data, repo, branch, url: `https://github.com/${repo}/tree/${branch}`, text: own ?? branch } }
    }
    if (entity.type === 'github.item') {
      // Shaped like a Slack message, so the same views draw it.
      return { ...entity, data: { ...entity.data, authorKey: entity.data.author, markdown: entity.data.text } }
    }
    return entity
  },
}
