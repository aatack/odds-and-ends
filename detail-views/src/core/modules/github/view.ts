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
 * What stands between a PR and being merged, most pressing first, worked out
 * from what GitHub says and from my own approval here. The first is what I
 * should know at a glance; the rest are what follows once it's done. Empty
 * until enough is known.
 *
 * In order: merged or closed (nothing more to do); a draft; my review (mine:
 * approving it here; theirs: reviewing it, especially if asked); changes
 * requested; unresolved threads; reviews from others; failing CI; conflicts;
 * behind its base; CI still running; and then, clear: merging (or auto-merge
 * doing it).
 */
export function blockers(data: Record<string, unknown>, locallyApproved: boolean): Blocker[] {
  if (data.mine === undefined) return []
  if (data.state === 'MERGED') return [{ id: 'merged', label: 'Merged', turn: 'none', tone: 'purple' }]
  if (data.state === 'CLOSED') return [{ id: 'closed', label: 'Closed', turn: 'none', tone: 'gray' }]
  const mine = Boolean(data.mine)
  const theirs = !mine
  const out: Blocker[] = []
  const add = (id: string, label: string, turn: Turn, tone: Blocker['tone']) => out.push({ id, label, turn, tone })
  const others = Array.isArray(data.reviewers) ? (data.reviewers as string[]) : []

  if (data.draft) add('draft', mine ? 'Draft: mark it ready' : 'Draft', mine ? 'me' : 'them', 'yellow')
  if (mine && !locallyApproved) add('self-review', 'Review it', 'me', 'yellow')
  if (theirs && !data.approvedByMe && !data.changesRequestedByMe) {
    add('review', data.reviewRequestedOfMe ? 'Review requested of you' : 'Not reviewed by you', 'me', 'yellow')
  }
  if (data.review === 'CHANGES_REQUESTED') {
    if (mine) add('changes', 'Changes requested', 'me', 'red')
    else if (data.changesRequestedByMe) add('changes', 'You requested changes', 'them', 'yellow')
    else add('changes', 'Changes requested', 'them', 'yellow')
  }
  const unresolved = Number(data.unresolved ?? 0)
  if (unresolved > 0) add('threads', `${unresolved} unresolved ${unresolved === 1 ? 'thread' : 'threads'}`, mine ? 'me' : 'them', 'yellow')
  if (data.review !== 'APPROVED' && data.review !== 'CHANGES_REQUESTED' && (mine ? !data.approvedByOthers : data.approvedByMe)) {
    add('awaiting', others.length ? `Awaiting review: ${others.join(', ')}` : 'Awaiting review', 'them', 'yellow')
  }
  if (data.checks === 'failing') add('ci', 'CI failing', mine ? 'me' : 'them', 'red')
  if (data.conflicts || data.mergeState === 'DIRTY') add('conflicts', 'Merge conflicts', mine ? 'me' : 'them', 'red')
  if (data.mergeState === 'BEHIND') add('behind', 'Behind its base', mine ? 'me' : 'them', 'yellow')
  if (data.checks === 'pending') add('pending', 'CI running', 'none', 'yellow')
  if (!out.length) {
    if (mine && data.autoMerge) add('auto', 'Will auto-merge', 'none', 'green')
    else if (mine) add('merge', 'Ready: merge it', 'me', 'green')
    else add('clear', data.approvedByMe ? 'Approved by you' : 'Ready', 'none', 'green')
  }
  return out
}

/** The mark beside a PR's name: its first blocker, as a shape and a colour. */
export function badge(data: Record<string, unknown>, locallyApproved: boolean): Badge | null {
  const [first] = blockers(data, locallyApproved)
  if (!first) return null
  const shape = first.id === 'ci' ? 'cross' : first.tone === 'green' ? 'tick' : 'dot'
  return { shape, tone: first.tone, reason: first.label }
}

/** My own approval of my own PR: an owned child, so it outlives the cache. */
export function localApproval(lens: Lens, prId: string): Entity | null {
  for (const child of lens.children(prId)) {
    const entity = lens.read(child)
    if (entity?.type === 'github.localApproval') return entity
  }
  return null
}

export const githubView: ModuleView = {
  id: 'github',
  name: 'GitHub',
  root: githubIds.root,

  typeOf(id): ItemType | null {
    if (id === githubIds.root) return 'github.home'
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
    } else {
      // GitHub won't let me approve my own PR, so "approved" is GitHub's
      // review decision, or my own approval here. Approving turns on
      // auto-merge, so once both are true there is nothing left to do.
      const approved = entity.data.review === 'APPROVED' || Boolean(localApproval(lens, entity.id))
      actions.push({
        id: 'approve',
        label: 'Approve',
        prompt: approved
          ? 'Already approved: Enter to turn on auto-merge'
          : 'Approve locally and turn on auto-merge: Enter to confirm',
        disabled: approved && entity.data.autoMerge ? 'Approved, and auto-merge is on' : undefined,
      })
      actions.push({ id: 'close', label: 'Close', prompt: 'Close and delete the branch: comment (optional), Enter to close' })
    }
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
        data: { ...data, locallyApproved, blockers: blocking, badge: badge(data, locallyApproved), name, text: name ?? String(data.url), label: name ?? String(data.url) },
      }
    }
    if (entity.type === 'github.item') {
      // Shaped like a Slack message, so the same views draw it.
      return { ...entity, data: { ...entity.data, authorKey: entity.data.author, markdown: entity.data.text } }
    }
    return entity
  },
}
