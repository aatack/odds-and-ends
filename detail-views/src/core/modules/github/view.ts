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

/**
 * Merged, anyone's: purple.
 * Someone else's PR: whether it is approved, and whether by me.
 * Mine: what stands in its way, worst first, then how approved it is.
 * Null until enough is known.
 */
export function badge(data: Record<string, unknown>, locallyApproved: boolean): Badge | null {
  if (data.mine === undefined) return null
  // Merged is the end of the story, whoever's it is.
  if (data.state === 'MERGED') return { shape: 'dot', tone: 'purple', reason: 'merged' }
  if (!data.mine) {
    if (data.approvedByMe) return { shape: 'tick', tone: 'green', reason: 'approved by me' }
    if (data.approvedByOthers) return { shape: 'dot', tone: 'green', reason: 'approved' }
    return { shape: 'dot', tone: 'yellow', reason: 'not approved' }
  }
  if (data.conflicts) return { shape: 'dot', tone: 'red', reason: 'merge conflicts' }
  if (data.checks === 'failing') return { shape: 'cross', tone: 'red', reason: 'CI failing' }
  if (!data.approvedByOthers) return { shape: 'dot', tone: 'yellow', reason: 'no approvals' }
  if (locallyApproved) return { shape: 'tick', tone: 'green', reason: 'approved by me and others' }
  return { shape: 'dot', tone: 'green', reason: 'approved by others' }
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
      return { ...entity, data: { ...data, locallyApproved, badge: badge(data, locallyApproved), name, text: name ?? String(data.url), label: name ?? String(data.url) } }
    }
    if (entity.type === 'github.item') {
      // Shaped like a Slack message, so the same views draw it.
      return { ...entity, data: { ...entity.data, authorKey: entity.data.author, markdown: entity.data.text } }
    }
    return entity
  },
}
