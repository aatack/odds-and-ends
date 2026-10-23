// Lucide (lucide.dev, ISC): its robot is what a bot looks like here, and the
// rest come from it too so that they match.
import { Bot, Check, GitPullRequest, Layers, Link, MessageCircle, Settings, SquareCode, User, type LucideIcon } from 'lucide-react'
import type { HandoffKind, Item } from '../../shared/types'

type Icon = LucideIcon

/** One icon a handoff type: what the item waits for, at a glance. */
export const TYPE_ICON: Record<HandoffKind, Icon> = {
  human: User,
  bot: Bot,
  github: GitPullRequest,
  slack: MessageCircle,
  items: Layers,
  script: SquareCode,
}

const TYPE_NAME: Record<HandoffKind, string> = {
  human: 'You',
  bot: 'Bot',
  github: 'GitHub',
  slack: 'Slack',
  items: 'Other items',
  script: 'Script',
}

/** What an item waits for now: its handoff's type, the human's when it has none, or done. */
export function ItemIcon({ item }: { item: Item }) {
  if (item.closed) return <Check className="icon" aria-label="Closed" />
  const type = item.current?.type ?? 'human'
  const Icon = TYPE_ICON[type]
  return <Icon className="icon" aria-label={TYPE_NAME[type]} />
}

/** A handoff type's icon, named for whoever hovers it. */
export function TypeIcon({ type }: { type: HandoffKind }) {
  const Icon = TYPE_ICON[type]
  return (
    <span className="type-icon" title={TYPE_NAME[type]}>
      <Icon className="icon" />
    </span>
  )
}

/** Whoever set a handoff, as a profile picture: bots, scripts, the app, a source or an outside client. */
export function Avatar({ createdBy }: { createdBy: string }) {
  const [Icon, name]: [Icon, string] =
    createdBy === 'bot'
      ? [Bot, 'Bot']
      : createdBy.startsWith('script')
        ? [SquareCode, 'Script']
        : createdBy === 'system'
          ? [Settings, 'Workflows']
          : createdBy === 'mcp'
            ? [Link, 'MCP client']
            : createdBy.startsWith('github')
              ? [GitPullRequest, createdBy]
              : createdBy.startsWith('slack')
                ? [MessageCircle, createdBy]
                : createdBy === 'human'
                  ? [User, 'You']
                  : [Settings, createdBy]
  return (
    <span className="avatar" title={name}>
      <Icon className="icon" />
    </span>
  )
}
