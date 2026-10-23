// Lucide (lucide.dev, ISC): its robot is what a bot looks like here, and the
// rest come from it too so that they match.
import {
  Bot,
  Check,
  GitPullRequest,
  Layers,
  Link,
  MessageCircle,
  MessageSquare,
  Play,
  Settings,
  ShieldCheck,
  SquareCode,
  User,
  Workflow,
  type LucideIcon,
} from 'lucide-react'
import type { EventKind, Item } from '../../shared/types'

/** One icon an event type. */
export const TYPE_ICON: Record<EventKind, LucideIcon> = {
  item: Layers,
  message: MessageSquare,
  action: Play,
  approval: ShieldCheck,
  bot: Bot,
  github: GitPullRequest,
  slack: MessageCircle,
  after: Workflow,
}

export const TYPE_NAME: Record<EventKind, string> = {
  item: 'Item',
  message: 'Message',
  action: 'Action',
  approval: 'Approval',
  bot: 'Bot',
  github: 'GitHub',
  slack: 'Slack',
  after: 'After',
}

/** What an item waits on: a bot, else its first other blocker, else the human; a tick once closed. */
export function ItemIcon({ item }: { item: Item }) {
  if (item.closed) return <Check className="icon" aria-label="Closed" />
  if (item.withHuman) return <User className="icon" aria-label="You" />
  const waiting = item.blockers.find((b) => b.type === 'bot') ?? item.blockers.find((b) => b.type !== 'after') ?? item.blockers[0]
  const Icon = TYPE_ICON[waiting.type]
  return <Icon className="icon" aria-label={TYPE_NAME[waiting.type]} />
}

/** An event type's icon, named for whoever hovers it. */
export function TypeIcon({ type }: { type: EventKind }) {
  const Icon = TYPE_ICON[type]
  return (
    <span className="type-icon" title={TYPE_NAME[type]}>
      <Icon className="icon" />
    </span>
  )
}

/** Whoever added an event, as a profile picture: bots, scripts, the app, a source or an outside client. */
export function Avatar({ by, type }: { by: string; type?: EventKind }) {
  const [Icon, name]: [LucideIcon, string] =
    by === 'bot'
      ? [Bot, 'Bot']
      : by.startsWith('script')
        ? [SquareCode, 'Script']
        : by === 'mcp'
          ? [Link, 'MCP client']
          : by.startsWith('github')
            ? [GitPullRequest, by]
            : by.startsWith('slack')
              ? [MessageCircle, by]
              : by === 'human'
                ? [User, 'You']
                : type
                  ? [TYPE_ICON[type], TYPE_NAME[type]]
                  : [Settings, 'Workflows']
  return (
    <span className="avatar" title={name}>
      <Icon className="icon" />
    </span>
  )
}
