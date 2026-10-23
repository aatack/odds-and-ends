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
  script: SquareCode,
}

export const TYPE_NAME: Record<EventKind, string> = {
  item: 'Task',
  message: 'Message',
  action: 'Action',
  approval: 'Approval',
  bot: 'Bot',
  github: 'GitHub',
  slack: 'Slack',
  after: 'After',
  script: 'Script',
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

/** Who someone is, as an icon: the human, a bot, a script, an outside client, a source, or the app. */
function whoIcon(by: string): [LucideIcon, string] {
  if (by === 'human') return [User, 'You']
  if (by === 'bot') return [Bot, 'Bot']
  if (by.startsWith('script')) return [SquareCode, 'Script']
  if (by === 'mcp') return [Link, 'MCP client']
  if (by.startsWith('github')) return [GitPullRequest, by]
  if (by.startsWith('slack')) return [MessageCircle, by]
  return [Settings, 'Workflows']
}

/**
 * The one icon for an event, wherever it is drawn: a message (or an action) is
 * whoever said (or ran) it; anything else is its type. The chat's pictures, the
 * blocker rows and the view header all use this.
 */
export function eventIcon(e: { type: EventKind; createdBy: string }): [LucideIcon, string] {
  return e.type === 'message' || e.type === 'action' ? whoIcon(e.createdBy) : [TYPE_ICON[e.type], TYPE_NAME[e.type]]
}

/** An event's icon, inline. */
export function EventIcon({ event }: { event: { type: EventKind; createdBy: string } }) {
  const [Icon, name] = eventIcon(event)
  return (
    <span className="type-icon" title={name}>
      <Icon className="icon" />
    </span>
  )
}

/** An event's icon as a profile picture, beside its message in the chat. */
export function Avatar({ event }: { event: { type: EventKind; createdBy: string } }) {
  const [Icon, name] = eventIcon(event)
  return (
    <span className="avatar" title={name}>
      <Icon className="icon" />
    </span>
  )
}
