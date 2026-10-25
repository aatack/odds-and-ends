// Lucide (lucide.dev, ISC): its robot is what a bot looks like here, and the
// rest come from it too so that they match.
import {
  Bot,
  Check,
  Compass,
  Eye,
  Flag,
  GitPullRequest,
  Layers,
  Link,
  MessageCircle,
  MessageSquare,
  Moon,
  Paperclip,
  PencilLine,
  Play,
  ScrollText,
  Settings,
  ShieldCheck,
  SquareCode,
  User,
  UserPlus,
  Workflow,
  type LucideIcon,
} from 'lucide-react'
import { HUMAN, type EventKind, type Item, type ItemEvent } from '../../shared/types'

/** One icon an event type. */
export const TYPE_ICON: Record<EventKind, LucideIcon> = {
  item: Layers,
  message: MessageSquare,
  attachment: Paperclip,
  rename: PencilLine,
  priority: Flag,
  action: Play,
  approval: ShieldCheck,
  bot: Bot,
  github: GitPullRequest,
  slack: MessageCircle,
  after: Workflow,
  script: SquareCode,
  summary: ScrollText,
  directive: Compass,
  snooze: Moon,
  agent: Bot,
  configure: Settings,
  assign: UserPlus,
  watch: Eye,
}

export const TYPE_NAME: Record<EventKind, string> = {
  item: 'Task',
  message: 'Message',
  attachment: 'Attachment',
  rename: 'Rename',
  priority: 'Priority',
  action: 'Action',
  approval: 'Approval',
  bot: 'Bot',
  github: 'GitHub',
  slack: 'Slack',
  after: 'After',
  script: 'Script',
  summary: 'Summary',
  directive: 'Directive',
  snooze: 'Snooze',
  agent: 'Bot',
  configure: 'Settings',
  assign: 'Assign',
  watch: 'Watch',
}

/** The blocker an open item that is not the human's turn is shown as waiting on: its first that is not an after, else its first; null when only a bot has it. */
export const waitingOn = (item: Item): ItemEvent | null => item.blockers.find((b) => b.type !== 'after') ?? item.blockers[0] ?? null

/** Whether a bot has the item (and so, with nothing else blocking it, it is the bot's to do). */
export const withBot = (item: Item): boolean => item.assignees.some((a) => a !== HUMAN)

/** What an item waits on: a bot while one is on it, else its blocker (`waitingOn`), else the bot that has it; the human; a tick once closed. */
export function ItemIcon({ item }: { item: Item }) {
  if (item.closed) return <Check className="icon" aria-label="Closed" />
  if (item.withHuman) return <User className="icon" aria-label="You" />
  const waiting = waitingOn(item)
  if (item.bot || !waiting) return <Bot className="icon" aria-label={item.bot ? 'A bot is on it' : 'With a bot'} />
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
  if (by === 'bot' || by.startsWith('bot:')) return [Bot, 'Bot']
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
