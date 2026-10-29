// Lucide (lucide.dev, ISC): its robot is what a bot looks like here, and the
// rest come from it too so that they match.
import { AppWindow, Bot, Search, Check, GitPullRequest, Hash, Layers, Link, MessageCircle, MessageSquare, Settings, ShieldCheck, SquareCode, SquareTerminal, User, type LucideIcon } from 'lucide-react'
import type { ItemKind, Item, ItemInfo } from '../../shared/types'

/** One icon an item type. */
export const TYPE_ICON: Record<ItemKind, LucideIcon> = {
  task: Layers,
  note: MessageSquare,
  approval: ShieldCheck,
  script: SquareCode,
  process: SquareTerminal,
  widget: AppWindow,
  link: Link,
  search: Search,
  slack_message: MessageCircle,
  slack_channel: Hash,
  bot: Bot,
  user: User,
}

export const TYPE_NAME: Record<ItemKind, string> = {
  task: 'Task',
  note: 'Note',
  approval: 'Approval',
  script: 'Script',
  process: 'Process',
  widget: 'Widget',
  link: 'Link',
  search: 'Search page',
  slack_message: 'Slack message',
  slack_channel: 'Slack channel',
  bot: 'Bot',
  user: 'Person',
}

/** The blocker an open item that is not the human's turn is shown as waiting on: its first; null when only someone has it. */
export const waitingOn = (item: Item): ItemInfo | null => item.blockers[0] ?? null

/** Whether a bot has the item: one of `bots` is among who has it (a script having it is not a bot). */
export const withBot = (item: Item, bots: { id: string }[]): boolean => item.assignees.some((a) => bots.some((b) => b.id === a))

/** What an item waits on: a bot while one is on it, else its blocker (`waitingOn`), else the bot that has it; the human; a tick once closed. */
export function ItemIcon({ item }: { item: Item }) {
  if (item.closed) return <Check className="icon" aria-label="Closed" />
  if (item.withYou) return <User className="icon" aria-label="You" />
  const waiting = waitingOn(item)
  if (item.bot || !waiting) return <Bot className="icon" aria-label={item.bot ? 'A bot is on it' : 'With a bot'} />
  const Icon = TYPE_ICON[waiting.type]
  return <Icon className="icon" aria-label={TYPE_NAME[waiting.type]} />
}

/** An item type's icon, named for whoever hovers it. */
export function TypeIcon({ type }: { type: ItemKind }) {
  const Icon = TYPE_ICON[type]
  return (
    <span className="type-icon" title={TYPE_NAME[type]}>
      <Icon className="icon" />
    </span>
  )
}

/** Who someone is, as an icon: the human, a bot, a script, an outside client, a source, or the app. */
function whoIcon(by: string): [LucideIcon, string] {
  if (by === 'bot' || by.startsWith('bot:')) return [Bot, 'Bot']
  if (by.startsWith('script')) return [SquareCode, 'Script']
  if (by === 'mcp') return [Link, 'MCP client']
  if (by.startsWith('github')) return [GitPullRequest, by]
  if (by.startsWith('slack')) return [MessageCircle, by]
  if (by === 'system') return [Settings, 'YAWN']
  return [User, by]
}

/**
 * The one icon for an item, wherever it is drawn: a note is
 * whoever said (or ran) it; anything else is its type. The chat's pictures, the
 * blocker rows and the view header all use this.
 */
export function itemInfoIcon(e: { type: ItemKind; createdBy: string }): [LucideIcon, string] {
  return e.type === 'note' ? whoIcon(e.createdBy) : [TYPE_ICON[e.type], TYPE_NAME[e.type]]
}

/** An item's icon, inline. */
export function ItemInfoIcon({ info }: { info: { type: ItemKind; createdBy: string } }) {
  const [Icon, name] = itemInfoIcon(info)
  return (
    <span className="type-icon" title={name}>
      <Icon className="icon" />
    </span>
  )
}
