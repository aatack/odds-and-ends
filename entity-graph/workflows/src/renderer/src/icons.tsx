import {
  Check,
  CodeSquare01,
  CpuChip01,
  GitPullRequest,
  LayersThree01,
  Link01,
  MessageChatCircle,
  Settings01,
  User01,
} from '@untitledui/icons'
import type { ComponentType, SVGProps } from 'react'
import type { HandoffKind, Item } from '../../shared/types'

type Icon = ComponentType<SVGProps<SVGSVGElement> & { size?: number }>

/** One icon a handoff type: what the item waits for, at a glance. */
export const TYPE_ICON: Record<HandoffKind, Icon> = {
  human: User01,
  bot: CpuChip01,
  github: GitPullRequest,
  slack: MessageChatCircle,
  items: LayersThree01,
  script: CodeSquare01,
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
      ? [CpuChip01, 'Bot']
      : createdBy.startsWith('script')
        ? [CodeSquare01, 'Script']
        : createdBy === 'system'
          ? [Settings01, 'Workflows']
          : createdBy === 'mcp'
            ? [Link01, 'MCP client']
            : createdBy.startsWith('github')
              ? [GitPullRequest, createdBy]
              : createdBy.startsWith('slack')
                ? [MessageChatCircle, createdBy]
                : createdBy === 'human'
                  ? [User01, 'You']
                  : [Settings01, createdBy]
  return (
    <span className="avatar" title={name}>
      <Icon className="icon" />
    </span>
  )
}
