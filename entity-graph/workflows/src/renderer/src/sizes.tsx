// The four sizes anything is drawn at, from one place:
//
// - avatar: its picture, beside it in the chat (who said a note; any other item, its type);
// - pill: inline in text, as `[id]()` is drawn: its icon and name, which open it;
// - card: in a chat or a list, by its type (`CARDS`);
// - focus: on its own, filling the view (`FOCUS`): a task or a bot as its chat, anything else as its details.

import type { ReactNode } from 'react'
import type { Catalog, ItemKind, ItemInfo } from '../../shared/types'
import { Approval } from './Approval'
import { Markdown } from './components/ui/Markdown'
import { useItemMenu } from './ItemMenu'
import { itemInfoIcon } from './icons'
import { SlackCard } from './SlackView'
import { Widget } from './Widget'
import { FileCard } from './FileView'
import { TaskCard } from './TaskCard'
import { Marked } from './components/ui/Marked'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * An item's picture, beside its message in the chat. Given who made it (`owner`: a bot or a person), hovering it
 * names them and pressing it opens them; otherwise it is named for what it is.
 */
export function ItemAvatar({ info, owner }: { info: { type: ItemKind; createdBy: string }; owner?: { name: string; onOpen: () => void } | null }) {
  const [Icon, name] = itemInfoIcon(info)
  if (!owner)
    return (
      <span className="avatar" title={name}>
        <Icon className="icon" />
      </span>
    )
  return (
    <button data-ui className="avatar" title={owner.name} aria-label={owner.name} onClick={owner.onOpen}>
      <Icon className="icon" />
    </button>
  )
}

/**
 * Anything, inline in text: its icon (or `icon`, drawn as given) and its name, outlined, opening it when pressed;
 * right-click for its menu. Tasks, bots and people are all drawn so (`[id]()` in any text).
 */
export function Pill({ id, label, icon, onOpen }: { id: string; label: ReactNode; icon?: ReactNode; onOpen?: () => void }) {
  const menu = useItemMenu()
  return (
    <button data-ui className="link item-ref" onClick={onOpen} {...menu(id)}>
      {icon}
      <span className="pill-label">{label}</span>
    </button>
  )
}

export type CardProps = { info: ItemInfo; find: string; catalog: Catalog; act: Act; onOpen: (id: string) => void }

/** Each type's card: what it shows in a chat. A type left out shows what it says (`message`, or its summary). */
export const CARDS: Partial<Record<ItemKind, (p: CardProps) => ReactNode>> = {
  note: ({ info: e, find }) => <Markdown text={String(e.params.text ?? '')} highlight={find} />,
  task: ({ info: e, onOpen, find }) => (
    <button data-ui className="link card-link" onClick={() => onOpen(e.id)}>
      <TaskCard info={e} find={find} />
    </button>
  ),
  widget: ({ info: e }) => <Widget info={e} />,
  file: ({ info: e, onOpen }) => <FileCard info={e} onOpen={onOpen} />,
  link: ({ info: e, find }) => (
    <a className="link-card" href={String(e.params.url)} target="_blank" rel="noreferrer">
      <Marked text={String(e.params.title || e.params.url)} phrase={find} />
    </a>
  ),
  slack_message: ({ info: e, find }) => <SlackCard info={e} find={find} />,
  slack_channel: ({ info: e, find }) => <SlackCard info={e} find={find} />,
  approval: ({ info: e, find, catalog, act }) => (
    <div className="item-card">
      <Markdown text={e.resolved ? (e.message ?? e.summary) : e.summary} highlight={find} />
      {!e.resolved && <Approval b={e} catalog={catalog} act={act} />}
    </div>
  ),
}

/** An item's card, by its type. */
export function ItemCard(props: CardProps) {
  const card = CARDS[props.info.type]
  return card ? <>{card(props)}</> : <Markdown text={props.info.message ?? props.info.summary} highlight={props.find} />
}

/** How each type fills the view on its own: as a chat (its items, and the chat input), or as its details. */
export const FOCUS: Record<ItemKind, 'chat' | 'details'> = {
  task: 'chat',
  bot: 'chat',
  note: 'details',
  approval: 'details',
  script: 'details',
  process: 'details',
  widget: 'details',
  link: 'details',
  search: 'details',
  slack_message: 'details',
  slack_channel: 'details',
  // Its own view (FileFocus): the file itself, filling it.
  file: 'details',
  user: 'details',
}

