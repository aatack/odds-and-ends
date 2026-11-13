// The four sizes anything is drawn at, from one place:
//
// - avatar: its picture, beside it in the chat (who said a note; any other item, its type);
// - pill: inline in text, as `[id]()` is drawn: its icon, its short title (`pillText`), which open it, and a dot for how it stands;
// - card: in a chat or a list, by its type (`CARDS`); else all its text (`cardText`) and how it stands (status.tsx);
// - focus: on its own, filling the view (`displayOf`): its focus, comments, notes or assignments.

import type { ReactNode } from 'react'
import type { Catalog, ItemKind, ItemInfo } from '../../shared/types'
import { Markdown } from './components/ui/Markdown'
import { useItemMenu } from './ItemMenu'
import { itemInfoIcon } from './icons'
import { SlackCard } from './SlackView'
import { FileCard } from './FileView'
import { TaskCard } from './TaskCard'
import { Marked } from './components/ui/Marked'
import { cardText } from '../../shared/text'
import { StatusDot, StatusLine } from './status'
import { ScriptCard } from './ScriptCard'

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
      <StatusDot id={id} />
    </button>
  )
}

export type CardProps = { info: ItemInfo; find: string; catalog: Catalog; act: Act; onOpen: (id: string) => void }

/** Each type's card: what it shows in a chat. A type left out shows what it says (`message`, or its summary). */
export const CARDS: Partial<Record<ItemKind, (p: CardProps) => ReactNode>> = {
  // A topic with a status (a thing to be done) is a card that opens it; one with none (a comment) is what it says.
  topic: ({ info: e, onOpen, find }) =>
    typeof e.params.open === 'boolean' ? (
      <button data-ui className="link card-link" onClick={() => onOpen(e.id)}>
        <TaskCard info={e} find={find} />
      </button>
    ) : (
      <Markdown text={String(e.params.text ?? '')} highlight={find} />
    ),
  file: ({ info: e, onOpen }) => <FileCard info={e} onOpen={onOpen} />,
  // A script: its name, what it does, how it stands, and a play button that runs it on the item it sits in.
  script: ({ info: e, act }) => <ScriptCard info={e} on={e.parentId} act={act} />,
  link: ({ info: e, find }) => (
    <a className="link-card" href={String(e.params.url)} target="_blank" rel="noreferrer">
      <Marked text={String(e.params.title || e.params.url)} phrase={find} />
    </a>
  ),
  slack_message: ({ info: e, find }) => <SlackCard info={e} find={find} />,
  slack_channel: ({ info: e, find }) => <SlackCard info={e} find={find} />,
}

/** An item's card, by its type; else all the text it holds (`cardText`), and how it stands, if that says anything. */
export function ItemCard(props: CardProps) {
  const card = CARDS[props.info.type]
  if (card) return <>{card(props)}</>
  return (
    <>
      <Markdown text={cardText(props.info)} highlight={props.find} />
      <StatusLine id={props.info.id} />
    </>
  )
}

export { DISPLAY } from '../../shared/views'
