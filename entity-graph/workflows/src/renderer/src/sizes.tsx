// The four sizes anything is drawn at, from one place:
//
// - avatar: its picture, beside it in the chat (who said a note; any other event, its type);
// - pill: inline in text, as `[id]()` is drawn: its icon and name, which open it;
// - card: in a chat or a list, by its type (`CARDS`);
// - focus: on its own, filling the view (`FOCUS`): a task or a bot as its chat, anything else as its details.

import { CircleCheck, Play, Timer } from 'lucide-react'
import type { ReactNode } from 'react'
import type { Catalog, ItemKind, ItemInfo } from '../../shared/types'
import { Approval } from './Approval'
import { fenced, Markdown } from './components/ui/Markdown'
import { useItemMenu } from './ItemMenu'
import { itemInfoIcon } from './icons'
import { SlackCard } from './SlackView'
import { Widget } from './Widget'
import { TaskCard } from './TaskCard'
import { minutes, time, useNow } from './time'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * An event's picture, beside its message in the chat. Given who made it (`owner`: a bot or a person), hovering it
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
  task: ({ info: e, onOpen }) => (
    <button data-ui className="link card-link" onClick={() => onOpen(e.id)}>
      <TaskCard info={e} />
    </button>
  ),
  script: ({ info: e, act, find }) => <ScriptCard info={e} act={act} find={find} />,
  widget: ({ info: e }) => <Widget info={e} />,
  slack_message: ({ info: e, find }) => <SlackCard info={e} find={find} />,
  slack_channel: ({ info: e, find }) => <SlackCard info={e} find={find} />,
  approval: ({ info: e, find, catalog, act }) => (
    <div className="item-card">
      <Markdown text={e.resolved ? (e.message ?? e.summary) : e.summary} highlight={find} />
      {!e.resolved && <Approval b={e} catalog={catalog} act={act} />}
    </div>
  ),
}

/** An event's card, by its type. */
export function ItemCard(props: CardProps) {
  const card = CARDS[props.info.type]
  return card ? <>{card(props)}</> : <Markdown text={props.info.message ?? props.info.summary} highlight={props.find} />
}

/** How each type fills the view on its own: as a chat (its events, and the chat input), or as its details. */
export const FOCUS: Record<ItemKind, 'chat' | 'details'> = {
  task: 'chat',
  bot: 'chat',
  note: 'details',
  approval: 'details',
  script: 'details',
  process: 'details',
  widget: 'details',
  search: 'details',
  slack_message: 'details',
  slack_channel: 'details',
  user: 'details',
}

/**
 * A script: what it does; then a tick and what it finished with, once done. While open: when it runs next (if it
 * asked to), and a play button, always: pressed, it runs now, with the unsafe tools a person's press brings.
 */
function ScriptCard({ info: e, act, find }: { info: ItemInfo; act: Act; find: string }) {
  const now = useNow()
  const error = typeof e.data?.error === 'string' ? e.data.error : null
  const result = e.data?.message ?? (e.data?.result === undefined || e.data?.result === null ? null : e.data.result)
  return (
    <div className="item-card script-card">
      <div className="script-line">
        <Markdown className="script-what" text={e.summary} highlight={find} />
        {e.resolved ? (
          <CircleCheck className={`icon ${error ? 'script-failed' : 'script-done'}`} aria-label={error ? 'Failed' : 'Done'} />
        ) : (
          <>
            {e.nextCheckAt !== null && (
              <span className="script-timer muted" title={time(e.nextCheckAt)}>
                <Timer className="icon" />
                {e.runs}× · {e.nextCheckAt > now ? `in ${minutes(e.nextCheckAt - now)}` : 'now'}
              </span>
            )}
            <button data-ui className="script-play" title="Run it now" aria-label="Run it now" onClick={() => void act(() => api().runNow(e.id))}>
              <Play className="icon" />
            </button>
          </>
        )}
      </div>
      {!e.resolved && e.nextCheckAt === null && e.runs > 0 && <div className="muted script-runs">Run {e.runs}× and not done yet.</div>}
      {e.lastError && !e.resolved && <div className="error-text">{e.lastError}</div>}
      {e.resolved && error && <div className="error-text">{error}</div>}
      {e.resolved && !error && result !== null && (
        <div className="script-output">
          {typeof result === 'string' ? <Markdown text={result} highlight={find} /> : <Markdown text={fenced(JSON.stringify(result, null, 2), 'json')} />}
        </div>
      )}
    </div>
  )
}
