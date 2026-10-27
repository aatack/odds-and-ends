import { ThumbsUp } from 'lucide-react'
import { memo, useCallback, useRef } from 'react'
import type { Catalog, ItemDetail, ItemEvent, Overview } from '../../shared/types'
import { isPerson, nameOf } from './people'
import { useEventMenu } from './EventMenu'
import { EventAvatar, EventCard } from './sizes'
import { shortTime, time } from './time'
import { useAtomValue } from 'jotai'
import { shows } from './components/ui/Markdown'
import { findAtom } from './views'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

export { minutes, time } from './time'

/** The words an event shows in the chat, for Ctrl+F. */
const words = (e: ItemEvent) => (e.type === 'note' ? String(e.params.text ?? '') : [e.summary, e.message].filter(Boolean).join('\n'))

/**
 * The task's chat: every event in it (its `contains` links), in the order they were added, each as its card. What
 * the person looking added is on the right; everything else on the left, with who it came from as its picture (and,
 * for other people, their name). A note is its words; a child task, its card (which opens it); a script, what it
 * waits for and where it stands; an approval, its form while open. Cancelled events are small notes in the middle.
 * While Ctrl+F's find line has a phrase in it, only what shows the phrase is left, with the phrase marked.
 */
export function Chat(props: { detail: ItemDetail; overview: Overview; catalog: Catalog; act: Act; onOpen: (id: string) => void }) {
  const { detail, overview, catalog } = props
  const menu = useEventMenu()
  const find = useAtomValue(findAtom)?.trim() ?? ''
  // The callbacks are the same from one render to the next (calling the latest given), so a message that hasn't
  // changed isn't drawn again: a long chat stays quick.
  const latest = useRef(props)
  latest.current = props
  const act = useCallback<Act>((fn) => latest.current.act(fn), [])
  const onOpen = useCallback((id: string) => latest.current.onOpen(id), [])
  const shown = detail.events.filter((e) => !find || shows(words(e), find))
  return (
    <section className="chat">
      {find && !shown.length && <div className="chat-note">Nothing here says “{find}”.</div>}
      {shown.map((e) =>
        e.cancelled && e.type !== 'task' ? (
          <div key={e.id} className="chat-note" title={time(e.createdAt)} {...menu(e.id)}>
            {e.message}
          </div>
        ) : (
          <Message
            key={e.id}
            event={e}
            mine={e.createdBy === overview.me.id}
            me={overview.me.id}
            who={isPerson(overview, e.createdBy) ? nameOf(overview, e.createdBy) : null}
            find={find}
            catalog={catalog}
            act={act}
            onOpen={onOpen}
          />
        ),
      )}
    </section>
  )
}

type MessageProps = { event: ItemEvent; mine: boolean; me: string; who: string | null; find: string; catalog: Catalog; act: Act; onOpen: (id: string) => void }

/** One event, as a message: right-click it to open it or debug it. Drawn again only when something it shows changed. */
const Message = memo(function Message(props: MessageProps) {
  const { event: e, mine, me, who, act } = props
  const menu = useEventMenu()
  // Thumbs-ups: a count under the bubble (pressing it toggles yours), or, with none yet, a button shown on hover.
  const reacted = e.reactions.includes(me)
  const react = () => void act(() => api().react(e.id, !reacted))
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'}`}>
      {!mine && <EventAvatar event={e} />}
      <div className="bubble-stack">
        <div className={`bubble bubble-${e.type}`} title={`${who ? `${who}, ` : ''}${time(e.createdAt)}`} {...menu(e.id)}>
          {!mine && who && <span className="bubble-who">{who}</span>}
          <EventCard {...props} />
          <span className="bubble-time">{shortTime(e.createdAt)}</span>
        </div>
        <button
          data-ui
          className={`reaction ${e.reactions.length ? 'some' : ''} ${reacted ? 'on' : ''}`}
          title={e.reactions.length ? `${e.reactions.length} thumbs up` : 'Thumbs up'}
          onClick={react}
        >
          <ThumbsUp className="icon" />
          {e.reactions.length > 0 && e.reactions.length}
        </button>
      </div>
    </div>
  )
}, (a, b) => {
  const { event: ea, ...ra } = a
  const { event: eb, ...rb } = b
  return (Object.keys(ra) as (keyof typeof ra)[]).every((k) => ra[k] === rb[k]) && JSON.stringify(ea) === JSON.stringify(eb)
})
