import { useEffect, useState } from 'react'
import { MessageCircle } from 'lucide-react'
import type { ItemInfo, SlackMessage, SlackView } from '../../shared/types'
import { Markdown } from './components/ui/Markdown'
import { api } from './useWorkflows'

const when = (ts: string) => new Date(Number(ts) * 1000).toLocaleString([], { dateStyle: 'medium', timeStyle: 'short' })

/** A Slack item's messages, read through the backend's cache as it is shown. */
function useSlackView(id: string): { view: SlackView | null; error: string | null } {
  const [view, setView] = useState<SlackView | null>(null)
  const [error, setError] = useState<string | null>(null)
  useEffect(() => {
    let live = true
    setView(null)
    setError(null)
    api()
      .slackView(id)
      .then(
        (v) => live && setView(v),
        (e: Error) => live && setError(e.message.replace(/^Error invoking remote method '\w+': (Error: )?/, '')),
      )
    return () => void (live = false)
  }, [id])
  return { view, error }
}

/** The quote of a message in a chat: its first few lines. */
const quote = (text: string, lines = 6) => {
  const all = text.split('\n')
  return [...all.slice(0, lines), ...(all.length > lines ? ['…'] : [])].join('\n')
}

/**
 * A Slack item in a chat: for a message, who said it, where, and what (quoted), and how many replies; for a
 * channel, its name, the range, and how many messages. Read from Slack (through the cache) as it is shown.
 */
export function SlackCard({ info: e, find }: { info: ItemInfo; find: string }) {
  const { view, error } = useSlackView(e.id)
  if (error) return <div className="item-card"><Markdown text={e.summary} highlight={find} /><p className="error-text">{error}</p></div>
  if (!view) return <div className="item-card muted">Reading Slack…</div>
  if (e.type === 'slack_channel') {
    const threads = view.messages.filter((m) => m.replies).length
    return (
      <div className="item-card slack-card">
        <b>{view.channelName}</b>{' '}
        <span className="muted">
          {e.summary.replace(/^Slack [^,]*, /, '')}: {view.messages.length} {view.messages.length === 1 ? 'message' : 'messages'}
          {threads ? `, ${threads} with threads` : ''}
        </span>
      </div>
    )
  }
  const m = view.messages.find((x) => x.ts === view.focus)
  if (!m) return <div className="item-card muted">That Slack message is not there any more.</div>
  return (
    <div className="item-card slack-card">
      <div className="slack-meta">
        <MessageCircle className="icon" /> <b>{m.author}</b> <span className="muted">in {view.channelName}{m.thread_ts && m.thread_ts !== m.ts ? ', in a thread' : ''}</span>
      </div>
      <blockquote className="slack-quote">
        <Markdown text={quote(m.text)} highlight={find} />
      </blockquote>
      {m.replies ? <span className="muted">{m.replies} {m.replies === 1 ? 'reply' : 'replies'}</span> : null}
    </div>
  )
}

/**
 * A slack_message's thread, or a slack_channel's messages, read from Slack
 * through the cache as the view opens. A message with a thread opens it in place.
 */
export function SlackMessages({ info }: { info: ItemInfo }) {
  const { view, error } = useSlackView(info.id)
  return (
    <section className="card">
      <h2>{view ? view.channelName : 'Slack'}</h2>
      {error && <p className="error-text">{error}</p>}
      {!view && !error && <p className="muted">Reading Slack…</p>}
      {view && view.messages.length === 0 && <p className="muted">No messages.</p>}
      {view && (
        <div className="slack-messages">
          {view.messages.map((m) => (
            <Message key={m.ts} m={m} itemId={info.id} focus={view.focus === m.ts} threaded={!view.focus} />
          ))}
        </div>
      )}
    </section>
  )
}

function Message({ m, itemId, focus, threaded }: { m: SlackMessage; itemId: string; focus?: boolean; threaded: boolean }) {
  const [replies, setReplies] = useState<SlackMessage[] | null>(null)
  const [open, setOpen] = useState(false)
  const toggle = async () => {
    setOpen(!open)
    if (!replies) setReplies((await api().slackView(itemId, m.ts)).messages)
  }
  const reply = m.thread_ts && m.thread_ts !== m.ts
  return (
    <div className={`slack-message${focus ? ' focus' : ''}${reply && !threaded ? ' reply' : ''}`}>
      <div className="slack-meta">
        <b>{m.author}</b>{' '}
        {m.permalink ? (
          <a className="muted" href={m.permalink} target="_blank" rel="noreferrer">
            {when(m.ts)}
          </a>
        ) : (
          <span className="muted">{when(m.ts)}</span>
        )}
      </div>
      {m.text && <Markdown text={m.text} />}
      {m.files && <p className="muted">Attached: {m.files.join(', ')}</p>}
      {threaded && m.replies ? (
        <button data-ui className="link" onClick={() => void toggle()}>
          {open ? 'Hide' : 'Show'} {m.replies} {m.replies === 1 ? 'reply' : 'replies'}
        </button>
      ) : null}
      {open && (
        <div className="slack-thread">
          {replies ? replies.map((r) => <Message key={r.ts} m={r} itemId={itemId} threaded={false} />) : <p className="muted">Reading…</p>}
        </div>
      )}
    </div>
  )
}
