import { Hash, Lock, MessageCircle, Users } from 'lucide-react'
import { useEffect, useState } from 'react'
import { api } from './useWorkflows'

type Conversation = Awaited<ReturnType<ReturnType<typeof api>['slackConversations']>>[number]
const ICON = { channel: Hash, private: Lock, group: Users, dm: MessageCircle }

/**
 * Your Slack conversations, at the bottom of your own Assignments page (not anyone else's): channels, private channels,
 * group DMs and DMs, while Slack is set up. Pressing one opens it as a Slack channel item (its id is its link, made the
 * first time: `setInferredType`), on its last day of messages.
 */
export function SlackChannels({ onOpen }: { onOpen: (id: string) => void }) {
  const [all, setAll] = useState<Conversation[] | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [shown, setShown] = useState(12)
  useEffect(() => {
    void api().slackConversations().then(setAll, (e: Error) => setError(e.message))
  }, [])
  if (error) return <p className="muted slack-channels-error">Slack: {error}</p>
  if (!all?.length) return null
  const open = (c: Conversation) =>
    void api()
      .setInferredType(c.link)
      .catch(() => {})
      .then(() => onOpen(c.link))
  return (
    <section className="list items-group slack-channels">
      <h3>
        Slack <span className="count">{all.length}</span>
      </h3>
      {all.slice(0, shown).map((c) => {
        const Icon = ICON[c.kind]
        return (
          <div key={`${c.workspace}:${c.id}`} className="row" onClick={() => open(c)} title={c.workspace}>
            <Icon className="icon" />
            <span className="row-title">{c.label}</span>
          </div>
        )
      })}
      {shown < all.length && (
        <div className="row more" onClick={() => setShown(all.length)}>
          <span className="muted">…{all.length - shown} more</span>
        </div>
      )}
    </section>
  )
}
