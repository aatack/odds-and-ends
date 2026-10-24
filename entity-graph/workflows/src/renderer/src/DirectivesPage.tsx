import { Trash2 } from 'lucide-react'
import { useCallback, useEffect, useState } from 'react'
import type { ItemEvent } from '../../shared/types'
import { Markdown } from './components/ui/Markdown'
import { IconButton } from './components/ui/IconButton'
import { useEventMenu } from './EventMenu'
import { GrowingTextarea } from './GrowingTextarea'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/** A directive is meant to last about a day: older ones are drawn faded. */
const DAY = 24 * 60 * 60_000

/** How long ago, roughly: minutes, then hours, then days. */
const age = (ms: number) => {
  const m = Math.floor(ms / 60_000)
  if (m < 1) return 'just now'
  if (m < 60) return `${m}m ago`
  if (m < 24 * 60) return `${Math.floor(m / 60)}h ago`
  return `${Math.floor(m / (24 * 60))}d ago`
}

/**
 * What the human is working on at the moment: every directive that holds, the
 * newest first, each with its age. Bots read the latest when they need the
 * broad picture. Add one at the top; deleting one cancels it.
 */
export function DirectivesPage({ act, onOpen }: { act: Act; onOpen: (id: string) => void }) {
  const [directives, setDirectives] = useState<ItemEvent[] | null>(null)
  const [text, setText] = useState('')
  const menu = useEventMenu()
  const refresh = useCallback(async () => setDirectives(await api().directives()), [])
  useEffect(() => {
    void refresh()
    return api().onChange(() => void refresh())
  }, [refresh])
  // The ages move on while the page is open.
  const [now, setNow] = useState(Date.now())
  useEffect(() => {
    const t = setInterval(() => setNow(Date.now()), 60_000)
    return () => clearInterval(t)
  }, [])
  const add = () =>
    text.trim() &&
    act(async () => {
      await api().addDirective(text)
      setText('')
    })
  return (
    <div className="page directives-page">
      <section className="card add">
        <div className="builder-row directive-input">
          <GrowingTextarea
            value={text}
            placeholder="What are we working on?"
            onChange={(e) => setText(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === 'Enter' && e.ctrlKey) {
                e.preventDefault()
                void add()
              }
            }}
          />
        </div>
        <button data-ui className="primary" disabled={!text.trim()} onClick={() => void add()} title="Ctrl+Enter">
          Add
        </button>
      </section>
      {directives?.length === 0 && <p className="muted">No directives yet.</p>}
      {directives?.map((d) => (
        <section key={d.id} className={`card directive ${now - d.createdAt > DAY ? 'stale' : ''}`} {...menu(d.id)}>
          <div className="directive-head">
            <button data-ui className="link" title={new Date(d.createdAt).toLocaleString()} onClick={() => onOpen(d.id)}>
              {age(Math.max(0, now - d.createdAt))}
              {d.createdBy !== 'human' && ` · from ${d.createdBy}`}
            </button>
            <IconButton title="Delete" className="text-error-600 hover:text-error-600" onClick={() => void act(() => api().cancel(d.id))}>
              <Trash2 className="size-[15px]" />
            </IconButton>
          </div>
          <Markdown text={String(d.params.text ?? '')} />
        </section>
      ))}
    </div>
  )
}
