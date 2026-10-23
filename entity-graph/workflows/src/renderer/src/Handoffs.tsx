import { useEffect, useRef, useState } from 'react'
import type { Catalog, Handoff, HandoffKind, HandoffSpec, ItemDetail } from '../../shared/types'
import { CompactForm, fieldOrder } from './CompactForm'
import { SendHorizontal } from 'lucide-react'
import { Avatar, TypeIcon } from './icons'

const time = (ms: number) => new Date(ms).toLocaleString()
const minutes = (ms: number) => (ms < 60_000 ? `${Math.round(ms / 1000)}s` : `${Math.round(ms / 60_000)}m`)

/** One line saying what a handoff is for. Its type is its icon, so the line doesn't say it. */
export function summary(h: HandoffSpec): string {
  const p = h.params
  switch (h.type) {
    case 'human':
      return `${p.needed}${p.approval ? ` (approve ${(p.approval as { action: string }).action})` : ''}`
    case 'bot':
      return String(p.instructions)
    case 'github':
      return `${p.repo}#${p.number}: until ${p.until}`
    case 'slack':
      return `${p.message ? `Post “${p.message}”, then w` : 'W'}ait for a reply in ${p.channel}`
    case 'items':
      return `Wait for ${(p.item_ids as string[]).length} item(s) to close`
    case 'script':
      return String(p.description ?? 'Route on what came before')
  }
}

/** What a resolution comes to, in a line. */
export function outcome(h: Handoff): string {
  const r = h.resolution ?? {}
  if (h.status === 'cancelled') return 'cancelled'
  return String(r.error ? `failed: ${r.error}` : (r.detail ?? r.decision ?? r.outcome ?? ''))
}

/** The field that holds a type's words, carried over when the type changes so nothing typed is lost. */
const WORDS: Partial<Record<HandoffKind, string>> = { bot: 'instructions', human: 'needed', slack: 'message', script: 'source' }

/** What an empty box says: its type, and what should happen. In the type's main field, which comes first. */
const HINTS: Record<HandoffKind, { field: string; name: string; text: string }> = {
  human: { field: 'needed', name: 'Human', text: 'what is needed from you' },
  bot: { field: 'instructions', name: 'Bot', text: 'what the bot should do' },
  github: { field: 'repo', name: 'GitHub', text: 'the repository (owner/name) of the pull request to wait on' },
  slack: { field: 'message', name: 'Slack', text: 'a message to post, then wait for a reply' },
  items: { field: 'item_ids', name: 'Items', text: 'the items to wait for, comma separated' },
  script: { field: 'source', name: 'Script', text: 'code that decides what happens next' },
}

const filled = (v: unknown) => v !== undefined && v !== '' && !(Array.isArray(v) && !v.length)

/**
 * A list of handoffs to put at the front of the queue. Each is a box with its
 * type's icon in the corner; Tab and Shift+Tab in a box step through the types,
 * and Enter sends.
 */
export function HandoffBuilder(props: {
  catalog: Catalog
  value: HandoffSpec[]
  onChange: (v: HandoffSpec[]) => void
  /** Enter sends, as in any chat; Shift+Enter is a new line. */
  onSend: () => void
}) {
  const { catalog, value, onChange, onSend } = props
  const set = (i: number, h: HandoffSpec) => onChange(value.map((x, j) => (j === i ? h : x)))
  const types = catalog.handoffs.map((t) => t.type)
  // A box's fields are replaced when its type changes; the focus goes to the new first field.
  const rows = useRef<(HTMLDivElement | null)[]>([])
  const [refocus, setRefocus] = useState<number | null>(null)
  // A box shows one field at a time, so it stays one row high whatever its type: which one, by box.
  const [at, setAt] = useState<Record<number, number>>({})
  useEffect(() => {
    if (refocus === null) return
    rows.current[refocus]?.querySelector<HTMLElement>('input, textarea, select')?.focus()
    setRefocus(null)
  }, [refocus, value])
  const step = (i: number, by: number) => {
    const h = value[i]
    const type = types[(types.indexOf(h.type) + by + types.length) % types.length]
    const words = WORDS[h.type] && h.params[WORDS[h.type]!]
    set(i, { type, params: words !== undefined && WORDS[type] ? { [WORDS[type]!]: words } : {} })
    setAt({ ...at, [i]: 0 })
    setRefocus(i)
  }
  return (
    <div className="builder">
      {value.map((h, i) => {
        const type = catalog.handoffs.find((t) => t.type === h.type)!
        return (
          <div
            key={i}
            ref={(el) => (rows.current[i] = el)}
            className="builder-row compact"
            onMouseDown={(e) => {
              // Anywhere in the box that isn't a control puts the cursor in its text.
              if ((e.target as HTMLElement).closest('input, textarea, select, button')) return
              e.preventDefault()
              const box = rows.current[i]
              const words = WORDS[h.type] && box?.querySelector<HTMLElement>(`[aria-label^="${WORDS[h.type]}"]`)
              ;(words || box?.querySelector<HTMLElement>('textarea, input, select'))?.focus()
            }}
            onKeyDown={(e) => {
              if (e.key === 'Enter' && !e.shiftKey && !e.nativeEvent.isComposing) {
                e.preventDefault()
                onSend()
              }
              if (e.key !== 'Tab') return
              e.preventDefault()
              // An empty box steps through the types; once something is in it, Tab moves between its fields.
              if (!Object.values(h.params).some(filled)) return step(i, e.shiftKey ? -1 : 1)
              const count = fieldOrder(type.params, HINTS[h.type].field).length
              const to = (at[i] ?? 0) + (e.shiftKey ? -1 : 1)
              if (to < 0 || to >= count) return
              setAt({ ...at, [i]: to })
              setRefocus(i)
            }}
          >
            <span className="builder-icon" title={`${h.type}: ${type.description} (Tab to change)`}>
              <TypeIcon type={h.type} />
            </span>
            <div className="builder-fields">
              <CompactForm
                key={h.type}
                schema={type.params}
                value={h.params}
                onChange={(params) => set(i, { ...h, params })}
                only={at[i] ?? 0}
                hint={{
                  field: HINTS[h.type].field,
                  content: (
                    <>
                      <b>{HINTS[h.type].name}</b> — {HINTS[h.type].text}
                    </>
                  ),
                }}
              />
            </div>
            {value.length > 1 && (
              <button data-ui className="link builder-remove" title="Remove" onClick={() => onChange(value.filter((_, j) => j !== i))}>
                ×
              </button>
            )}
            {i === value.length - 1 && (
              // The type icon's twin, in the other corner.
              <span className="builder-icon right" title="Send (Enter)" role="button" aria-label="Send" onClick={onSend}>
                <span className="type-icon">
                  <SendHorizontal className="icon" />
                </span>
              </span>
            )}
          </div>
        )
      })}
    </div>
  )
}

/** Who set a handoff, in words, for the details. */
function sender(createdBy: string): string {
  if (createdBy === 'bot') return 'a bot'
  if (createdBy === 'script' || createdBy === 'script:bot') return 'a script'
  if (createdBy === 'system') return 'the app'
  if (createdBy === 'mcp') return 'an MCP client'
  if (createdBy === 'human') return 'you'
  return createdBy
}

/** What the human did with a human handoff, as their reply; null when the handoffs they set say it. */
function reply(h: Handoff): string | null {
  const r = h.resolution ?? {}
  const action = r.action as { name: string; changed: string[] } | undefined
  if (r.decision === 'approved' && action)
    return `Ran ${action.name}${action.changed.length ? ` (changed ${action.changed.join(', ')})` : ''}`
  if (r.decision === 'closed') return 'Closed'
  if (r.decision === 'handled_outside') return 'Done outside the tool'
  return null
}

const WAITS = ['github', 'slack', 'items']

const shortTime = (ms: number) => new Date(ms).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })

type Entry =
  | { at: number; order: number; kind: 'handoff'; handoff: Handoff }
  | { at: number; order: number; kind: 'reply'; handoff: Handoff; text: string }
  | { at: number; order: number; kind: 'event'; event: ItemDetail['events'][number] }
  | { at: number; order: number; kind: 'note'; text: string }

/**
 * The item's history as a chat: every handoff a bubble, from whoever set it.
 * What the human set or did is on the right; everything else, on the left.
 * Events between them are small notes in the middle, and what is queued comes
 * last, faded. A bubble shows its words and the time; pressing it shows the rest.
 */
export function Chat({ detail }: { detail: ItemDetail }) {
  const shown = detail.handoffs.filter((h) => h.status !== 'queued')
  const queued = detail.handoffs.filter((h) => h.status === 'queued')
  const entries: Entry[] = []
  shown.forEach((h, order) => {
    entries.push({ at: h.createdAt, order, kind: 'handoff', handoff: h })
    const text =
      h.resolution?.outcome === 'taken_over' && h.resolvedBy === 'human'
        ? 'Took over'
        : h.type === 'human' && h.resolvedBy === 'human'
          ? reply(h)
          : null
    if (text) entries.push({ at: h.resolvedAt!, order, kind: 'reply', handoff: h, text })
    // What a wait came to is news in itself: nothing after it may say so.
    if (WAITS.includes(h.type) && h.status === 'resolved' && h.resolvedBy === 'system')
      entries.push({ at: h.resolvedAt!, order, kind: 'note', text: outcome(h) })
  })
  detail.events
    .filter((e) => !['created', 'handled_outside'].includes(e.kind))
    .forEach((e) => entries.push({ at: e.at, order: shown.length, kind: 'event', event: e }))
  entries.sort((a, b) => a.at - b.at || a.order - b.order)
  return (
    <section className="chat">
      {entries.map((e, i) =>
        e.kind === 'handoff' ? (
          <Bubble key={i} h={e.handoff} detail={detail} />
        ) : e.kind === 'reply' ? (
          <div key={i} className="bubble-row mine">
            <div className="bubble">
              {e.text}
              <span className="bubble-time">{shortTime(e.at)}</span>
            </div>
          </div>
        ) : e.kind === 'note' ? (
          <div key={i} className="chat-note" title={time(e.at)}>
            {e.text}
          </div>
        ) : (
          <div key={i} className="chat-note" title={time(e.event.at)}>
            {e.event.text}
          </div>
        ),
      )}
      {queued.map((h) => (
        <Bubble key={h.id} h={h} detail={detail} />
      ))}
    </section>
  )
}

function Bubble({ h, detail }: { h: Handoff; detail: ItemDetail }) {
  const [open, setOpen] = useState(false)
  const mine = h.createdBy === 'human'
  const run = detail.runs.find((r) => r.handoffId === h.id)
  const output = h.resolution?.output as string | undefined
  const status =
    h.status === 'active'
      ? 'waiting on this now'
      : h.status === 'queued'
        ? 'queued'
        : h.status === 'cancelled'
          ? 'cancelled'
          : `${outcome(h) || 'done'}${h.startedAt && h.resolvedAt ? ` · took ${minutes(h.resolvedAt - h.startedAt)}` : ''}`
  return (
    <div className={`bubble-row ${mine ? 'mine' : 'theirs'} ${h.status}`}>
      {!mine && <Avatar createdBy={h.createdBy} />}
      <div className={`bubble ${open ? 'open' : ''}`} onClick={() => setOpen(!open)}>
        {h.type !== 'human' && <TypeIcon type={h.type} />}
        {summary(h)}
        <span className="bubble-time">{shortTime(h.createdAt)}</span>
        {open && (
          <div className="more" onClick={(e) => e.stopPropagation()}>
            <div className="bubble-meta">
              Set by {sender(h.createdBy)} · {status}
              {run?.costUsd != null && ` · $${run.costUsd.toFixed(2)}`}
            </div>
            <Timeline times={h.times} />
            {output && <pre className="output">{output}</pre>}
            <pre className="output">{JSON.stringify({ params: h.params, resolution: h.resolution && { ...h.resolution, output: undefined } }, null, 2)}</pre>
          </div>
        )}
      </div>
    </div>
  )
}

/** When each thing happened to a handoff, as far as it has got. */
function Timeline({ times: t }: { times: Handoff['times'] }) {
  const rows: [string, number | null, string?][] = [
    ['Set', t.set],
    ['Started', t.started],
    ['First seen', t.firstViewed, t.views > 1 ? `seen ${t.views} times, last ${time(t.lastViewed!)}` : undefined],
    ['Skipped', t.skips ? t.lastViewed : null, t.skips ? `${t.skips} times` : undefined],
    ['Bot started', t.firstRunStarted, t.runs > 1 ? `${t.runs} runs` : undefined],
    ['Bot finished', t.firstRunEnded],
    ['First checked', t.firstChecked, t.checks > 1 ? `checked ${t.checks} times, last ${time(t.lastChecked!)}` : undefined],
    ['Resolved', t.resolved],
    ['Cancelled', t.cancelled],
  ]
  return (
    <table className="timeline">
      <tbody>
        {rows
          .filter(([, at]) => at !== null)
          .map(([label, at, extra]) => (
            <tr key={label}>
              <td className="muted">{label}</td>
              <td>{time(at!)}</td>
              <td className="muted">{extra}</td>
            </tr>
          ))}
      </tbody>
    </table>
  )
}
