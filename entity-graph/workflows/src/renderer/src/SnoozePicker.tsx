import { useState } from 'react'
import { Modal } from './components/ui/Modal'

const HOUR = 3_600_000

/** A day from today (0 is today), at a given hour, local time. */
function at(days: number, hour: number, from = new Date()): number {
  const d = new Date(from)
  d.setDate(d.getDate() + days)
  d.setHours(hour, 0, 0, 0)
  return d.getTime()
}

/** The usual times to snooze until, from now: those still to come. */
function choices(now = Date.now()): { label: string; until: number }[] {
  const today = new Date(now)
  // Days until next Monday: a week from a Monday.
  const monday = (8 - today.getDay()) % 7 || 7
  return [
    { label: 'In an hour', until: now + HOUR },
    { label: 'In three hours', until: now + 3 * HOUR },
    { label: 'This evening', until: at(0, 18, today) },
    { label: 'Tomorrow morning', until: at(1, 9, today) },
    { label: 'Monday morning', until: at(monday, 9, today) },
    { label: 'In a week', until: at(7, 9, today) },
  ].filter((c) => c.until > now + 10 * 60_000)
}

/** A time as a datetime-local input reads it. */
const local = (ms: number) => {
  const d = new Date(ms - new Date(ms).getTimezoneOffset() * 60_000)
  return d.toISOString().slice(0, 16)
}

/** When to wake a task: one of the usual times, or any other. */
export function SnoozePicker({ title, onPick, onClose }: { title: string; onPick: (until: number) => void; onClose: () => void }) {
  const [custom, setCustom] = useState(() => local(at(1, 9)))
  const chosen = new Date(custom).getTime()
  const ok = Number.isFinite(chosen) && chosen > Date.now()
  return (
    <Modal title={title} blur onClose={onClose}>
      <div className="snooze-picker">
        {choices().map((c) => (
          <button data-ui key={c.label} className="snooze-choice" onClick={() => onPick(c.until)}>
            <span>{c.label}</span>
            <span className="muted">{new Date(c.until).toLocaleString(undefined, { weekday: 'short', hour: 'numeric', minute: '2-digit' })}</span>
          </button>
        ))}
        <div className="snooze-custom">
          <input type="datetime-local" value={custom} onChange={(e) => setCustom(e.target.value)} />
          <button data-ui className="primary" disabled={!ok} onClick={() => onPick(chosen)}>
            Snooze
          </button>
        </div>
      </div>
    </Modal>
  )
}
