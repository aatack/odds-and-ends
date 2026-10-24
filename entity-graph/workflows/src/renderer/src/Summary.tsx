import { useAtom } from 'jotai'
import { X } from 'lucide-react'
import { Markdown } from './components/ui/Markdown'
import { summaryAtom } from './views'

/** Ctrl+S's summary of the task on screen, above its chat, until its × (or Escape in it) closes it. */
export function Summary({ itemId }: { itemId: string }) {
  const [summary, setSummary] = useAtom(summaryAtom)
  if (summary?.id !== itemId) return null
  return (
    <section className="summary" tabIndex={-1} onKeyDown={(e) => e.key === 'Escape' && setSummary(null)}>
      <div className="summary-head">
        <span>Where it stands</span>
        <button className="blocker-cancel" title="Close" aria-label="Close" onClick={() => setSummary(null)}>
          <X className="icon" />
        </button>
      </div>
      {summary.text === null ? <p className="muted">Summarising…</p> : <Markdown text={summary.text} />}
    </section>
  )
}
