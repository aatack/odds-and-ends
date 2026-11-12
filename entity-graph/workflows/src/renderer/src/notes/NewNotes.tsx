import { NotebookPen } from 'lucide-react'
import { useState } from 'react'
import { api } from '../useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/** New notes: its title typed in place, then Enter makes the note (pinned to `pinTo`) and opens its tree. */
export function NewNotes({ pinTo, act, onOpen }: { pinTo: string; act: Act; onOpen: (id: string) => void }) {
  const [title, setTitle] = useState<string | null>(null)
  if (title === null)
    return (
      <button data-ui className="new-notes" onClick={() => setTitle('')}>
        <NotebookPen className="icon" />
        New notes
      </button>
    )
  return (
    <input
      className="new-notes-title"
      autoFocus
      value={title}
      placeholder="Title, then Enter"
      aria-label="Title of the new notes"
      onChange={(e) => setTitle(e.target.value)}
      onBlur={() => !title.trim() && setTitle(null)}
      onKeyDown={(e) => {
        if (e.key === 'Escape') {
          e.preventDefault()
          e.stopPropagation()
          setTitle(null)
        } else if (e.key === 'Enter' && title.trim()) {
          e.preventDefault()
          const text = title.trim()
          setTitle(null)
          void act(async () => onOpen(await api().newNotes(text, pinTo)))
        }
      }}
    />
  )
}
