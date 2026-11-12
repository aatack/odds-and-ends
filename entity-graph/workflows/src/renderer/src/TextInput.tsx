import { forwardRef, useImperativeHandle, useRef, useState, type TextareaHTMLAttributes } from 'react'
import type { Item } from '../../shared/types'
import { GrowingTextarea } from './GrowingTextarea'
import { ItemPicker } from './ItemPicker'
import type { Recent } from './sections'

/** What `@` picks from: the items, the recent ones, and bots and people to offer first. */
export type Mentions = { items: Item[]; recent: Recent; bots?: { id: string; name: string; bot?: boolean }[] }

/**
 * The app's one text box for what is written (the chat input, a note in a tree): it grows with its text, and an `@`
 * typed at the start of a word opens the finder, bots first; what is picked takes the `@`'s place as a reference,
 * `[id]()`, which Markdown draws as the item's pill. Escape leaves the `@` as typed. `onText` is told every change.
 */
export const TextInput = forwardRef<
  HTMLTextAreaElement,
  Omit<TextareaHTMLAttributes<HTMLTextAreaElement>, 'value' | 'onChange'> & { value: string; onText: (text: string) => void; mentions: Mentions }
>(function TextInput({ value, onText, mentions, ...rest }, outer) {
  const box = useRef<HTMLTextAreaElement>(null)
  useImperativeHandle(outer, () => box.current!)
  // Where the `@` being completed is, while the finder is open for it.
  const [mentionAt, setMentionAt] = useState<number | null>(null)
  /** Puts the cursor back in the box, at `caret` (else at the end), once the finder over it has closed. */
  const refocus = (caret?: number) => {
    const put = () => {
      const el = box.current
      if (!el) return
      el.focus()
      const at = caret ?? el.value.length
      el.setSelectionRange(at, at)
    }
    requestAnimationFrame(put)
    setTimeout(put, 50)
  }
  const insertRef = (id: string) => {
    if (mentionAt === null) return
    const ref = `[${id}]()`
    onText(value.slice(0, mentionAt) + ref + value.slice(mentionAt + 1))
    setMentionAt(null)
    refocus(mentionAt + ref.length)
  }
  return (
    <>
      <GrowingTextarea
        {...rest}
        ref={box}
        value={value}
        onChange={(e) => {
          const next = e.target.value
          const caret = e.target.selectionStart
          // One `@` typed at the start of a word: find what it refers to.
          if (next.length === value.length + 1 && next[caret - 1] === '@' && /(^|\s)$/.test(next.slice(0, caret - 1))) setMentionAt(caret - 1)
          onText(next)
        }}
      />
      {mentionAt !== null && (
        <ItemPicker
          title="Mention"
          bots={mentions.bots}
          items={mentions.items}
          recent={mentions.recent}
          verb="Insert"
          onPick={insertRef}
          onClose={() => {
            setMentionAt(null)
            refocus()
          }}
        />
      )}
    </>
  )
})
