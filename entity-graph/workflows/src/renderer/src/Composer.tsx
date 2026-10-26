import { SendHorizontal } from 'lucide-react'
import { useAtom } from 'jotai'
import { useEffect, useRef, useState } from 'react'
import type { Catalog, Item, PresetInfo } from '../../shared/types'
import type { Someone } from './people'
import { GrowingTextarea } from './GrowingTextarea'
import { TypeIcon } from './icons'
import { ItemPicker } from './ItemPicker'
import type { Recent } from './sections'
import { draftsAtom, withoutEmpty } from './views'

/**
 * The chat input. What is typed goes through a preset (a note by default; a PR
 * to wait on…); with nothing typed, the choices are instead to hand the task to
 * a bot ("Assign to @Threepio"), the likeliest first: whoever else had it last,
 * else the bot the human handed something to last. The icon, for what Enter
 * would do now, is in the top left; Tab and Shift+Tab step through the choices,
 * and Enter sends. Ctrl+Enter twice resolves the item: the first press only
 * asks, in the hint, and any other key takes it back. What is typed is kept, by
 * `draftKey`, until it is sent. An `@` typed at the start of a word opens the
 * finder, bots first; what is picked (or made, with Tab) takes the `@`'s place
 * as a reference, `[id]()`. A bot referred to in a note is handed the task as it
 * is sent, and the human is taken off it. Escape leaves the `@` as typed.
 */
export function Composer({
  catalog,
  draftKey,
  onSend,
  onResolve,
  mention,
  assignees,
}: {
  catalog: Catalog
  draftKey: string
  onSend: (preset: string, text: string) => Promise<boolean>
  onResolve: () => void
  /** What `@` picks from, and how it makes a new item. */
  mention: { items: Item[]; recent: Recent; create: (title: string) => Promise<string> }
  /** Who to offer to hand the task to (bots and people), the likeliest first. */
  assignees: Someone[]
}) {
  const [atText, setAtText] = useState(0)
  const [atEmpty, setAtEmpty] = useState(0)
  const [drafts, setDrafts] = useAtom(draftsAtom)
  const text = drafts[draftKey] ?? ''
  const setText = (t: string) => setDrafts((d) => withoutEmpty({ ...d, [draftKey]: t }))
  // Empty drafts are dropped as they empty; this sweeps up any left behind, every minute.
  useEffect(() => {
    const sweep = () => setDrafts((d) => withoutEmpty(d))
    sweep()
    const t = setInterval(sweep, 60_000)
    return () => clearInterval(t)
  }, [setDrafts])
  const [resolving, setResolving] = useState(false)
  const box = useRef<HTMLTextAreaElement>(null)
  // With nothing typed, the choices are who to hand the task to; else the presets.
  const assigns: PresetInfo[] = assignees.map((b) => ({
    id: `assign:${b.id}`,
    name: `Assign to @${b.name}`,
    hint: `hand this task to @${b.name}`,
    icon: b.bot ? 'bot' : 'user',
  }))
  const empty = !text.trim() && assigns.length > 0
  const choices = empty ? assigns : catalog.presets
  const at = (empty ? atEmpty : atText) % choices.length
  const setAt = empty ? setAtEmpty : setAtText
  const preset = choices[at]
  // What Enter would do now: a note that @s someone hands them the task too.
  const mentioned = assignees.filter((b) => text.includes(`[${b.id}]()`))
  const shown = mentioned.length && preset.id === 'note' ? { ...preset, name: `Note for @${mentioned.map((b) => b.name).join(', @')}` } : preset
  // Focused as it opens (and as the preset changes), with the cursor after any draft rather than before it.
  useEffect(() => {
    const el = box.current
    if (!el) return
    el.focus()
    el.setSelectionRange(el.value.length, el.value.length)
  }, [at])
  // Where the `@` being completed is, while the finder is open for it.
  const [mentionAt, setMentionAt] = useState<number | null>(null)
  /** Puts the cursor back in the text box, at `caret` (else at the end), once a modal over it has closed. */
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
    setText(text.slice(0, mentionAt) + ref + text.slice(mentionAt + 1))
    setMentionAt(null)
    // The cursor goes back to the text box, after the reference, once the modal has closed (and whatever it does with focus).
    refocus(mentionAt + ref.length)
  }
  const send = async () => {
    if (await onSend(preset.id, text)) setText('')
  }
  return (
    <div className="builder-row compact" onMouseDown={(e) => {
      if ((e.target as HTMLElement).closest('textarea, button, [role=button]')) return
      e.preventDefault()
      box.current?.focus()
    }}>
      <span className="builder-icon" title={`${shown.name} (Tab to change)`}>
        <TypeIcon type={shown.icon} />
      </span>
      <div className="builder-fields hinted">
        <GrowingTextarea
          ref={box}
          value={text}
          aria-label={preset.name}
          onChange={(e) => {
            const value = e.target.value
            const caret = e.target.selectionStart
            // One `@` typed at the start of a word: find what it refers to.
            if (value.length === text.length + 1 && value[caret - 1] === '@' && /(^|\s)$/.test(value.slice(0, caret - 1)))
              setMentionAt(caret - 1)
            setText(value)
          }}
          onBlur={() => setResolving(false)}
          onKeyDown={(e) => {
            if (e.key === 'Enter' && e.ctrlKey && !e.shiftKey && !e.altKey && !e.metaKey) {
              e.preventDefault()
              if (resolving) onResolve()
              setResolving(!resolving)
              return
            }
            if (!['Control', 'Shift', 'Alt', 'Meta'].includes(e.key)) setResolving(false)
            if (e.key === 'Tab') {
              e.preventDefault()
              setAt((at + (e.shiftKey ? -1 : 1) + choices.length) % choices.length)
            }
            if (e.key === 'Enter' && !e.shiftKey && !e.nativeEvent.isComposing) {
              e.preventDefault()
              void send()
            }
          }}
        />
        {resolving ? (
          <span className={`hint ${text ? 'below' : ''}`}>
            <b>Resolve?</b> — press Ctrl+Enter again to resolve this task
          </span>
        ) : (
          !text && (
            <span className="hint">
              <b>{shown.name}</b> — {shown.hint}
            </span>
          )
        )}
      </div>
      <span className="builder-icon right" title="Send (Enter)" role="button" aria-label="Send" onClick={() => void send()}>
        <span className="type-icon">
          <SendHorizontal className="icon" />
        </span>
      </span>
      {mentionAt !== null && (
        <ItemPicker
          title="Mention"
          bots={assignees}
          items={mention.items}
          recent={mention.recent}
          verb="Insert"
          onPick={insertRef}
          onCreate={(title) => void mention.create(title).then(insertRef)}
          onClose={() => {
            setMentionAt(null)
            refocus()
          }}
        />
      )}
    </div>
  )
}
