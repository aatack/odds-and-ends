import { Mic, SendHorizontal, Square } from 'lucide-react'
import { useHasDeepgram, useRecorder } from './recorder'
import { SLACK_LINK } from '../../shared/slack'
import { useAtom } from 'jotai'
import { useEffect, useRef, useState } from 'react'
import type { Catalog, Item, PresetInfo } from '../../shared/types'
import type { Someone } from './people'
import { GrowingTextarea } from './GrowingTextarea'
import { TypeIcon } from './icons'
import { ItemPicker } from './ItemPicker'
import { ToolRunner } from './ToolRunner'
import type { Recent } from './sections'
import { draftsAtom, withoutEmpty } from './views'
import type { DefaultAction } from './defaultActions'

/** The end of what is being said, as the record button shows it: the last two sentences (ended by `.`, `!` or `?`). */
const lastSentences = (text: string): string => {
  const sentences = text.split(/(?<=[.!?…])\s+/)
  return (sentences.length > 2 ? '… ' : '') + sentences.slice(-2).join(' ')
}

/**
 * The chat input. What is typed goes through a preset (a note by default; a PR
 * to wait on…); with nothing typed, the choices are instead the default actions
 * (defaultActions.ts: assign to the last bot, wait for Slack, snooze). The icon, for what Enter
 * would do now, is in the top left; Tab and Shift+Tab step through the choices,
 * and Enter sends. Ctrl+Enter twice resolves the item: the first press only
 * asks, in the hint, and any other key takes it back. What is typed is kept, by
 * `draftKey`, until it is sent. An `@` typed at the start of a word opens the
 * finder, bots first; what is picked (or made, with Tab) takes the `@`'s place
 * as a reference, `[id]()`. A bot referred to in a note is handed the task as it
 * is sent, and the human is taken off it. Escape leaves the `@` as typed. `/` typed first runs a tool (ToolRunner).
 */
export function Composer({
  catalog,
  draftKey,
  onSend,
  onResolve,
  mention,
  assignees,
  defaults,
  mobile = false,
}: {
  catalog: Catalog
  draftKey: string
  onSend: (preset: string, text: string) => Promise<boolean>
  onResolve: () => void
  /** What `@` picks from, and how it makes a new item. */
  mention: { items: Item[]; recent: Recent }
  /** Who `@` offers first (bots and people), the likeliest first. */
  assignees: Someone[]
  /** What the input offers with nothing typed (defaultActions.ts), in order; Enter runs the one shown. */
  defaults: DefaultAction[]
  /** On a phone: the icon is tapped to change what is sent, Enter is a new line (Send sends), and it is never focused for the person, which would raise the keyboard. */
  mobile?: boolean
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
  // With nothing typed, the choices are the default actions; else the presets.
  const empty = !text.trim() && defaults.length > 0
  const choices: PresetInfo[] = empty ? defaults : catalog.presets
  const at = (empty ? atEmpty : atText) % choices.length
  const setAt = empty ? setAtEmpty : setAtText
  const preset = choices[at]
  // What Enter would do now: a note that @s someone hands them the task too.
  const mentioned = assignees.filter((b) => text.includes(`[${b.id}]()`))
  const shown = mentioned.length && preset.id === 'note' ? { ...preset, name: `Note for @${mentioned.map((b) => b.name).join(', @')}` } : preset
  // Focused as it opens (and as the preset changes), with the cursor after any draft rather than before it.
  useEffect(() => {
    const el = box.current
    if (!el || mobile) return
    el.focus()
    el.setSelectionRange(el.value.length, el.value.length)
  }, [at])
  // Where the `@` being completed is, while the finder is open for it.
  const [mentionAt, setMentionAt] = useState<number | null>(null)
  // `/` typed into an empty input: run a tool.
  const [runningTool, setRunningTool] = useState(false)
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
    if (empty) return void (await defaults[at]?.run())
    if (await onSend(preset.id, text)) setText('')
  }
  // Recording: each finished utterance goes as a note, as if typed and sent (it hands the task to nobody).
  const recorder = useRecorder((said) => void onSend('note', said))
  const canRecord = useHasDeepgram()
  return (
    <div className="builder-row compact" onMouseDown={(e) => {
      if ((e.target as HTMLElement).closest('textarea, button, [role=button]')) return
      e.preventDefault()
      box.current?.focus()
    }}>
      <span
        className="builder-icon"
        title={`${shown.name} (${mobile ? 'tap' : 'Tab'} to change)`}
        {...(mobile ? { role: 'button', 'aria-label': `${shown.name}: tap to change`, onClick: () => setAt((at + 1) % choices.length) } : {})}
      >
        <TypeIcon type={shown.icon} />
      </span>
      <div className={`builder-fields hinted ${(recorder.on || recorder.error) && !text ? 'hearing' : ''}`}>
        <GrowingTextarea
          ref={box}
          value={text}
          aria-label={preset.name}
          onPaste={(e) => {
            // A Slack message's link, pasted on its own into an empty input, pulls the message in at once.
            const pasted = e.clipboardData.getData('text').trim()
            if (text.trim() || !new RegExp(`^${SLACK_LINK.source}$`).test(pasted)) return
            e.preventDefault()
            void onSend('note', pasted)
          }}
          onChange={(e) => {
            const value = e.target.value
            const caret = e.target.selectionStart
            // `/` as the first thing typed: pick a tool to run, rather than typing it.
            if (!text && value === '/') return setRunningTool(true)
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
            if (e.key === 'Enter' && !e.shiftKey && !e.nativeEvent.isComposing && !mobile) {
              e.preventDefault()
              void send()
            }
          }}
        />
        {recorder.on || recorder.error ? (
          <span className={`hint recording ${recorder.error ? 'error-text' : ''}`}>
            {recorder.error ?? (recorder.hearing ? lastSentences(recorder.hearing) : <b>Listening…</b>)}
          </span>
        ) : resolving ? (
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
      {/* With nothing typed, a record button beside Send: what is said is transcribed live and sent as notes. */}
      {canRecord && (!text || recorder.on) && (
        <span
          className={`builder-icon right record ${recorder.on ? 'on' : ''}`}
          title={recorder.on ? 'Stop recording' : 'Record: what you say is sent as notes'}
          role="button"
          aria-label={recorder.on ? 'Stop recording' : 'Record'}
          onClick={recorder.on ? recorder.stop : recorder.start}
        >
          <span className="type-icon">{recorder.on ? <Square className="icon" /> : <Mic className="icon" />}</span>
        </span>
      )}
      <span className="builder-icon right" title="Send (Enter)" role="button" aria-label="Send" onClick={() => void send()}>
        <span className="type-icon">
          <SendHorizontal className="icon" />
        </span>
      </span>
      {runningTool && (
        <ToolRunner
          itemId={draftKey}
          catalog={catalog}
          onClose={() => {
            setRunningTool(false)
            refocus()
          }}
        />
      )}
      {mentionAt !== null && (
        <ItemPicker
          title="Mention"
          bots={assignees}
          items={mention.items}
          recent={mention.recent}
          verb="Insert"
          onPick={insertRef}
          onClose={() => {
            setMentionAt(null)
            refocus()
          }}
        />
      )}
    </div>
  )
}
