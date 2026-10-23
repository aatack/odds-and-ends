import { SendHorizontal } from 'lucide-react'
import { useAtom } from 'jotai'
import { useEffect, useRef, useState } from 'react'
import type { Catalog } from '../../shared/types'
import { GrowingTextarea } from './GrowingTextarea'
import { TypeIcon } from './icons'
import { draftsAtom, withoutEmpty } from './views'

/**
 * The chat input. What is typed goes through a preset (a plain message by
 * default, which runs a bot when sent empty; a message for a bot; a PR to wait
 * on…): its icon, for what Enter would do now, is in the top left, Tab and
 * Shift+Tab step through them, and Enter sends. Ctrl+Enter twice resolves the
 * item: the first press only asks, in the hint, and any other key takes it back.
 * What is typed is kept, by `draftKey`, until it is sent.
 */
export function Composer({
  catalog,
  draftKey,
  onSend,
  onResolve,
}: {
  catalog: Catalog
  draftKey: string
  onSend: (preset: string, text: string) => Promise<boolean>
  onResolve: () => void
}) {
  const [at, setAt] = useState(0)
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
  const preset = catalog.presets[at]
  // What Enter would do now: some presets do something else with nothing typed.
  const shown = !text.trim() && preset.whenEmpty ? preset.whenEmpty : preset
  useEffect(() => box.current?.focus(), [at])
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
          onChange={(e) => setText(e.target.value)}
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
              setAt((at + (e.shiftKey ? -1 : 1) + catalog.presets.length) % catalog.presets.length)
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
    </div>
  )
}
