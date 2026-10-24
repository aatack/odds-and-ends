import { SendHorizontal } from 'lucide-react'
import { useEffect, useRef, useState } from 'react'
import type { Catalog } from '../../shared/types'
import { GrowingTextarea } from './GrowingTextarea'
import { TypeIcon } from './icons'

/**
 * The chat input. What is typed goes through a preset (a message for a bot, a
 * plain message, a PR to wait on…): its icon is in the top left, Tab and
 * Shift+Tab step through them, and Enter sends.
 */
export function Composer({ catalog, onSend }: { catalog: Catalog; onSend: (preset: string, text: string) => Promise<boolean> }) {
  const [at, setAt] = useState(0)
  const [text, setText] = useState('')
  const box = useRef<HTMLTextAreaElement>(null)
  const preset = catalog.presets[at]
  useEffect(() => box.current?.focus(), [at])
  const send = async () => {
    if (await onSend(preset.id, text)) setText('')
  }
  // Tapping the preset icon steps through presets, as Tab does, for touch.
  const cyclePreset = () => setAt((at + 1) % catalog.presets.length)
  return (
    <div className="builder-row compact" onMouseDown={(e) => {
      if ((e.target as HTMLElement).closest('textarea, button, [role=button]')) return
      e.preventDefault()
      box.current?.focus()
    }}>
      <span
        className="builder-icon"
        role="button"
        aria-label={`${preset.name} (change preset)`}
        title={`${preset.name} (Tab to change)`}
        onClick={cyclePreset}
      >
        <TypeIcon type={preset.icon} />
      </span>
      <div className="builder-fields hinted">
        <GrowingTextarea
          ref={box}
          value={text}
          aria-label={preset.name}
          onChange={(e) => setText(e.target.value)}
          onKeyDown={(e) => {
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
        {!text && (
          <span className="hint">
            <b>{preset.name}</b> — {preset.hint}
          </span>
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
