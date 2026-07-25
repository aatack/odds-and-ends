import { type KeyboardEvent, useEffect, useLayoutEffect, useRef, useState } from 'react'
import { cn } from '../../helpers/cn'

// The single free-text editing primitive for prose surfaces. Autosizes: fills
// its width, then grows onto new lines. With `eager`, every keystroke calls
// setValue; otherwise setValue fires on blur or Enter (Shift+Enter = newline).
export function TextEditor({
  value,
  setValue,
  eager = false,
  placeholder,
  className,
  style,
  autoFocus,
  onKeyDown,
  onBlur,
}: {
  value: string
  setValue: (value: string) => void
  eager?: boolean
  placeholder?: string
  className?: string
  /** For type that can't be a utility class — a section heading's size, say. */
  style?: React.CSSProperties
  autoFocus?: boolean
  // Runs before the built-in Enter handling; call preventDefault to override it.
  onKeyDown?: (e: KeyboardEvent<HTMLTextAreaElement>) => void
  // Called after the value has been flushed on blur. With `eager` the value is
  // already current, so this is where an eager caller commits.
  onBlur?: () => void
}): React.JSX.Element {
  const [draft, setDraft] = useState(value)
  const ref = useRef<HTMLTextAreaElement>(null)
  // Tracks the last value we handed to setValue. `value` can't do this job: it
  // only catches up after the async action round-trips, so a commit followed by
  // the blur it triggers would both see the stale prop and fire twice.
  const committed = useRef(value)

  useEffect(() => {
    setDraft(value)
    committed.current = value
  }, [value])

  // Autofocus alone leaves the caret at the start of the value, so an edit
  // started on existing text would type in front of it. Focus is taken here
  // rather than through the attribute so the selection can be placed with it.
  useEffect(() => {
    const el = ref.current
    if (!autoFocus || !el) return
    el.focus()
    const end = el.value.length
    el.setSelectionRange(end, end)
  }, [autoFocus])

  // Re-measured on the type as well as the text: a section's heading size lands
  // as an inline style, and the box has to grow to match it.
  useLayoutEffect(() => {
    const el = ref.current
    if (!el) return
    el.style.height = 'auto'
    el.style.height = `${el.scrollHeight}px`
  }, [draft, style?.fontSize, style?.lineHeight])

  const commit = (next: string): void => {
    if (next !== committed.current) {
      committed.current = next
      setValue(next)
    }
  }

  return (
    <textarea
      ref={ref}
      rows={1}
      value={draft}
      placeholder={placeholder}
      style={style}
      onChange={(e) => {
        setDraft(e.target.value)
        if (eager) setValue(e.target.value)
      }}
      onBlur={() => {
        if (!eager) commit(draft)
        onBlur?.()
      }}
      onKeyDown={(e) => {
        onKeyDown?.(e)
        if (e.defaultPrevented) return
        if (e.key === 'Enter' && !e.shiftKey) {
          e.preventDefault()
          if (!eager) commit(draft)
          e.currentTarget.blur()
        }
      }}
      className={cn(
        'w-full resize-none bg-transparent outline-none placeholder:text-gray-400',
        className,
      )}
    />
  )
}
