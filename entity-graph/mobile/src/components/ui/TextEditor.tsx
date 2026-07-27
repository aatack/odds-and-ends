import React, { useEffect, useLayoutEffect, useRef } from 'react'
import { cn } from '../../helpers/cn'

/**
 * The one free-text control for editing an entity in place: a textarea that grows
 * with its content, so a row being typed into is exactly as tall as the row it will
 * become.
 *
 * Enter is *not* intercepted. On a desktop keyboard, Enter commits and Shift+Enter
 * breaks the line; a soft keyboard has no Shift, so intercepting Enter would leave
 * no way to type a second line at all. Committing is therefore an explicit button
 * in the bar above the keyboard, and Enter means what the key says it means.
 */
export function TextEditor({
  value,
  setValue,
  onBlur,
  placeholder,
  className,
  style,
  autoFocus,
}: {
  value: string
  setValue: (next: string) => void
  onBlur?: () => void
  placeholder?: string
  className?: string
  style?: React.CSSProperties
  autoFocus?: boolean
}): React.JSX.Element {
  const ref = useRef<HTMLTextAreaElement>(null)

  // Height follows content. Measured every render rather than on input, so a draft
  // restored from state opens at the right height instead of one line tall.
  useLayoutEffect(() => {
    const el = ref.current
    if (!el) return
    el.style.height = 'auto'
    el.style.height = `${el.scrollHeight}px`
  }, [value])

  // Bring the box into view once the keyboard has taken its share of the screen.
  // `interactive-widget=resizes-content` shrinks the viewport, and the scroll has
  // to happen after that or it scrolls to where the row used to be.
  useEffect(() => {
    if (!autoFocus) return
    const el = ref.current
    if (!el) return
    el.focus()
    // Put the caret at the end: an edit opens on existing text, and starting from
    // character zero would mean scrubbing to the end before typing.
    el.setSelectionRange(el.value.length, el.value.length)
    const timer = window.setTimeout(
      () => el.scrollIntoView({ block: 'center', behavior: 'auto' }),
      120,
    )
    return () => window.clearTimeout(timer)
  }, [autoFocus])

  return (
    <textarea
      ref={ref}
      value={value}
      onChange={(e) => setValue(e.target.value)}
      onBlur={onBlur}
      placeholder={placeholder}
      rows={1}
      // Autocorrect stays on — this is prose — but the first letter of a bullet
      // isn't the start of a sentence often enough to be capitalised for you.
      autoCapitalize="sentences"
      spellCheck
      className={cn(
        'w-full resize-none overflow-hidden bg-transparent p-0 outline-none',
        'placeholder:text-gray-400',
        className,
      )}
      style={style}
    />
  )
}
