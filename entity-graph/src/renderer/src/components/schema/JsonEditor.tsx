import React, { useEffect, useState } from 'react'
import { cn } from '../../helpers/cn'
import { TextEditor } from '../ui/TextEditor'
import { pretty } from './schema'

/**
 * A value as JSON text. Typing that parses is written through at once; typing
 * that doesn't is kept as it is, with the reason underneath, until it does — so
 * a half-typed object is never mistaken for a value. A value that changes from
 * outside replaces the text.
 */
export function JsonEditor({
  value,
  setValue,
  className,
  placeholder,
}: {
  value: unknown
  setValue: (value: unknown) => void
  className?: string
  placeholder?: string
}): React.JSX.Element {
  const [text, setText] = useState(() => pretty(value))
  const [error, setError] = useState<string | null>(null)

  // Follow the value when it moves for any reason but this box's own typing.
  useEffect(() => {
    setText((current) => {
      try {
        if (current.trim() && JSON.stringify(JSON.parse(current)) === JSON.stringify(value)) {
          return current
        }
      } catch {
        // The box holds something half-typed; the value from outside wins.
      }
      return pretty(value)
    })
    setError(null)
  }, [value])

  const onText = (next: string): void => {
    setText(next)
    if (!next.trim()) {
      setError(null)
      setValue(undefined)
      return
    }
    try {
      const parsed = JSON.parse(next)
      setError(null)
      setValue(parsed)
    } catch (e) {
      setError(e instanceof Error ? e.message : 'Not valid JSON')
    }
  }

  return (
    <div className={className}>
      <div className="max-h-[50vh] overflow-auto rounded-md bg-gray-100 shadow-xs">
        <TextEditor
          multiline
          eager
          value={text}
          setValue={onText}
          placeholder={placeholder}
          className={cn('font-mono text-[12.5px] leading-5 text-gray-900', 'px-2.5 py-1.5')}
        />
      </div>
      {error && <p className="pt-1 text-[11px] text-error-600">{error}</p>}
    </div>
  )
}
