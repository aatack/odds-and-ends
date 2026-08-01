import React, { useEffect, useState } from 'react'
import { CODE_GROUND, CODE_PAD, CODE_TYPE, CodeText } from './CodeBlock'
import { TextEditor } from './TextEditor'
import { cn } from '../../helpers/cn'

/**
 * A {@link CodeBlock} you can type into: the same ground, the same type, the same
 * highlighting — with a textarea laid over it whose own text is invisible, so
 * what you see is the highlighted copy and what you edit is a plain text field.
 *
 * Two layers rather than one because nothing highlights a textarea's contents in
 * place. The box is the one in the flow, so it drives the height and the caret,
 * the selection and the click position are all the browser's own; the highlighted
 * text is pinned over it, painted after and therefore *above* the selection
 * rectangle, which is why selected code stays readable. They line up only because
 * both are set at `CODE_TYPE`/`CODE_PAD` and both wrap the same way.
 *
 * Committing is `TextEditor`'s: on blur, and on Enter (Shift+Enter for a newline).
 */
export function CodeEditor({
  value,
  setValue,
  placeholder,
  language,
  className,
}: {
  value: string
  setValue: (value: string) => void
  /** Shown while the box is empty. */
  placeholder?: string
  /** Prism language. Defaults to TypeScript, like the rest of the app's code. */
  language?: string
  className?: string
}): React.JSX.Element {
  // What is being typed, which is not what has been committed: the layer
  // underneath has to keep up with the keystrokes, and the write happens on blur.
  const [draft, setDraft] = useState(value)
  useEffect(() => setDraft(value), [value])

  return (
    <div className={cn('relative my-px w-full min-w-0 overflow-hidden', CODE_GROUND, className)}>
      <TextEditor
        value={value}
        setValue={setValue}
        onDraft={setDraft}
        placeholder={placeholder}
        // Invisible text over a visible copy of it. The caret is given a colour of
        // its own, since it would otherwise be transparent too.
        className={cn(CODE_TYPE, CODE_PAD, 'text-transparent caret-gray-900')}
      />
      <div aria-hidden className="pointer-events-none absolute inset-0">
        <CodeText code={draft} language={language} wrap />
      </div>
    </div>
  )
}
