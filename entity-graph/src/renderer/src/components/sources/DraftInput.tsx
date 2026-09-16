import React, { useEffect, useState } from 'react'
import { cn } from '../../helpers/cn'
import { Input } from '../ui/Input'

/**
 * A field whose value is committed rather than written per keystroke: a path is
 * not a path until it is finished being typed, and each write rebuilds every
 * pensive downstream of the node.
 */
export function DraftInput({
  value,
  onCommit,
  ...rest
}: {
  value: string
  onCommit: (next: string) => void
  mono?: boolean
} & Omit<React.InputHTMLAttributes<HTMLInputElement>, 'value' | 'onChange'>): React.JSX.Element {
  const [draft, setDraft] = useState(value)
  // Somebody else may have changed it — a node deleted upstream, a rebuild.
  useEffect(() => setDraft(value), [value])
  return (
    <Input
      {...rest}
      // Typing in a node must not drag it, and a click in a field must not pan.
      className={cn('nodrag nopan', rest.className)}
      value={draft}
      onChange={(e) => setDraft(e.target.value)}
      onBlur={() => draft !== value && onCommit(draft)}
      onKeyDown={(e) => {
        if (e.key === 'Enter') e.currentTarget.blur()
        if (e.key === 'Escape') setDraft(value)
      }}
    />
  )
}
