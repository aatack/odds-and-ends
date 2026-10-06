import { createContext, memo, useContext, useEffect, useRef } from 'react'
import type { ReactNode } from 'react'
import { prEntityId } from '../../../core/types.ts'
import type { PeekTarget, Rect } from '../state.ts'

/** A list row that keeps itself on screen while it holds the cursor. */
export const Row = memo(function Row(props: {
  id: string
  selected: boolean
  className?: string
  onSelect(id: string): void
  children: ReactNode
}) {
  const ref = useRef<HTMLDivElement>(null)
  useEffect(() => {
    if (props.selected) ref.current?.scrollIntoView({ block: 'nearest' })
  }, [props.selected])
  return (
    <div
      ref={ref}
      className={`row${props.selected ? ' selected' : ''}${props.className ? ` ${props.className}` : ''}`}
      onMouseDown={() => props.onSelect(props.id)}
    >
      {props.children}
    </div>
  )
})

export function Composer(props: {
  kind: 'slack' | 'slack-token' | 'task' | 'action'
  draft: string
  composing: boolean
  placeholder?: string
  onDraft(text: string): void
  onCompose(composing: boolean): void
}) {
  const ref = useRef<HTMLTextAreaElement & HTMLInputElement>(null)
  useEffect(() => {
    const element = ref.current
    if (!element) return
    if (props.composing && document.activeElement !== element) element.focus()
    if (!props.composing && document.activeElement === element) element.blur()
  }, [props.composing])
  const shared = {
    ref,
    className: 'composer',
    value: props.draft,
    placeholder: props.placeholder,
    spellCheck: props.kind !== 'slack-token',
    onChange: (event: { target: { value: string } }) => props.onDraft(event.target.value),
    onFocus: () => props.onCompose(true),
    onBlur: () => props.onCompose(false),
  }
  if (props.kind === 'slack-token') return <input {...shared} type="password" autoFocus />
  const rows = Math.min(8, props.draft.split('\n').length)
  return <textarea {...shared} rows={rows} />
}

export function Status(props: { error: string | null }) {
  return props.error ? <div className="status">{props.error}</div> : null
}

/** Gestures anything peekable forwards, wherever it is drawn. Provided by App. */
export interface PeekGestures {
  onPeekEnter(target: PeekTarget, anchor: Rect): void
  onPeekLeave(): void
}

export const PeekContext = createContext<PeekGestures>({ onPeekEnter: () => {}, onPeekLeave: () => {} })

/** Hover handlers that peek at `target`; spread onto any element. */
export function usePeek(target: PeekTarget | null) {
  const { onPeekEnter, onPeekLeave } = useContext(PeekContext)
  if (!target) return {}
  return {
    onMouseEnter: (event: { currentTarget: Element }) => {
      const { x, y, width, height } = event.currentTarget.getBoundingClientRect()
      onPeekEnter(target, { x, y, width, height })
    },
    onMouseLeave: onPeekLeave,
  }
}

/**
 * Every link in the app. Hovering peeks at the page; a click does nothing
 * else, so the browser is only ever opened from the peek.
 */
export function Link(props: { href: string; children: ReactNode; page?: boolean }) {
  // A link to something the app tracks peeks at the item, not the page,
  // unless the page itself is wanted.
  const entity = props.page ? null : prEntityId(props.href)
  const peek = usePeek(entity ? { kind: 'entity', id: entity } : { kind: 'url', url: props.href })
  return (
    <a
      className="link"
      href={props.href}
      {...peek}
      onMouseDown={(event) => event.stopPropagation()}
      onClick={(event) => event.preventDefault()}
    >
      {props.children}
    </a>
  )
}
