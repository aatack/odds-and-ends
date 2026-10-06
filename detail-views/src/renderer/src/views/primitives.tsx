import { createContext, memo, useContext, useEffect, useRef } from 'react'
import type { Badge as BadgeData, Entity } from '../../../core/types.ts'
import type { Kinds, RowProps } from './kindTypes.ts'
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

/** Items mentioned on screen, and the gestures on them. Provided by App. */
export interface ItemGestures {
  /** The item, as far as it has loaded; reading it is what loads it. */
  item(id: string): Entity | null
  onOpen(id: string): void
}

export const ItemContext = createContext<ItemGestures>({ item: () => null, onOpen: () => {} })

/** The status mark an item carries, as its module worked it out. */
export function Badge(props: { badge: BadgeData | null | undefined }) {
  const { badge } = props
  if (!badge) return <span className="badge dot none" />
  return (
    <span className={`badge ${badge.shape} ${badge.tone}`} title={badge.reason}>
      {badge.shape === 'tick' ? '✓' : badge.shape === 'cross' ? '✕' : ''}
    </span>
  )
}

/** The item views, by type; provided by App so any view can draw any item. */
export const KindsContext = createContext<Kinds | null>(null)

/** An item's pill content, through the registry. */
export function PillContent(props: { entity: Entity; fallback?: ReactNode }) {
  const kinds = useContext(KindsContext)
  const Pill = kinds?.[props.entity.type]?.Pill
  return Pill ? <Pill entity={props.entity} fallback={props.fallback} /> : <span className="item-name">{props.entity.id}</span>
}

/** An item's row, through the registry. */
export function RowFor(props: RowProps) {
  const kinds = useContext(KindsContext)
  const Row = kinds?.[props.entity.type]?.Row
  return Row ? <Row {...props} /> : null
}

/** An item's pill heading a view or a window: nothing to do on it there. */
export function HeaderPill(props: { entity: Entity | null | undefined }) {
  if (!props.entity) return null
  return (
    <span className="item-pill header">
      <PillContent entity={props.entity} />
    </span>
  )
}

/**
 * An in-app item named somewhere, as a pill. Hovering peeks at it; a click
 * pushes it.
 */
export function ItemPill(props: { id: string; fallback: ReactNode }) {
  const { item, onOpen } = useContext(ItemContext)
  const peek = usePeek({ kind: 'entity', id: props.id })
  const entity = item(props.id)
  return (
    <span
      className="item-pill"
      {...peek}
      onMouseDown={(event) => {
        event.stopPropagation()
        onOpen(props.id)
      }}
    >
      {entity ? (
        <PillContent entity={entity} fallback={props.fallback} />
      ) : (
        <>
          <Badge badge={null} />
          <span className="item-name">{props.fallback}</span>
        </>
      )}
    </span>
  )
}

/**
 * Every link in the app. Hovering peeks at the page; a click does nothing
 * else, so the browser is only ever opened from the peek.
 */
export function Link(props: { href: string; children: ReactNode; page?: boolean }) {
  // A link to something the app tracks is that item, unless the page itself is wanted.
  const entity = props.page ? null : prEntityId(props.href)
  if (entity) return <ItemPill id={entity} fallback={props.children} />
  return <PageLink href={props.href}>{props.children}</PageLink>
}

function PageLink(props: { href: string; children: ReactNode }) {
  const peek = usePeek({ kind: 'url', url: props.href })
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
