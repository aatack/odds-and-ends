import { createContext, useContext, useEffect, useRef } from 'react'
import type { Badge as BadgeData, Entity } from '../../../core/types.ts'
import { tints, type Kinds, type Tint } from './kindTypes.ts'
import type { ReactNode } from 'react'
import { prEntityId } from '../../../core/types.ts'
import type { PeekTarget, Rect } from '../state.ts'

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
  /** Where a tap on a link goes, where there is no hover to peek with (the phone). */
  onLinkTap?(url: string): void
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

/**
 * The time, ticking each second (the session's clock), for anything deep in
 * a view that says how long ago or how long for. Provided by App.
 */
export const ClockContext = createContext<number>(Date.now())

/** The item views, by type; provided by App so any view can draw any item. */
export const KindsContext = createContext<Kinds | null>(null)

/** An item's pill content, through the registry. */
export function PillContent(props: { entity: Entity; fallback?: ReactNode; findText?: string }) {
  const kinds = useContext(KindsContext)
  const Pill = kinds?.[props.entity.type]?.Pill
  return Pill ? <Pill entity={props.entity} fallback={props.fallback} findText={props.findText} /> : <span className="item-name">{props.entity.id}</span>
}

/**
 * Text with what a find matched marked, case-insensitively. Every renderer
 * draws its text through this, so a find shows where it matched.
 */
export function Highlight(props: { text: string; find?: string }) {
  const needle = props.find?.trim().toLowerCase()
  if (!needle) return <>{props.text}</>
  const parts: ReactNode[] = []
  const lower = props.text.toLowerCase()
  let at = 0
  for (let found = lower.indexOf(needle); found >= 0; found = lower.indexOf(needle, at)) {
    if (found > at) parts.push(props.text.slice(at, found))
    parts.push(<mark key={found}>{props.text.slice(found, found + needle.length)}</mark>)
    at = found + needle.length
  }
  if (at < props.text.length) parts.push(props.text.slice(at))
  return <>{parts}</>
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
  const { onLinkTap } = useContext(PeekContext)
  return (
    <a
      className="link"
      href={props.href}
      {...peek}
      onMouseDown={(event) => event.stopPropagation()}
      onClick={(event) => {
        event.preventDefault()
        onLinkTap?.(props.href)
      }}
    >
      {props.children}
    </a>
  )
}

/**
 * Every button in the app. Hover and press show it can be pressed; `busy`
 * says its action is under way (and blocks pressing again); `disabled` is the
 * reason it can't be pressed now, shown as its tooltip.
 */
export function Button(props: {
  label: ReactNode
  /** Its key, as a key cap shows it (`hotkeyOf`). */
  hotkey?: string
  onClick(): void
  busy?: boolean
  busyLabel?: string
  active?: boolean
  disabled?: string | false | null
  title?: string
  quiet?: boolean
}) {
  const blocked = Boolean(props.busy || props.disabled)
  return (
    <button
      className={`button${props.active ? ' active' : ''}${props.busy ? ' busy' : ''}${props.quiet ? ' quiet' : ''}`}
      disabled={blocked}
      aria-busy={props.busy || undefined}
      title={props.disabled || props.title}
      // Pressing a button never starts a drag or raises what it sits in.
      onPointerDown={(event) => event.stopPropagation()}
      onClick={props.onClick}
    >
      {props.hotkey && <code className="key">{props.hotkey}</code>}
      {props.busy ? (props.busyLabel ?? (typeof props.label === 'string' ? `${props.label}…` : props.label)) : props.label}
    </button>
  )
}

/** A box over everything, closed by a click outside it (or Escape, through the tools). */
export function Modal(props: { title: ReactNode; wide?: boolean; onClose(): void; children: ReactNode }) {
  return (
    <div className="dialog-backdrop" onMouseDown={props.onClose}>
      <div className={`dialog${props.wide ? ' wide' : ''}`} onMouseDown={(event) => event.stopPropagation()}>
        <div className="dialog-title">{props.title}</div>
        {props.children}
      </div>
    </div>
  )
}

/** An item's tint: its own `tint` value if it has a valid one, else its type's, else none. */
export function tintOf(kinds: Kinds, entity: Entity): Tint | null {
  const own = entity.data.tint
  if (typeof own === 'string' && (tints as readonly string[]).includes(own)) return own as Tint
  return kinds[entity.type]?.tint?.(entity) ?? null
}

/** The class a tint draws with (`.tinted .tint-<name>`), or none. */
export function tintClass(tint: Tint | null): string {
  return tint ? ` tinted tint-${tint}` : ''
}
