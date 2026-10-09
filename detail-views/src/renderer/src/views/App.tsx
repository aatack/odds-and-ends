import type { Entity, ModuleInfo, View } from '../../../core/types.ts'
import { inertRows, type Peek, type PeekTarget, type Pick, type Rect, type SessionDialog } from '../state.ts'
import { imageSrc } from '../images.ts'
import { PeekWindow } from './Peek.tsx'
import { ViewPane } from './View.tsx'
import { kinds } from './kinds.tsx'
import { ItemContext, KindsContext, PeekContext } from './primitives.tsx'
import type { ItemGestures, PeekGestures } from './primitives.tsx'
import type { ViewProps } from './types.ts'
import { Button, ClockContext, HeaderPill, Modal, PillContent } from './primitives.tsx'
import { useMemo, useRef, useState } from 'react'
import type { PointerEvent as ReactPointerEvent } from 'react'

export interface PeekProps {
  peeks: Peek[]
  views: Record<string, View>
  gestures: PeekGestures
  hover(target: PeekTarget, anchor: Rect, origin: string | null): void
  enter(key: string): void
  leave(window: string | null): void
  onPlace(key: string, rect: Rect): void
  onRaise(key: string): void
  onClose(key: string): void
  onOpen(target: PeekTarget): void
  /** A peek's view is scrolled near its end: walk further. */
  loadMore(root: string): void
  /** What is under way, by entity, so a peek's buttons can say so. */
  working: Record<string, string[]>
  /** An action waiting in a peek, by the peek's key, and its prompt. */
  acting: { action: string; peek: string; draft: string; composing: boolean } | null
  /** Starts one of a peek's root's actions. */
  onAction(key: string, action: string): void
  onDraft(text: string): void
  onCompose(composing: boolean): void
}

export function App(props: {
  viewing: string | null
  onImage(ref: string | null): void
  peek: PeekProps
  items: ItemGestures
  modules: ModuleInfo[]
  module: string | null
  view: ViewProps | null
  /** A move or link waiting for its other end, and the item it started on. */
  picking: { pick: Pick; subject: Entity | null } | null
  onModule(root: string): void
  /** The panel on the right: an item in context with where I am. */
  side: SideProps | null
  /** On a phone: the bar of buttons standing in for keys. */
  phone: PhoneBarProps | null
  /** The stack of views to here, oldest first; the last is the one on screen. */
  crumbs: (Entity | null)[]
  /** A form being filled in, and what it can offer. */
  dialog: DialogProps | null
  /** The entity being inspected, as JSON. */
  inspect: { id: string; json: string; onClose(): void } | null
  /** A passing message. */
  toast: string | null
  /** The time, ticking each second. */
  now: number
  onCrumb(at: number): void
  /** Wiping the cache store, at the foot of the sidebar: each press a step closer, until the last wipes. */
  wipe: { label: string; stage: number; onWipe(): void; onCancel(): void; cancelLabel: string }
}) {
  const { peek } = props
  return (
    <ClockContext.Provider value={props.now}>
    <KindsContext.Provider value={kinds}>
    <ItemContext.Provider value={props.items}>
      <PeekContext.Provider value={peek.gestures}>
        <div className={`app${props.phone ? ' phone' : ''}`}>
          <nav className="modules">
            {props.modules.map((module) => (
              <div
                key={module.id}
                className={`module${module.id === props.module ? ' active' : ''}`}
                onMouseDown={() => props.onModule(module.root)}
              >
                {module.name}
              </div>
            ))}
            <div className="sidebar-foot">
              <Button label={props.wipe.label} active={props.wipe.stage > 0} onClick={props.wipe.onWipe} quiet={props.wipe.stage === 0} />
              {props.wipe.stage > 0 && <Button label={props.wipe.cancelLabel} onClick={props.wipe.onCancel} />}
            </div>
          </nav>
          <main className="focus">
            {props.crumbs.length > 1 && (
              <div className="crumbs">
                {props.crumbs.map((crumb, at) => (
                  <span key={at} className="crumb" onMouseDown={() => props.onCrumb(at)}>
                    {crumb ? <PillContent entity={crumb} /> : '…'}
                  </span>
                ))}
              </div>
            )}
            {props.view && <ViewPane {...props.view} />}
            {props.phone && <PhoneBar {...props.phone} />}
          </main>
          {props.side && <SidePanel {...props.side} onImage={props.onImage} />}
          {props.picking && <PickBar {...props.picking} />}
          {props.dialog && <SessionForm {...props.dialog} />}
          {props.inspect && <Inspector {...props.inspect} />}
          {props.toast && <div className="toast">{props.toast}</div>}
          {peek.peeks.map((one) => (
            <PeekWindow
              key={one.key}
              peek={one}
              view={one.target.kind === 'entity' ? peek.views[one.target.id] : undefined}
              hover={peek.hover}
              onEnter={peek.enter}
              onLeave={peek.leave}
              onPlace={peek.onPlace}
              onRaise={peek.onRaise}
              onClose={peek.onClose}
              onOpen={peek.onOpen}
              onOpenEntity={(id) => props.view?.onOpen(id)}
              onImage={props.onImage}
              onNearEnd={() => one.target.kind === 'entity' && peek.loadMore(one.target.id)}
              working={one.target.kind === 'entity' ? peek.working[one.target.id] : undefined}
              acting={peek.acting?.peek === one.key ? peek.acting : null}
              onAction={(action) => peek.onAction(one.key, action)}
              onDraft={peek.onDraft}
              onCompose={peek.onCompose}
            />
          ))}
          {props.viewing && (
            <div className="viewer" onMouseDown={() => props.onImage(null)}>
              <img src={imageSrc(props.viewing)} />
            </div>
          )}
        </div>
      </PeekContext.Provider>
    </ItemContext.Provider>
    </KindsContext.Provider>
    </ClockContext.Provider>
  )
}

export interface SideProps {
  id: string
  view: View | null
  working: string[] | undefined
  draft: string
  composing: boolean
  onDraft(text: string): void
  onCompose(composing: boolean): void
  onOpen(id: string): void
  onNearEnd(): void
  onClose(): void
  /** Its width once dragged, or null for the default. */
  width: number | null
  onResize(width: number): void
}

/** The narrowest the side panel goes, and the least it leaves the rest of the window. */
const sideMin = 320
const mainMin = 400

/**
 * The panel on the right (Ctrl+Enter): an item in context with where I am,
 * mostly a Claude session. Its view takes no cursor, as in a peek; a chat's
 * prompt box works. Its left edge drags to resize it; the × closes it.
 */
function SidePanel(props: SideProps & { onImage(ref: string | null): void }) {
  const { view } = props
  const shown = useMemo(() => view && inertRows(view), [view])
  // While dragging, the width follows the pointer here; it is kept when let go.
  const [live, setLive] = useState<number | null>(null)
  const grab = useRef<{ x: number; start: number } | null>(null)
  const panel = useRef<HTMLElement>(null)
  const clamp = (width: number) => Math.max(sideMin, Math.min(width, window.innerWidth - mainMin))
  const begin = (event: ReactPointerEvent) => {
    if (event.button !== 0 || !panel.current) return
    event.preventDefault()
    const start = panel.current.getBoundingClientRect().width
    grab.current = { x: event.clientX, start }
    setLive(start)
  }
  const drag = (event: ReactPointerEvent) => {
    if (grab.current) setLive(clamp(grab.current.start + grab.current.x - event.clientX))
  }
  const end = () => {
    if (grab.current && live !== null) props.onResize(live)
    grab.current = null
    setLive(null)
  }
  const width = live ?? props.width
  const prompt = useMemo(
    () => ({ draft: props.draft, composing: props.composing, onDraft: props.onDraft, onCompose: props.onCompose }),
    [props.draft, props.composing, props.onDraft, props.onCompose],
  )
  return (
    <aside ref={panel} className="side" style={width !== null ? { width: clamp(width) } : undefined}>
      <div className="side-resize" onPointerDown={begin} />
      {live !== null && <div className="peek-capture" onPointerMove={drag} onPointerUp={end} onPointerCancel={end} />}
      <div className="peek-bar">
        <span className="grow">
          <HeaderPill entity={view?.root} />
        </span>
        <Button label="Open" onClick={() => props.onOpen(props.id)} />
        <Button quiet label="×" title="Close" onClick={props.onClose} />
      </div>
      <div className="peek-body">
        {view && shown ? (
          <ViewPane
            view={view}
            shown={shown}
            interactive={false}
            prompt={prompt}
            edit={null}
            picking={null}
            working={props.working}
            acting={null}
            draft=""
            composing={false}
            onSelect={noop}
            onOpen={props.onOpen}
            onImage={props.onImage}
            onDraft={noop}
            onCompose={noop}
            find={null}
            findFocus={0}
            onFind={noop}
            onFindFocused={noop}
            onNearEnd={props.onNearEnd}
            onEditDraft={noop}
            onCommitEdit={noop}
            onCancelEdit={noop}
            onClose={props.onClose}
          />
        ) : null}
      </div>
    </aside>
  )
}

function noop(): void {}

const pickWords = {
  move: ['Moving', 'Select its new parent and press x'],
  link: ['Linking', 'Select what goes under it and press r'],
  linkReverse: ['Linking', 'Select what it goes under and press Shift+R'],
  context: ['Sharing a context', 'Select the other item and press c'],
} as const

/** Says a move or link is waiting for its other end, and how to give it one. */
function PickBar(props: { pick: Pick; subject: Entity | null }) {
  const [verb, how] = pickWords[props.pick.tool]
  return (
    <div className="pick-bar">
      {verb} {props.subject ? <span className="item-pill header"><PillContent entity={props.subject} /></span> : null} · {how} · Esc to cancel
    </div>
  )
}

export interface PhoneAction {
  id: string
  label: string
}

export interface PhoneBarProps {
  /** Always there, greyed when they can't be done: back, open, note, edit. */
  primary: (PhoneAction & { enabled: boolean })[]
  /** Every other action that can be done now. */
  more: PhoneAction[]
  /** Actions under way on the view, by id or by what they are (`older`). */
  busy: string[]
  menuOpen: boolean
  onRun(id: string): void
  onMenu(open: boolean): void
}

/**
 * The phone's keys, as buttons: the four used most along the bottom, and
 * everything else the keys can do, right now, under More. Both lists come
 * from the tool registry, so the phone can do what the desktop can.
 */
function PhoneBar(props: PhoneBarProps) {
  return (
    <>
      {props.menuOpen && (
        <div className="phone-menu" onMouseDown={() => props.onMenu(false)}>
          <div className="phone-menu-list" onMouseDown={(event) => event.stopPropagation()}>
            {props.more.map((action) => (
              <Button key={action.id} label={action.label} onClick={() => props.onRun(action.id)} />
            ))}
          </div>
        </div>
      )}
      <nav className="phone-bar">
        {props.primary.map((action) => (
          <Button key={action.id} label={action.label} disabled={!action.enabled && 'Not here'} onClick={() => props.onRun(action.id)} />
        ))}
        <Button label="More" active={props.menuOpen} disabled={!props.more.length && 'Nothing more here'} onClick={() => props.onMenu(!props.menuOpen)} />
      </nav>
    </>
  )
}

export interface DialogProps {
  dialog: SessionDialog
  /** Directories used before, most recent first, to pick from. */
  cwds: string[]
  busy: boolean
  onChange(fields: Partial<SessionDialog>): void
  onSubmit(): void
  onCancel(): void
}

/**
 * A new Claude session: its name, where it runs (blank for a new temporary
 * directory), and whether in a new worktree there. Enter makes it, Escape
 * gives up.
 */
function SessionForm(props: DialogProps) {
  const { dialog } = props
  const hasCwd = Boolean(dialog.cwd.trim())
  return (
    <Modal title="New Claude session" onClose={props.onCancel}>
        <input
          className="find"
          data-field="dialog-name"
          autoFocus
          onFocus={(event) => event.currentTarget.select()}
          placeholder="Name"
          value={dialog.name}
          onChange={(event) => props.onChange({ name: event.target.value })}
        />
        <input
          className="find"
          data-field="dialog-cwd"
          list="claude-cwds"
          placeholder="Directory (blank: a new temporary one)"
          spellCheck={false}
          value={dialog.cwd}
          onChange={(event) => props.onChange({ cwd: event.target.value })}
        />
        <datalist id="claude-cwds">
          {props.cwds.map((cwd) => (
            <option key={cwd} value={cwd} />
          ))}
        </datalist>
        {/* Only with a directory: a worktree is of the repo it is in. */}
        {hasCwd && (
          <label className="dialog-check">
            <input
              type="checkbox"
              data-field="dialog-worktree"
              checked={dialog.worktree}
              onChange={(event) => props.onChange({ worktree: event.target.checked })}
            />
            In a new worktree, on a branch of its own
          </label>
        )}
        <div className="dialog-buttons">
          <Button label="Cancel" hotkey="Esc" onClick={props.onCancel} />
          <Button label="Start" hotkey="↵" busy={props.busy} busyLabel="Starting…" onClick={props.onSubmit} />
        </div>
    </Modal>
  )
}

/** The selected entity as it stands in the cache, as JSON: its values and its links. */
function Inspector(props: { id: string; json: string; onClose(): void }) {
  return (
    <Modal title={<code>{props.id}</code>} wide onClose={props.onClose}>
      <pre className="inspect">{props.json}</pre>
    </Modal>
  )
}
