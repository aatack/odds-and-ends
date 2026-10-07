import type { Entity, ModuleInfo, View } from '../../../core/types.ts'
import type { Peek, PeekTarget, Pick, Rect } from '../state.ts'
import { imageSrc } from '../images.ts'
import { PeekWindow } from './Peek.tsx'
import { ViewPane } from './View.tsx'
import { kinds } from './kinds.tsx'
import { ItemContext, KindsContext, PeekContext } from './primitives.tsx'
import type { ItemGestures, PeekGestures } from './primitives.tsx'
import type { ViewProps } from './types.ts'
import { Button, PillContent } from './primitives.tsx'

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
  /** On a phone: the bar of buttons standing in for keys. */
  phone: PhoneBarProps | null
  /** The stack of views to here, oldest first; the last is the one on screen. */
  crumbs: (Entity | null)[]
  onCrumb(at: number): void
}) {
  const { peek } = props
  return (
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
          {props.picking && <PickBar {...props.picking} />}
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
  )
}

const pickWords = {
  move: ['Moving', 'Select its new parent and press x'],
  link: ['Linking', 'Select what goes under it and press r'],
  linkReverse: ['Linking', 'Select what it goes under and press Shift+R'],
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

export interface PhoneBarProps {
  canBack: boolean
  canOpen: boolean
  canOlder: boolean
  olderBusy: boolean
  onBack(): void
  onOpen(): void
  onNote(): void
  onEdit(): void
  onOlder(): void
}

/** The phone's keys, as buttons along the bottom: back, open, a note, edit, older. */
function PhoneBar(props: PhoneBarProps) {
  return (
    <nav className="phone-bar">
      <Button label="Back" disabled={!props.canBack && 'Nothing to go back to'} onClick={props.onBack} />
      <Button label="Open" disabled={!props.canOpen && 'Select a row to open'} onClick={props.onOpen} />
      <Button label="Note" onClick={props.onNote} />
      <Button label="Edit" onClick={props.onEdit} />
      {props.canOlder && <Button label="Older" busyLabel="Loading…" busy={props.olderBusy} onClick={props.onOlder} />}
    </nav>
  )
}
