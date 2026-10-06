import type { Focus as FocusData, ModuleInfo } from '../../../core/types.ts'
import type { Peek, PeekTarget, Rect } from '../state.ts'
import { Focus } from './Focus.tsx'
import { PeekWindow } from './Peek.tsx'
import { kinds } from './kinds.tsx'
import { ItemContext, KindsContext, PeekContext } from './primitives.tsx'
import type { ItemGestures, PeekGestures } from './primitives.tsx'
import type { FocusProps } from './types.ts'

export interface PeekProps {
  peeks: Peek[]
  foci: Record<string, FocusData>
  gestures: PeekGestures
  onHold(): void
  onPlace(key: string, rect: Rect): void
  onRaise(key: string): void
  onClose(key: string): void
  onOpen(target: PeekTarget): void
}

export function App(props: {
  viewing: string | null
  onImage(ref: string | null): void
  peek: PeekProps
  items: ItemGestures
  modules: ModuleInfo[]
  module: string | null
  focus: FocusData | null
  view: Omit<FocusProps, 'focus'>
  onModule(root: string): void
}) {
  const { peek } = props
  return (
    <KindsContext.Provider value={kinds}>
    <ItemContext.Provider value={props.items}>
      <PeekContext.Provider value={peek.gestures}>
        <div className="app">
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
          <main className="focus">{props.focus && <Focus focus={props.focus} {...props.view} />}</main>
          {peek.peeks.map((one) => (
            <PeekWindow
              key={one.key}
              peek={one}
              focus={one.target.kind === 'entity' ? peek.foci[one.target.id] : undefined}
              onEnter={peek.onHold}
              onLeave={peek.gestures.onPeekLeave}
              onPlace={peek.onPlace}
              onRaise={peek.onRaise}
              onClose={peek.onClose}
              onOpen={peek.onOpen}
              onOpenEntity={props.view.onOpen}
              onImage={props.onImage}
            />
          ))}
          {props.viewing && (
            <div className="viewer" onMouseDown={() => props.onImage(null)}>
              <img src={`slack-image://${props.viewing}`} />
            </div>
          )}
        </div>
      </PeekContext.Provider>
    </ItemContext.Provider>
    </KindsContext.Provider>
  )
}
