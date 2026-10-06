import type { Focus as FocusData, ModuleInfo } from '../../../core/types.ts'
import type { Preview as PreviewState } from '../state.ts'
import { Focus } from './Focus.tsx'
import { LinkContext } from './primitives.tsx'
import type { LinkGestures } from './primitives.tsx'
import { Preview } from './Preview.tsx'
import type { FocusProps } from './types.ts'

export function App(props: {
  viewing: string | null
  preview: PreviewState | null
  links: LinkGestures
  onPreviewEnter(): void
  onOpenExternal(url: string): void
  onImage(ref: string | null): void
  modules: ModuleInfo[]
  module: string | null
  focus: FocusData | null
  view: Omit<FocusProps, 'focus'>
  onModule(root: string): void
}) {
  return (
    <LinkContext.Provider value={props.links}>
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
        {props.viewing && (
          <div className="viewer" onMouseDown={() => props.onImage(null)}>
            <img src={`slack-image://${props.viewing}`} />
          </div>
        )}
        {props.preview && (
          <Preview
            preview={props.preview}
            onEnter={props.onPreviewEnter}
            onLeave={props.links.onLinkLeave}
            onOpen={props.onOpenExternal}
          />
        )}
      </div>
    </LinkContext.Provider>
  )
}
