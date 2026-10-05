import type { Focus as FocusData, ModuleInfo } from '../../../core/types.ts'
import { Focus } from './Focus.tsx'
import type { FocusProps } from './types.ts'

export function App(props: {
  modules: ModuleInfo[]
  module: string | null
  focus: FocusData | null
  view: Omit<FocusProps, 'focus'>
  onModule(root: string): void
}) {
  return (
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
    </div>
  )
}
