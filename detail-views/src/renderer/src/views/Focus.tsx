import { kinds } from './kinds.tsx'
import { Status } from './primitives.tsx'
import { Generic } from './Tasks.tsx'
import type { FocusProps } from './types.ts'

/** The full view of whatever is focused. */
export function Focus(props: FocusProps) {
  if (!props.focus.entity) {
    return (
      <div className="pane centred">
        <Status error={props.focus.error} />
      </div>
    )
  }
  const Full = kinds[props.focus.entity.type]?.Full ?? Generic
  return <Full {...props} />
}
