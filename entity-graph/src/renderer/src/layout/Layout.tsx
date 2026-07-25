import React from 'react'
import { useFocus, useLayoutState } from '../state/hooks'
import { orderedGroups } from '../state/store'
import { TabGroupView } from './TabGroupView'

/**
 * The VS Code-style shell: tab groups laid out side by side, each showing the top
 * frame of its active tab. No key handling here — there is one router, at the top
 * level, which resolves keys against the focus chain instead.
 */
export function Layout(): React.JSX.Element {
  const layout = useLayoutState()
  const focus = useFocus()
  const groups = orderedGroups(layout)
  const visible = layout.expanded ? groups.filter((g) => g.id === focus.groupId) : groups

  return (
    <div className="flex h-full w-full">
      {visible.map((group) => (
        <TabGroupView key={group.id} group={group} selected={group.id === focus.groupId} />
      ))}
    </div>
  )
}
