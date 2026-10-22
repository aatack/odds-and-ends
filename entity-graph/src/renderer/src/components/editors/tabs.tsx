import React from 'react'
import { ChevronDown } from '@untitledui/icons'
import { EntityPill } from '../EntityPill'
import { Dropdown } from '../ui/Dropdown'
import { MenuItem, type Editors } from '../schema'
import { tabRootId } from '../../state/derive'
import { useLayoutState } from '../../state/hooks'
import { orderedGroups } from '../../state/store'

// Editors that read the app: what is open in it, for a value to point at.

/** The entities the open tabs show, each once, in the order the tabs sit. */
function useTabEntities(): string[] {
  const layout = useLayoutState()
  const ids: string[] = []
  for (const group of orderedGroups(layout)) {
    for (const tabId of group.tabIds) {
      const id = tabRootId(layout, tabId)
      if (id && !ids.includes(id)) ids.push(id)
    }
  }
  return ids
}

/**
 * A pick from the entities the open tabs show, each drawn as its tab draws it.
 * The value is the entity's id.
 */
function TabEntityPicker({
  value,
  onChange,
}: {
  value: unknown
  onChange: (value: unknown) => void
}): React.JSX.Element {
  const ids = useTabEntities()
  const chosen = typeof value === 'string' && value ? value : null
  return (
    <div className="w-full">
      <Dropdown
        trigger={({ toggle }) => (
          <button
            className="flex h-8 w-full items-center gap-1 px-2.5 text-left text-[13px] focus:outline-none"
            onClick={toggle}
          >
            <span className="flex min-w-0 flex-1 items-center">
              {chosen ? (
                <EntityPill id={chosen} bare className="text-gray-900" />
              ) : (
                <span className="text-gray-400">Choose a tab…</span>
              )}
            </span>
            <ChevronDown size={12} className="shrink-0 text-gray-400" />
          </button>
        )}
      >
        {(close) =>
          ids.length ? (
            ids.map((id) => (
              <MenuItem
                key={id}
                on={id === chosen}
                onClick={() => {
                  if (id !== chosen) onChange(id)
                  close()
                }}
              >
                <EntityPill id={id} bare />
              </MenuItem>
            ))
          ) : (
            <p className="px-3 py-1.5 text-[13px] text-gray-400">No tabs are open.</p>
          )
        }
      </Dropdown>
    </div>
  )
}

export const TAB_EDITORS: Editors = {
  tabEntity: {
    name: 'Open tab',
    kinds: ['string'],
    render: ({ value, onChange }) => <TabEntityPicker value={value} onChange={onChange} />,
  },
}
