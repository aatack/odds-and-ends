import React, { useState } from 'react'
import { Sheet } from '../components/ui/Sheet'
import { Input } from '../components/ui/Field'
import { useEntityLabel } from '../state/hooks'
import { closeSheet } from '../state/ui'
import { dispatch } from '../tools/dispatch'
import { groupedTools, listedTools } from '../tools/registry'
import { useToolContext } from './useToolContext'

// The command palette, as a sheet.
//
// Same idea as the desktop's: one list of everything the app can do, filtered to
// what applies, with the search over labels and aliases. What it doesn't do is take
// arguments here — a tool that needs one opens its own small form (./ArgSheet) or
// asks for a row to be tapped, because a sheet that is a list and a form at once
// would be neither.
//
// The search field is not focused on open, deliberately: the keyboard would cover
// the list, and most of the time the thing wanted is one of the first few lines.

export function ActionSheet(): React.JSX.Element {
  const ctx = useToolContext()
  const [search, setSearch] = useState('')
  const label = useEntityLabel(ctx.entityId ?? '')
  const groups = groupedTools(listedTools(ctx, search))

  const run = (id: string): void => {
    // Closed first: running a tool may open the sheet that asks for its arguments.
    closeSheet()
    dispatch(id)
  }

  return (
    <Sheet title={ctx.entityId ? label : 'Actions'} onClose={closeSheet}>
      <Input
        value={search}
        onChange={(e) => setSearch(e.target.value)}
        placeholder="Search actions…"
        enterKeyHint="done"
        className="mb-2"
      />
      {groups.length === 0 && (
        <p className="px-1 py-6 text-center text-[13px] text-gray-400">
          Nothing matches “{search}”.
        </p>
      )}
      {groups.map((group) => (
        <section key={group.hint} className="pb-2">
          <h3 className="px-3 pt-2 pb-1 text-[11px] font-semibold tracking-wide text-gray-400 uppercase">
            {group.hint}
          </h3>
          {group.tools.map((tool) => (
            <button
              key={tool.id}
              type="button"
              onClick={() => run(tool.id)}
              className="flex min-h-12 w-full items-center rounded-xl px-3 text-left text-[15px] text-gray-900 active:bg-gray-100"
            >
              {tool.label}
            </button>
          ))}
        </section>
      ))}
    </Sheet>
  )
}
