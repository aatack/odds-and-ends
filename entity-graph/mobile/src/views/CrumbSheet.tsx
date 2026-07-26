import React from 'react'
import { Sheet, SheetRow } from '../components/ui/Sheet'
import * as A from '../state/actions'
import { useCrumbs } from '../state/hooks'
import { closeSheet } from '../state/ui'

// The navigation stack, as a list.
//
// The desktop shows a tab's frame stack as a row of crumbs along the top, which
// needs width the phone hasn't got — so the header shows the last two and this is
// where the rest of the trail lives. It is the route the user took to get here, not
// the entity's ancestry in the graph: the same thing when you drilled in, and the
// honest answer when you didn't.

export function CrumbSheet(): React.JSX.Element {
  const crumbs = useCrumbs()
  return (
    <Sheet title="Where you are" onClose={closeSheet}>
      <div className="pb-2">
        {crumbs.map((crumb, index) => (
          <SheetRow
            key={`${crumb.rootId}-${index}`}
            label={crumb.label}
            detail={index === crumbs.length - 1 ? 'here' : undefined}
            selected={index === crumbs.length - 1}
            onClick={() => {
              closeSheet()
              A.popToLevel(crumb.index)
            }}
          />
        ))}
      </div>
    </Sheet>
  )
}
