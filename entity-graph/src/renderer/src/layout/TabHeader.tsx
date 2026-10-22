import React from 'react'
import { ChevronRight, X } from '@untitledui/icons'
import { EntityPill, PillContent, PillWrapper } from '../components/EntityPill'
import { Button } from '../components/ui/Button'
import * as A from '../state/actions'
import { useCrumbs } from '../state/hooks'

// The top of a tab — its title and its crumbs — drawn the same way wherever a
// tab is: in a group's strip, and along the bar of a peek.

/**
 * A tab is the entity it is showing: a pill on a background of its own — the
 * header's own two buttons, the filled one for the tab you are in and the quiet
 * one for the rest — with a way to close it. The pill brings the entity gestures
 * with it, so a tab can be right-clicked for the tool list or middle-clicked to
 * open the same entity again elsewhere.
 */
export function TabTitle({
  rootId,
  active,
  onSelect,
  onClose,
}: {
  rootId: string | undefined
  active: boolean
  onSelect?: () => void
  /** Absent: no × (a peek has its own). */
  onClose?: () => void
}): React.JSX.Element {
  const tab = (
    <Button
      variant={active ? 'secondary' : 'tertiary'}
      size="sm"
      className="group shrink-0"
      onClick={onSelect}
    >
      {rootId ? <PillContent id={rootId} bounded /> : <span className="text-gray-400">Empty</span>}
      {onClose && (
        <span
          role="button"
          tabIndex={-1}
          className="opacity-0 hover:text-gray-700 group-hover:opacity-100"
          onClick={(e) => {
            e.stopPropagation()
            onClose()
          }}
        >
          <X size={11} />
        </span>
      )}
    </Button>
  )
  return rootId ? (
    <PillWrapper id={rootId} className="shrink-0">
      {tab}
    </PillWrapper>
  ) : (
    tab
  )
}

/**
 * The way back out: the frames the tab drilled through to get here, each of which
 * pops everything above it when clicked — which the frame history can then undo.
 * Only the ones you came through. Where you are now is the tree below it and the
 * tab above it, so a crumb for it would say the same thing a third time and be the
 * one crumb with nothing to click; the trailing chevron is what says you are
 * inside. Absent with a single frame on the stack, where there is no way back.
 *
 * Each crumb is a bare pill — no surface, capped in width — that previews on
 * hover, since it refers to somewhere other than where you are.
 */
export function Breadcrumb({ tabId }: { tabId: string }): React.JSX.Element | null {
  const crumbs = useCrumbs(tabId).slice(0, -1)
  if (!crumbs.length) return null
  return (
    <nav className="flex shrink-0 items-center gap-1 overflow-x-auto px-3 py-1.5 text-[11px] text-gray-400">
      {crumbs.map((crumb) => (
        <React.Fragment key={crumb.frameId}>
          <EntityPill
            id={crumb.rootId}
            bare
            bounded
            peek
            className="shrink-0"
            onClick={() => A.popToFrame(tabId, crumb.frameId)}
          />
          <ChevronRight size={11} className="shrink-0 text-gray-300" />
        </React.Fragment>
      ))}
    </nav>
  )
}
