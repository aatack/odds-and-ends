import { useCallback } from 'react'
import * as A from '../state/actions'
import type { EntityRow } from '../state/derive'
import { viewRows } from '../state/query'
import { useRows, useSheet } from '../state/hooks'
import { getView } from '../state/store'
import { samePath } from '../state/types'
import { closeSheet, openSheet, pickingSheet } from '../state/ui'
import { dispatch } from '../tools/dispatch'
import type { OutlineProps } from './Outline'

// The outline's logic: what each gesture means. The component that renders it holds
// no state and makes no decisions, so this is the whole of the interaction model.
//
// Every callback reads the atoms directly rather than closing over rendered values,
// which is what lets them all be stable — a row is memoised, and a callback that
// changed identity every render would defeat that on a list of two hundred.

export function useOutline(): OutlineProps {
  const { rows, loading, error } = useRows()
  const sheet = useSheet()
  const picking = sheet?.kind === 'pick'

  /**
   * Tapping a row means one of three things, in order: supply the argument being
   * pointed at, edit the row already selected, or select this one.
   *
   * "Tap again to edit" rather than "tap to edit" because a phone has no hover and
   * no right-click: the first tap has to be free to mean "this is the row I'm
   * talking about" for every other control on screen, and a single tap that opened
   * the keyboard would make choosing a row cost a dismissal every time.
   */
  const onTapRow = useCallback((row: EntityRow) => {
    const pick = pickingSheet()
    if (pick) {
      closeSheet()
      dispatch(pick.toolId, { ...pick.args, [pick.argName]: row.id })
      return
    }
    // Read the selection now rather than trusting the rendered row: a blur landing
    // on this same tap may have just committed a create, which moves it.
    const selected = samePath(viewRows().selectedPath, row.path)
    if (selected && getView().edit == null) A.startEdit(row.path, row.text ?? '')
    else A.selectPath(row.path)
  }, [])

  /** The mark carries the two gestures that belong to the glyph itself. */
  const onTapMark = useCallback(
    (row: EntityRow) => {
      if (pickingSheet()) {
        onTapRow(row)
        return
      }
      // Selected first, and not only so the bar acts on it next: the toggle below
      // reads the selection, as every tool that takes no argument does.
      A.selectPath(row.path)
      // A checkbox row's box ticks; anything else with children folds. A row with
      // neither has a bullet, and a bullet does nothing.
      if (row.open !== undefined) dispatch('toggle.checkbox')
      else if (row.hasChildren) A.toggleCollapse(row.id)
    },
    [onTapRow],
  )

  /** Long-press is the right-click: select, then offer everything applicable. */
  const onLongPressRow = useCallback((row: EntityRow) => {
    if (pickingSheet()) return
    A.selectPath(row.path)
    openSheet({ kind: 'actions' })
  }, [])

  const onDraft = useCallback((text: string) => A.setDraft(text), [])
  const onCommit = useCallback(() => dispatch('edit.commit'), [])
  const onNearEnd = useCallback(() => dispatch('view.loadMore'), [])

  return {
    rows,
    loading,
    error,
    picking,
    onTapRow,
    onTapMark,
    onLongPressRow,
    onDraft,
    onCommit,
    onNearEnd,
  }
}
