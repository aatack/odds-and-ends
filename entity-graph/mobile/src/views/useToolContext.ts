import { useMemo } from 'react'
import { useAtomValue, useView } from '../state/hooks'
import { pagesAtom } from '../state/query'
import { currentContext } from '../tools/context'
import type { ToolContext } from '../tools/types'

/**
 * The context as it stands, for the components that need to know whether a tool
 * applies — the bottom bar greys out what can't be done, and the action sheet omits
 * it. The same function the dispatcher calls, so a button that looks available and
 * a tool that refuses can't disagree.
 *
 * Lives with the views rather than in `state/hooks` because it reaches into the
 * tools layer, and the state layer is not allowed to know that layer exists.
 */
export function useToolContext(): ToolContext {
  const view = useView()
  const pages = useAtomValue(pagesAtom)
  return useMemo(() => currentContext(), [view, pages])
}
