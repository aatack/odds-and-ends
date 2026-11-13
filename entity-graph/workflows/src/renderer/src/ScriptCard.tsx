import { useAtomValue } from 'jotai'
import { Play, SquareCode } from 'lucide-react'
import type { ItemInfo } from '../../shared/types'
import { pillText } from '../../shared/text'
import { StatusLine } from './status'
import { api } from './useWorkflows'
import { viewsAtom } from './views'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/** The item on screen: what a script pressed is given as its focus. */
export function useOnScreen(): string | null {
  const views = useAtomValue(viewsAtom)
  const top = views[views.length - 1]
  return top && 'id' in top ? (top.id ?? null) : null
}

/**
 * A script as a card: its name, what it does, how it stands, and a play button that runs it on `on` (the item it sits
 * in, is pinned to, or whose right pane it is in) with the item on screen as its focus. `whole`, the card itself is
 * the button (the right pane's scripts for an item's type).
 */
export function ScriptCard({ info, on, act, whole = false }: { info: ItemInfo; on: string | null; act: Act; whole?: boolean }) {
  const focus = useOnScreen()
  const run = () => void act(() => api().runNow(info.id, { item: on, focus }))
  const description = typeof info.params.description === 'string' && info.params.name ? info.params.description : ''
  const body = (
    <>
      <SquareCode className="icon" />
      <span className="script-card-text">
        <span className="script-card-name">{pillText(info)}</span>
        {description && <span className="muted script-card-description">{description}</span>}
        <StatusLine id={info.id} />
      </span>
    </>
  )
  if (whole)
    return (
      <button data-ui className="script-card whole" title={`Run ${pillText(info)} on this`} onClick={run}>
        {body}
        <Play className="icon script-play" />
      </button>
    )
  return (
    <div className="script-card">
      {body}
      {!info.closed && (
        <button data-ui className="script-play-button" title={`Run ${pillText(info)}${on ? ' on this' : ''}`} aria-label="Run" onClick={run}>
          <Play className="icon" />
        </button>
      )}
    </div>
  )
}
