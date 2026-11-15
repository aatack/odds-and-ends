import { Bot, MessageSquare, NotebookText, Pin, SquareCode, User } from 'lucide-react'
import { useContext } from 'react'
import { useAtom } from 'jotai'
import type { ItemInfo } from '../../shared/types'
import { nameOf } from './people'
import { OverviewContext } from './status'
import { previewAtom } from './views'

/**
 * In the margin to the right of a topic's row in a chat or a tree (outside the column, on a desktop): that it has notes
 * (in a chat) or comments (in a tree), and pins, each as an icon with how many that opens them beside it (pressed again,
 * shuts them); and who else has it, as their pictures (lit while a bot works on it). So a tree's sections can be handed
 * to bots and watched from the tree, and what is pinned to a note found from it. With none yet, an icon shows only while
 * its row is hovered (or its thread is open), to start them there. Only a topic someone has shows its margin unhovered
 * (hovered, or a thread of it open, any does), leaving what someone is to action; one you have has its icons lit.
 */
export function Margin({ info, other }: { info: ItemInfo; other: 'comments' | 'notes' }) {
  const o = useContext(OverviewContext)
  const [preview, setPreview] = useAtom(previewAtom)
  if (info.type !== 'topic') return null
  const others = (info.assignees ?? []).filter((a) => a !== o?.me.id)
  const yours = !!o && (info.assignees ?? []).includes(o.me.id)
  const working = o?.items.find((i) => i.id === info.id)?.bot ?? null
  const link = (as: 'comments' | 'notes' | 'pins') => {
    const count = (as === 'notes' ? info.notes : as === 'comments' ? info.comments : info.pins.length) ?? 0
    const Icon = as === 'notes' ? NotebookText : as === 'comments' ? MessageSquare : Pin
    const showing = preview?.id === info.id && preview.as === as
    const noun = as === 'notes' ? 'note' : as === 'comments' ? 'comment' : 'pin'
    return (
      <button
        data-ui
        className={`margin-link ${showing ? 'chosen' : ''} ${count ? '' : 'none'}`}
        title={count ? `${count} ${noun}${count === 1 ? '' : 's'}: open` : as === 'notes' ? 'Start notes here' : as === 'comments' ? 'Start a discussion here' : 'Pin something here'}
        onClick={(e) => {
          const anchor = e.currentTarget.closest<HTMLElement>('.bubble-row, .note-row')
          setPreview(showing || !anchor ? null : { id: info.id, as, anchor, button: e.currentTarget })
        }}
      >
        <Icon className="icon" />
        {count > 0 && count}
      </button>
    )
  }
  return (
    <span className={`margin-status ${info.assignees?.length ? '' : 'quiet'} ${yours ? 'yours' : ''}`} onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()}>
      {link(other)}
      {link('pins')}
      {others.map((a) => {
        const A = o?.bots.some((b) => b.id === a) ? Bot : o?.scripts.some((s) => s.id === a) ? SquareCode : User
        return (
          <span key={a} className={`assignee-avatar ${working === a ? 'working' : ''}`} title={`${o ? nameOf(o, a) : a}${working === a ? ', working on it' : ''}`}>
            <A className="icon" />
          </span>
        )
      })}
    </span>
  )
}
