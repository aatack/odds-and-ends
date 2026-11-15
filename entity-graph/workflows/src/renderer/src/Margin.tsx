import { Bot, MessageSquare, NotebookText, SquareCode, User } from 'lucide-react'
import { useContext } from 'react'
import { useAtom } from 'jotai'
import type { ItemInfo } from '../../shared/types'
import { nameOf } from './people'
import { OverviewContext } from './status'
import { previewAtom } from './views'

/**
 * In the margin to the right of a topic's row in a chat or a tree (outside the column, on a desktop): that it has notes
 * (in a chat) or comments (in a tree), as an icon that opens them beside its row (pressed again, shuts them); and who
 * else has it, as their pictures (lit while a bot works on it). So a tree's sections can be handed to bots and watched
 * from the tree. With none yet, the icon shows only while its row is hovered (or its thread is open), to start them there.
 */
export function Margin({ info, other }: { info: ItemInfo; other: 'comments' | 'notes' }) {
  const o = useContext(OverviewContext)
  const [preview, setPreview] = useAtom(previewAtom)
  if (info.type !== 'topic') return null
  const count = (other === 'notes' ? info.notes : info.comments) ?? 0
  const others = (info.assignees ?? []).filter((a) => a !== o?.me.id)
  const working = o?.items.find((i) => i.id === info.id)?.bot ?? null
  const Icon = other === 'notes' ? NotebookText : MessageSquare
  const showing = preview?.id === info.id && preview.as === other
  return (
    <span className="margin-status" onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()}>
      <button
        data-ui
        className={`margin-link ${showing ? 'chosen' : ''} ${count ? '' : 'none'}`}
        title={
          count
            ? `${count} ${other === 'notes' ? (count === 1 ? 'note' : 'notes') : count === 1 ? 'comment' : 'comments'}: preview`
            : other === 'notes'
              ? 'Start notes here'
              : 'Start a discussion here'
        }
        onClick={(e) => {
          const anchor = e.currentTarget.closest<HTMLElement>('.bubble-row, .note-row')
          setPreview(showing || !anchor ? null : { id: info.id, as: other, anchor, button: e.currentTarget })
        }}
      >
        <Icon className="icon" />
        {count > 0 && count}
      </button>
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
