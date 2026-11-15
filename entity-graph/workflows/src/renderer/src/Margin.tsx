import { Bot, MessageSquare, NotebookText, SquareCode, User } from 'lucide-react'
import { useContext } from 'react'
import { useSetAtom } from 'jotai'
import type { ItemInfo } from '../../shared/types'
import { nameOf } from './people'
import { OverviewContext } from './status'
import { previewAtom } from './views'

/**
 * Beside a topic in a chat or a tree: that it has notes (in a chat) or comments (in a tree), as an icon that opens
 * them in a preview; and who else has it, as their pictures (lit while a bot works on it). So a tree's sections can be
 * handed to bots and watched from the tree.
 */
export function Margin({ info, other }: { info: ItemInfo; other: 'comments' | 'notes' }) {
  const o = useContext(OverviewContext)
  const preview = useSetAtom(previewAtom)
  if (info.type !== 'topic') return null
  const count = (other === 'notes' ? info.notes : info.comments) ?? 0
  const others = (info.assignees ?? []).filter((a) => a !== o?.me.id)
  if (!count && !others.length) return null
  const working = o?.items.find((i) => i.id === info.id)?.bot ?? null
  const Icon = other === 'notes' ? NotebookText : MessageSquare
  return (
    <span className="margin-status" onClick={(e) => e.stopPropagation()} onMouseDown={(e) => e.stopPropagation()}>
      {count > 0 && (
        <button
          data-ui
          className="margin-link"
          title={`${count} ${other === 'notes' ? (count === 1 ? 'note' : 'notes') : count === 1 ? 'comment' : 'comments'}: preview`}
          onClick={() => preview({ id: info.id, as: other })}
        >
          <Icon className="icon" />
          {count}
        </button>
      )}
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
