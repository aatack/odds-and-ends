import React from 'react'
import {
  ArrowRight,
  CornerDownRight,
  DotsHorizontal,
  Edit03,
  Plus,
} from '@untitledui/icons'
import { BarButton, Button } from '../components/ui/Button'
import { useView } from '../state/hooks'
import { isEnabled, toolById } from '../tools/registry'
import { dispatch } from '../tools/dispatch'
import { useToolContext } from './useToolContext'

// The bar the thumb lives on. Two modes, because the phone has two: choosing, and
// typing.
//
// Every button here is a tool id — the bar is a set of shortcuts into the registry,
// exactly as a hotkey is on the desktop, and it greys itself out by asking the same
// `enabled` predicate the action sheet does. Nothing is implemented here.

/** Fixed rather than in flow: `interactive-widget=resizes-content` (see index.html)
 * shrinks the viewport when the keyboard opens, which puts this directly above it. */
const BAR = 'fixed inset-x-0 bottom-0 z-20 bg-white pb-[var(--inset-bottom)] shadow-lg'

export function BottomBar(): React.JSX.Element {
  const view = useView()
  return view.edit ? <EditBar /> : <SelectionBar />
}

function SelectionBar(): React.JSX.Element {
  const ctx = useToolContext()
  const can = (id: string): boolean => {
    const tool = toolById(id)
    return tool ? isEnabled(tool, ctx) : false
  }
  return (
    <nav className={BAR} aria-label="Actions">
      <div className="flex items-stretch gap-0.5 px-1.5 py-1">
        <BarButton
          icon={<ArrowRight size={19} />}
          caption="Open"
          disabled={!can('view.open')}
          onClick={() => dispatch('view.open')}
        />
        <BarButton
          icon={<Plus size={19} />}
          caption="Below"
          disabled={!can('create.sibling')}
          onClick={() => dispatch('create.sibling')}
        />
        <BarButton
          icon={<CornerDownRight size={19} />}
          caption="Child"
          disabled={!can('create.child')}
          onClick={() => dispatch('create.child')}
        />
        <BarButton
          icon={<Edit03 size={19} />}
          caption="Edit"
          disabled={!can('edit.start')}
          onClick={() => dispatch('edit.start')}
        />
        <BarButton
          icon={<DotsHorizontal size={19} />}
          caption="More"
          onClick={() => dispatch('app.actions')}
        />
      </div>
    </nav>
  )
}

/**
 * While typing. `onPointerDown` is where the presses are caught, with the default
 * prevented: a tap on a button blurs the textarea first, the blur commits, and by
 * the time the click arrived there would be no edit left for it to act on. Keeping
 * focus also keeps the keyboard up, which is the whole point of "and another".
 */
function EditBar(): React.JSX.Element {
  const keepFocus = (e: React.PointerEvent): void => e.preventDefault()
  return (
    <nav className={BAR} aria-label="Editing">
      <div className="flex items-center gap-2 px-3 py-2">
        <Button tone="quiet" onPointerDown={keepFocus} onClick={() => dispatch('edit.cancel')}>
          Cancel
        </Button>
        <span className="flex-1" />
        <Button tone="plain" onPointerDown={keepFocus} onClick={() => dispatch('edit.commitAndNext')}>
          + Another
        </Button>
        <Button tone="primary" onPointerDown={keepFocus} onClick={() => dispatch('edit.commit')}>
          Done
        </Button>
      </div>
    </nav>
  )
}
