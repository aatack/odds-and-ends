import type { View } from '../../../core/types.ts'
import type { Edit, Pick, ShownView } from '../state.ts'

/**
 * What a view is given: the tree, with the selection and any edit laid over
 * it, and the gestures it forwards. Nothing is decided here.
 */
export interface ViewProps {
  view: View
  shown: ShownView
  /** False in a peek: it shows, but takes no cursor and no typing. */
  interactive: boolean
  edit: Edit | null
  picking: Pick | null
  /** What is under way on the view's root: `older`, `hide`, or an action's id. */
  working?: string[]
  /** The time, ticking each second, for anything that says how long ago. */
  now?: number
  /** The action waiting on the prompt, if any, and the prompt's text. */
  acting: string | null
  draft: string
  composing: boolean
  onSelect(path: string[]): void
  onOpen(id: string): void
  onImage(ref: string | null): void
  onAction?(action: string): void
  /** Loads further back than what is cached. */
  onOlder?(): void
  /** Hides the chat the selected (or root) message is in. */
  onHideChat?(): void
  onDraft(text: string): void
  onCompose(composing: boolean): void
  onEditDraft(text: string): void
  onCommitEdit(): void
  onCancelEdit(): void
}
