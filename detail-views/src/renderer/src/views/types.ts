import type { Focus } from '../../../core/types.ts'

/** What every focus view is given. Gestures out, nothing decided here. */
export interface FocusProps {
  focus: Focus
  cursor: number
  draft: string
  composing: boolean
  /** The action waiting on the prompt, if any. */
  acting?: string | null
  onAction?(action: string): void
  /** False where something else already names the item, as a peek's bar does. */
  headed?: boolean
  onSelect(id: string): void
  onOpen(id: string): void
  onDraft(text: string): void
  onCompose(composing: boolean): void
  onImage(ref: string | null): void
}
