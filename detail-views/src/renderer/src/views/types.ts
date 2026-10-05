import type { Focus } from '../../../core/types.ts'

/** What every focus view is given. Gestures out, nothing decided here. */
export interface FocusProps {
  focus: Focus
  cursor: number
  draft: string
  composing: boolean
  onSelect(id: string): void
  onOpen(id: string): void
  onDraft(text: string): void
  onCompose(composing: boolean): void
}
