import type { View } from '../../../core/types.ts'
import type { ChatMode, Edit, Pick, ShownView } from '../state.ts'
import type { WindowsProps } from './App.tsx'

/**
 * What a view is given: the tree, with the selection and any edit laid over
 * it, and the gestures it forwards. Nothing is decided here.
 */
export interface ViewProps {
  view: View
  shown: ShownView
  /** False in a peek or the side panel: the main view's own controls (find, its composer, Slack's buttons) aren't there. */
  interactive: boolean
  /** Whether the keys act in this view's window; its selection shows faintly while they don't. */
  focused: boolean
  /**
   * The view's prompt box, where it has one and isn't `interactive` (a chat
   * in the side panel or a peek): its draft, whether it has the keyboard, and
   * its gestures.
   */
  prompt?: { draft: string; composing: boolean; onDraft(text: string): void; onCompose(composing: boolean): void }
  /**
   * Closes the window the view is in, however that window closes: the main
   * view pops the stack, a peek closes, the side panel closes.
   */
  onClose?(): void
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
  /** Pauses Slack's watch, or resumes it. */
  onPause?(): void
  /** Opens the form for a new Claude session. */
  onNewChat?(): void
  /** The items a chat's context pane has opened out, each walked as a view of its own, by id. */
  contextViews?: Record<string, View>
  /** Moves a Claude session to another directory, or a new worktree of its repo. */
  onSetDirectory?(cwd: string, worktree: boolean): void
  /** A topic's path picker: what is typed in its box, typing, and adding a path (`cwd`, or a new worktree of it). */
  pathDraft?: string
  onPathDraft?(text: string): void
  onAddPath?(cwd: string, worktree: boolean): void
  /** Resolves the topic on screen: out of the list, its worktree removed. */
  onResolveTopic?(): void
  /** What a chat's box writes: a prompt or a note (←/→ switch it). */
  chatMode?: ChatMode
  /** The windows the keys can be in (each opened-out item is one), for a page that has some of its own. */
  windows?: WindowsProps
  /** Opens an item in a chat's context pane out to its whole view, or folds it back. */
  onContextToggle?(id: string): void
  /** The width of the panel on the right (a chat's, or the side panel), once dragged; and dragging it. */
  sideWidth?: number | null
  onSideResize?(width: number): void
  onDraft(text: string): void
  onCompose(composing: boolean): void
  /** The view's find: null while it has none, the text while the field is open. */
  find: string | null
  /** A request (a nonce, 0 for none) to bring the selected row to the middle once the layout settles. */
  centre?: number
  /** A request (a nonce, 0 for none) to put the keyboard in the find field. */
  findFocus: number
  onFind(text: string): void
  /** The find field took the keyboard. */
  onFindFocused(): void
  /** Ticks or unticks a row's box, by a click on it. */
  onToggle?(path: string[]): void
  /** The view is scrolled near its end, or doesn't fill the screen: walk further. */
  onNearEnd?(): void
  onEditDraft(text: string): void
  onCommitEdit(): void
  onCancelEdit(): void
}
