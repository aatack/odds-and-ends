// The editor's command registry: every action the user can trigger against the
// open entity tree, defined once and separately from the state it mutates. Both
// the keyboard handler and the command palette dispatch through this list, so a
// hotkey and its palette entry can never drift apart.
//
// Each action's `run` receives an EditorController — a small imperative handle
// the useEditor hook provides — so the definitions here hold no state of their
// own.

import type { KeyBinding } from './keys'

/** The imperative operations the editor exposes for actions to call. */
export interface EditorController {
  moveSelection: (delta: number) => void
  selectParent: () => void
  collapseSelected: () => void
  expandSelected: () => void
  startEdit: () => void
  startCreate: () => void
  /** Create a child entity marked `type: code`. */
  startCreateCode: () => void
  /** Run the selected code entity's source (no-op if it isn't code). */
  runSelectedCode: () => void
  /** Interrupt the selected code entity's run. */
  stopSelectedCode: () => void
  unlinkSelected: () => void
  toggleMove: () => void
  toggleLink: (reverse: boolean) => void
  cancelPending: () => void
  exportSelected: () => void
  debugSelected: () => void
}

export interface EditorAction {
  id: string
  label: string
  /** Extra terms the palette's fuzzy search matches, e.g. synonyms for the label. */
  aliases?: string[]
  /** Keys that trigger it. Omitted for actions only reachable via menu/palette. */
  keys?: KeyBinding[]
  /** Whether it appears in the command palette (default true). */
  palette?: boolean
  run: (c: EditorController) => void
}

export const EDITOR_ACTIONS: EditorAction[] = [
  { id: 'move-up', label: 'Move selection up', aliases: ['previous', 'prev'], keys: [{ key: 'w' }], run: (c) => c.moveSelection(-1) },
  { id: 'move-down', label: 'Move selection down', aliases: ['next'], keys: [{ key: 's' }], run: (c) => c.moveSelection(1) },
  { id: 'select-parent', label: 'Select parent', aliases: ['up', 'ancestor'], keys: [{ key: 'a' }], run: (c) => c.selectParent() },
  { id: 'collapse', label: 'Collapse', aliases: ['close', 'fold', 'hide'], keys: [{ key: 'ArrowLeft' }], run: (c) => c.collapseSelected() },
  { id: 'expand', label: 'Expand', aliases: ['open', 'unfold', 'show'], keys: [{ key: 'ArrowRight' }], run: (c) => c.expandSelected() },
  { id: 'edit', label: 'Edit text', aliases: ['rename', 'change', 'modify'], keys: [{ key: 'e' }], run: (c) => c.startEdit() },
  { id: 'create-child', label: 'Create child', aliases: ['add', 'new', 'insert'], keys: [{ key: 'Enter' }], run: (c) => c.startCreate() },
  { id: 'create-code', label: 'Create code block', aliases: ['script', 'run', 'quickjs', 'ts', 'typescript'], keys: [{ key: 'c' }], run: (c) => c.startCreateCode() },
  { id: 'run-code', label: 'Run code', aliases: ['execute', 'eval', 'play'], keys: [{ key: 'Enter', mod: true }], run: (c) => c.runSelectedCode() },
  { id: 'stop-code', label: 'Stop code', aliases: ['interrupt', 'halt', 'cancel'], palette: false, run: (c) => c.stopSelectedCode() },
  {
    id: 'unlink',
    label: 'Remove from parent',
    aliases: ['delete', 'detach', 'disconnect'],
    keys: [{ key: 'Backspace' }, { key: 'Delete' }],
    run: (c) => c.unlinkSelected(),
  },
  { id: 'move', label: 'Move to…', aliases: ['reparent', 'relocate'], keys: [{ key: 'x' }], run: (c) => c.toggleMove() },
  { id: 'link', label: 'Link to…', aliases: ['connect', 'relate', 'reference'], keys: [{ key: 'r', shift: false }], run: (c) => c.toggleLink(false) },
  {
    id: 'link-reverse',
    label: 'Link to… (reversed)',
    aliases: ['connect', 'relate', 'reference', 'backlink'],
    keys: [{ key: 'r', shift: true }],
    run: (c) => c.toggleLink(true),
  },
  {
    id: 'cancel',
    label: 'Cancel pending move/link',
    keys: [{ key: 'Escape' }],
    palette: false,
    run: (c) => c.cancelPending(),
  },
  { id: 'export', label: 'Export subtree as markdown', aliases: ['download', 'save', 'copy', 'md'], run: (c) => c.exportSelected() },
  { id: 'debug', label: 'Debug entity', aliases: ['inspect', 'info'], run: (c) => c.debugSelected() },
]
