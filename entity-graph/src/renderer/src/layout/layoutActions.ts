// The layout command registry: every action against the tab/frame structure,
// defined once so the top-level hotkeys and the command palette dispatch through
// the same list. Mirrors appActions.ts and editorActions.ts. Each `run` receives
// a LayoutController — the imperative handle useLayout provides — so the
// definitions here hold no state of their own.

import type { KeyBinding } from '../actions/keys'

/** The imperative operations the layout exposes for actions to call. */
export interface LayoutController {
  /** Toggle "focus mode": show only the focused group, full-width. */
  toggleSolo: () => void
  /** Move focus to the previous (-1) or next (+1) tab group. */
  focusAdjacentGroup: (dir: -1 | 1) => void
  /** Cycle the focused group's active tab: previous (-1) / next (+1), wrapping. */
  cycleTab: (dir: -1 | 1) => void
  /** Move the focused tab into the group to its left (-1) / right (+1). */
  moveTab: (dir: -1 | 1) => void
  /** Push a new entity frame for the focused view's selected entity. */
  focusSelectedEntity: () => void
  /** Pop the top frame off the focused tab (onto its undo history). */
  popFrame: () => void
  /** Restore the most recently popped frame. */
  undoPop: () => void
  /** Pop the top frame into a fresh tab beside the current one. */
  popIntoNewTab: () => void
  /** Add a canvas frame seeded from the focused view. */
  openCanvas: () => void
  /** Canvas: add the active panel's selected entity as its own node. */
  addSelectedToCanvas: () => void
  /** Canvas: close (remove) the active panel. */
  closePanel: () => void
  newTab: () => void
  closeTab: () => void
}

export interface LayoutAction {
  id: string
  label: string
  aliases?: string[]
  keys?: KeyBinding[]
  /** Faint category label shown in the palette when the action has no hotkey. */
  hint?: string
  palette?: boolean
  run: (c: LayoutController) => void
}

export const LAYOUT_ACTIONS: LayoutAction[] = [
  {
    id: 'focus-solo',
    label: 'Focus tab group',
    aliases: ['maximize', 'solo', 'fullscreen', 'zoom'],
    keys: [{ key: 'm' }],
    run: (c) => c.toggleSolo(),
  },
  {
    id: 'focus-prev-group',
    label: 'Focus previous tab group',
    aliases: ['left group'],
    keys: [{ key: 'ArrowLeft', alt: true }],
    run: (c) => c.focusAdjacentGroup(-1),
  },
  {
    id: 'focus-next-group',
    label: 'Focus next tab group',
    aliases: ['right group'],
    keys: [{ key: 'ArrowRight', alt: true }],
    run: (c) => c.focusAdjacentGroup(1),
  },
  {
    id: 'move-tab-left',
    label: 'Move tab left',
    keys: [{ key: 'ArrowLeft', alt: true, mod: true }],
    run: (c) => c.moveTab(-1),
  },
  {
    id: 'move-tab-right',
    label: 'Move tab right',
    keys: [{ key: 'ArrowRight', alt: true, mod: true }],
    run: (c) => c.moveTab(1),
  },
  {
    id: 'next-tab',
    label: 'Next tab',
    aliases: ['cycle tab'],
    keys: [{ key: 'Tab', mod: true }],
    run: (c) => c.cycleTab(1),
  },
  {
    id: 'prev-tab',
    label: 'Previous tab',
    aliases: ['cycle tab back'],
    keys: [{ key: 'Tab', mod: true, shift: true }],
    run: (c) => c.cycleTab(-1),
  },
  {
    id: 'focus-entity',
    label: 'Focus selected entity',
    aliases: ['open', 'drill in', 'push frame'],
    keys: [{ key: 'd' }],
    run: (c) => c.focusSelectedEntity(),
  },
  {
    id: 'pop-frame',
    label: 'Pop frame',
    aliases: ['back', 'close view'],
    keys: [{ key: 'a', shift: true }],
    run: (c) => c.popFrame(),
  },
  {
    id: 'undo-pop',
    label: 'Undo pop frame',
    aliases: ['forward', 'reopen'],
    keys: [{ key: 'd', shift: true }],
    run: (c) => c.undoPop(),
  },
  {
    id: 'pop-into-tab',
    label: 'Pop frame into new tab',
    aliases: ['split', 'detach'],
    hint: 'Layout',
    run: (c) => c.popIntoNewTab(),
  },
  {
    id: 'open-canvas',
    label: 'Open canvas',
    aliases: ['graph', 'board', 'spatial', 'map'],
    hint: 'Layout',
    run: (c) => c.openCanvas(),
  },
  {
    id: 'add-to-canvas',
    label: 'Add selected to canvas',
    aliases: ['pop out', 'new node', 'node'],
    hint: 'Canvas',
    run: (c) => c.addSelectedToCanvas(),
  },
  {
    id: 'close-panel',
    label: 'Close panel',
    aliases: ['remove node', 'remove panel'],
    hint: 'Canvas',
    run: (c) => c.closePanel(),
  },
  { id: 'new-tab', label: 'New tab', aliases: ['add tab'], hint: 'Layout', run: (c) => c.newTab() },
  {
    id: 'close-tab',
    label: 'Close tab',
    aliases: ['remove tab'],
    hint: 'Layout',
    run: (c) => c.closeTab(),
  },
]
