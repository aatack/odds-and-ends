import { entities } from '../../../core/cache'
import { subtreeMarkdown } from '../../../core/markdown'
import { lastPath } from '../../../core/query'
import * as A from '../state/actions'
import { entityRows, frameQuery, type EntityRow, type Row } from '../state/derive'
import { findField, requestFocus } from '../state/focusRequest'
import { loadMore, rowsOf } from '../state/query'
import * as R from '../state/reducers'
import { focusOf, getLayout, updateLayout } from '../state/store'
import { uiAtom } from '../state/ui'
import {
  directionOf,
  frameDepth,
  last,
  nudgeDepth,
  type FrameState,
  type LayoutState,
  type TabState,
} from '../state/types'
import { runCode, stopCode } from '../helpers/codeRunner'
import { createEntity, link, writeValue } from '../source/entity'
import type { ToolSpec } from './types'

// Tools scoped to the focused frame: moving the selection, folding, the in-place
// editor, and the frame stack. They take no arguments — they act on whatever the
// frame's own state says is selected — which is exactly why they can be bound to
// bare keys.

/** Everything a frame tool needs, resolved from the current focus. */
interface Target {
  layout: LayoutState
  frame: FrameState
  tab: TabState
  rows: Row[]
  /**
   * The rows with the input box for a new child taken out. Computed on first
   * access and not before: this is built for every frame tool and for the
   * `enabled` of several, on every keystroke, and only two tools want the list
   * flattened — filtering it eagerly meant an array the length of the frame per
   * press, for nothing.
   */
  entities: EntityRow[]
  /** The resolved selection, not the latent one. */
  selectedPath: string[]
  /** Its index in {@link rows}; -1 when the selection isn't among them. */
  selectedIndex: number
  selected: EntityRow | null
}

function target(): Target | null {
  const layout = getLayout()
  const { frameId } = focusOf(layout)
  const frame = frameId ? layout.frames[frameId] : null
  const tab = frame ? layout.tabs[frame.tabId] : null
  if (!frame || !tab) return null
  const { rows, selectedPath, selectedIndex } = rowsOf(frame.id, layout)
  // Handed over by the derivation, which found it by looking its key up rather
  // than by searching the rows for the one that says it is selected.
  const at = rows[selectedIndex]
  let flattened: EntityRow[] | null = null
  return {
    layout,
    frame,
    tab,
    rows,
    get entities(): EntityRow[] {
      return (flattened ??= entityRows(rows))
    },
    selectedPath,
    selectedIndex,
    selected: at?.kind === 'entity' ? at : null,
  }
}

const selectedId = (t: Target): string | undefined => last(t.selectedPath)

/** Step the selection through the visible entity rows. */
function moveSelection(delta: number): void {
  const t = target()
  if (!t || t.rows.length === 0) return
  if (t.selectedIndex < 0) {
    const first = t.rows.find((r) => r.kind === 'entity')
    if (first?.kind === 'entity') A.selectPath(t.frame.id, first.path)
    return
  }
  // By index, over the rows as they stand. The only row that isn't an entity is
  // the box for a child being created, so stepping past it is one step at most.
  let next = t.selectedIndex + delta
  while (t.rows[next] && t.rows[next].kind !== 'entity') next += delta
  const row = t.rows[next]
  if (row?.kind === 'entity') A.selectPath(t.frame.id, row.path)
}

/**
 * How many times the frame's ceiling may be doubled to reach the row jumped to.
 * The walk is bounded by a page until the view scrolls, and the whole point of
 * jumping to the end is not having scrolled — so the ceiling is raised until the
 * row is among the rows. Doubling means a handful of walks rather than one per
 * page; the cap is a backstop against a store nobody could reach the bottom of
 * anyway, and stopping short only means the selection is somewhere the frame has
 * yet to unroll, which is what it would have been regardless.
 */
const UNROLLS = 12

/**
 * The end of the frame's tree: the last child, then *its* last child, and on
 * until there are none — the bottom of a list, without having scrolled through
 * it. Deliberately the end of the *walk* rather than the last row on screen,
 * which is only as far as the frame has unrolled.
 */
function jumpToEnd(): void {
  const t = target()
  if (!t) return
  const collapsed = t.layout.tabs[t.frame.tabId]?.collapsed ?? []
  const { traversal, filters } = frameQuery(t.frame, collapsed)
  A.selectPath(t.frame.id, lastPath([t.frame.rootId], entities().get, traversal, filters))
  for (let i = 0; i < UNROLLS; i++) {
    const rows = rowsOf(t.frame.id)
    if (rows.selectedIndex >= 0 || rows.complete) return
    loadMore(t.frame.id)
  }
}

function fold(collapsed: boolean): void {
  const t = target()
  const id = t && selectedId(t)
  if (!t || !id) return
  A.setCollapsed(t.tab.id, id, collapsed)
}

const hasEdit = (): boolean => {
  const layout = getLayout()
  const { frameId } = focusOf(layout)
  return !!(frameId && layout.frames[frameId]?.edit)
}

export const FRAME_TOOLS: ToolSpec[] = [
  {
    id: 'select.up',
    label: 'Move selection up',
    aliases: ['previous', 'prev'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'w' }, { key: 'ArrowUp' }],
    run: () => moveSelection(-1),
  },
  {
    id: 'select.down',
    label: 'Move selection down',
    aliases: ['next'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 's' }, { key: 'ArrowDown' }],
    run: () => moveSelection(1),
  },
  {
    id: 'select.parent',
    label: 'Select parent',
    aliases: ['up', 'ancestor'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'a' }],
    run: () => {
      const t = target()
      if (t && t.selectedPath.length > 1) A.selectPath(t.frame.id, t.selectedPath.slice(0, -1))
    },
  },
  {
    id: 'select.start',
    label: 'Jump to start',
    aliases: ['top', 'home', 'first', 'root'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'Home' }],
    // The frame's own root, which is what selecting the parent repeatedly arrives
    // at — there is nothing above it in this frame.
    run: () => {
      const t = target()
      if (t) A.selectPath(t.frame.id, [t.frame.rootId])
    },
  },
  {
    id: 'select.end',
    label: 'Jump to end',
    aliases: ['bottom', 'last', 'deepest'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'End' }],
    run: jumpToEnd,
  },
  {
    id: 'collapse',
    label: 'Collapse',
    aliases: ['close', 'fold', 'hide'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'ArrowLeft' }],
    run: () => fold(true),
  },
  {
    id: 'expand',
    label: 'Expand',
    aliases: ['open', 'unfold', 'show'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'ArrowRight' }],
    run: () => fold(false),
  },

  // --- The in-place editor -------------------------------------------------
  {
    id: 'edit.start',
    label: 'Edit text',
    aliases: ['rename', 'change', 'modify'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'e' }],
    run: () => {
      const t = target()
      if (t?.selected) A.startEdit(t.frame.id, t.selectedPath, t.selected.text ?? '')
    },
  },
  {
    id: 'edit.commit',
    label: 'Commit edit',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    listed: false,
    enabled: hasEdit,
    // Takes no arguments: the draft is part of the frame's persisted edit state,
    // so committing means "write what's in there".
    // Blur commits too, so this runs with nothing to do fairly often; those
    // cases report `mutated: false` to spare every frame a refetch.
    // Names the frame rather than resolving it from focus: a blur can land after
    // the click that moved focus elsewhere, and the write must still go to the
    // frame that was being edited. The key path fills this from the context.
    args: [{ name: 'frameId', label: 'Frame id', fromContext: 'frameId' }],
    run: async ({ frameId }) => {
      const frame = getLayout().frames[String(frameId)]
      const edit = frame?.edit
      if (!frame || !edit) return { mutated: false }
      const subject = last(edit.path)
      A.setEdit(frame.id, null)
      if (!subject) return { mutated: false }
      // An empty box means "never mind", whichever mode it is in: creating
      // writes no entity, and editing leaves the text as it was rather than
      // blanking it. Clearing text deliberately is what `entity.rename` is for.
      if (!edit.draft.trim()) return { mutated: false }
      if (edit.mode === 'edit') {
        await writeValue(subject, 'text', edit.draft)
        return {}
      }
      const values = { text: edit.draft, ...edit.values }
      // A row below another means "links to it" in a reversed frame, so the new
      // entity is linked the other way round — otherwise it would be created out
      // of sight of the frame that asked for it. Two calls rather than one batch,
      // but within the same instant, so undo still takes them together.
      let created: string
      if (directionOf(frame) === 'in') {
        created = await createEntity(values)
        await link(created, subject)
      } else {
        created = await createEntity(values, subject)
      }
      A.selectPath(frame.id, [...edit.path, created])
      return {}
    },
  },
  {
    id: 'edit.cancel',
    label: 'Cancel edit',
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'Escape' }],
    listed: false,
    enabled: hasEdit,
    run: () => {
      const t = target()
      if (t) A.setEdit(t.frame.id, null)
    },
  },
  {
    id: 'create.child',
    label: 'Create child',
    aliases: ['add', 'new', 'insert'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'Enter' }],
    run: () => {
      const t = target()
      if (t?.selected) A.startCreate(t.frame.id, t.selectedPath)
    },
  },
  {
    id: 'create.section',
    label: 'Create section',
    aliases: ['heading', 'header', 'title'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: '/' }],
    run: () => {
      const t = target()
      if (t?.selected) A.startCreate(t.frame.id, t.selectedPath, { section: true })
    },
  },
  {
    id: 'create.checkbox',
    label: 'Create checkbox',
    aliases: ['todo', 'task', 'checkbox', 'open'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: '?', shift: true }],
    run: () => {
      const t = target()
      if (t?.selected) A.startCreate(t.frame.id, t.selectedPath, { open: true })
    },
  },
  {
    id: 'create.code',
    label: 'Create code block',
    aliases: ['script', 'run', 'quickjs', 'ts', 'typescript'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'c' }],
    run: () => {
      const t = target()
      if (t?.selected) A.startCreate(t.frame.id, t.selectedPath, { type: 'code' })
    },
  },

  // --- Toggles against the selection --------------------------------------
  {
    id: 'toggle.section',
    label: 'Toggle section',
    aliases: ['heading', 'header'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    keys: [{ key: '/', mod: true }],
    run: async () => {
      const t = target()
      if (!t?.selected) return
      await writeValue(t.selected.id, 'section', t.selected.section ? null : true)
    },
  },
  {
    id: 'toggle.checkbox',
    label: 'Toggle checkbox',
    aliases: ['todo', 'task', 'done', 'check'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    keys: [{ key: '>', shift: true }],
    run: async () => {
      const t = target()
      if (!t?.selected) return
      // null → open box → ticked → null
      const next = t.selected.open === undefined ? true : t.selected.open === true ? false : null
      await writeValue(t.selected.id, 'open', next)
    },
  },

  // --- Code ---------------------------------------------------------------
  {
    id: 'code.run',
    label: 'Run code',
    aliases: ['execute', 'eval', 'play'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'Enter', mod: true }],
    // The script is handed the context this call was made in, which is folded
    // along the path to the selection — the code entity itself.
    run: (_args, call) => {
      const t = target()
      if (t?.selected?.type === 'code' && t.selected.text)
        runCode(t.selected.id, t.selected.text, call.context)
    },
  },
  {
    id: 'code.stop',
    label: 'Stop code',
    aliases: ['interrupt', 'halt'],
    scope: 'frame',
    reach: 'ui',
    listed: false,
    run: () => stopCode(),
  },

  // --- Export -------------------------------------------------------------
  {
    id: 'export.subtree',
    label: 'Export subtree as markdown',
    aliases: ['download', 'save', 'copy', 'md'],
    scope: 'frame',
    reach: 'ui',
    run: async () => {
      const t = target()
      if (!t) return
      const at = t.entities.findIndex((r) => r.selected)
      if (at < 0) return
      const markdown = subtreeMarkdown(t.entities, at)
      await navigator.clipboard.writeText(markdown)
      const count = markdown ? markdown.split('\n').length : 0
      return { message: `Copied ${count} item${count === 1 ? '' : 's'} to the clipboard` }
    },
  },

  // --- The frame stack ----------------------------------------------------
  {
    id: 'frame.pop',
    label: 'Pop frame',
    aliases: ['back', 'close view'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'a', shift: true }],
    run: () => {
      const { tabId } = focusOf(getLayout())
      if (tabId) updateLayout((s) => R.popFrame(s, tabId))
    },
  },
  {
    id: 'frame.undoPop',
    label: 'Undo pop frame',
    aliases: ['forward', 'reopen'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'd', shift: true }],
    run: () => {
      const { tabId } = focusOf(getLayout())
      if (tabId) updateLayout((s) => R.undoPop(s, tabId))
    },
  },
  {
    id: 'frame.popIntoTab',
    label: 'Pop frame into new tab',
    aliases: ['split', 'detach'],
    hint: 'Layout',
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'd', mod: true }],
    run: () => {
      const { groupId, tabId } = focusOf(getLayout())
      if (groupId && tabId) updateLayout((s) => R.popIntoNewTab(s, groupId, tabId))
    },
  },

  // --- Find and depth -----------------------------------------------------
  {
    // `d`, `q` and ⌘F in one press, because that is what looking for something in
    // a deep tree is every single time: open the thing you are standing on, cut it
    // down to its headings, and start typing. Tab, type, Enter, Tab, type, Enter
    // is then a search that descends — Enter hands the keyboard back, which is
    // what leaves Tab free to mean this again.
    id: 'frame.searchInside',
    label: 'Search inside',
    aliases: ['drill in', 'find within', 'outline search', 'descend'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'Tab' }],
    // The one bare key worth checking before taking: Tab is how a keyboard moves
    // between controls, and the app's other letters are only ever letters. So it
    // is the tree's while the tree is what is on screen, and everywhere else it
    // is left to do what it has always done.
    enabled: () => {
      const ui = uiAtom.get()
      return ui.page === 'editor' && !ui.inspectEntityId && !ui.debugSource && !ui.activityOpen
    },
    run: () => {
      const t = target()
      if (!t) return
      const id = last(t.selectedPath)
      // The same guard `entity.open` has: a frame rooted at what is selected is
      // the frame you are already in, so there is nowhere to go and the rest of
      // this searches where you stand.
      if (id && id !== t.frame.rootId) A.pushFrame(t.tab.id, id)
      const { frameId } = focusOf(getLayout())
      if (!frameId) return
      // Set rather than toggled. `q` is the toggle; this means "show me the
      // headings", which pressing again should not undo.
      A.setSectionsOnly(frameId, true)
      if (getLayout().frames[frameId]?.find == null) A.setFind(frameId, '')
      requestFocus(findField(frameId))
    },
  },
  {
    // Takes no text: it opens the frame's own field, which edits `find` directly
    // from then on. Asking for the text up front would mean typing the filter
    // somewhere other than where it ends up, with no way to refine it after.
    id: 'frame.find',
    label: 'Find in frame',
    aliases: ['search', 'filter'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'f', mod: true }],
    run: () => {
      const layout = getLayout()
      const { frameId } = focusOf(layout)
      if (!frameId) return
      // Empty string, not null: that is what puts the field on screen. Run
      // against a field already open it leaves the text alone and only asks for
      // the keyboard, so the key means "find" whatever state the frame is in.
      if (layout.frames[frameId]?.find == null) A.setFind(frameId, '')
      requestFocus(findField(frameId))
    },
  },
  {
    id: 'frame.find.clear',
    label: 'Clear find',
    aliases: ['unfilter', 'show all'],
    scope: 'frame',
    reach: 'ui',
    // Escape's last resort in a frame: an in-place edit is declared earlier in
    // this list and so cancels first, and a pending call never gets this far —
    // the router settles that before it consults any tool.
    keys: [{ key: 'Escape' }],
    listed: false,
    enabled: () => {
      const layout = getLayout()
      const { frameId } = focusOf(layout)
      return !!(frameId && layout.frames[frameId]?.find != null)
    },
    run: () => {
      const { frameId } = focusOf(getLayout())
      if (frameId) A.setFind(frameId, null)
    },
  },
  {
    id: 'frame.sections',
    label: 'Show sections only',
    aliases: ['outline', 'headings', 'contents', 'toc', 'filter'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'q' }],
    // A toggle rather than a one-way switch, so the key that turns the outline on
    // turns it off again; the pill's own button runs `frame.sections.clear`.
    run: () => {
      const layout = getLayout()
      const { frameId } = focusOf(layout)
      if (frameId) A.setSectionsOnly(frameId, !layout.frames[frameId]?.sectionsOnly)
    },
  },
  {
    id: 'frame.sections.clear',
    label: 'Show everything',
    aliases: ['unfilter', 'show all', 'all rows'],
    scope: 'frame',
    reach: 'ui',
    // After the find field in this list, so Escape gives up the text box first
    // and only then the outline.
    keys: [{ key: 'Escape' }],
    listed: false,
    enabled: () => {
      const layout = getLayout()
      const { frameId } = focusOf(layout)
      return !!(frameId && layout.frames[frameId]?.sectionsOnly)
    },
    run: () => {
      const { frameId } = focusOf(getLayout())
      if (frameId) A.setSectionsOnly(frameId, false)
    },
  },
  {
    id: 'frame.open',
    label: 'Show open items only',
    aliases: ['tasks', 'todo', 'unfinished', 'checkboxes', 'filter'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'q', shift: true }],
    // Beside the outline on ⇧Q, and a toggle for the same reason: what is left to
    // do is the other way of reading a page of notes.
    run: () => {
      const layout = getLayout()
      const { frameId } = focusOf(layout)
      if (frameId) A.setOpenOnly(frameId, !layout.frames[frameId]?.openOnly)
    },
  },
  {
    id: 'frame.open.clear',
    label: 'Show finished items too',
    aliases: ['unfilter', 'show all', 'all rows'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'Escape' }],
    listed: false,
    enabled: () => {
      const layout = getLayout()
      const { frameId } = focusOf(layout)
      return !!(frameId && layout.frames[frameId]?.openOnly)
    },
    run: () => {
      const { frameId } = focusOf(getLayout())
      if (frameId) A.setOpenOnly(frameId, false)
    },
  },
  {
    // A property of the frame, not a different kind of view: the same tree, read
    // the other way round. `f` toggles, so the key that turns it on turns it off;
    // the pill it raises does the same with the mouse.
    id: 'frame.reverse',
    label: 'Reverse the query direction',
    aliases: ['inbound', 'backlinks', 'references', 'parents', 'upside down', 'flip'],
    hint: 'Frame',
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'f' }],
    run: () => {
      const layout = getLayout()
      const { frameId } = focusOf(layout)
      if (!frameId) return
      const reversed = directionOf(layout.frames[frameId]) === 'in'
      A.setDirection(frameId, reversed ? 'out' : 'in')
      return { message: reversed ? 'Following outbound links' : 'Following inbound links' }
    },
  },
  {
    id: 'frame.maxDepth',
    label: 'Limit depth below entity',
    aliases: ['depth', 'collapse below', 'levels'],
    hint: 'Frame',
    scope: 'frame',
    reach: 'ui',
    args: [
      { name: 'entityId', label: 'Entity id', kind: 'entity', fromContext: 'entityId' },
      {
        name: 'depth',
        label: 'Maximum depth',
        kind: 'number',
        optional: true,
        placeholder: 'Blank for no limit',
      },
    ],
    run: ({ entityId, depth }) => {
      const { frameId } = focusOf(getLayout())
      if (frameId) A.setMaxDepth(frameId, String(entityId), depth == null ? null : Number(depth))
    },
  },
  {
    // The frame's own limit, on the arrow keys: ⇧← takes a level off and ⇧→ adds
    // one, so the outline is read at the depth it makes sense at rather than at
    // the one a filter chose. Both go through `nudgeDepth`, which is where "no
    // limit" sits on that scale — the bottom of it, so ⇧← held down ends at the
    // whole tree.
    id: 'frame.depth.less',
    label: 'Show one level less',
    aliases: ['shallower', 'depth', 'levels'],
    hint: 'Frame',
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'ArrowLeft', shift: true }],
    run: () => nudge(-1),
  },
  {
    id: 'frame.depth.more',
    label: 'Show one level more',
    aliases: ['deeper', 'depth', 'levels'],
    hint: 'Frame',
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'ArrowRight', shift: true }],
    run: () => nudge(1),
  },
  {
    // No Escape: that key gives up a filter, and a depth is not one — it is how
    // much of the tree you are reading, which ⇧← walks off in its own time.
    id: 'frame.depth.clear',
    label: 'Show every level',
    aliases: ['no depth limit', 'depth', 'levels'],
    hint: 'Frame',
    scope: 'frame',
    reach: 'ui',
    enabled: () => frameDepth(frameOf()) != null,
    run: () => {
      const { frameId } = focusOf(getLayout())
      if (frameId) A.setFrameDepth(frameId, null)
    },
  },
]

/** The focused frame, for the tools that only want to read one value off it. */
const frameOf = (): FrameState | null => {
  const layout = getLayout()
  const { frameId } = focusOf(layout)
  return frameId ? (layout.frames[frameId] ?? null) : null
}

// No message back: a depth is read off the pill in the corner, and these are
// keys meant to be leant on — a toast per press would be a stack of them.
function nudge(by: 1 | -1): void {
  const frame = frameOf()
  if (!frame) return
  A.setFrameDepth(frame.id, nudgeDepth(frameDepth(frame), by))
}
