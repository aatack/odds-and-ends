import * as A from '../state/actions'
import { entityRows, frameRows, type EntityRow, type Row } from '../state/derive'
import { findField, requestFocus } from '../state/focusRequest'
import { queryAtom } from '../state/query'
import * as R from '../state/reducers'
import { focusOf, getLayout, updateLayout } from '../state/store'
import { directionOf, last, type FrameState, type LayoutState, type TabState } from '../state/types'
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
  entities: EntityRow[]
  /** The resolved selection, not the latent one. */
  selectedPath: string[]
  selected: EntityRow | null
}

function target(): Target | null {
  const layout = getLayout()
  const { frameId } = focusOf(layout)
  const frame = frameId ? layout.frames[frameId] : null
  const tab = frame ? layout.tabs[frame.tabId] : null
  if (!frame || !tab) return null
  const { rows, selectedPath } = frameRows(layout, queryAtom.get(), frame.id)
  const entities = entityRows(rows)
  return {
    layout,
    frame,
    tab,
    rows,
    entities,
    selectedPath,
    selected: entities.find((r) => r.selected) ?? null,
  }
}

const selectedId = (t: Target): string | undefined => last(t.selectedPath)

/** Step the selection through the visible entity rows. */
function moveSelection(delta: number): void {
  const t = target()
  if (!t || t.entities.length === 0) return
  const at = t.entities.findIndex((r) => r.selected)
  if (at < 0) {
    A.selectPath(t.frame.id, t.entities[0].path)
    return
  }
  const next = at + delta
  if (next >= 0 && next < t.entities.length) A.selectPath(t.frame.id, t.entities[next].path)
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

/** Markdown has six heading levels; a deeper section takes the last of them. */
const HEADING_LIMIT = 6

// Format the entity at `start` and its visible descendants as markdown. Children
// of folded rows aren't in `rows` to begin with, and a folded row is left out
// along with them: exporting the title of a branch whose contents are hidden
// reads as a complete list when it isn't. Folding is how you say "not this"; the
// export takes it at its word. The row being exported is the exception — it was
// named explicitly, so it comes out folded or not.
//
// Three things the tree says that markdown has words for: a section becomes a
// heading, one `#` per level it sits at below the row being exported; a checkbox
// becomes a task box; everything else a bullet. The exported row itself is the
// title of what you asked for rather than an item in it, so it gets no marker at
// all — you are pasting it somewhere that will give it one.
//
// Indentation is measured from the nearest heading rather than from the top,
// since a heading ends whatever list preceded it: continuing to count from the
// root would push a deep bullet past four spaces, which markdown reads as code.
function subtreeMarkdown(rows: Row[], start: number): string {
  const from = rows[start]
  if (!from || from.kind !== 'entity') return ''
  const lines: string[] = []
  /** Depths of the headings we are inside, outermost first. */
  const headings: number[] = []
  for (let i = start; i < rows.length; i++) {
    const row = rows[i]
    if (i > start && row.depth <= from.depth) break
    if (row.kind !== 'entity') continue
    if (i > start && row.collapsed && row.hasChildren) continue
    const depth = row.depth - from.depth
    const text = row.text ?? ''
    while (headings.length && row.depth <= headings[headings.length - 1]) headings.pop()
    if (row.section) {
      lines.push(`${'#'.repeat(Math.min(depth + 1, HEADING_LIMIT))} ${text}`)
      headings.push(row.depth)
      continue
    }
    if (i === start) {
      lines.push(text)
      continue
    }
    const base = headings.length ? headings[headings.length - 1] : from.depth
    const box = row.open === true ? '[ ] ' : row.open === false ? '[x] ' : ''
    lines.push(`${'  '.repeat(row.depth - base - 1)}- ${box}${text}`)
  }
  return lines.join('\n')
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
    run: () => {
      const t = target()
      if (t?.selected?.type === 'code' && t.selected.text) runCode(t.selected.id, t.selected.text)
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
      const at = t.rows.findIndex((r) => r.kind === 'entity' && r.selected)
      if (at < 0) return
      const markdown = subtreeMarkdown(t.rows, at)
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
]
