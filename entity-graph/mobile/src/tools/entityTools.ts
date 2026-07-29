import { subtreeMarkdown } from '../../../src/core/markdown'
import { copyText } from '../helpers/clipboard'
import * as A from '../state/actions'
import { childOrder } from '../state/query'
import { getView } from '../state/store'
import { last } from '../state/types'
import { createEntity, link, nudge, reparent, unlink, writeValue } from '../source/entity'
import { asId, previousSibling, requireId, rowAbove, siblingsUnder } from './context'
import { commitEdit } from './edit'
import { needsForward, needsParent, needsRow, type ArgSpec, type ToolSpec } from './types'

// Everything that writes, plus the in-place editor that most writing goes through.
//
// The arguments are filled from the context, so one declaration serves every
// gesture: a button in the bottom bar (everything filled, runs at once), a
// long-press (the row under the finger), and the action sheet (type the ids
// yourself). Arguments that are *pointed at* rather than typed — the new parent of
// a move, the far end of a link — carry `pick`, and the outline stays on screen
// while one is chosen.

const entityArg = (name = 'entityId', label = 'Entity id', from = 'entityId'): ArgSpec => ({
  name,
  label,
  kind: 'entity',
  fromContext: from,
})

const pickArg = (name: string, label: string): ArgSpec => ({
  name,
  label,
  kind: 'entity',
  pick: true,
})

export const ENTITY_TOOLS: ToolSpec[] = [
  // --- The in-place editor -------------------------------------------------
  {
    id: 'edit.start',
    label: 'Edit text',
    aliases: ['rename', 'change', 'type', 'modify'],
    hint: 'Edit',
    enabled: needsRow,
    run: (_args, ctx) => {
      if (ctx.row) A.startEdit(ctx.path, ctx.row.text ?? '')
    },
  },
  {
    id: 'edit.commit',
    label: 'Done',
    hint: 'Edit',
    listed: false,
    mutates: true,
    enabled: () => getView().edit != null,
    // Takes no arguments: the draft is part of the persisted edit state, so
    // committing means "write what's in there". Blur commits too, so this runs with
    // nothing to do fairly often — hence the honest `mutated: false`, which spares
    // the outline a refetch.
    run: async () => {
      const done = await commitEdit()
      return { mutated: done?.mutated ?? false }
    },
  },
  {
    // The flow that makes an outliner usable with a thumb: commit this line and
    // open the next one, without the keyboard closing in between. The next box is
    // opened *before* the write finishes for exactly that reason — see
    // `setCreateAfter`.
    id: 'edit.commitAndNext',
    label: 'Done, and another',
    aliases: ['next line', 'new sibling', 'again'],
    hint: 'Edit',
    listed: false,
    mutates: true,
    enabled: () => getView().edit != null,
    run: async () => {
      const state = getView()
      const captured = state.edit
      if (!captured) return { mutated: false }

      // A create makes another child of the same parent; an edit makes a sibling of
      // the row that was being edited.
      const parentPath = captured.mode === 'create' ? captured.path : captured.path.slice(0, -1)
      const values = captured.mode === 'create' ? captured.values : {}
      const subject = last(captured.path)
      if (parentPath.length === 0) {
        // Editing the level's own root: there is no parent to add a sibling to, so
        // this degrades to an ordinary commit.
        const done = await commitEdit()
        return { mutated: done?.mutated ?? false }
      }
      A.startCreate(parentPath, values, captured.mode === 'edit' ? (subject ?? null) : null)

      const done = await commitEdit(captured)
      // Now the id exists, tell the box already on screen what it follows.
      if (done?.created) A.setCreateAfter(done.created)
      return { mutated: done?.mutated ?? false }
    },
  },
  {
    id: 'edit.cancel',
    label: 'Cancel',
    hint: 'Edit',
    listed: false,
    enabled: () => getView().edit != null,
    run: () => A.setEdit(null),
  },

  // --- Creating -------------------------------------------------------------
  {
    id: 'create.child',
    label: 'Add a child',
    aliases: ['new', 'add', 'insert', 'indent under'],
    hint: 'Create',
    enabled: needsRow,
    run: (_args, ctx) => A.startCreate(ctx.path, {}, null),
  },
  {
    id: 'create.sibling',
    label: 'Add a line below',
    aliases: ['new', 'add', 'insert', 'sibling', 'next'],
    hint: 'Create',
    enabled: needsParent,
    // Directly below the selected row rather than at the end of the parent's
    // children — see `forwardSteps` in source/entity for how that lands.
    run: (_args, ctx) => A.startCreate(ctx.path.slice(0, -1), {}, ctx.entityId),
  },
  {
    id: 'create.section',
    label: 'Add a section',
    aliases: ['heading', 'header', 'title'],
    hint: 'Create',
    enabled: needsRow,
    run: (_args, ctx) => A.startCreate(ctx.path, { section: true }, null),
  },
  {
    id: 'create.checkbox',
    label: 'Add a task',
    aliases: ['todo', 'checkbox', 'tick', 'open'],
    hint: 'Create',
    enabled: needsRow,
    run: (_args, ctx) => A.startCreate(ctx.path, { open: true }, null),
  },
  {
    // The typed version, for the action sheet: no editor, just the text.
    id: 'entity.create',
    label: 'Add a child with text…',
    aliases: ['new', 'insert'],
    hint: 'Create',
    mutates: true,
    args: [entityArg('parentId', 'Parent id'), { name: 'text', label: 'Text', kind: 'text' }],
    run: async ({ parentId, text }, ctx) => {
      const parent = requireId(parentId, 'Parent id')
      await createEntity({ text: String(text ?? '') }, parent, {
        siblings: siblingsUnder(ctx, parent),
        after: null,
      })
    },
  },

  // --- Toggles --------------------------------------------------------------
  {
    id: 'toggle.checkbox',
    label: 'Tick / untick',
    aliases: ['todo', 'task', 'done', 'check', 'complete'],
    hint: 'Edit',
    mutates: true,
    enabled: needsRow,
    run: async (_args, ctx) => {
      const row = ctx.row
      if (!row) return { mutated: false }
      // null → open box → ticked → null
      const next = row.open === undefined ? true : row.open === true ? false : null
      await writeValue(row.id, 'open', next)
      return {}
    },
  },
  {
    id: 'toggle.section',
    label: 'Make a section / plain',
    aliases: ['heading', 'header', 'title'],
    hint: 'Edit',
    mutates: true,
    enabled: needsRow,
    run: async (_args, ctx) => {
      if (!ctx.row) return { mutated: false }
      await writeValue(ctx.row.id, 'section', ctx.row.section ? null : true)
      return {}
    },
  },

  // --- Structure ------------------------------------------------------------
  {
    id: 'entity.indent',
    label: 'Indent',
    aliases: ['demote', 'nest', 'move under previous'],
    hint: 'Structure',
    mutates: true,
    enabled: (ctx) => needsForward(ctx) && ctx.parentId != null && previousSibling(ctx) != null,
    run: async (_args, ctx) => {
      const subject = requireId(ctx.entityId, 'Entity id')
      const from = requireId(ctx.parentId, 'Parent id')
      const under = previousSibling(ctx)
      if (!under) throw new Error('There is no line above to nest under')
      // At the end of its new parent's children, which is where an indent puts it.
      await reparent(subject, from, under, { siblings: childOrder(under), after: null })
      A.selectPath([...ctx.path.slice(0, -1), under, subject])
      return {}
    },
  },
  {
    id: 'entity.outdent',
    label: 'Outdent',
    aliases: ['promote', 'unnest', 'move out'],
    hint: 'Structure',
    mutates: true,
    // Needs a grandparent: the path is root … grandparent, parent, row.
    enabled: (ctx) => needsForward(ctx) && ctx.path.length > 2,
    run: async (_args, ctx) => {
      const subject = requireId(ctx.entityId, 'Entity id')
      const from = requireId(ctx.parentId, 'Parent id')
      const to = ctx.path[ctx.path.length - 3]
      // Directly after the parent it just came out of, which is where the eye is.
      await reparent(subject, from, to, { siblings: childOrder(to), after: from })
      A.selectPath([...ctx.path.slice(0, -2), subject])
      return {}
    },
  },
  {
    id: 'entity.moveUp',
    label: 'Move up',
    aliases: ['reorder', 'earlier', 'raise'],
    hint: 'Structure',
    mutates: true,
    enabled: (ctx) => needsForward(ctx) && previousSibling(ctx) != null,
    run: async (_args, ctx) => {
      await nudge(requireId(ctx.parentId, 'Parent id'), requireId(ctx.entityId, 'Entity id'), 'up')
      return {}
    },
  },
  {
    id: 'entity.moveDown',
    label: 'Move down',
    aliases: ['reorder', 'later', 'lower'],
    hint: 'Structure',
    mutates: true,
    enabled: (ctx) => {
      if (!needsForward(ctx) || !ctx.parentId || !ctx.entityId) return false
      const siblings = siblingsUnder(ctx, ctx.parentId)
      const at = siblings.indexOf(ctx.entityId)
      return at >= 0 && at < siblings.length - 1
    },
    run: async (_args, ctx) => {
      await nudge(requireId(ctx.parentId, 'Parent id'), requireId(ctx.entityId, 'Entity id'), 'down')
      return {}
    },
  },
  {
    id: 'entity.move',
    label: 'Move to…',
    aliases: ['reparent', 'relocate', 'drag'],
    hint: 'Structure',
    mutates: true,
    enabled: needsParent,
    args: [entityArg(), entityArg('fromParentId', 'From parent', 'parentId'), pickArg('toParentId', 'the new parent')],
    run: async ({ entityId, fromParentId, toParentId }, ctx) => {
      const subject = requireId(entityId, 'Entity id')
      const from = requireId(fromParentId, 'From parent')
      const to = requireId(toParentId, 'New parent')
      if (from === to) return { mutated: false, message: 'Already there' }
      if (to === subject) throw new Error('An entity cannot be its own parent')
      if (ctx.direction === 'in') {
        // Upside down, the link runs from the row to its parent, so moving it
        // re-points that link's far end rather than swapping its source.
        await unlink(subject, from)
        await link(subject, to)
        return {}
      }
      await reparent(subject, from, to, { siblings: childOrder(to), after: null })
      return { message: 'Moved' }
    },
  },
  {
    id: 'entity.link',
    label: 'Link to…',
    aliases: ['connect', 'relate', 'reference'],
    hint: 'Structure',
    mutates: true,
    enabled: needsRow,
    args: [entityArg('sourceId', 'Source id'), pickArg('destinationId', 'what to link to')],
    run: async ({ sourceId, destinationId }) => {
      const from = requireId(sourceId, 'Source id')
      const to = requireId(destinationId, 'Destination id')
      if (from === to) throw new Error('An entity cannot be linked to itself')
      await link(from, to)
      return { message: 'Linked' }
    },
  },
  {
    id: 'entity.unlink',
    label: 'Remove from parent',
    aliases: ['delete', 'detach', 'disconnect', 'unlink'],
    hint: 'Structure',
    mutates: true,
    // Nothing to unlink at the root of the level.
    enabled: needsParent,
    args: [entityArg('childId', 'Entity id'), entityArg('parentId', 'Parent id', 'parentId')],
    run: async ({ childId, parentId }, ctx) => {
      // The arguments name what's on screen — the row and the row it hangs off.
      // Which of the two is the link's source depends on the direction.
      const reversed = ctx.direction === 'in'
      // Resolved before the write, while the row is still there, so the selection
      // walks up the list rather than jumping out to the parent.
      const above = rowAbove(ctx)
      await unlink(asId(reversed ? childId : parentId), asId(reversed ? parentId : childId))
      if (above) A.selectPath(above)
      return { message: 'Removed' }
    },
  },

  // --- Values ---------------------------------------------------------------
  {
    id: 'entity.rename',
    label: 'Set the text…',
    aliases: ['rename', 'retitle', 'write'],
    hint: 'Edit',
    mutates: true,
    args: [entityArg(), { name: 'text', label: 'New text', kind: 'text' }],
    run: async ({ entityId, text }) => {
      await writeValue(requireId(entityId, 'Entity id'), 'text', String(text ?? ''))
      return {}
    },
  },
  {
    id: 'entity.value.set',
    label: 'Set a value…',
    aliases: ['write', 'attribute', 'property', 'key', 'type'],
    hint: 'Edit',
    mutates: true,
    args: [
      entityArg(),
      { name: 'key', label: 'Key', placeholder: 'e.g. type' },
      { name: 'value', label: 'Value (JSON)', kind: 'json', optional: true, placeholder: 'Blank to clear' },
    ],
    run: async ({ entityId, key, value }) => {
      await writeValue(requireId(entityId, 'Entity id'), String(key), value ?? null)
      return {}
    },
  },

  // --- Taking things away with you -----------------------------------------
  {
    id: 'entity.copy',
    label: 'Copy the text',
    aliases: ['clipboard', 'share'],
    hint: 'Export',
    enabled: needsRow,
    run: async (_args, ctx) => {
      const text = ctx.row?.text ?? ''
      if (!text) throw new Error('Nothing to copy')
      await copyText(text)
      return { message: 'Copied' }
    },
  },
  {
    id: 'entity.copyId',
    label: 'Copy the id',
    aliases: ['clipboard', 'uuid', 'reference'],
    hint: 'Export',
    enabled: needsRow,
    run: async (_args, ctx) => {
      await copyText(requireId(ctx.entityId, 'Entity id'))
      return { message: 'Copied the id' }
    },
  },
  {
    id: 'export.subtree',
    label: 'Copy as markdown',
    aliases: ['export', 'share', 'download', 'md', 'outline'],
    hint: 'Export',
    enabled: needsRow,
    run: async (_args, ctx) => {
      const markdown = subtreeMarkdown(ctx.rows, ctx.rows.findIndex((r) => r.selected))
      if (!markdown) throw new Error('Nothing to copy')
      await copyText(markdown)
      const count = markdown.split('\n').length
      return { message: `Copied ${count} line${count === 1 ? '' : 's'}` }
    },
  },
]
