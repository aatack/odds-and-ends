import * as A from '../state/actions'
import { frameRows } from '../state/derive'
import { queryAtom } from '../state/query'
import { focusOf, getLayout } from '../state/store'
import { directionOf, last, type LinkDirection } from '../state/types'
import { updateUi } from '../state/ui'
import { createEntity, link, moveEntity, unlink, writeValue } from '../source/entity'
import type { ArgSpec, CallInfo, ToolSpec } from './types'

// Tools that name the entity they act on, rather than implying it. Their
// arguments are filled from the call's context, so the same declaration serves
// three gestures: a hotkey (everything filled, runs at once), a right-click (the
// entity under the cursor), and the palette (type the ids yourself).
//
// The arguments that are *pointed at* rather than typed — the new parent of a
// move, the far end of a link — carry `pick`, which is what makes "press x,
// move to the target, press x again" work without a special case.

/** The entity under the selection: filled from the context, never typed by default. */
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
  pick: 'entity',
})

const id = (v: unknown): string => String(v ?? '')

/**
 * An id for a write that *adds* something. Blank is refused loudly rather than
 * written: a link or value against an empty id conjures a phantom entity that
 * then shows up as a text-less row most tools quietly decline to touch. Removals
 * use the permissive `id` instead, so damage already in the store can be undone.
 */
function requireId(v: unknown, label: string): string {
  const value = id(v).trim()
  if (!value) throw new Error(`${label} is required`)
  return value
}

/**
 * Both link directions share this. Linking an entity to itself is refused: it
 * makes the entity its own child, which the query's cycle guard then hides, so
 * the only visible effect is a row that won't expand.
 */
async function linkEntities(sourceId: unknown, destinationId: unknown): Promise<void> {
  const from = requireId(sourceId, 'Source id')
  const to = requireId(destinationId, 'Destination id')
  if (from === to) throw new Error('An entity cannot be linked to itself')
  await link(from, to)
}

/** The selection's parent, or null at the root of the frame. */
function selectedParent(): string | null {
  const layout = getLayout()
  const { frameId } = focusOf(layout)
  const { selectedPath } = frameRows(layout, queryAtom.get(), frameId)
  return selectedPath.length > 1 ? selectedPath[selectedPath.length - 2] : null
}

/**
 * Which way round the link between a row and the row above it runs, in the frame
 * the call was started in. A reversed frame draws the same tree upside down: the
 * row hangs off its parent by a link that runs *from* the row *to* the parent, so
 * every tool that edits that link has to know which frame it was invoked from.
 */
const callDirection = (call: CallInfo): LinkDirection => {
  const frameId = call.context.frameId
  return directionOf(frameId ? getLayout().frames[frameId] : null)
}

export const ENTITY_TOOLS: ToolSpec[] = [
  {
    id: 'entity.open',
    label: 'Open entity',
    aliases: ['focus', 'drill in', 'push frame'],
    scope: 'frame',
    reach: 'ui',
    keys: [{ key: 'd' }],
    args: [entityArg()],
    run: ({ entityId }) => {
      const layout = getLayout()
      const { tabId, frameId } = focusOf(layout)
      const target = id(entityId)
      if (!tabId || !target) return
      // Don't stack a frame whose root is already what's showing — that just
      // makes a duplicate to pop straight back off.
      if (frameId && layout.frames[frameId]?.rootId === target) return
      A.pushFrame(tabId, target)
    },
  },
  {
    id: 'entity.debug',
    label: 'Debug entity',
    aliases: ['inspect', 'info', 'raw', 'events'],
    scope: 'frame',
    reach: 'ui',
    args: [entityArg()],
    run: ({ entityId }) => updateUi({ debugEntityId: requireId(entityId, 'Entity id') }),
  },
  {
    // The context-filled version above can never prompt — that's the point of
    // auto-skip — so inspecting an id you've read off a link needs its own tool.
    id: 'entity.inspect',
    label: 'Inspect entity by id…',
    aliases: ['debug', 'values', 'links', 'lookup', 'find id'],
    hint: 'Entity',
    scope: 'frame',
    reach: 'ui',
    args: [{ name: 'entityId', label: 'Entity id', kind: 'entity', placeholder: 'Paste an id' }],
    run: ({ entityId }) => updateUi({ debugEntityId: requireId(entityId, 'Entity id') }),
  },
  {
    id: 'entity.rename',
    label: 'Rename entity',
    aliases: ['set text', 'retitle'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [entityArg(), { name: 'text', label: 'New text' }],
    run: ({ entityId, text }) =>
      writeValue(requireId(entityId, 'Entity id'), 'text', String(text ?? '')).then(() => undefined),
  },
  {
    id: 'entity.create',
    label: 'Create child of entity',
    aliases: ['add', 'new', 'insert'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [entityArg('parentId', 'Parent id'), { name: 'text', label: 'Child text' }],
    run: async ({ parentId, text }) => {
      await createEntity({ text: String(text ?? '') }, requireId(parentId, 'Parent id'))
    },
  },
  {
    id: 'entity.unlink',
    label: 'Remove from parent',
    aliases: ['delete', 'detach', 'disconnect'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    keys: [{ key: 'Backspace' }, { key: 'Delete' }],
    // Nothing to unlink at the root of a frame, so the key stays inert there
    // rather than opening a wizard for the missing parent.
    enabled: () => selectedParent() != null,
    args: [entityArg('childId', 'Entity id'), entityArg('parentId', 'Parent id', 'parentId')],
    run: async ({ childId, parentId }, call) => {
      // The arguments name what's on screen — the row and the row it hangs off.
      // Which of the two is the link's source depends on the frame's direction.
      const reversed = callDirection(call) === 'in'
      await unlink(id(reversed ? childId : parentId), id(reversed ? parentId : childId))
      // Selection was on the row just removed; step up to its parent.
      const layout = getLayout()
      const frameId = call.context.frameId
      const frame = frameId ? layout.frames[frameId] : null
      if (frame && last(frame.selectedPath) === id(childId)) {
        A.selectPath(frame.id, frame.selectedPath.slice(0, -1))
      }
    },
  },
  {
    id: 'entity.move',
    label: 'Move entity to…',
    aliases: ['reparent', 'relocate'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    keys: [{ key: 'x' }],
    enabled: () => selectedParent() != null,
    args: [
      entityArg(),
      entityArg('fromParentId', 'From parent id', 'parentId'),
      pickArg('toParentId', 'To parent id'),
    ],
    run: async ({ entityId, fromParentId, toParentId }, call) => {
      const subject = requireId(entityId, 'Entity id')
      const from = requireId(fromParentId, 'From parent id')
      const to = requireId(toParentId, 'To parent id')
      if (from === to) return { mutated: false }
      if (to === subject) throw new Error('An entity cannot be its own parent')
      if (callDirection(call) === 'in') {
        // Upside down, the link runs from the row to its parent, so moving it
        // re-points that link's far end rather than swapping its source.
        await unlink(subject, from)
        await link(subject, to)
        return {}
      }
      await moveEntity(subject, from, to)
      return {}
    },
  },
  {
    id: 'entity.link',
    label: 'Link entity to…',
    aliases: ['connect', 'relate', 'reference'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    keys: [{ key: 'r' }],
    args: [entityArg('sourceId', 'Source id'), pickArg('destinationId', 'Destination id')],
    run: ({ sourceId, destinationId }) => linkEntities(sourceId, destinationId),
  },
  {
    id: 'entity.link.reverse',
    label: 'Link entity from…',
    aliases: ['connect', 'relate', 'reference', 'backlink', 'reversed'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    keys: [{ key: 'r', shift: true }],
    args: [entityArg('destinationId', 'Destination id'), pickArg('sourceId', 'Source id')],
    run: ({ sourceId, destinationId }) => linkEntities(sourceId, destinationId),
  },
  {
    id: 'entity.section.set',
    label: 'Set entity section',
    aliases: ['heading', 'header', 'title'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [
      entityArg(),
      { name: 'section', label: 'Section', kind: 'select', options: ['on', 'off'] },
    ],
    run: async ({ entityId, section }) => {
      await writeValue(requireId(entityId, 'Entity id'), 'section', section === 'on' ? true : null)
    },
  },
  {
    id: 'entity.checkbox.set',
    label: 'Set entity checkbox',
    aliases: ['todo', 'task', 'done', 'open', 'check'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [
      entityArg(),
      { name: 'open', label: 'Checkbox', kind: 'select', options: ['on', 'off', 'none'] },
    ],
    // on = open box, off = ticked, none = back to a plain bullet.
    run: async ({ entityId, open }) => {
      await writeValue(
        requireId(entityId, 'Entity id'),
        'open',
        open === 'on' ? true : open === 'off' ? false : null,
      )
    },
  },
  {
    id: 'entity.value.set',
    label: 'Set entity value',
    aliases: ['write', 'attribute', 'property', 'key'],
    hint: 'Entity',
    scope: 'frame',
    reach: 'source',
    mutates: true,
    args: [
      entityArg(),
      { name: 'key', label: 'Value key', placeholder: 'e.g. type' },
      {
        name: 'value',
        label: 'Value (JSON)',
        kind: 'json',
        optional: true,
        placeholder: 'Blank to clear',
      },
    ],
    run: async ({ entityId, key, value }) => {
      await writeValue(requireId(entityId, 'Entity id'), String(key), value ?? null)
    },
  },
]
