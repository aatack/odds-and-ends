import { createEntity, readResource, writeResource } from '../source/entity'
import { saveFile } from '../source/files'
import type { ToolSpec } from './types'

// Tools over stored bytes. Only one so far: taking what is on the clipboard and
// making an entity of it.
//
// Every argument here declares `hasDefault`, which looks odd for arguments that
// are all effectively required, and is deliberate: a default is never "empty", so
// the call always runs straight through instead of stopping to collect anything.
// It must — a pending or cancelled call is written to localStorage, and a pasted
// screenshot is far too big to put there. The tool validates what it was given
// instead, and is unlisted, since there is no typing a picture into a palette.

const text = (v: unknown): string => (typeof v === 'string' ? v : '')

/**
 * A plausible extension for bytes that arrived without a name — from the
 * clipboard, say. Only for something short and word-like after the slash, so an
 * exotic mime type ends up with no extension rather than a silly one.
 */
function extensionFor(mimeType: string): string {
  const subtype = mimeType.split('/')[1]?.split('+')[0]?.replace(/[^a-z0-9]/gi, '') ?? ''
  return subtype && subtype.length <= 5 ? `.${subtype}` : ''
}

export const RESOURCE_TOOLS: ToolSpec[] = [
  {
    id: 'resource.paste',
    label: 'Paste a resource',
    aliases: ['image', 'file', 'attachment', 'screenshot'],
    scope: 'frame',
    reach: 'source',
    mutates: true,
    listed: false,
    args: [
      {
        name: 'parentId',
        label: 'Parent id',
        kind: 'entity',
        fromContext: 'entityId',
        hasDefault: true,
      },
      { name: 'mimeType', label: 'Mime type', fromContext: 'resourceMimeType', hasDefault: true },
      { name: 'data', label: 'Base64 data', fromContext: 'resourceData', hasDefault: true },
      { name: 'name', label: 'File name', fromContext: 'resourceName', hasDefault: true },
    ],
    run: async ({ parentId, mimeType, data, name }) => {
      const parent = text(parentId).trim()
      const mime = text(mimeType).trim()
      if (!parent) throw new Error('Select something to paste under first')
      if (!mime || !text(data)) throw new Error('Nothing on the clipboard to paste')
      // The mime type is on the entity as well as the resource, so a row knows
      // what it is showing before the bytes have arrived. The name stays with the
      // resource: it is the file's, not the entity's, and `text` is free for a
      // caption the user writes.
      const created = await createEntity({ type: 'file', mimeType: mime }, parent)
      await writeResource(created, mime, text(data), text(name) || null)
      return { message: `Pasted ${mime}` }
    },
  },
  {
    id: 'resource.save',
    label: 'Save file',
    aliases: ['download', 'export file', 'write to disk'],
    hint: 'Entity',
    scope: 'frame',
    // It leaves the app entirely, which is also why the call is worth keeping in
    // the log: a file on disk is a thing that happened.
    reach: 'external',
    args: [{ name: 'entityId', label: 'Entity id', kind: 'entity', fromContext: 'entityId' }],
    run: async ({ entityId }) => {
      const target = text(entityId).trim()
      if (!target) throw new Error('Entity id is required')
      const resource = await readResource(target)
      if (!resource) throw new Error('This entity has no bytes stored')
      const name = resource.name ?? `${target}${extensionFor(resource.mimeType)}`
      const path = await saveFile(name, resource.data)
      return { message: `Saved to ${path}` }
    },
  },
]
