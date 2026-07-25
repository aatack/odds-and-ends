import { createEntity, writeResource } from '../source/entity'
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
]
