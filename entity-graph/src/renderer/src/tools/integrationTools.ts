import type { ToolMeta } from '../../../core/client'
import { atom } from '../state/atom'
import type { ArgKind, ArgSpec, ToolSpec } from './types'

// The server's integrations — GitHub, Slack, Claude — as tools of the app.
//
// They are declared once, on the server, and arrive here as JSON Schema; the
// palette's argument prompts are built from that rather than restated, so a tool
// gained or an argument renamed on the server needs nothing doing here. What the
// app supplies is the half the server can't: a label, an ordering, and the fact
// that a call is worth keeping in the log.

/** Runtime only, and pointedly not persisted: this is a cache of the server's. */
export const integrationsAtom = atom<ToolSpec[]>([])

/** Which server's integrations these are. Null when no source is open. */
let serverId: string | null = null

/**
 * Point the integrations at a server and load its tool list. Failure is not an
 * error: a server with no admin access, or an older one with no `/tools`, simply
 * has no integrations, and the palette should say nothing about it.
 */
export function setIntegrationServer(next: string | null): void {
  serverId = next
  integrationsAtom.set([])
  if (!next) return
  void window.entityGraph
    .integrationTools(next)
    .then((tools) => {
      // Guard against a slow load landing after the source has moved on.
      if (serverId === next) integrationsAtom.set(tools.map(toolSpec))
    })
    .catch(() => undefined)
}

// --- JSON Schema → argument prompts ----------------------------------------

interface PropertySchema {
  type?: string
  enum?: unknown[]
  default?: unknown
  description?: string
}

interface ObjectSchema {
  properties?: Record<string, PropertySchema>
  required?: string[]
}

const KINDS: Record<string, ArgKind> = {
  string: 'string',
  number: 'number',
  integer: 'number',
  boolean: 'boolean',
}

/** `pullRequest` → "Pull request". */
function labelOf(name: string): string {
  const words = name.replace(/([a-z0-9])([A-Z])/g, '$1 $2').toLowerCase()
  return words.charAt(0).toUpperCase() + words.slice(1)
}

function argSpec(name: string, schema: PropertySchema, required: boolean): ArgSpec {
  return {
    name,
    label: labelOf(name),
    // Anything that isn't a scalar is entered as JSON — there is nothing better
    // to offer a one-line field, and it keeps the mapping total.
    kind: schema.enum ? 'select' : (KINDS[schema.type ?? ''] ?? 'json'),
    ...(schema.enum ? { options: schema.enum.map(String) } : {}),
    // A schema default is the tool's own: leaving the field alone sends `null`,
    // which is how the source contract spells "use the default".
    ...(schema.default !== undefined ? { hasDefault: true } : {}),
    ...(required ? {} : { optional: true }),
    ...(schema.description ? { placeholder: schema.description } : {}),
  }
}

const GROUPS: Record<string, string> = { github: 'GitHub', claude: 'Claude', slack: 'Slack' }

const groupOf = (id: string): string => {
  const prefix = id.split('.')[0]
  return GROUPS[prefix] ?? prefix.charAt(0).toUpperCase() + prefix.slice(1)
}

const clip = (text: string, limit = 160): string =>
  text.length > limit ? `${text.slice(0, limit - 1)}…` : text

/**
 * The most telling thing a result says about itself, in the order it's worth
 * saying: what the service said back, then what the thing *is*, then where it
 * is, and failing all three how many of them came back. The toast is a
 * confirmation — the whole result is in the activity log.
 */
const TELLING = ['output', 'text', 'title', 'permalink', 'url']

function summarise(data: unknown): string | null {
  if (typeof data === 'string') return data.trim() ? clip(data.trim()) : null
  if (Array.isArray(data)) return `${data.length} result${data.length === 1 ? '' : 's'}`
  if (data && typeof data === 'object') {
    const record = data as Record<string, unknown>
    for (const key of TELLING) {
      const value = record[key]
      if (typeof value === 'string' && value.trim()) return clip(value.trim())
    }
    const rows = Object.values(record).find(Array.isArray)
    if (rows) return `${rows.length} result${rows.length === 1 ? '' : 's'}`
  }
  return null
}

function toolSpec(meta: ToolMeta): ToolSpec {
  const schema = meta.args as ObjectSchema
  const required = new Set(schema.required ?? [])
  const args = Object.entries(schema.properties ?? {}).map(([name, property]) =>
    argSpec(name, property, required.has(name)),
  )
  return {
    id: meta.id,
    label: meta.name,
    // The id carries the service, so typing "slack" finds all of them.
    aliases: [meta.id, groupOf(meta.id)],
    hint: groupOf(meta.id),
    // Nothing about them belongs to a frame or a tab: they act on the world.
    scope: 'app',
    // Which is also why every call is kept: a merged pull request is a thing
    // that happened, and the log is the only record of it.
    reach: 'external',
    ...(args.length ? { args } : {}),
    run: async (values) => {
      if (!serverId) throw new Error('No server is open')
      const data = await window.entityGraph.runIntegrationTool(serverId, meta.id, values)
      return { data, message: summarise(data) ?? meta.name }
    },
  }
}
