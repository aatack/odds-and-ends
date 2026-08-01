import type { ToolMeta } from '../../../core/client'
import { atom } from '../state/atom'
import { argsFromSchema, summarise } from './declared'
import type { ToolSpec } from './types'

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

/**
 * Run one of the server's integrations directly, without going through the tool
 * machine. For a tool of the app's own that is *composed* of server calls — the
 * changeset tools, which fetch, branch, prompt, commit, push and raise a pull
 * request between them — where each step is not a thing the user did and has no
 * business in the log, the toasts or the undo stack. A gesture reaches these the
 * ordinary way, through the registry.
 */
export async function runIntegration(id: string, args: Record<string, unknown>): Promise<unknown> {
  if (!serverId) throw new Error('No server is open, so nothing can reach this machine')
  return window.entityGraph.runIntegrationTool(serverId, id, args)
}

// --- A server tool as one of the app's -------------------------------------

const GROUPS: Record<string, string> = { github: 'GitHub', claude: 'Claude', slack: 'Slack' }

const groupOf = (id: string): string => {
  const prefix = id.split('.')[0]
  return GROUPS[prefix] ?? prefix.charAt(0).toUpperCase() + prefix.slice(1)
}

function toolSpec(meta: ToolMeta): ToolSpec {
  const args = argsFromSchema(meta.args)
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
