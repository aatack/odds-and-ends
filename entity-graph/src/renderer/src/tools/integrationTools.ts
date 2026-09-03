import type { ToolMeta } from '../../../core/client'
import { atom } from '../state/atom'
import { argsFromSchema, summarise } from './declared'
import type { ToolSpec } from './types'

// The app's integrations — GitHub, Slack, Claude, git, a terminal — as tools of
// the palette.
//
// They are declared in the main process, where the hands are, and arrive here as
// JSON Schema; the palette's argument prompts are built from that rather than
// restated, so a tool gained or an argument renamed there needs nothing doing
// here. What this side supplies is the half the declaration can't: a label, an
// ordering, and the fact that a call is worth keeping in the log.
//
// They belong to no pensive, which is deliberate — a store that is published to
// somebody else must not carry a shell — so they are loaded once, when the app
// starts, rather than when a source opens.

/** Runtime only, and pointedly not persisted: a cache of the main process's. */
export const integrationsAtom = atom<ToolSpec[]>([])

/**
 * Read the integrations into the registry. Failure is not an error: an app whose
 * `.env` says nothing still has the tools, and each one complains for itself
 * when it is called.
 */
export function loadIntegrations(): void {
  void window.entityGraph
    .integrationTools()
    .then((tools) => integrationsAtom.set(tools.map(toolSpec)))
    .catch(() => undefined)
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
    // The tool may have written to the graph on its way — a Claude session
    // writes its notes over MCP, which nothing here sees. So the cache is read
    // again afterwards. It costs one scan of what is on screen, per gesture,
    // which is affordable in a way doing it per keystroke was not.
    writesUnseen: true,
    ...(args.length ? { args } : {}),
    run: async (values) => {
      const data = await window.entityGraph.runIntegrationTool(meta.id, values)
      return { data, message: summarise(data) ?? meta.name }
    },
  }
}
