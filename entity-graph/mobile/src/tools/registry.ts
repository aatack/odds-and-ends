import { APP_TOOLS } from './appTools'
import { ENTITY_TOOLS } from './entityTools'
import { VIEW_TOOLS } from './viewTools'
import type { ToolContext, ToolSpec } from './types'

// The registry: every command the app has, in one list.
//
// The desktop app's registry is not a constant — the server's integrations
// (GitHub, Slack, Claude) are fetched when a source opens and folded in. There is
// none of that here on purpose: this client reads and writes one source and does
// nothing else, so the list is fixed at build time and `allTools()` is a function
// only so the two call sites read the same either way.

const TOOLS: ToolSpec[] = [...VIEW_TOOLS, ...ENTITY_TOOLS, ...APP_TOOLS]

export const allTools = (): ToolSpec[] => TOOLS

export const toolById = (id: string): ToolSpec | undefined => TOOLS.find((t) => t.id === id)

/** Whether a tool applies right now. Tools with no opinion always do. */
export const isEnabled = (tool: ToolSpec, ctx: ToolContext): boolean =>
  tool.enabled ? tool.enabled(ctx) : true

/**
 * The tools the action sheet offers: listed, applicable, and — when the user has
 * typed something — matching it. The match is deliberately dumb (substring over the
 * label, the aliases and the id), because the list is short and a fuzzy matcher
 * would be one more dependency to install on a phone.
 */
export function listedTools(ctx: ToolContext, search = ''): ToolSpec[] {
  const q = search.trim().toLowerCase()
  return TOOLS.filter((tool) => {
    if (tool.listed === false) return false
    if (!isEnabled(tool, ctx)) return false
    if (!q) return true
    const terms = [tool.label, tool.id, ...(tool.aliases ?? [])].join(' ').toLowerCase()
    return q.split(/\s+/).every((word) => terms.includes(word))
  })
}

/** The tools, grouped under their hints, in the order the registry declares them. */
export function groupedTools(tools: ToolSpec[]): { hint: string; tools: ToolSpec[] }[] {
  const groups: { hint: string; tools: ToolSpec[] }[] = []
  for (const tool of tools) {
    const hint = tool.hint ?? 'Other'
    const group = groups.find((g) => g.hint === hint)
    if (group) group.tools.push(tool)
    else groups.push({ hint, tools: [tool] })
  }
  return groups
}
