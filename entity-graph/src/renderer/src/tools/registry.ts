import fuzzysort from 'fuzzysort'
import { APP_TOOLS, GROUP_TOOLS } from './appTools'
import { CHANGESET_TOOLS } from './changesetTools'
import { ENTITY_TOOLS } from './entityTools'
import { FRAME_TOOLS } from './frameTools'
import { integrationsAtom } from './integrationTools'
import { RESOURCE_TOOLS } from './resourceTools'
import { UNDO_TOOLS } from './undoTools'
import { userToolsAtom } from './userTools'
import type { ToolScope, ToolSpec } from './types'

// The one registry. Order matters twice: it's the order the palette lists tools
// in with no search text (entity tools first, since they act on what the user is
// looking at), and the order the key router resolves collisions in within a
// scope.
//
// Most of it is fixed at build time. Two parts are not: the integrations, which
// are declared on the server and arrive when a source is opened, and the user's
// own tools, which are notes in the store. So the list is a function rather than
// a constant, and the palette re-reads it when either lands.
//
// Both trail the built-ins, which settles every collision between a declared tool
// and one of the app's own in the app's favour — a store cannot rebind `d` out
// from under the user by naming a tool badly.

const BUILT_IN: ToolSpec[] = [
  ...ENTITY_TOOLS,
  ...FRAME_TOOLS,
  ...CHANGESET_TOOLS,
  ...RESOURCE_TOOLS,
  ...GROUP_TOOLS,
  ...UNDO_TOOLS,
  ...APP_TOOLS,
]

/** Everything invocable right now. Declared tools trail the built-ins. */
export const allTools = (): ToolSpec[] => [
  ...BUILT_IN,
  ...integrationsAtom.get(),
  ...userToolsAtom.get(),
]

const byId = new Map(BUILT_IN.map((t) => [t.id, t]))

export const findTool = (id: string): ToolSpec | undefined =>
  byId.get(id) ??
  integrationsAtom.get().find((t) => t.id === id) ??
  userToolsAtom.get().find((t) => t.id === id)

/** Tools offered in the palette's list. */
export const listedTools = (): ToolSpec[] => allTools().filter((t) => t.listed !== false)

export const toolsInScope = (scope: ToolScope): ToolSpec[] =>
  allTools().filter((t) => t.scope === scope)

// --- Naming a tool in code --------------------------------------------------

// A script says `tool.sendSlackMessage(…)`, so every tool needs a name that is a
// JavaScript identifier. It is derived from the label rather than declared: the
// label is the one thing every tool already has, including the integrations,
// which are named on the server and would otherwise need a second name here.

/** Words a label carries for the reader and an identifier is better without. */
const FILLER = new Set(['a', 'an', 'the'])

/**
 * A tool's label as a script writes it: "Send a Slack message" →
 * `sendSlackMessage`. Words keep their own capitals (so "GitHub" survives), and
 * only the first letter of the whole name is forced to lower case.
 */
export function toolName(label: string): string {
  const words = label
    // A possessive would otherwise read as a word of its own, since the
    // apostrophe is a boundary: "a repo's pull requests" → `repoSPullRequests`.
    .replace(/['’]s\b/g, '')
    .split(/[^A-Za-z0-9]+/)
    .filter(Boolean)
    .filter((word, i) => i === 0 || !FILLER.has(word.toLowerCase()))
  const joined = words.map((w, i) => (i === 0 ? w : w.charAt(0).toUpperCase() + w.slice(1))).join('')
  return joined.charAt(0).toLowerCase() + joined.slice(1)
}

/**
 * The tool a script named — by that derived name, or by its id, which is the
 * stable handle and the way out of any collision between two labels.
 */
export function findToolByName(name: string): ToolSpec | undefined {
  const tools = allTools()
  return tools.find((t) => t.id === name) ?? tools.find((t) => toolName(t.label) === name)
}

/** The nearest names to one that didn't resolve, for the error to suggest. */
export const nearestToolNames = (name: string, limit = 3): string[] =>
  fuzzysort.go(name, allTools().map((t) => toolName(t.label)), { limit }).map((r) => r.target)
