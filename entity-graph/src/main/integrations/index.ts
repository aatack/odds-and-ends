import { ToolNotFoundError, invokeTool, type ToolDef } from '../../core/pensive/index'
import { CLAUDE_TOOLS } from './claude'
import { GIT_TOOLS } from './git'
import { GITHUB_TOOLS } from './github'
import { SLACK_TOOLS } from './slack'
import { TERMINAL_TOOLS } from './terminal'

// The integrations: everything the app can do that reaches outside itself.
//
// They are deliberately *not* a pensive, and cannot be given to one. A pensive
// is a store of notes, composed and published by whoever owns it; these are this
// machine's own hands — a shell, a checkout, an account somebody is signed into —
// and handing them out with a bearer token would hand out the machine. So they
// live here, in one registry the app calls directly, and nothing a broadcast or
// an MCP node serves can reach them.

export const INTEGRATION_TOOLS: ToolDef[] = [
  ...GITHUB_TOOLS,
  ...GIT_TOOLS,
  ...CLAUDE_TOOLS,
  ...SLACK_TOOLS,
  ...TERMINAL_TOOLS,
]

const byId = new Map(INTEGRATION_TOOLS.map((t) => [t.id, t]))

export const findIntegrationTool = (id: string): ToolDef | undefined => byId.get(id)

/** Validate the arguments against the tool's schema, then run it. */
export async function runIntegrationTool(id: string, args: unknown): Promise<unknown> {
  const tool = byId.get(id)
  if (!tool) throw new ToolNotFoundError(id)
  return invokeTool(tool, args)
}
