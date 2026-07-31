import { ToolNotFoundError, invokeTool, type ToolDef } from '../../../src/core/source/index'
import { CLAUDE_TOOLS } from './claude'
import { GIT_TOOLS } from './git'
import { GITHUB_TOOLS } from './github'
import { SLACK_TOOLS } from './slack'

// The integrations: everything the server can do that reaches outside itself.
//
// They are deliberately *not* a source. A source is a set of tools over an event
// store, composed and handed out per-token; these are the server's own hands,
// held in one registry behind one endpoint (`POST /runTool`), so that the only
// way to reach GitHub, Slack, Claude or a repository on this machine is by naming
// a tool that exists.

export const INTEGRATION_TOOLS: ToolDef[] = [
  ...GITHUB_TOOLS,
  ...GIT_TOOLS,
  ...CLAUDE_TOOLS,
  ...SLACK_TOOLS,
]

const byId = new Map(INTEGRATION_TOOLS.map((t) => [t.id, t]))

export const findIntegrationTool = (id: string): ToolDef | undefined => byId.get(id)

/** Validate the arguments against the tool's schema, then run it. */
export async function runIntegrationTool(id: string, args: unknown): Promise<unknown> {
  const tool = byId.get(id)
  if (!tool) throw new ToolNotFoundError(id)
  return invokeTool(tool, args)
}
