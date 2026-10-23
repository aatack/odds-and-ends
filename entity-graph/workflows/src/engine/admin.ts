// What the Integrations and MCP pages show and change.

import type { IntegrationsView, McpView } from '../shared/types'
import type { Engine } from './engine'
import { describeTools, type Mcp } from './mcp'
import { slackAuthTest, type Settings } from './settings'

const firstLine = (s: string) => s.trim().split('\n')[0]

export class Admin {
  constructor(
    private engine: Engine,
    private settings: Settings,
    private mcp: Mcp,
    private workers: number,
    private fetchFn: typeof fetch = fetch,
  ) {}

  async integrations(): Promise<IntegrationsView> {
    const check = async (fn: () => Promise<string>) => {
      try {
        return { ok: true, detail: await fn() }
      } catch (e) {
        return { ok: false, detail: (e as Error).message }
      }
    }
    const [github, claude] = await Promise.all([
      check(async () => `Signed in as ${firstLine(await this.engine.exec('gh', ['api', 'user', '--jq', '.login']))}`),
      check(async () => firstLine(await this.engine.exec('claude', ['--version']))),
    ])
    return {
      github,
      claude: { ...claude, workers: this.workers },
      slack: this.settings.slackWorkspaces().map(({ token, ...w }) => ({ ...w, tokenHint: `${token.slice(0, 5)}…${token.slice(-4)}` })),
    }
  }

  /** Checks the token with Slack before keeping it. */
  async addSlack(name: string, token: string): Promise<void> {
    token = token.trim()
    if (!token.startsWith('xoxp-')) throw new Error('That is not a Slack user token: they start with xoxp-.')
    const { team, user } = await slackAuthTest(token, this.fetchFn)
    this.settings.addSlackWorkspace({ name: name.trim() || team, team, user, token })
  }

  removeSlack(id: string): void {
    this.settings.removeSlackWorkspace(id)
  }

  async mcpView(): Promise<McpView> {
    const { url, error } = this.mcp.operator()
    const [runTools, operatorTools] = await Promise.all([
      describeTools(this.engine, { kind: 'run', runId: 'example' }),
      describeTools(this.engine, { kind: 'operator' }),
    ])
    return { url, token: this.settings.mcpToken(), port: this.settings.mcpPort(), error, runTools, operatorTools }
  }

  async regenerateMcpToken(): Promise<void> {
    this.settings.regenerateMcpToken()
  }

  async setMcpPort(port: number): Promise<void> {
    if (!Number.isInteger(port) || port < 1024 || port > 65535) throw new Error('The port must be a number from 1024 to 65535.')
    this.settings.set('mcp.port', port)
    await this.mcp.restartOperator()
  }
}
