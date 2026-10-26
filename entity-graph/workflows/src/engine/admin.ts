// What the Integrations and MCP pages show and change.

import type { IntegrationsView, McpView } from '../shared/types'
import type { Engine } from './engine'
import type { Mcp } from './mcp'

const firstLine = (s: string) => s.trim().split('\n')[0]

export class Admin {
  constructor(
    private engine: Engine,
    private mcp: Mcp,
  ) {}

  async integrations(): Promise<IntegrationsView> {
    let claude: { ok: boolean; detail: string }
    try {
      claude = { ok: true, detail: firstLine(await this.engine.exec('claude', ['--version'])) }
    } catch (e) {
      claude = { ok: false, detail: (e as Error).message }
    }
    return { claude: { ...claude, bots: this.engine.agents().length } }
  }

  mcpView(): McpView {
    const { url, error } = this.mcp.operator()
    return { url, token: this.engine.settings.mcpToken(), port: this.engine.settings.mcpPort(), error }
  }

  async regenerateMcpToken(): Promise<void> {
    this.engine.settings.regenerateMcpToken()
  }

  async setMcpPort(port: number): Promise<void> {
    if (!Number.isInteger(port) || port < 1024 || port > 65535) throw new Error('The port must be a number from 1024 to 65535.')
    this.engine.settings.set('mcp.port', port)
    await this.mcp.restartOperator()
  }
}
