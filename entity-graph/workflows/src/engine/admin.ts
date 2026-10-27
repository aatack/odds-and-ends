// What the Integrations, MCP and Phone pages show and change.

import type { IntegrationsView, McpView, PhoneView } from '../shared/types'
import type { Engine } from './engine'
import type { Mcp } from './mcp'
import { slackAuthTest } from './settings'

const firstLine = (s: string) => s.trim().split('\n')[0]

/** The backend's phone listener, as the Phone page sees it (src/backend/http.ts). */
export interface Phone {
  status: () => { url: string; port: number; error: string | null }
  restart: () => Promise<void>
}

/** The HTTPS port on the tailnet the phone opens: not 443, which is left for whatever else this machine serves. */
export const PHONE_TAILNET_PORT = 8443

export class Admin {
  /** The phone listener, once the backend has one; none in the tests of the engine alone. */
  phone: Phone | null = null

  constructor(
    private engine: Engine,
    private mcp: Mcp,
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
      claude: { ...claude, bots: this.engine.agents().length },
      deepgram: (() => {
        const d = this.engine.settings.deepgramKey()
        return d ? { from: d.from, hint: `${d.key.slice(0, 4)}…${d.key.slice(-4)}` } : null
      })(),
      slack: this.engine.settings.slackWorkspaces().map(({ token, ...w }) => ({ ...w, tokenHint: `${token.slice(0, 5)}…${token.slice(-4)}` })),
    }
  }

  /** Checks the token with Slack before keeping it. */
  /** Slack says which workspace the token is for, so the token is all it needs. */
  async addSlack(token: string): Promise<void> {
    token = token.trim()
    if (!token.startsWith('xoxp-')) throw new Error('That is not a Slack user token: they start with xoxp-.')
    const { team, user, userId } = await slackAuthTest(token, this.fetchFn)
    this.engine.settings.addSlackWorkspace({ name: team, team, user, userId, token })
  }

  removeSlack(id: string): void {
    this.engine.settings.removeSlackWorkspace(id)
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

  async phoneView(): Promise<PhoneView> {
    const s = this.engine.settings
    const { url, port, error } = this.phone?.status() ?? { url: `http://127.0.0.1:${s.phonePort()}/`, port: s.phonePort(), error: 'The phone listener is not running.' }
    let tailnetName: string | null = null
    try {
      // Only read: what Tailscale serves is changed by the person, from a shell.
      const status = JSON.parse(await this.engine.exec('tailscale', ['status', '--json'])) as { Self?: { DNSName?: string } }
      tailnetName = status.Self?.DNSName?.replace(/\.$/, '') || null
    } catch {
      // No Tailscale here, or it is down.
    }
    return {
      url,
      port,
      token: s.phoneToken(),
      tailnet: s.phoneTailnet(),
      error,
      tailnetName,
      serveCommand: `tailscale serve --bg --https=${PHONE_TAILNET_PORT} http://127.0.0.1:${port}`,
      phoneUrl: tailnetName ? `https://${tailnetName}:${PHONE_TAILNET_PORT}/` : null,
    }
  }

  setPhoneTailnet(on: boolean): void {
    this.engine.settings.set('phone.tailnet', on)
  }

  async setPhonePort(port: number): Promise<void> {
    if (!Number.isInteger(port) || port < 1024 || port > 65535) throw new Error('The port must be a number from 1024 to 65535.')
    this.engine.settings.set('phone.port', port)
    await this.phone?.restart()
  }

  /** The token is read on every request, so the old one stops working at once. */
  regeneratePhoneToken(): void {
    this.engine.settings.regeneratePhoneToken()
  }
}
