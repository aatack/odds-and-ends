// What the Integrations and MCP pages show and change.

import type { IntegrationsView, McpView, PhoneView } from '../shared/types'
import type { Engine } from './engine'
import type { Mcp } from './mcp'
import type { Phone } from './server'
import { slackAuthTest } from './settings'

const firstLine = (s: string) => s.trim().split('\n')[0]

export class Admin {
  /** The phone server, once main has started it. Null in the engine tests, which never touch the phone. */
  private phone: Phone | null = null

  constructor(
    private engine: Engine,
    private mcp: Mcp,
    private workers: number,
    private fetchFn: typeof fetch = fetch,
  ) {}

  attachPhone(phone: Phone): void {
    this.phone = phone
  }

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

  // ---- the phone (mobile PWA) endpoint; desktop-only, never exposed over HTTP ----

  async phoneView(): Promise<PhoneView> {
    const s = this.engine.settings
    const port = s.phonePort()
    const serveCommand = `tailscale serve --bg --https=8443 http://127.0.0.1:${port}`
    let error = this.phone?.error ?? null
    let dnsName: string | null = null
    try {
      const status = JSON.parse(await this.engine.exec('tailscale', ['status', '--json'])) as { Self?: { DNSName?: string } }
      dnsName = status.Self?.DNSName ? status.Self.DNSName.replace(/\.$/, '') : null
      if (!dnsName) error ??= 'Tailscale is up but has no MagicDNS name yet.'
    } catch (e) {
      error ??= `Tailscale is not available: ${firstLine((e as Error).message)}`
    }
    return {
      enabled: s.phoneEnabled(),
      port,
      token: s.phoneToken(),
      loopbackUrl: `http://127.0.0.1:${port}/`,
      tailnetUrl: dnsName,
      serveCommand,
      phoneUrl: dnsName ? `https://${dnsName}:8443/` : null,
      error,
    }
  }

  async setPhoneEnabled(on: boolean): Promise<void> {
    this.engine.settings.setPhoneEnabled(on)
    await this.phone?.restart()
  }

  async setPhonePort(port: number): Promise<void> {
    if (!Number.isInteger(port) || port < 1024 || port > 65535) throw new Error('The port must be a number from 1024 to 65535.')
    this.engine.settings.setPhonePort(port)
    await this.phone?.restart()
  }

  async regeneratePhoneToken(): Promise<void> {
    // The token is read from settings on each request, so nothing needs restarting.
    this.engine.settings.regeneratePhoneToken()
  }

  /** Runs the exact `tailscale serve` command through engine.exec, returning its output or the error. */
  async runTailscaleServe(): Promise<string> {
    const port = this.engine.settings.phonePort()
    try {
      return await this.engine.exec('tailscale', ['serve', '--bg', '--https=8443', `http://127.0.0.1:${port}`])
    } catch (e) {
      return (e as Error).message
    }
  }
}
