// What the Integrations, MCP and Phone pages show and change.

import type { IntegrationsView, McpView, PhoneView } from '../shared/types'
import type { Engine } from './engine'
import type { Mcp } from './mcp'
import { slackAuthTest } from './settings'
import { clearSlackCache, slackCacheStats, slackView } from './slack'

const firstLine = (s: string) => s.trim().split('\n')[0]

/** The backend's phone listener, as the Phone page sees it (src/backend/http.ts). */
export interface Phone {
  status: () => { url: string; port: number; error: string | null }
  restart: () => Promise<void>
}


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
    const claude = await check(async () => firstLine(await this.engine.exec('claude', ['--version'])))
    const registered = await Promise.all(
      this.engine.integrations.map(async (i) => ({
        id: i.id,
        name: i.name,
        description: i.description,
        auth: i.auth,
        status: i.status ? await check(() => i.status!(this.engine)) : null,
        setup: i.setup.map((f) => {
          const value = this.engine.settings.integrationValue(i.id, f.key, f.secret)
          return { key: f.key, label: f.label, instructions: f.instructions, secret: !!f.secret, value: f.secret ? null : value, set: value !== null }
        }),
        read: i.tools.filter((t) => !t.unsafe).map((t) => ({ name: t.name, description: t.description })),
        write: i.tools.filter((t) => t.unsafe).map((t) => ({ name: t.name, description: t.description })),
        triggers: i.triggers,
      })),
    )
    return {
      registered,
      claude: { ...claude, bots: this.engine.agents().length },
      deepgram: (() => {
        const d = this.engine.settings.deepgramKey()
        return d ? { from: d.from, hint: `${d.key.slice(0, 4)}…${d.key.slice(-4)}` } : null
      })(),
      slackCache: slackCacheStats(this.engine),
      slack: this.engine.settings.slackWorkspaces().map(({ token, ...w }) => ({ ...w, tokenHint: `${token.slice(0, 5)}…${token.slice(-4)}` })),
    }
  }

  /** Checks the token with Slack before keeping it. */
  /** Slack says which workspace the token is for, so the token is all it needs. */
  async addSlack(token: string): Promise<void> {
    token = token.trim()
    if (!token.startsWith('xoxp-')) throw new Error('That is not a Slack user token: they start with xoxp-.')
    const { team, user, userId, url } = await slackAuthTest(token, this.fetchFn)
    this.engine.settings.addSlackWorkspace({ name: team, team, user, userId, token, ...(url ? { url } : {}) })
  }

  setIntegrationValue(id: string, key: string, value: string | null): void {
    const field = this.engine.integrations.find((i) => i.id === id)?.setup.find((f) => f.key === key)
    if (!field) throw new Error(`No setup value ${key} for ${id}.`)
    this.engine.settings.setIntegrationValue(id, key, value, field.secret)
  }

  removeSlack(id: string): void {
    this.engine.settings.removeSlackWorkspace(id)
  }

  clearSlackCache(): void {
    clearSlackCache(this.engine)
  }

  /** A Slack event's messages, for its card and view; with `thread`, the replies to one of them. */
  async slackView(eventId: string, thread?: string) {
    const event = this.engine.event(eventId)
    if (!event) throw new Error(`No event ${eventId}.`)
    return slackView(this.engine, event, thread)
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
    const tailnetPort = s.phoneTailnetPort()
    let tailnetName: string | null = null
    let serving: boolean | null = null
    try {
      const status = JSON.parse(await this.engine.exec('tailscale', ['status', '--json'])) as { Self?: { DNSName?: string } }
      tailnetName = status.Self?.DNSName?.replace(/\.$/, '') || null
      // Whether Tailscale serves this port now: some HTTPS handler on it.
      const serve = JSON.parse((await this.engine.exec('tailscale', ['serve', 'status', '--json'])) || '{}') as { TCP?: Record<string, unknown> }
      serving = !!serve.TCP?.[String(tailnetPort)]
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
      tailnetPort,
      serving,
      serveCommand: `tailscale serve --bg --https=${tailnetPort} http://127.0.0.1:${port}`,
      phoneUrl: tailnetName ? `https://${tailnetName}:${tailnetPort}/` : null,
    }
  }

  /**
   * Puts the phone on the tailnet, or takes it off: `tailscale serve` (run here, as the person's own press on the Phone
   * page; the phone itself may not call this) for its tailnet port, and the setting that lets what comes through it in.
   */
  async setPhoneTailnet(on: boolean): Promise<void> {
    const s = this.engine.settings
    if (on) {
      try {
        await this.engine.exec('tailscale', ['serve', '--bg', `--https=${s.phoneTailnetPort()}`, `http://127.0.0.1:${s.phonePort()}`])
      } catch (e) {
        throw new Error(
          `Tailscale would not serve it: ${(e as Error).message}\nIf it asks for root, run once in a shell: sudo tailscale set --operator=$USER`,
        )
      }
      s.set('phone.tailnet', true)
    } else {
      s.set('phone.tailnet', false)
      try {
        await this.engine.exec('tailscale', ['serve', `--https=${s.phoneTailnetPort()}`, 'off'])
      } catch {
        // Nothing was served there.
      }
    }
  }

  async setPhonePort(port: number): Promise<void> {
    if (!Number.isInteger(port) || port < 1024 || port > 65535) throw new Error('The port must be a number from 1024 to 65535.')
    this.engine.settings.set('phone.port', port)
    await this.phone?.restart()
    // Served on the tailnet: served again, to the new port.
    if (this.engine.settings.phoneTailnet()) await this.setPhoneTailnet(true)
  }

  /** The HTTPS port on the tailnet; served there again if it is on. */
  async setPhoneTailnetPort(port: number): Promise<void> {
    if (!Number.isInteger(port) || port < 1 || port > 65535) throw new Error('The port must be a number from 1 to 65535.')
    const was = this.engine.settings.phoneTailnet()
    if (was) await this.setPhoneTailnet(false)
    this.engine.settings.set('phone.tailnetPort', port)
    if (was) await this.setPhoneTailnet(true)
  }

  /** The token is read on every request, so the old one stops working at once. */
  regeneratePhoneToken(): void {
    this.engine.settings.regeneratePhoneToken()
  }
}
