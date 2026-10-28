// The app's configuration, kept in the store beside the items. Secrets go
// through `Secrets`, which the backend backs with a key of its own (src/backend/secrets.ts).
// The integrations are kept here for when their types come back: nothing uses them yet.

import { randomBytes, randomUUID } from 'node:crypto'
import type { Store } from './store'

export interface Secrets {
  encrypt: (plain: string) => string
  decrypt: (stored: string) => string
}

/** For the tests, and wherever no keyring is available. */
export const plainSecrets: Secrets = { encrypt: (s) => `plain:${s}`, decrypt: (s) => s.replace(/^plain:/, '') }

export interface SlackWorkspace {
  id: string
  name: string
  team: string
  user: string
  userId: string
  token: string
  /** Its address, e.g. https://acme.slack.com/: how a message's link is matched to it. */
  url?: string
}

/** Not the older app's (47390), so that the two can run side by side. */
export const DEFAULT_MCP_PORT = 47391

/** The phone listener's port, unless the settings say otherwise: one that lasts, for `tailscale serve` to point at. */
export const DEFAULT_PHONE_PORT = 47392

export class Settings {
  constructor(
    private store: Store,
    private secrets: Secrets = plainSecrets,
  ) {}

  get<T>(key: string, fallback: T): T {
    return this.store.settings.has(key) ? (this.store.settings.get(key) as T) : fallback
  }

  set(key: string, value: unknown): void {
    this.store.setSetting(key, value)
  }

  // ---- deepgram (transcribing what is said into the chat input) ----

  /** The Deepgram API key: the one kept here (encrypted), else the backend's DEEPGRAM_API_KEY; null with neither. */
  deepgramKey(): { key: string; from: 'settings' | 'env' } | null {
    const stored = this.get<string | null>('deepgram.key', null)
    const kept = stored ? this.readSecret(stored) : ''
    if (kept) return { key: kept, from: 'settings' }
    return process.env.DEEPGRAM_API_KEY ? { key: process.env.DEEPGRAM_API_KEY, from: 'env' } : null
  }

  /** Keeps a Deepgram key (encrypted), or forgets it (null). */
  setDeepgramKey(key: string | null): void {
    this.set('deepgram.key', key?.trim() ? this.secrets.encrypt(key.trim()) : null)
  }

  // ---- integrations' setup values (Integration.setup): `integration.<id>.<key>`, a secret encrypted ----

  integrationValue(id: string, key: string, secret = false): string | null {
    const stored = this.get<string | null>(`integration.${id}.${key}`, null)
    if (!stored) return null
    return secret ? this.readSecret(stored) || null : stored
  }

  /** Sets a setup value, or (null, or nothing) removes it. */
  setIntegrationValue(id: string, key: string, value: string | null, secret = false): void {
    const v = value?.trim()
    this.set(`integration.${id}.${key}`, v ? (secret ? this.secrets.encrypt(v) : v) : null)
  }

  // ---- slack ----

  slackWorkspaces(): SlackWorkspace[] {
    return this.get<(Omit<SlackWorkspace, 'token'> & { token: string })[]>('slack.workspaces', []).map((w) => ({
      ...w,
      token: this.readSecret(w.token),
    }))
  }

  /** A secret as plain text; empty when it can't be read.  */
  private readSecret(stored: string): string {
    try {
      return this.secrets.decrypt(stored)
    } catch {
      return ''
    }
  }

  addSlackWorkspace(w: Omit<SlackWorkspace, 'id'>): SlackWorkspace {
    const added = { ...w, id: randomUUID() }
    const stored = this.get<SlackWorkspace[]>('slack.workspaces', []).filter((x) => x.team !== w.team)
    this.set('slack.workspaces', [...stored, { ...added, token: this.secrets.encrypt(w.token) }])
    return added
  }

  /** Keeps a workspace's address, for one added before addresses were kept. */
  setSlackUrl(id: string, url: string): void {
    this.set('slack.workspaces', this.get<SlackWorkspace[]>('slack.workspaces', []).map((w) => (w.id === id ? { ...w, url } : w)))
  }

  removeSlackWorkspace(id: string): void {
    this.set('slack.workspaces', this.get<SlackWorkspace[]>('slack.workspaces', []).filter((w) => w.id !== id))
  }

  // ---- the operator's MCP endpoint ----

  mcpToken(): string {
    const token = this.get<string | null>('mcp.token', null)
    return token ?? this.regenerateMcpToken()
  }

  regenerateMcpToken(): string {
    const token = randomBytes(24).toString('base64url')
    this.set('mcp.token', token)
    return token
  }

  mcpPort(): number {
    return this.get('mcp.port', DEFAULT_MCP_PORT)
  }

  // ---- the phone: its listener's token (encrypted, as a secret), its port, and whether the tailnet is let in ----

  phoneToken(): string {
    const stored = this.get<string | null>('phone.token', null)
    return (stored && this.readSecret(stored)) || this.regeneratePhoneToken()
  }

  regeneratePhoneToken(): string {
    const token = randomBytes(24).toString('base64url')
    this.set('phone.token', this.secrets.encrypt(token))
    return token
  }

  phonePort(): number {
    return this.get('phone.port', DEFAULT_PHONE_PORT)
  }

  /** The HTTPS port `tailscale serve` takes the phone listener to, on the tailnet. */
  phoneTailnetPort(): number {
    return this.get('phone.tailnetPort', 8443)
  }

  /** Off by default: only this machine reaches the phone listener. */
  phoneTailnet(): boolean {
    return this.get('phone.tailnet', false)
  }
}

/** Checks a Slack user token and answers whose it is. */
export async function slackAuthTest(
  token: string,
  fetchFn: typeof fetch = fetch,
): Promise<{ team: string; user: string; userId: string; url?: string }> {
  const res = await fetchFn('https://slack.com/api/auth.test', {
    method: 'POST',
    headers: { Authorization: `Bearer ${token}` },
  })
  const body = (await res.json()) as { ok: boolean; error?: string; team?: string; user?: string; user_id?: string; url?: string }
  if (!body.ok) throw new Error(`Slack refused the token: ${body.error ?? res.status}`)
  return { team: body.team ?? '', user: body.user ?? '', userId: body.user_id ?? '', ...(body.url ? { url: body.url } : {}) }
}
