// The app's configuration, kept in the store beside the items. Secrets go
// through `Secrets`, which the app backs with Electron's safeStorage.

import { randomBytes, randomUUID } from 'node:crypto'
import type { Db } from './db'

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
  token: string
}

export const DEFAULT_MCP_PORT = 47390

export class Settings {
  constructor(
    private db: Db,
    private secrets: Secrets = plainSecrets,
  ) {
    db.exec('CREATE TABLE IF NOT EXISTS settings (key TEXT PRIMARY KEY, value TEXT NOT NULL)')
  }

  get<T>(key: string, fallback: T): T {
    const r = this.db.prepare('SELECT value FROM settings WHERE key = ?').get(key) as { value: string } | undefined
    return r ? (JSON.parse(r.value) as T) : fallback
  }

  set(key: string, value: unknown): void {
    this.db.prepare('INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?)').run(key, JSON.stringify(value))
  }

  // ---- slack ----

  slackWorkspaces(): SlackWorkspace[] {
    return this.get<(Omit<SlackWorkspace, 'token'> & { token: string })[]>('slack.workspaces', []).map((w) => ({
      ...w,
      token: this.secrets.decrypt(w.token),
    }))
  }

  addSlackWorkspace(w: Omit<SlackWorkspace, 'id'>): SlackWorkspace {
    const added = { ...w, id: randomUUID() }
    const stored = this.get<SlackWorkspace[]>('slack.workspaces', []).filter((x) => x.team !== w.team)
    this.set('slack.workspaces', [...stored, { ...added, token: this.secrets.encrypt(w.token) }])
    return added
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
}

/** Checks a Slack user token and answers whose it is. */
export async function slackAuthTest(token: string, fetchFn: typeof fetch = fetch): Promise<{ team: string; user: string }> {
  const res = await fetchFn('https://slack.com/api/auth.test', {
    method: 'POST',
    headers: { Authorization: `Bearer ${token}` },
  })
  const body = (await res.json()) as { ok: boolean; error?: string; team?: string; user?: string }
  if (!body.ok) throw new Error(`Slack refused the token: ${body.error ?? res.status}`)
  return { team: body.team ?? '', user: body.user ?? '' }
}
