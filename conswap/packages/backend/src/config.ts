import { homedir } from 'node:os'
import { join } from 'node:path'

export interface Config {
  port: number
  /** The single sqlite file. Everything lives in here. */
  databasePath: string
  /** Where claude worktrees are made. */
  worktreeRoot: string
  slack: { token: string | null; pollSeconds: number; requestsPerMinute: number }
  github: { enabled: boolean; pollSeconds: number }
  claude: { binary: string; model: string }
  tickSeconds: number
}

const dataDirectory = process.env.CONSWAP_HOME ?? join(homedir(), '.conswap')

function integer(value: string | undefined, fallback: number): number {
  const parsed = Number.parseInt(value ?? '', 10)
  return Number.isFinite(parsed) ? parsed : fallback
}

export function loadConfig(overrides: Partial<Config> = {}): Config {
  return {
    port: integer(process.env.CONSWAP_PORT, 4319),
    databasePath: process.env.CONSWAP_DATABASE ?? join(dataDirectory, 'conswap.sqlite'),
    worktreeRoot: process.env.CONSWAP_WORKTREES ?? join(dataDirectory, 'worktrees'),
    slack: {
      token: process.env.SLACK_USER_TOKEN ?? null,
      pollSeconds: integer(process.env.CONSWAP_SLACK_POLL, 15),
      requestsPerMinute: integer(process.env.CONSWAP_SLACK_RATE, 45),
    },
    github: {
      enabled: process.env.CONSWAP_GITHUB !== 'off',
      pollSeconds: integer(process.env.CONSWAP_GITHUB_POLL, 180),
    },
    claude: {
      binary: process.env.CONSWAP_CLAUDE_BINARY ?? 'claude',
      model: process.env.CONSWAP_CLAUDE_MODEL ?? 'opus',
    },
    tickSeconds: integer(process.env.CONSWAP_TICK, 5),
    ...overrides,
  }
}

export { dataDirectory }
