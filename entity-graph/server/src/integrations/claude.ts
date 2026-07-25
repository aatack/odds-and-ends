import { randomUUID } from 'crypto'
import { z } from 'zod'
import type { ToolDef } from '../../../src/core/source/index'
import { optionalEnv, requireEnv } from '../env'
import { fetchJson } from './http'

// Claude Code in the cloud, through the routines ("remote trigger") API. A
// routine is the only handle that API gives on *starting* a session, so that is
// what starting one is here: a one-off routine, created disabled so it can never
// fire twice, and run immediately.
//
// This is a stopgap and is shaped like one — see `server/docs/integrations.md`.

const BETA = 'ccr-triggers-2026-01-30'

const DEFAULT_MODEL = 'claude-sonnet-5'

/** What a cloud session is allowed to do unless the routine says otherwise. */
const ALLOWED_TOOLS = ['Bash', 'Read', 'Write', 'Edit', 'Glob', 'Grep']

const baseUrl = (): string =>
  (optionalEnv('CLAUDE_CODE_API_BASE_URL') ?? 'https://api.anthropic.com').replace(/\/+$/, '')

const headers = (): Record<string, string> => ({
  Authorization: `Bearer ${requireEnv('CLAUDE_CODE_OAUTH_TOKEN')}`,
  'anthropic-beta': BETA,
})

const call = <T>(
  path: string,
  init: { method?: 'GET' | 'POST'; body?: unknown; query?: Record<string, string | number> } = {},
): Promise<T> => fetchJson<T>(`${baseUrl()}${path}`, { ...init, headers: headers() })

/**
 * A repository as the API wants it. `owner/repo` is the shorthand worth
 * accepting; anything else is passed through as the URL it already is.
 */
function repositoryUrl(repo: string): string {
  const trimmed = repo.trim().replace(/\.git$/, '')
  if (/^https?:\/\//i.test(trimmed)) return trimmed
  if (/^[\w.-]+\/[\w.-]+$/.test(trimmed)) return `https://github.com/${trimmed}`
  throw new Error(`"${repo}" doesn't name a repository — use owner/repo or its URL`)
}

/**
 * One user turn, in the shape both the routine's initial events and a session
 * follow-up take.
 */
const userEvent = (prompt: string, sessionId = ''): Record<string, unknown> => ({
  data: {
    uuid: randomUUID(),
    session_id: sessionId,
    type: 'user',
    parent_tool_use_id: null,
    message: { role: 'user', content: prompt },
  },
})

/** A routine needs a name; the first line of the prompt is the honest one. */
const nameFor = (prompt: string): string => {
  const line = prompt.trim().split('\n')[0].trim()
  return line.length > 60 ? `${line.slice(0, 57)}…` : line || 'Session'
}

interface Trigger {
  id: string
}

export const CLAUDE_TOOLS: ToolDef[] = [
  {
    id: 'claude.startSession',
    name: 'Start a Claude session',
    description:
      'Kick off a cloud Claude Code session on a repository. Creates a one-off routine and runs it straight away; the routine is left disabled so it never fires again on its own.',
    safety: 'dangerous',
    args: z.object({
      prompt: z.string().min(1).describe('What the session should do — it starts with no context'),
      repo: z
        .string()
        .optional()
        .describe('owner/repo, or its URL. Defaults to $CLAUDE_DEFAULT_REPO'),
      model: z.string().optional().describe(`Defaults to $CLAUDE_DEFAULT_MODEL, else ${DEFAULT_MODEL}`),
      environmentId: z
        .string()
        .optional()
        .describe('Cloud environment to run in. Defaults to $CLAUDE_ENVIRONMENT_ID'),
      name: z.string().optional().describe('Routine name. Defaults to the prompt’s first line'),
    }),
    handler: async (args) => {
      const repo = args.repo ?? optionalEnv('CLAUDE_DEFAULT_REPO')
      if (!repo) throw new Error('Give a repo, or set CLAUDE_DEFAULT_REPO in server/.env')
      const environmentId = args.environmentId ?? requireEnv('CLAUDE_ENVIRONMENT_ID')

      const created = await call<Trigger>('/v1/code/triggers', {
        method: 'POST',
        body: {
          name: args.name ?? nameFor(args.prompt),
          // The API insists on a future firing time even for a routine that is
          // only ever going to be run by hand, so it gets one it will never see.
          run_once_at: new Date(Date.now() + 60 * 60 * 1000).toISOString().replace(/\.\d{3}Z$/, 'Z'),
          enabled: false,
          job_config: {
            ccr: {
              environment_id: environmentId,
              session_context: {
                model: args.model ?? optionalEnv('CLAUDE_DEFAULT_MODEL') ?? DEFAULT_MODEL,
                sources: [{ git_repository: { url: repositoryUrl(repo) } }],
                allowed_tools: ALLOWED_TOOLS,
              },
              events: [userEvent(args.prompt)],
            },
          },
        },
      })

      const run = await call<unknown>(`/v1/code/triggers/${created.id}/run`, {
        method: 'POST',
        body: {},
      })
      return {
        triggerId: created.id,
        url: `https://claude.ai/code/routines/${created.id}`,
        run,
      }
    },
  },

  {
    id: 'claude.followUpSession',
    name: 'Follow up on a Claude session',
    description:
      'Send another turn to a cloud session that is already running, as if you had typed it into the session.',
    safety: 'dangerous',
    args: z.object({
      sessionId: z.string().min(1).describe('Session id — from “List Claude sessions”'),
      prompt: z.string().min(1).describe('What to say next'),
    }),
    handler: async ({ sessionId, prompt }) => {
      const sent = await call<unknown>(
        `/v1/code/sessions/${encodeURIComponent(sessionId)}/events`,
        { method: 'POST', body: { events: [userEvent(prompt, sessionId)] } },
      )
      return { sessionId, sent }
    },
  },

  {
    id: 'claude.listSessions',
    name: 'List Claude sessions',
    description:
      'Cloud sessions, most recent first — this is where a session id for a follow-up comes from.',
    safety: 'dangerous',
    args: z.object({
      limit: z.number().int().min(1).max(100).default(20).describe('How many to fetch'),
    }),
    handler: ({ limit }) => call('/v1/code/sessions', { query: { limit } }),
  },
]
