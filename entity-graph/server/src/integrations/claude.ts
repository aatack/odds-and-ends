import { createHash } from 'crypto'
import { z } from 'zod'
import type { ToolDef } from '../../../src/core/source/index'
import { directory, run, type CommandResult } from './exec'

// Claude Code on this machine, through `claude --print`. One tool: a directory,
// a prompt, and a name for the conversation. It runs a headless session there,
// waits for it, and hands back the JSON the CLI printed.
//
// This replaces a stopgap that drove *cloud* sessions through the undocumented
// routines API. The repositories worth working on are on this machine, `claude`
// is already installed and already signed in, and one blocking call is a far
// smaller thing than a routine that can't be deleted.

const CLI = 'claude'

/**
 * Every run: print and exit, JSON out, and no permission prompts — there is
 * nobody here to answer one, and a session that can only read is not worth
 * starting. This is the whole reason the tool is `dangerous`: it is arbitrary
 * code execution on this machine, on purpose.
 */
const PRINT = ['--print', '--output-format', 'json', '--permission-mode', 'bypassPermissions']

/**
 * None. A session that is working is working, and there is no length that
 * distinguishes one wedged from one grinding through a large change — a half-hour
 * ceiling only ever killed the second kind. What interrupts a run is the app's
 * Stop, which is a decision rather than a guess.
 *
 * `run` passes this straight to `execFile`, where zero means no timer at all.
 */
const TIMEOUT_MS = 0

/** What the CLI says when the session id names nothing in this directory. */
const NO_SESSION = /no conversation found/i

const UUID = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i

/**
 * The CLI will take nothing but a UUID as a session id, and a caller has
 * something more useful to hand: an entity id, or a name for the conversation.
 * So anything that isn't a UUID is hashed into one. The same name always names
 * the same session, which is the whole contract — the caller passes a name it
 * has used before to carry on, and a new one to start fresh.
 */
function sessionUuid(name: string): string {
  const trimmed = name.trim()
  if (!trimmed) throw new Error('Give a session id — any string; an unused one starts a session')
  if (UUID.test(trimmed)) return trimmed.toLowerCase()
  const bytes = createHash('sha256').update(`entity-graph:claude:${trimmed}`).digest()
  // Version 4 and variant 1 in the two nibbles that say so, so the CLI's own
  // check passes; everything else is the digest.
  bytes[6] = (bytes[6] & 0x0f) | 0x40
  bytes[8] = (bytes[8] & 0x3f) | 0x80
  const hex = bytes.subarray(0, 16).toString('hex')
  return [
    hex.slice(0, 8),
    hex.slice(8, 12),
    hex.slice(12, 16),
    hex.slice(16, 20),
    hex.slice(20, 32),
  ].join('-')
}

const asJson = (text: string): Record<string, unknown> | null => {
  try {
    const parsed: unknown = JSON.parse(text)
    return parsed && typeof parsed === 'object' ? (parsed as Record<string, unknown>) : null
  } catch {
    return null
  }
}

/** The readable part of a result, if it has one. */
const saidBy = (output: Record<string, unknown> | null): string =>
  typeof output?.result === 'string' ? output.result.trim() : ''

/**
 * Why a run failed, in the words most worth reading. The CLI's own complaints go
 * to standard error; a turn that failed part-way still prints its JSON, and the
 * readable part of that is `result` — the rest is token accounting, and no use in
 * a toast.
 */
function complaint(result: CommandResult): string {
  const stderr = result.stderr.trim()
  if (stderr) return stderr
  return saidBy(asJson(result.stdout)) || `\`${CLI}\` exited with code ${result.exitCode}`
}

/** One invocation, with whichever of the two session flags is being tried. */
const attempt = (
  session: string[],
  prompt: string,
  cwd: string,
  system: string[],
): Promise<CommandResult> =>
  run(CLI, [...PRINT, ...session, ...system], { cwd, stdin: prompt, timeoutMs: TIMEOUT_MS })

export const CLAUDE_TOOLS: ToolDef[] = [
  {
    id: 'claude.runPrompt',
    name: 'Run a Claude prompt',
    description: [
      'Run a headless Claude Code session on this machine and wait for it to finish.',
      'Returns the CLI’s JSON verbatim — `result` is what Claude said, `session_id`',
      'the conversation it said it in, alongside cost and token counts.',
      '',
      'The session id is a name for the conversation *in that directory*: pass one',
      'you have used before to carry on where it left off, and an unused one to',
      'start fresh. It need not be a UUID.',
      '',
      '`systemPrompt` is appended to the session’s system prompt. It only means',
      'anything on the turn that starts a conversation — a resumed one already has',
      'the one it was started with — so send it with the first prompt or not at all.',
      '',
      'There is no time limit: a session runs until it is done. The session runs',
      'with permissions bypassed and can do anything you can.',
    ].join('\n'),
    safety: 'dangerous',
    args: z.object({
      path: z
        .string()
        .min(1)
        .describe('Directory to run in — `~/repos/local-helpers` works'),
      prompt: z
        .string()
        .min(1)
        .describe('What to ask. Passed on standard input, so it can be as long as you like'),
      sessionId: z
        .string()
        .min(1)
        .describe('Names the conversation in that directory; an unused name starts a new one'),
      systemPrompt: z
        .string()
        .optional()
        .describe('Appended to the system prompt. Only read on the turn that starts a session'),
    }),
    handler: async ({ path, prompt, sessionId, systemPrompt }) => {
      const cwd = directory(path)
      const session = sessionUuid(sessionId)
      // The one thing here that has to go in the argument vector: the CLI takes it
      // no other way. Keep it to rules and ids — anything long belongs in the
      // prompt, which goes over standard input.
      const system = systemPrompt ? ['--append-system-prompt', systemPrompt] : []

      // There is no "resume it, or start it if it isn't there" flag, so the two
      // are tried in turn. Resuming goes first because being wrong about it is
      // free: the CLI looks for the transcript before it does anything else, and
      // says so in a line and an exit code without reaching the API.
      let result = await attempt(['--resume', session], prompt, cwd, system)
      if (result.exitCode !== 0 && NO_SESSION.test(complaint(result))) {
        result = await attempt(['--session-id', session], prompt, cwd, system)
      }
      if (result.exitCode !== 0) throw new Error(complaint(result))

      const output = asJson(result.stdout)
      if (!output) {
        throw new Error(`\`${CLI}\` did not return JSON: ${result.stdout.trim().slice(0, 300)}`)
      }
      // A session can fail inside a turn and still exit cleanly. That is a failed
      // call, not a result to hand back as though it worked.
      if (output.is_error) throw new Error(saidBy(output) || `\`${CLI}\` reported an error`)
      return output
    },
  },
]
