import { randomUUID } from 'node:crypto'
import { existsSync, mkdtempSync, statSync } from 'node:fs'
import { homedir, tmpdir } from 'node:os'
import { basename, join } from 'node:path'
import { link, value, values, type AppEvent } from '../../graph/events.ts'
import type { Entity } from '../../types.ts'
import { mcpConfig, mcpName, type McpAddress } from '../../mcp.ts'
import type { Module, ModuleContext } from '../module.ts'
import { me } from '../tasks/tasks.ts'
import { claudeIds as ids, claudeView } from './view.ts'

/** The author of what Claude writes back: not mine to undo. */
const author = 'claude'
/** How a session decides what it may do without asking: Claude Code's auto mode. */
export const defaultPermissionMode = 'auto'
/** The model every session runs on, for now. */
export const model = 'claude-opus-5-5'
/** How many working directories the new-session dialog remembers. */
const rememberedCwds = 10

export interface NewSession {
  name: string
  /** Where to run it; blank for a new temporary directory. */
  cwd: string
  /** Run in a new git worktree of `cwd`'s repo, on a branch of its own. */
  worktree: boolean
}

/** `~` and `~/x` as the home directory: what a shell would make of them, which a spawned program never does. */
export function expandHome(path: string): string {
  return path === '~' ? homedir() : path.startsWith('~/') ? join(homedir(), path.slice(2)) : path
}

/**
 * The claude program: on the PATH, or where its installer puts it. An app
 * started from a launcher may not have the shell's PATH, so look there too.
 */
function claudeProgram(): string {
  for (const dir of (process.env.PATH ?? '').split(':')) {
    if (dir && existsSync(join(dir, 'claude'))) return 'claude'
  }
  for (const candidate of [join(homedir(), '.local', 'bin', 'claude'), join(homedir(), '.claude', 'local', 'claude')]) {
    if (existsSync(candidate)) return candidate
  }
  return 'claude'
}

function slug(text: string): string {
  return (
    text
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-+|-+$/g, '')
      .slice(0, 40) || 'session'
  )
}

/**
 * What a session's first prompt starts with when the session has a context:
 * the other items in it, by id, for Claude to read through the app's MCP
 * server (whose instructions say what the line means). Null when there are none.
 */
export function contextLine(session: string, lens: ModuleContext['lens']): string | null {
  const context = lens.read(session)?.data.context
  if (typeof context !== 'string' || !context) return null
  const others = lens.children(context).filter((id) => id !== session)
  return others.length ? `[Session context: ${others.join(', ')}]` : null
}

/**
 * The notes I added to a session's chat since its last prompt (its children
 * with no type, after the newest prompt), by id: told to Claude with the next
 * prompt, so it can read them. Null when there are none.
 */
export function notesLine(session: string, lens: ModuleContext['lens']): string | null {
  const chat = lens.children(session)
  const lastPrompt = chat.findLastIndex((id) => lens.read(id)?.type === 'claude.prompt')
  const notes = chat.slice(lastPrompt + 1).filter((id) => lens.read(id)?.type === 'note')
  return notes.length ? `[Notes since the last prompt: ${notes.join(', ')}]` : null
}

/** The app's MCP server for a session, and its tools allowed without asking (they read, and `merge_context` only links). */
function mcpArgs(address: McpAddress | null): string[] {
  return address ? ['--allowedTools', `mcp__${mcpName}`, '--mcp-config', mcpConfig(address)] : []
}

/**
 * Claude Code sessions, run with `claude -p` in a directory of the session's
 * own: a temporary one, the one I gave, or a new git worktree of its repo.
 * Everything is owned: sessions, prompts and responses are mine to keep.
 */
export class Claude implements Module {
  readonly view = claudeView

  private readonly context: ModuleContext
  /** One prompt at a time per session, in the order asked. */
  private readonly queues = new Map<string, Promise<unknown>>()

  constructor(context: ModuleContext) {
    this.context = context
  }

  /** A new session: its directory made, its item written, linked under the Claude root and nowhere else. */
  async createSession(input: NewSession): Promise<AppEvent[]> {
    const { run, now } = this.context
    const id = randomUUID()
    const short = id.slice(0, 8)
    const name = input.name.trim() || 'Session'
    const requested = expandHome(input.cwd.trim())
    if (requested && !(existsSync(requested) && statSync(requested).isDirectory())) {
      throw new Error(`No such directory: ${requested}`)
    }
    let cwd: string
    let worktree: string | null = null
    let branch: string | null = null
    let repo: string | null = null
    if (!requested) {
      cwd = mkdtempSync(join(tmpdir(), 'claude-'))
    } else if (!input.worktree) {
      cwd = requested
    } else {
      // Outside every repo (the app's own data directory), so no repo ever commits it.
      repo = (await run('git', ['rev-parse', '--show-toplevel'], requested)).trim()
      branch = `claude/${slug(name)}-${short}`
      worktree = join(this.context.dataDir, 'worktrees', `${basename(repo)}-${short}`)
      // From the remote's newest default branch, fetched now; my checkout is left as it is.
      const main = await run('git', ['symbolic-ref', '--short', 'refs/remotes/origin/HEAD'], repo)
        .then((ref) => ref.trim().replace(/^origin\//, ''))
        .catch(() => 'master')
      await run('git', ['fetch', 'origin', main], repo)
      await run('git', ['worktree', 'add', '--no-track', '-b', branch, worktree, `origin/${main}`], repo)
      cwd = worktree
    }
    const at = now()
    const events: AppEvent[] = [
      ...values(id, { type: 'claude.session', text: name, cwd, worktree, branch, repo, permissionMode: defaultPermissionMode }, at, me),
      link(ids.root, id, at, me),
    ]
    this.context.owned.write(events)
    if (requested) {
      const known = this.context.data.get(ids.root).cwds
      const cwds = [requested, ...(Array.isArray(known) ? (known as string[]) : []).filter((one) => one !== requested)]
      this.context.data.set(ids.root, { cwds: cwds.slice(0, rememberedCwds) })
    }
    return events
  }

  /**
   * A prompt to a session: the prompt item under the session, and an empty
   * response after it, now; Claude's answer fills the response when it comes.
   * The session's children are its chat, in order. Returns what was written now.
   */
  prompt(session: string, text: string): AppEvent[] {
    const at = this.context.now()
    // Before this prompt is written, so the notes are those since the one before.
    const notes = notesLine(session, this.context.lens)
    const promptId = randomUUID()
    const responseId = randomUUID()
    const events: AppEvent[] = [
      ...values(promptId, { type: 'claude.prompt', text, session }, at, me),
      link(session, promptId, at, me),
      ...values(responseId, { type: 'claude.response', text: '', running: true }, at, author),
      // A moment later, so it sorts after the prompt.
      link(session, responseId, at + 1, author),
    ]
    this.context.owned.write(events)
    const before = this.queues.get(session) ?? Promise.resolve()
    const next = before.then(() => this.answer(session, responseId, text, notes))
    this.queues.set(session, next.catch(() => {}))
    return events
  }

  /** The box under a session's chat: a prompt to it. */
  async submit(entity: Entity, text: string): Promise<AppEvent[]> {
    return entity.type === 'claude.session' ? this.prompt(entity.id, text) : []
  }

  private async answer(session: string, responseId: string, text: string, notes: string | null): Promise<void> {
    const data = this.context.data.get(session)
    const cwd = expandHome(String(data.cwd))
    const mode = String(data.permissionMode ?? defaultPermissionMode)
    const started = Boolean(data.started)
    // The session says it is running, and since when, while an answer is under way.
    const since = this.context.now()
    this.context.owned.write([value(session, 'runningSince', since, since, author)])
    // The first prompt says what the session is in context with; later ones resume it, so it knows.
    const lines = [started ? null : contextLine(session, this.context.lens), notes].filter((line) => line !== null)
    const args = [
      '-p',
      lines.length ? `${lines.join('\n')}\n\n${text}` : text,
      '--output-format',
      'json',
      '--model',
      model,
      '--permission-mode',
      mode,
      ...(started ? ['--resume', session] : ['--session-id', session]),
      // The app's own MCP server (read-only), its tools allowed. Last: both flags take lists.
      ...mcpArgs(this.context.mcp()),
    ]
    try {
      const out = JSON.parse(await this.context.run(claudeProgram(), args, cwd)) as { result?: string; is_error?: boolean; total_cost_usd?: number }
      // A failure is said in place of the answer: Claude's own words for it, if it gave any.
      const failed = out.is_error ? (out.result || 'Claude reported an error') : null
      const at = this.context.now()
      // The answer whole, in the one response item (`b` splits it if I want).
      this.context.owned.write([
        ...values(responseId, { text: failed ?? out.result ?? '', running: false, error: failed, cost: out.total_cost_usd ?? null }, at, author),
        ...(started ? [] : [value(session, 'started', true, this.context.now(), author)]),
        value(session, 'runningSince', null, at, author),
      ])
    } catch (error) {
      // Claude may still have said why, as JSON, on its way out.
      const said = (() => {
        try {
          return (JSON.parse(String((error as { stdout?: string }).stdout ?? '')) as { result?: string }).result
        } catch {
          return undefined
        }
      })()
      const message = error instanceof Error ? error.message : String(error)
      const failed = said ? `${said}\n\n${message}` : message
      const at = this.context.now()
      this.context.owned.write([
        ...values(responseId, { text: failed, running: false, error: failed }, at, author),
        value(session, 'runningSince', null, at, author),
      ])
    }
  }
}
