import { randomUUID } from 'node:crypto'
import { copyFileSync, existsSync, mkdirSync, mkdtempSync, readdirSync, statSync } from 'node:fs'
import { homedir, tmpdir } from 'node:os'
import { basename, join } from 'node:path'
import { link, value, values, type AppEvent } from '../../graph/events.ts'
import type { Entity } from '../../types.ts'
import { mcpConfig, mcpName, type McpAddress } from '../../mcp.ts'
import type { Module, ModuleContext } from '../module.ts'
import { me } from '../tasks/tasks.ts'
import { claudeIds as ids, claudeView } from './view.ts'

/** The author of what Claude writes back: not mine to undo. */
export const author = 'claude'
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

/** A session's directory, made or chosen: where it runs, and what it is a worktree of. */
interface Place {
  cwd: string
  /** The worktree it runs in, or null. */
  worktree: string | null
  branch: string | null
  repo: string | null
  /** The directory I chose, when it runs in a worktree made from it. */
  origin: string | null
  /** The worktree's repo on GitHub (`owner/name`), when its `origin` remote is there: where its branch is. */
  github: string | null
}

/** `owner/name` of a remote's URL on GitHub (`git@github.com:o/n.git`, `https://github.com/o/n`), or null. */
export function githubRepo(url: string): string | null {
  const match = /github\.com[:/]([^/\s]+\/[^/\s]+?)(?:\.git)?\/?$/.exec(url.trim())
  return match ? match[1] : null
}

/** Where Claude Code keeps its files (its sessions' transcripts, under `projects/`). */
function claudeHome(): string {
  return process.env.CLAUDE_CONFIG_DIR ?? join(homedir(), '.claude')
}

/** The folder Claude Code keeps a directory's sessions in: the path with everything but letters and digits as `-`. */
function projectDir(cwd: string): string {
  return join(claudeHome(), 'projects', cwd.replace(/[^a-zA-Z0-9]/g, '-'))
}

/**
 * Takes a session's transcript to where Claude Code looks for it from `cwd`
 * (it resumes only sessions of the directory it runs in). False when there is
 * no transcript to take: the session then starts afresh there.
 */
function carryTranscript(session: string, cwd: string): boolean {
  const projects = join(claudeHome(), 'projects')
  const file = `${session}.jsonl`
  const from = existsSync(projects) ? readdirSync(projects).map((dir) => join(projects, dir, file)).find((path) => existsSync(path)) : undefined
  if (!from) return false
  const to = projectDir(cwd)
  if (join(to, file) === from) return true
  mkdirSync(to, { recursive: true })
  copyFileSync(from, join(to, file))
  return true
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
    const id = randomUUID()
    const name = input.name.trim() || 'Session'
    const requested = this.directory(input.cwd)
    const place = requested ? await this.place(requested, input.worktree, name, id) : { cwd: mkdtempSync(join(tmpdir(), 'claude-')), worktree: null, branch: null, repo: null, origin: null, github: null }
    const at = this.context.now()
    const events: AppEvent[] = [
      ...values(id, { type: 'claude.session', text: name, ...place, permissionMode: defaultPermissionMode }, at, me),
      link(ids.root, id, at, me),
    ]
    this.context.owned.write(events)
    if (requested) await this.remember(requested)
    return events
  }

  /**
   * A session's directory changed: to `cwd` as it is, or to a new worktree of
   * its repo (the directory chosen kept as `origin`). Its conversation goes
   * with it, so the next prompt resumes it there. Not while Claude answers.
   */
  async setDirectory(session: string, cwd: string, worktree: boolean): Promise<AppEvent[]> {
    const data = this.context.data.get(session)
    if (typeof data.runningSince === 'number') throw new Error('Claude is answering in this session: wait for it to finish')
    const requested = this.directory(cwd)
    if (!requested) throw new Error('No directory given')
    const place = await this.place(requested, worktree, String(data.text ?? 'session'), session)
    // Claude Code resumes a session only from its own directory: its transcript goes too.
    const started = Boolean(data.started) && carryTranscript(session, place.cwd)
    const events = values(session, { ...place, started }, this.context.now(), me)
    this.context.owned.write(events)
    await this.remember(requested)
    return events
  }

  /** A directory I typed, `~` expanded; refused if it isn't one. Empty for none. */
  private directory(input: string): string {
    const requested = expandHome(input.trim())
    if (requested && !(existsSync(requested) && statSync(requested).isDirectory())) throw new Error(`No such directory: ${requested}`)
    return requested
  }

  /** `requested` as it is, or a new worktree of its repo, on a branch of its own named after the session. */
  private async place(requested: string, worktree: boolean, name: string, id: string): Promise<Place> {
    if (!worktree) return { cwd: requested, worktree: null, branch: null, repo: null, origin: null, github: null }
    const { run } = this.context
    const short = id.slice(0, 8)
    // Outside every repo (the app's own data directory), so no repo ever commits it.
    const repo = (await run('git', ['rev-parse', '--show-toplevel'], requested)).trim()
    const branch = `claude/${slug(name)}-${short}`
    // A second worktree for the same session gets a name of its own.
    let path = join(this.context.dataDir, 'worktrees', `${basename(repo)}-${short}`)
    for (let n = 2; existsSync(path); n++) path = join(this.context.dataDir, 'worktrees', `${basename(repo)}-${short}-${n}`)
    // From the remote's newest default branch, fetched now; my checkout is left as it is.
    const main = await run('git', ['symbolic-ref', '--short', 'refs/remotes/origin/HEAD'], repo)
      .then((ref) => ref.trim().replace(/^origin\//, ''))
      .catch(() => 'master')
    await run('git', ['fetch', 'origin', main], repo)
    const branchName = await run('git', ['rev-parse', '--verify', '--quiet', `refs/heads/${branch}`], repo).then(
      () => `${branch}-${Date.now().toString(36)}`,
      () => branch,
    )
    await run('git', ['worktree', 'add', '--no-track', '-b', branchName, path, `origin/${main}`], repo)
    const github = await run('git', ['remote', 'get-url', 'origin'], repo).then(githubRepo, () => null)
    return { cwd: path, worktree: path, branch: branchName, repo, origin: requested, github }
  }

  /** A directory I chose, remembered first for next time, and whether it is in a git repo (so a worktree can be offered). */
  private async remember(dir: string): Promise<void> {
    const root = this.context.data.get(ids.root)
    const cwds = [dir, ...(Array.isArray(root.cwds) ? (root.cwds as string[]) : []).filter((one) => one !== dir)].slice(0, rememberedCwds)
    const known = Array.isArray(root.repos) ? (root.repos as string[]) : []
    const isRepo = await this.isRepo(dir)
    const repos = [...known.filter((one) => one !== dir && cwds.includes(one)), ...(isRepo ? [dir] : [])]
    this.context.data.set(ids.root, { cwds, repos })
  }

  /** Looks again at which remembered directories are in a git repo (at start: some were remembered before this was kept). */
  async refreshRepos(): Promise<void> {
    const root = this.context.data.get(ids.root)
    const cwds = Array.isArray(root.cwds) ? (root.cwds as string[]) : []
    const repos: string[] = []
    for (const dir of cwds) if (await this.isRepo(dir)) repos.push(dir)
    const known = Array.isArray(root.repos) ? (root.repos as string[]) : null
    if (!known || known.join('\0') !== repos.join('\0')) this.context.data.set(ids.root, { repos })
  }

  private isRepo(dir: string): Promise<boolean> {
    return this.context.run('git', ['rev-parse', '--show-toplevel'], expandHome(dir)).then(
      () => true,
      () => false,
    )
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
    // The first prompt says what the session is in context with; later ones resume it, so it knows.
    const started = Boolean(this.context.data.get(session).started)
    const lines = [started ? null : contextLine(session, this.context.lens), notes].filter((line) => line !== null)
    await ask(this.context, session, responseId, lines.length ? `${lines.join('\n')}\n\n${text}` : text)
  }
}

/**
 * Runs one prompt of a session (any item standing as one: its id is Claude's
 * session id, its `cwd`, `permissionMode` and `started` say how) with
 * `claude -p`, and puts the answer, or what went wrong, in `responseId`. The
 * session says it is running, and since when, while it does.
 */
export async function ask(context: ModuleContext, session: string, responseId: string, prompt: string): Promise<void> {
  const data = context.data.get(session)
  const cwd = expandHome(String(data.cwd))
  const mode = String(data.permissionMode ?? defaultPermissionMode)
  const started = Boolean(data.started)
  const since = context.now()
  context.owned.write([value(session, 'runningSince', since, since, author)])
  const args = [
    '-p',
    prompt,
    '--output-format',
    'json',
    '--model',
    model,
    '--permission-mode',
    mode,
    ...(started ? ['--resume', session] : ['--session-id', session]),
    // The app's own MCP server (read-only), its tools allowed. Last: both flags take lists.
    ...mcpArgs(context.mcp()),
  ]
  try {
    const out = JSON.parse(await context.run(claudeProgram(), args, cwd)) as { result?: string; is_error?: boolean; total_cost_usd?: number }
    // A failure is said in place of the answer: Claude's own words for it, if it gave any.
    const failed = out.is_error ? (out.result || 'Claude reported an error') : null
    const at = context.now()
    // The answer whole, in the one response item (`b` splits it if I want).
    context.owned.write([
      ...values(responseId, { text: failed ?? out.result ?? '', running: false, error: failed, cost: out.total_cost_usd ?? null }, at, author),
      ...(started ? [] : [value(session, 'started', true, context.now(), author)]),
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
    const at = context.now()
    context.owned.write([
      ...values(responseId, { text: failed, running: false, error: failed }, at, author),
      value(session, 'runningSince', null, at, author),
    ])
  }
}
