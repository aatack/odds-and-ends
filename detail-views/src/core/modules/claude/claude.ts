import { randomUUID } from 'node:crypto'
import { existsSync, mkdtempSync, statSync } from 'node:fs'
import { homedir, tmpdir } from 'node:os'
import { basename, join } from 'node:path'
import { link, value, values, type AppEvent } from '../../graph/events.ts'
import { prEntityId } from '../../types.ts'
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
  /** The item it is started from: it goes under it, and the item points at it (`claudeSessionId`). */
  attachTo: string
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

/** Owner and name from a GitHub remote URL, ssh or https. */
function repoOf(remote: string): { owner: string; name: string } | null {
  const match = /github\.com[:/]([^/]+)\/([^/]+?)(?:\.git)?\s*$/.exec(remote)
  return match ? { owner: match[1], name: match[2] } : null
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

  /** A new session: its directory made, its item written, linked under the item it came from. */
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
      await run('git', ['worktree', 'add', '-b', branch, worktree], repo)
      cwd = worktree
    }
    const at = now()
    const events: AppEvent[] = [
      ...values(id, { type: 'claude.session', text: name, cwd, worktree, branch, repo, permissionMode: defaultPermissionMode }, at, me),
      link(ids.root, id, at, me),
    ]
    if (input.attachTo !== ids.root) {
      // A pill beside the item it came from, not a child under it.
      events.push(value(input.attachTo, 'pills', this.withPill(input.attachTo, id), at, me), value(input.attachTo, 'claudeSessionId', id, at, me))
    }
    this.context.owned.write(events)
    if (requested) {
      const known = this.context.data.get(ids.root).cwds
      const cwds = [requested, ...(Array.isArray(known) ? (known as string[]) : []).filter((one) => one !== requested)]
      this.context.data.set(ids.root, { cwds: cwds.slice(0, rememberedCwds) })
    }
    return events
  }

  /**
   * A prompt to a session: the prompt item under `parent` (and under the
   * session), and an empty response under it, now; Claude's answer fills the
   * response when it comes. Returns what was written now.
   */
  prompt(session: string, parent: string, text: string): AppEvent[] {
    const at = this.context.now()
    const promptId = randomUUID()
    const responseId = randomUUID()
    const events: AppEvent[] = [
      ...values(promptId, { type: 'claude.prompt', text, session }, at, me),
      link(parent, promptId, at, me),
      ...(parent === session ? [] : [link(session, promptId, at, me)]),
      ...values(responseId, { type: 'claude.response', text: '', running: true }, at, author),
      link(promptId, responseId, at, author),
    ]
    this.context.owned.write(events)
    const before = this.queues.get(session) ?? Promise.resolve()
    const next = before.then(() => this.answer(session, responseId, text))
    this.queues.set(session, next.catch(() => {}))
    return events
  }

  private async answer(session: string, responseId: string, text: string): Promise<void> {
    const data = this.context.data.get(session)
    const cwd = expandHome(String(data.cwd))
    const mode = String(data.permissionMode ?? defaultPermissionMode)
    const started = Boolean(data.started)
    const args = [
      '-p',
      text,
      '--output-format',
      'json',
      '--model',
      model,
      '--permission-mode',
      mode,
      ...(started ? ['--resume', session] : ['--session-id', session]),
    ]
    try {
      const out = JSON.parse(await this.context.run(claudeProgram(), args, cwd)) as { result?: string; is_error?: boolean; total_cost_usd?: number }
      // A failure is said in place of the answer: Claude's own words for it, if it gave any.
      const failed = out.is_error ? (out.result || 'Claude reported an error') : null
      this.context.owned.write([
        ...values(responseId, { text: failed ?? out.result ?? '', running: false, error: failed, cost: out.total_cost_usd ?? null }, this.context.now(), author),
        ...(started ? [] : [value(session, 'started', true, this.context.now(), author)]),
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
      this.context.owned.write(values(responseId, { text: failed, running: false, error: failed }, this.context.now(), author))
    }
    await this.linkPullRequest(session, cwd).catch(() => {})
  }

  /** The branch checked out where the session runs, if it has a PR: a pill on the session. */
  private async linkPullRequest(session: string, cwd: string): Promise<void> {
    const { run } = this.context
    const branch = (await run('git', ['rev-parse', '--abbrev-ref', 'HEAD'], cwd)).trim()
    const repo = repoOf(await run('git', ['remote', 'get-url', 'origin'], cwd))
    if (!branch || branch === 'HEAD' || !repo) return
    const query = `query($owner: String!, $name: String!, $branch: String!) { repository(owner: $owner, name: $name) {
      pullRequests(headRefName: $branch, first: 1, orderBy: { field: CREATED_AT, direction: DESC }) { nodes { url } } } }`
    const raw = await this.context.gh(['api', 'graphql', '-f', `query=${query}`, '-F', `owner=${repo.owner}`, '-F', `name=${repo.name}`, '-F', `branch=${branch}`])
    const url = (JSON.parse(raw) as { data?: { repository?: { pullRequests?: { nodes?: { url: string }[] } } } }).data?.repository?.pullRequests
      ?.nodes?.[0]?.url
    const pr = url && prEntityId(url)
    if (!pr || this.pillsOf(session).includes(pr)) return
    this.context.owned.write([value(session, 'pills', this.withPill(session, pr), this.context.now(), author)])
  }

  /** The ids an item's `pills` names. */
  private pillsOf(id: string): string[] {
    const pills = this.context.lens.read(id)?.data.pills
    return Array.isArray(pills) ? pills.filter((one): one is string => typeof one === 'string') : []
  }

  /** An item's `pills` with one more, at the end, unless it is there already. */
  private withPill(id: string, pill: string): string[] {
    const pills = this.pillsOf(id)
    return pills.includes(pill) ? pills : [...pills, pill]
  }
}
