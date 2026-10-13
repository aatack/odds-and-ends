import { randomUUID } from 'node:crypto'
import { existsSync, mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { basename, join, resolve, sep } from 'node:path'
import { link, value, values, type AppEvent } from '../../graph/events.ts'
import { prEntityId, type Entity } from '../../types.ts'
import { ask, author as claudeAuthor, expandHome, githubRepo } from '../claude/claude.ts'
import type { Module, ModuleContext } from '../module.ts'
import { me } from '../tasks/tasks.ts'
import { branchEntityId, branchOfUrl } from '../github/view.ts'
import { messageOfUrl } from '../slack/view.ts'
import { answering, promptPart, topicsIds as ids, topicsView, unsent } from './view.ts'

/** The author of what the app writes in a topic on its own (the prompt to commit): not mine to undo. */
export const appAuthor = 'module'

/** What the app asks Claude after an answer that left changes uncommitted. */
export const commitPrompt = 'There are uncommitted changes in your working directory. Commit them.'

/** A file pasted into a topic: its name, its type, and what is in it, base64. */
export interface PastedFile {
  name: string
  mime: string
  data: string
}

/**
 * Topics, each a chat I write into as I go: a message is only a note under
 * it, until a dispatch sends every message since the last one to Claude at
 * once, in the topic's directory.
 */
export class Topics implements Module {
  readonly view = topicsView

  private readonly context: ModuleContext
  /** One dispatch at a time per topic, in the order made. */
  private readonly queues = new Map<string, Promise<unknown>>()
  /** Topics whose dispatch isn't over: answered, but the follow-ups (commit, branch) still to come. */
  private readonly busy = new Set<string>()

  constructor(context: ModuleContext) {
    this.context = context
  }

  /** A new topic, named, under the topics root. */
  create(name: string): AppEvent[] {
    const text = name.trim()
    if (!text) throw new Error('A topic needs a name')
    const id = randomUUID()
    const at = this.context.now()
    const events = [...values(id, { type: 'topic', text }, at, me), link(ids.root, id, at, me)]
    this.context.owned.write(events)
    return events
  }

  /** The box under a topic: a message in it (a note), sent to nobody yet. */
  async submit(entity: Entity, text: string): Promise<AppEvent[]> {
    if (entity.type !== 'topic') return []
    const events = this.message(entity.id, text, this.context.now())
    this.context.owned.write(events)
    return events
  }

  /**
   * Text pasted into a topic, as what it is: a link to a PR, a Slack message
   * or a GitHub branch is that item, linked under the topic; anything else
   * (another link too) is a message of its text.
   */
  paste(topic: string, text: string): AppEvent[] {
    if (this.context.lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    const trimmed = text.trim()
    if (!trimmed) return []
    const at = this.context.now()
    const item = /^https?:\/\/\S+$/.test(trimmed) ? (prEntityId(trimmed) ?? messageOfUrl(trimmed) ?? branchOfUrl(trimmed)) : null
    const events = item ? [link(topic, item, at, me)] : this.message(topic, trimmed, at)
    this.context.owned.write(events)
    return events
  }

  /** A file pasted into a topic: kept under the app's data directory, as a file item under the topic. */
  attach(topic: string, file: PastedFile): AppEvent[] {
    if (this.context.lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    const id = randomUUID()
    const name = basename(file.name) || 'pasted'
    const dir = join(this.context.dataDir, 'files', id)
    mkdirSync(dir, { recursive: true })
    const path = join(dir, name)
    const bytes = Buffer.from(file.data, 'base64')
    writeFileSync(path, bytes)
    const at = this.context.now()
    const events = [...values(id, { type: 'file', text: name, path, mime: file.mime, size: bytes.length }, at, me), link(topic, id, at, me)]
    this.context.owned.write(events)
    return events
  }

  private message(topic: string, text: string, at: number): AppEvent[] {
    const id = randomUUID()
    return [value(id, 'text', text, at, me), link(topic, id, at, me)]
  }

  /**
   * Sends every message since the last dispatch to Claude as one prompt, and
   * puts a response after them for the answer to fill. It runs in the
   * topic's `cwd`; a topic without one is given a new temporary directory,
   * kept, so later dispatches resume the conversation there. Refused while
   * Claude is still answering the one before.
   */
  dispatch(topic: string): AppEvent[] {
    const lens = this.context.lens
    if (lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    // Never two prompts at once: the messages wait for the next dispatch.
    if (answering(topic, lens) || this.busy.has(topic)) throw new Error('Claude is still answering')
    const messages = unsent(topic, lens)
    if (!messages.length) throw new Error('Nothing new to send')
    const prompt = messages.map((id) => promptPart(id, lens)).join('\n\n')
    const at = this.context.now()
    const responseId = randomUUID()
    const cwd = this.context.data.get(topic).cwd
    const events: AppEvent[] = [
      // Claude's, like `started`: undoing my last edit doesn't take the conversation's directory away.
      ...(typeof cwd === 'string' && cwd ? [] : [value(topic, 'cwd', mkdtempSync(join(tmpdir(), 'claude-')), at, claudeAuthor)]),
      ...values(responseId, { type: 'claude.response', text: '', running: true }, at, claudeAuthor),
      // A moment later, so it sorts after the last message even when made the same instant.
      link(topic, responseId, at + 1, claudeAuthor),
    ]
    this.context.owned.write(events)
    const before = this.queues.get(topic) ?? Promise.resolve()
    this.busy.add(topic)
    const next = before.then(() => this.answer(topic, responseId, prompt)).finally(() => this.busy.delete(topic))
    this.queues.set(topic, next.catch(() => {}))
    return events
  }

  /**
   * Claude answers a dispatch. Then, if that left changes in its directory
   * uncommitted, it is asked once to commit them (a message of the app's
   * own, and its answer); and the branch checked out there, unless it is
   * the default one, goes into the chat as its item.
   */
  private async answer(topic: string, responseId: string, prompt: string): Promise<void> {
    await ask(this.context, topic, responseId, prompt)
    if (this.context.lens.read(responseId)?.data.error) return
    const cwd = this.cwdOf(topic)
    if (cwd && (await this.git(cwd, ['status', '--porcelain']))?.trim()) {
      const at = this.context.now()
      const followUp = randomUUID()
      const message = randomUUID()
      this.context.owned.write([
        value(message, 'text', commitPrompt, at, appAuthor),
        link(topic, message, at, appAuthor),
        ...values(followUp, { type: 'claude.response', text: '', running: true }, at, claudeAuthor),
        link(topic, followUp, at + 1, claudeAuthor),
      ])
      await ask(this.context, topic, followUp, commitPrompt)
    }
    if (cwd) await this.noteBranch(topic, cwd)
  }

  /** The branch checked out in `cwd`, unless detached or the default one, as a branch item in the topic (once), and on it as `branch`. */
  private async noteBranch(topic: string, cwd: string): Promise<void> {
    const branch = (await this.git(cwd, ['rev-parse', '--abbrev-ref', 'HEAD']))?.trim()
    if (!branch || branch === 'HEAD' || branch === (await this.defaultBranch(cwd))) return
    const remote = await this.git(cwd, ['remote', 'get-url', 'origin'])
    const github = remote ? githubRepo(remote) : null
    if (!github) return
    const id = branchEntityId(github, branch)
    const data = this.context.data.get(topic)
    const at = this.context.now()
    const events: AppEvent[] = [
      ...(data.branch === branch && data.github === github ? [] : values(topic, { branch, github }, at, claudeAuthor)),
      // Two moments later, so it sorts after the answer even when written the same instant (an answer is linked a moment after its message).
      ...(this.context.lens.children(topic).includes(id) ? [] : [link(topic, id, at + 2, claudeAuthor)]),
    ]
    if (events.length) this.context.owned.write(events)
  }

  /**
   * What would be lost resolving a topic: in its worktree (only one proved
   * to be one), changes not committed, commits not pushed, or not merged
   * into the default branch. Empty when there is nothing, or no worktree.
   */
  async resolveWarnings(topic: string): Promise<string[]> {
    const worktree = await this.provenWorktree(topic)
    if (!worktree) return []
    const { path } = worktree
    const warnings: string[] = []
    const status = await this.git(path, ['status', '--porcelain'])
    if (status === null) return [`Could not read the state of ${path}`]
    if (status.trim()) warnings.push(`Uncommitted changes in ${path}`)
    const count = (out: string | null) => (out ?? '').split('\n').filter((line) => line.trim()).length
    const unpushed = await this.git(path, ['log', '--oneline', 'HEAD', '--not', '--remotes'])
    if (unpushed === null) warnings.push('Could not tell which commits are pushed')
    else if (count(unpushed)) warnings.push(`${count(unpushed)} commit${count(unpushed) === 1 ? '' : 's'} not pushed`)
    const main = await this.defaultBranch(path)
    const unmerged = main ? await this.git(path, ['log', '--oneline', `origin/${main}..HEAD`]) : null
    if (unmerged === null) warnings.push('Could not tell which commits are merged')
    else if (count(unmerged) && !count(unpushed)) warnings.push(`${count(unmerged)} commit${count(unmerged) === 1 ? '' : 's'} not merged into ${main}`)
    return warnings
  }

  /**
   * Resolves a topic: it leaves the list (its link from the root undone, so
   * it is kept), and its worktree is removed, if it has one proved to be
   * one; any other directory is left alone. While there is work that would
   * be lost (`resolveWarnings`) it is refused, unless `force`. Its branch is
   * kept. Not while Claude answers.
   */
  async resolve(topic: string, force: boolean): Promise<AppEvent[]> {
    if (this.context.lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    if (answering(topic, this.context.lens) || this.busy.has(topic)) throw new Error('Claude is still answering')
    const worktree = await this.provenWorktree(topic)
    if (worktree) {
      if (!force) {
        const warnings = await this.resolveWarnings(topic)
        if (warnings.length) throw new Error(warnings.join('; '))
      }
      await this.context.run('git', ['worktree', 'remove', ...(force ? ['--force'] : []), worktree.path], worktree.repo)
    }
    const at = this.context.now()
    const events: AppEvent[] = [link(ids.root, topic, at, me, 1), value(topic, 'resolved', at, at, me), ...(worktree ? [value(topic, 'worktree', null, at, me)] : [])]
    this.context.owned.write(events)
    return events
  }

  /**
   * The topic's worktree, only if it is provably one the app made: its
   * `worktree` is under the app's own worktrees directory, exists, is the top
   * of a checkout, and its repo lists it as a worktree. Null otherwise.
   */
  private async provenWorktree(topic: string): Promise<{ path: string; repo: string } | null> {
    const data = this.context.data.get(topic)
    if (typeof data.worktree !== 'string' || !data.worktree || typeof data.repo !== 'string' || !data.repo) return null
    const path = resolve(data.worktree)
    const root = resolve(this.context.dataDir, 'worktrees')
    if (!path.startsWith(root + sep) || !existsSync(path)) return null
    if ((await this.git(path, ['rev-parse', '--show-toplevel']))?.trim() !== path) return null
    const listed = await this.git(data.repo, ['worktree', 'list', '--porcelain'])
    if (!listed?.split('\n').includes(`worktree ${path}`)) return null
    return { path, repo: data.repo }
  }

  /** The default branch of `cwd`'s remote, or null. */
  private async defaultBranch(cwd: string): Promise<string | null> {
    return (await this.git(cwd, ['symbolic-ref', '--short', 'refs/remotes/origin/HEAD']))?.trim().replace(/^origin\//, '') || null
  }

  /** git's output in `cwd`, or null if it failed (not a repo, say). */
  private git(cwd: string, args: string[]): Promise<string | null> {
    return this.context.run('git', args, cwd).then(
      (out) => out,
      () => null,
    )
  }

  private cwdOf(topic: string): string | null {
    const cwd = this.context.data.get(topic).cwd
    return typeof cwd === 'string' && cwd ? expandHome(cwd) : null
  }
}
