import { randomUUID } from 'node:crypto'
import { existsSync, mkdirSync, mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { basename, join, resolve, sep } from 'node:path'
import { link, value, values, type AppEvent } from '../../graph/events.ts'
import { prEntityId, type Entity } from '../../types.ts'
import { ask, author as claudeAuthor, carryTranscript, expandHome, githubRepo, type Place } from '../claude/claude.ts'
import type { Module, ModuleContext } from '../module.ts'
import { me } from '../tasks/tasks.ts'
import { prOfBranch } from '../github/github.ts'
import { branchOfUrl } from '../github/view.ts'
import { conversationOfUrl, messageOfUrl } from '../slack/view.ts'
import { answering, isTaskTopic, latestPath, promptPart, sendTargets, topicDisplays, topicsIds as ids, topicsView, unsent, waitOn, waitsOf, type SendTarget, type TopicDisplay, type Wait } from './view.ts'

/** The author of what the app writes in a topic on its own (the prompt to commit): not mine to undo. */
export const appAuthor = 'module'

/** What the app asks Claude after an answer that left changes uncommitted. */
export const commitPrompt = 'There are uncommitted changes in your working directory. Commit them.'

/** Where a path item says a topic runs: its directory, and the worktree it is, if one. */
function placeOf(data: Record<string, unknown>): Place {
  const text = (key: string) => (typeof data[key] === 'string' && data[key] ? (data[key] as string) : null)
  return { cwd: String(data.cwd), worktree: text('worktree'), branch: text('branch'), repo: text('repo'), origin: text('origin'), github: text('github') }
}

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

  /** Puts two items in one context (`Core.shareContext`), as Claude's. */
  private readonly share: (first: string, second: string) => void

  constructor(context: ModuleContext, share: (first: string, second: string) => void = () => {}) {
    this.context = context
    this.share = share
  }

  /** A new topic, named, under the topics root: a task (a chat with Claude), or thoughts (a tree of notes). */
  create(name: string, display: TopicDisplay = 'task'): AppEvent[] {
    const text = name.trim()
    if (!text) throw new Error('A topic needs a name')
    if (!topicDisplays.includes(display)) throw new Error(`No such kind of topic: ${display}`)
    const id = randomUUID()
    const at = this.context.now()
    const events = [...values(id, { type: 'topic', text, ...(display === 'thoughts' ? { display } : {}) }, at, me), link(ids.root, id, at, me)]
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
    const item = /^https?:\/\/\S+$/.test(trimmed) ? (prEntityId(trimmed) ?? messageOfUrl(trimmed) ?? conversationOfUrl(trimmed) ?? branchOfUrl(trimmed)) : null
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

  /** A path chosen for a topic (a directory, or a new worktree made from one): a `path` item under it. The next dispatch runs there. */
  addPath(topic: string, place: Place, id: string): AppEvent[] {
    this.task(topic)
    const at = this.context.now()
    const events = [...values(id, { type: 'path', text: place.cwd, ...place }, at, me), link(topic, id, at, me)]
    this.context.owned.write(events)
    return events
  }

  // --- Waits, and Slack sent from the box -------------------------------------------

  /**
   * Puts a topic down until something comes into each of `on` (Slack
   * conversations, or threads by their first message), or each other topic
   * in `on` is resolved, from now. One already waited for is waited for from
   * now again.
   */
  wait(topic: string, on: string[]): AppEvent[] {
    const since = this.context.now()
    const added = on.map((id): Wait => {
      if (/^slack:(conv:[A-Z0-9]+|msg:[A-Z0-9]+:\d+\.\d+)$/.test(id)) return { slack: id, since }
      // Another topic: back once it is resolved.
      if (id !== topic && this.context.lens.read(id)?.type === 'topic') return { topic: id, since }
      // A PR: back once it is blocked by me, merged or closed.
      if (id.startsWith('github:pr:') && this.context.lens.read(id)?.type === 'github.pr') return { pr: id, since }
      throw new Error('Only a Slack conversation or thread, another topic, or a PR can be waited on')
    })
    return this.setWaits(topic, (waits) => [...waits.filter((wait) => !on.includes(waitOn(wait))), ...added])
  }

  /** Puts a topic down until `until` (Unix ms), in place of any snooze it had. */
  snooze(topic: string, until: number): AppEvent[] {
    if (!Number.isFinite(until) || until <= this.context.now()) throw new Error('A snooze must end in the future')
    return this.setWaits(topic, (waits) => [...waits.filter((wait) => wait.until === undefined), { until, since: this.context.now() }])
  }

  /** Takes a wait away. */
  unwait(topic: string, on: string): AppEvent[] {
    return this.setWaits(topic, (waits) => waits.filter((wait) => waitOn(wait) !== on))
  }

  /** Passes over a message that came into a wait: it is no longer offered. */
  dismiss(topic: string, on: string, message: string): AppEvent[] {
    return this.setWaits(topic, (waits) => waits.map((wait) => (waitOn(wait) === on ? { ...wait, dismissed: [...(wait.dismissed ?? []), message] } : wait)))
  }

  private setWaits(topic: string, change: (waits: Wait[]) => Wait[]): AppEvent[] {
    const item = this.context.lens.read(topic)
    if (item?.type !== 'topic') throw new Error('Not a topic')
    const waits = change(waitsOf(item.data))
    const events = [value(topic, 'wait', waits.length ? waits : null, this.context.now(), me)]
    this.context.owned.write(events)
    return events
  }

  /** Where a topic's box may send `on`: only somewhere a Slack item in its chat offers. */
  sendTarget(topic: string, on: string): SendTarget {
    this.task(topic)
    const target = sendTargets(topic, this.context.lens).find((offered) => offered.on === on)
    if (!target) throw new Error('Nothing in this topic to send that to')
    return target
  }

  /** A Slack message sent from a topic's box goes into its chat. */
  sent(topic: string, id: string): AppEvent[] {
    const events = [link(topic, id, this.context.now(), me)]
    this.context.owned.write(events)
    return events
  }

  /** Refused unless `topic` is a task: what is Claude's (a path, a dispatch, git, Slack from the box) is not for thoughts. */
  task(topic: string): void {
    const item = this.context.lens.read(topic)
    if (item?.type !== 'topic') throw new Error('Not a topic')
    if (!isTaskTopic(item)) throw new Error('Only a task topic works with Claude, paths and git')
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
    this.task(topic)
    // Never two prompts at once: the messages wait for the next dispatch.
    if (answering(topic, lens) || this.busy.has(topic)) throw new Error('Claude is still answering')
    const messages = unsent(topic, lens)
    if (!messages.length) throw new Error('Nothing new to send')
    const prompt = messages.map((id) => promptPart(id, lens)).join('\n\n')
    const at = this.context.now()
    const responseId = randomUUID()
    const data = this.context.data.get(topic)
    const cwd = data.cwd
    const path = latestPath(topic, lens)
    const events: AppEvent[] = [
      // Claude's, like `started`: undoing my last edit doesn't take the conversation's directory away.
      ...(path
        ? path.data.cwd === cwd
          ? []
          : // The newest path: the topic moves there, its conversation (if started) with it, so the next prompt resumes it.
            values(topic, { ...placeOf(path.data), started: carryTranscript(topic, String(path.data.cwd)) }, at, claudeAuthor)
        : typeof cwd === 'string' && cwd
          ? []
          : [value(topic, 'cwd', mkdtempSync(join(tmpdir(), 'claude-')), at, claudeAuthor)]),
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
   * own, and its answer); and, in a worktree, the PR of the branch checked
   * out there goes into the chat and the topic's context (`notePr`).
   */
  private async answer(topic: string, responseId: string, prompt: string): Promise<void> {
    await ask(this.context, topic, responseId, prompt)
    if (this.context.lens.read(responseId)?.data.error) return
    const cwd = this.cwdOf(topic)
    if (cwd && (await this.git(cwd, ['status', '--porcelain']))?.trim()) {
      const at = this.context.now()
      const followUp = randomUUID()
      const message = randomUUID()
      // After the answer even when written the same instant as its dispatch (the answer is linked a moment after that).
      this.context.owned.write([
        value(message, 'text', commitPrompt, at, appAuthor),
        link(topic, message, at + 2, appAuthor),
        ...values(followUp, { type: 'claude.response', text: '', running: true }, at, claudeAuthor),
        link(topic, followUp, at + 3, claudeAuthor),
      ])
      await ask(this.context, topic, followUp, commitPrompt)
    }
    if (cwd) await this.notePr(topic, cwd)
  }

  /**
   * When `cwd` is a git worktree whose branch (not detached, not the
   * default one) has a PR on GitHub: the PR's item into the chat, unless it
   * is there already, and into the topic's context. Nothing for a branch
   * that only exists here, or has no PR.
   */
  private async notePr(topic: string, cwd: string): Promise<void> {
    const gitDir = await this.git(cwd, ['rev-parse', '--absolute-git-dir'])
    const commonDir = await this.git(cwd, ['rev-parse', '--path-format=absolute', '--git-common-dir'])
    // A worktree of a repo has a git directory of its own, apart from the repo's.
    if (!gitDir?.trim() || !commonDir?.trim() || gitDir.trim() === commonDir.trim()) return
    const branch = (await this.git(cwd, ['rev-parse', '--abbrev-ref', 'HEAD']))?.trim()
    if (!branch || branch === 'HEAD' || branch === (await this.defaultBranch(cwd))) return
    const remote = await this.git(cwd, ['remote', 'get-url', 'origin'])
    const github = remote ? githubRepo(remote) : null
    if (!github) return
    const url = await prOfBranch(this.context.gh, github, branch).catch(() => null)
    const id = url ? prEntityId(url) : null
    if (!id) return
    if (!this.context.lens.children(topic).includes(id)) {
      // Later than the answers before it, even when all are written the same instant (the last is linked three moments on).
      this.context.owned.write([link(topic, id, this.context.now() + 4, claudeAuthor)])
    }
    this.share(topic, id)
  }

  /**
   * What would be lost resolving a topic: in each of its worktrees (only
   * ones proved to be), changes not committed, commits not pushed, or not
   * merged into the default branch. Empty when there is nothing, or no
   * worktree.
   */
  async resolveWarnings(topic: string): Promise<string[]> {
    const warnings: string[] = []
    for (const { path } of await this.provenWorktrees(topic)) warnings.push(...(await this.worktreeWarnings(path)))
    return warnings
  }

  private async worktreeWarnings(path: string): Promise<string[]> {
    const warnings: string[] = []
    const status = await this.git(path, ['status', '--porcelain'])
    if (status === null) return [`Could not read the state of ${path}`]
    if (status.trim()) warnings.push(`Uncommitted changes in ${path}`)
    const count = (out: string | null) => (out ?? '').split('\n').filter((line) => line.trim()).length
    const unpushed = await this.git(path, ['log', '--oneline', 'HEAD', '--not', '--remotes'])
    if (unpushed === null) warnings.push(`Could not tell which commits are pushed in ${path}`)
    else if (count(unpushed)) warnings.push(`${count(unpushed)} commit${count(unpushed) === 1 ? '' : 's'} not pushed in ${path}`)
    const main = await this.defaultBranch(path)
    const unmerged = main ? await this.git(path, ['log', '--oneline', `origin/${main}..HEAD`]) : null
    if (unmerged === null) warnings.push(`Could not tell which commits are merged in ${path}`)
    else if (count(unmerged) && !count(unpushed)) warnings.push(`${count(unmerged)} commit${count(unmerged) === 1 ? '' : 's'} not merged into ${main} in ${path}`)
    return warnings
  }

  /**
   * Resolves a topic: it leaves the list (its link from the root undone, so
   * it is kept), and its worktrees (its own and its paths') are removed,
   * each only if proved to be one; any other directory is left alone. While there is work that would
   * be lost (`resolveWarnings`) it is refused, unless `force`. Its branch is
   * kept. Not while Claude answers.
   */
  async resolve(topic: string, force: boolean): Promise<AppEvent[]> {
    if (this.context.lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    if (answering(topic, this.context.lens) || this.busy.has(topic)) throw new Error('Claude is still answering')
    const worktrees = await this.provenWorktrees(topic)
    if (!force) {
      const warnings = await this.resolveWarnings(topic)
      if (warnings.length) throw new Error(warnings.join('; '))
    }
    for (const worktree of worktrees) await this.context.run('git', ['worktree', 'remove', ...(force ? ['--force'] : []), worktree.path], worktree.repo)
    const at = this.context.now()
    // What named a removed worktree no longer does.
    const removed = new Set(worktrees.map((worktree) => worktree.path))
    const holders = [topic, ...this.context.lens.children(topic)].filter((id) => {
      const worktree = this.context.lens.read(id)?.data.worktree
      return typeof worktree === 'string' && removed.has(resolve(worktree))
    })
    const events: AppEvent[] = [link(ids.root, topic, at, me, 1), value(topic, 'resolved', at, at, me), ...holders.map((id) => value(id, 'worktree', null, at, me))]
    this.context.owned.write(events)
    return events
  }

  /** The worktrees of a topic (its own, and its paths'), once each, only those provably ones the app made (`provenWorktree`). */
  private async provenWorktrees(topic: string): Promise<{ path: string; repo: string }[]> {
    const lens = this.context.lens
    const named = [this.context.data.get(topic), ...lens.children(topic).flatMap((id) => (lens.read(id)?.type === 'path' ? [lens.read(id)!.data] : []))]
    const proven: { path: string; repo: string }[] = []
    for (const data of named) {
      const worktree = await this.provenWorktree(data)
      if (worktree && !proven.some((one) => one.path === worktree.path)) proven.push(worktree)
    }
    return proven
  }

  /**
   * The worktree named in `data`, only if it is provably one the app made:
   * its `worktree` is under the app's own worktrees directory, exists, is the
   * top of a checkout, and its repo lists it as a worktree. Null otherwise.
   */
  private async provenWorktree(data: Record<string, unknown>): Promise<{ path: string; repo: string } | null> {
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

  // --- git, from the buttons at a topic's top right ---------------------------------

  /** Where a topic works now: its newest path, or else where it last ran. Its worktree, if it is one, and the repo that is of. */
  private where(topic: string): { dir: string; worktree: string | null; repo: string | null } {
    if (this.context.lens.read(topic)?.type !== 'topic') throw new Error('Not a topic')
    const data = latestPath(topic, this.context.lens)?.data ?? this.context.data.get(topic)
    const text = (key: string) => (typeof data[key] === 'string' && data[key] ? expandHome(data[key] as string) : null)
    const dir = text('cwd')
    if (!dir) throw new Error('No path: choose one first')
    const worktree = text('worktree')
    return { dir, worktree, repo: worktree ? text('repo') : null }
  }

  /** The repo a topic's work is in: its worktree's, or else the one its directory is in. */
  private async baseRepo(topic: string): Promise<string> {
    const { dir, repo } = this.where(topic)
    const base = repo ?? (await this.git(dir, ['rev-parse', '--show-toplevel']))?.trim()
    if (!base) throw new Error(`Not in a git repo: ${dir}`)
    return base
  }

  /** Refused when `repo`'s checkout has changes not committed: checking out over them could lose them. */
  private async clean(repo: string): Promise<void> {
    const status = await this.git(repo, ['status', '--porcelain'])
    if (status === null) throw new Error(`Could not read the state of ${repo}`)
    if (status.trim()) throw new Error(`Uncommitted changes in ${repo}: commit or stash them first`)
  }

  /**
   * **Check out**: the commit the topic's worktree is on, in the repo it is
   * a worktree of (detached: git won't check out a branch another worktree
   * has). Only with a worktree, and a clean checkout there.
   */
  async checkOut(topic: string): Promise<AppEvent[]> {
    this.task(topic)
    const { worktree } = this.where(topic)
    if (!worktree) throw new Error('No worktree to check out')
    const base = await this.baseRepo(topic)
    const commit = (await this.git(worktree, ['rev-parse', 'HEAD']))?.trim()
    if (!commit) throw new Error(`Could not read the commit of ${worktree}`)
    await this.clean(base)
    await this.context.run('git', ['checkout', '--detach', commit], base)
    return []
  }

  /** **Master**: the default branch (the remote's, else `master`) checked out in the topic's repo, if its checkout is clean. */
  async checkOutDefault(topic: string): Promise<AppEvent[]> {
    this.task(topic)
    const base = await this.baseRepo(topic)
    const main = (await this.defaultBranch(base)) ?? 'master'
    await this.clean(base)
    await this.context.run('git', ['checkout', main], base)
    return []
  }

  /**
   * **Push**: the branch checked out in the topic's worktree (or its path)
   * to the same branch on GitHub, made if new. Not a detached HEAD, not the
   * default branch, and only to an `origin` on GitHub; never forced. Not
   * while Claude answers. Its PR, if it has one, then joins the topic.
   */
  async push(topic: string): Promise<AppEvent[]> {
    this.task(topic)
    if (answering(topic, this.context.lens) || this.busy.has(topic)) throw new Error('Claude is still answering')
    const { dir, worktree } = this.where(topic)
    const from = worktree ?? dir
    const branch = (await this.git(from, ['rev-parse', '--abbrev-ref', 'HEAD']))?.trim()
    if (!branch) throw new Error(`Not in a git repo: ${from}`)
    if (branch === 'HEAD') throw new Error('Not on a branch: nothing to push to')
    if (branch === (await this.defaultBranch(from))) throw new Error(`Won't push ${branch}: work on a branch`)
    const remote = await this.git(from, ['remote', 'get-url', 'origin'])
    if (!remote || !githubRepo(remote)) throw new Error('origin is not on GitHub')
    await this.context.run('git', ['push', '-u', 'origin', branch], from)
    await this.notePr(topic, from)
    return []
  }
}
