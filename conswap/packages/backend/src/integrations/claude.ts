import { randomUUID } from 'node:crypto'
import { mkdirSync } from 'node:fs'
import { join } from 'node:path'
import type { IntegrationStatus, Topic, TopicId } from '@conswap/common/types'
import type { Context, Integration } from '../context.js'
import { addEvent } from '../lifecycle.js'
import { run } from '../shell.js'
import { getTopic, now, parentIds, readChildren, requireTopic, updateTopic } from '../topics.js'

interface ClaudeResult {
  type?: string
  subtype?: string
  result?: string
  session_id?: string
  is_error?: boolean
  total_cost_usd?: number
}

function slug(text: string): string {
  return (
    text
      .toLowerCase()
      .replace(/[^a-z0-9]+/g, '-')
      .replace(/^-+|-+$/g, '')
      .slice(0, 40) || 'topic'
  )
}

/** The metadata a topic needs before Claude can be pointed at a repository. */
function inheritedRepository(context: Context, topicId: TopicId): string | null {
  const seen = new Set<TopicId>()
  const frontier = [topicId]
  while (frontier.length > 0) {
    const id = frontier.pop() as TopicId
    if (seen.has(id)) continue
    seen.add(id)
    const topic = getTopic(context.db, id)
    if (topic && typeof topic.metadata.repository === 'string') return topic.metadata.repository
    for (const parent of parentIds(context.db, id)) frontier.push(parent)
  }
  return null
}

const eventHeadings: Record<string, (topic: Topic) => string> = {
  note: () => 'A note I wrote',
  slack_message: (topic) => {
    const metadata = topic.metadata as { userName?: string; channelName?: string }
    return `Slack — ${metadata.userName ?? 'someone'} in ${metadata.channelName ?? 'a channel'}`
  },
  claude_prompt: () => 'What I asked you last time',
  claude_response: () => 'What you replied last time',
  github_event: () => 'GitHub',
  system: () => 'The app',
  topic: (topic) => `Subtopic — ${topic.text}`,
}

/** Everything that has landed in the topic since Claude was last spoken to. */
function contextSince(context: Context, topicId: TopicId, since: string | null): string {
  const lines: string[] = []
  const walk = (nodes: ReturnType<typeof readChildren>, depth: number): void => {
    for (const node of nodes) {
      const topic = node.topic
      if (since === null || topic.createdAt > since) {
        const heading = (eventHeadings[topic.type] ?? (() => topic.type))(topic)
        lines.push(`${'#'.repeat(Math.min(depth + 2, 6))} ${heading}`)
        lines.push(topic.text.trim())
        lines.push('')
      }
      if (node.children) walk(node.children, depth + 1)
    }
  }
  walk(readChildren(context.db, topicId, 2, new Set()), 0)
  return lines.join('\n').trim()
}

export class ClaudeIntegration implements Integration {
  readonly name = 'claude'

  private active = 0
  private detail = 'idle'

  constructor(private readonly context: Context) {}

  status(): IntegrationStatus {
    return {
      name: this.name,
      enabled: true,
      state: this.active > 0 ? 'polling' : 'idle',
      detail: this.active > 0 ? `${this.active} running` : this.detail,
      lastRunAt: null,
    }
  }

  start(): void {
    // Runs are started by hand, so there is nothing to poll.
    this.markAbandonedRuns()
  }

  stop(): void {}

  /** Anything still marked running when the server restarts never came back. */
  private markAbandonedRuns(): void {
    this.context.db
      .prepare("UPDATE runs SET status = 'failed', error = 'the server restarted', finished_at = ? WHERE status = 'running'")
      .run(now())
  }

  /**
   * Sends a prompt on the topic's own session, making a worktree the first time so
   * that Claude never works in the repository I am sitting in.
   */
  async prompt(topicId: TopicId, text: string): Promise<{ runId: string }> {
    const topic = requireTopic(this.context.db, topicId)
    const runId = randomUUID()
    this.context.db
      .prepare(
        `INSERT INTO runs (id, topic_id, kind, status, started_at, prompt)
         VALUES (?, ?, 'claude', 'running', ?, ?)`,
      )
      .run(runId, topicId, now(), text)

    addEvent(this.context, topicId, {
      type: 'claude_prompt',
      text,
      metadata: { runId },
    }, { silent: true })

    void this.execute(runId, topic, text).catch((error) => {
      this.finish(runId, topicId, 'failed', '', String(error instanceof Error ? error.message : error))
    })
    return { runId }
  }

  private finish(runId: string, topicId: TopicId, status: string, output: string, error: string | null): void {
    this.active = Math.max(this.active - 1, 0)
    this.context.db
      .prepare('UPDATE runs SET status = ?, finished_at = ?, output = ?, error = ? WHERE id = ?')
      .run(status, now(), output, error, runId)
    addEvent(this.context, topicId, {
      type: 'claude_response',
      text: status === 'failed' ? `Claude could not finish: ${error ?? 'unknown'}` : output || '(no output)',
      metadata: { runId, status },
    })
  }

  private async execute(runId: string, topic: Topic, text: string): Promise<void> {
    this.active += 1
    const metadata = topic.metadata as {
      worktree?: string
      branch?: string
      claudeSessionId?: string
      claudeCursor?: string
    }

    let worktree = metadata.worktree ?? null
    let branch = metadata.branch ?? null
    if (!worktree) {
      const prepared = await this.prepareWorktree(topic)
      worktree = prepared.worktree
      branch = prepared.branch
      updateTopic(this.context.db, topic.id, { metadata: { worktree, branch, repository: prepared.repository } })
    }

    const since = metadata.claudeCursor ?? null
    const history = contextSince(this.context, topic.id, since)
    const prompt = [
      `You are working on a topic called "${topic.text || 'untitled'}".`,
      history ? `Here is everything that has happened on it since we last spoke:\n\n${history}` : '',
      `Now do this:\n\n${text}`,
      branch ? `\nYou are on the branch ${branch}. Leave your work uncommitted; it gets committed for you.` : '',
    ]
      .filter(Boolean)
      .join('\n\n')

    const args = ['-p', prompt, '--model', this.context.config.claude.model, '--output-format', 'json']
    if (metadata.claudeSessionId) args.push('--resume', metadata.claudeSessionId)
    if (process.env.CONSWAP_CLAUDE_PERMISSIONS !== 'ask') args.push('--permission-mode', 'acceptEdits')

    this.context.log('claude', `running on ${topic.id}`, worktree)
    const result = await run(this.context.config.claude.binary, args, { cwd: worktree, timeoutMs: 45 * 60_000 })
    if (result.code !== 0) {
      this.finish(runId, topic.id, 'failed', '', result.stderr.slice(0, 2000) || `exit ${result.code}`)
      return
    }

    let parsed: ClaudeResult = {}
    try {
      parsed = JSON.parse(result.stdout) as ClaudeResult
    } catch {
      parsed = { result: result.stdout }
    }

    updateTopic(this.context.db, topic.id, {
      metadata: {
        ...(parsed.session_id ? { claudeSessionId: parsed.session_id } : {}),
        claudeCursor: now(),
      },
    })

    const pushed = branch ? await this.commitAndPush(worktree, branch, text) : null
    const body = [parsed.result ?? '(no output)', pushed ? `\n\n---\n${pushed}` : ''].join('')
    this.finish(runId, topic.id, parsed.is_error ? 'failed' : 'succeeded', body, parsed.is_error ? 'Claude reported an error' : null)
  }

  private async prepareWorktree(topic: Topic): Promise<{ worktree: string; branch: string | null; repository: string | null }> {
    const repository = inheritedRepository(this.context, topic.id)
    const name = `${slug(topic.text)}-${topic.id.slice(0, 8)}`
    const worktree = join(this.context.config.worktreeRoot, name)

    if (!repository) {
      mkdirSync(worktree, { recursive: true })
      return { worktree, branch: null, repository: null }
    }

    mkdirSync(this.context.config.worktreeRoot, { recursive: true })
    await run('git', ['-C', repository, 'fetch', 'origin', '--prune'], { timeoutMs: 120_000 })
    const head = await run('git', ['-C', repository, 'symbolic-ref', '--short', 'refs/remotes/origin/HEAD'], {
      timeoutMs: 20_000,
    })
    const base = head.code === 0 ? head.stdout.trim() : 'origin/master'
    const branch = `conswap/${name}`
    const created = await run('git', ['-C', repository, 'worktree', 'add', '-b', branch, worktree, base], {
      timeoutMs: 180_000,
    })
    if (created.code !== 0) {
      // The branch may already exist from a previous life; check it out instead.
      const reused = await run('git', ['-C', repository, 'worktree', 'add', worktree, branch], { timeoutMs: 180_000 })
      if (reused.code !== 0) {
        this.context.log('claude', 'could not make a worktree', created.stderr.slice(0, 400))
        mkdirSync(worktree, { recursive: true })
        return { worktree: repository, branch: null, repository }
      }
    }
    return { worktree, branch, repository }
  }

  private async commitAndPush(worktree: string, branch: string, text: string): Promise<string | null> {
    const status = await run('git', ['-C', worktree, 'status', '--porcelain'], { timeoutMs: 30_000 })
    if (status.code !== 0) return null
    if (status.stdout.trim().length === 0) return 'No changes to commit.'
    await run('git', ['-C', worktree, 'add', '-A'], { timeoutMs: 60_000 })
    const message = text.split('\n')[0]?.slice(0, 72) ?? 'Work from conswap'
    const committed = await run('git', ['-C', worktree, 'commit', '-m', message], { timeoutMs: 60_000 })
    if (committed.code !== 0) return `Could not commit: ${committed.stderr.slice(0, 200)}`
    const pushed = await run('git', ['-C', worktree, 'push', '-u', 'origin', branch], { timeoutMs: 180_000 })
    if (pushed.code !== 0) return `Committed, but could not push: ${pushed.stderr.slice(0, 200)}`
    return `Committed and pushed to \`${branch}\`.`
  }
}
