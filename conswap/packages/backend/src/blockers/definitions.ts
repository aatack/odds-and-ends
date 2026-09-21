import type { Blocker, TopicId } from '@conswap/common/types'
import type { Context } from '../context.js'
import { run, runJson } from '../shell.js'
import { getTopic } from '../topics.js'
import type { BlockerDefinition } from './types.js'

function text(config: Record<string, unknown>, key: string): string | null {
  const value = config[key]
  return typeof value === 'string' && value.length > 0 ? value : null
}

function ids(config: Record<string, unknown>, key: string): TopicId[] {
  const value = config[key]
  return Array.isArray(value) ? value.filter((entry): entry is string => typeof entry === 'string') : []
}

function relative(iso: string): string {
  const seconds = Math.round((new Date(iso).getTime() - Date.now()) / 1000)
  if (seconds < 90) return `${Math.max(seconds, 0)}s`
  if (seconds < 5400) return `${Math.round(seconds / 60)}m`
  if (seconds < 172800) return `${Math.round(seconds / 3600)}h`
  return `${Math.round(seconds / 86400)}d`
}

/** Wait for the clock. */
const snooze: BlockerDefinition = {
  type: 'snooze',
  interval: 20,
  describe(_context, config) {
    const until = text(config, 'until')
    return until ? `snoozed for ${relative(until)}` : 'snoozed'
  },
  async check(_context, blocker) {
    const until = text(blocker.config, 'until')
    if (!until) return { satisfied: true }
    const remaining = new Date(until).getTime() - Date.now()
    if (remaining <= 0) return { satisfied: true, note: 'snooze finished' }
    return { satisfied: false, retryIn: Math.min(Math.ceil(remaining / 1000), 300) }
  },
}

/** Wait for anything at all to happen underneath this topic. */
const activity: BlockerDefinition = {
  type: 'activity',
  describe(context, config) {
    const sourceId = text(config, 'sourceId')
    if (!sourceId) return 'waiting for anything new'
    const topic = getTopic(context.db, sourceId)
    return `waiting for activity in ${topic ? topic.text.slice(0, 60) : 'a subtopic'}`
  },
  wakesOn(_context, blocker, signal) {
    if (signal.at <= blocker.createdAt) return false
    const sourceId = text(blocker.config, 'sourceId')
    if (!sourceId) return true
    return signal.path.includes(sourceId)
  },
}

/** Wait for a reply in a channel, or in one particular thread. */
const slackReply: BlockerDefinition = {
  type: 'slack_reply',
  describe(_context, config) {
    const channel = text(config, 'channelName') ?? text(config, 'channelId') ?? 'slack'
    return text(config, 'threadTs') ? `waiting for a reply in ${channel}` : `waiting for a message in ${channel}`
  },
  wakesOn(_context, blocker, signal) {
    if (signal.at <= blocker.createdAt) return false
    if (signal.topic.type !== 'slack_message') return false
    const metadata = signal.topic.metadata as { channelId?: string; threadTs?: string; fromMe?: boolean }
    if (metadata.fromMe === true) return false
    const channelId = text(blocker.config, 'channelId')
    if (channelId && metadata.channelId !== channelId) return false
    const threadTs = text(blocker.config, 'threadTs')
    if (threadTs && metadata.threadTs !== threadTs) return false
    return true
  },
}

/** Wait for a Claude run started from this topic to come back. */
const claudeRun: BlockerDefinition = {
  type: 'claude',
  interval: 10,
  describe(_context, _config) {
    return 'waiting for Claude'
  },
  async check(context, blocker) {
    const runId = text(blocker.config, 'runId')
    if (!runId) return { satisfied: true }
    const row = context.db.prepare('SELECT status, error FROM runs WHERE id = ?').get(runId) as
      | { status: string; error: string | null }
      | undefined
    if (!row) return { satisfied: true }
    if (row.status === 'running') return { satisfied: false, retryIn: 10 }
    return { satisfied: true, note: row.status === 'failed' ? `Claude failed: ${row.error ?? ''}` : 'Claude finished' }
  },
}

/** Wait for every subtopic that was opened in its own right to be signed off. */
const subtopics: BlockerDefinition = {
  type: 'subtopics',
  interval: 15,
  describe(context, config) {
    const list = ids(config, 'ids')
    if (list.length === 1) {
      const topic = getTopic(context.db, list[0] as string)
      return `waiting for ${topic ? topic.text.slice(0, 60) : 'a subtopic'}`
    }
    return `waiting for ${list.length} subtopics`
  },
  async check(context, blocker) {
    const list = ids(blocker.config, 'ids')
    if (list.length === 0) return { satisfied: true }
    const outstanding = list.filter((id) => {
      const topic = getTopic(context.db, id)
      return topic !== null && !topic.resolved
    })
    if (outstanding.length > 0) return { satisfied: false, retryIn: 15 }
    return { satisfied: true, note: 'every subtopic is resolved' }
  },
}

interface PullRequestState {
  state: string
  mergedAt: string | null
  reviewDecision: string | null
  statusCheckRollup: { name?: string; status?: string; conclusion?: string; state?: string }[] | null
}

async function pullRequest(repo: string, number: string): Promise<PullRequestState | null> {
  return runJson<PullRequestState>(
    'gh',
    [
      'pr',
      'view',
      number,
      '--repo',
      repo,
      '--json',
      'state,mergedAt,reviewDecision,statusCheckRollup',
    ],
    { timeoutMs: 30_000 },
  )
}

function checksVerdict(rollup: PullRequestState['statusCheckRollup']): 'pending' | 'passing' | 'failing' {
  if (!rollup || rollup.length === 0) return 'pending'
  let pending = false
  for (const check of rollup) {
    const conclusion = (check.conclusion ?? check.state ?? '').toUpperCase()
    const status = (check.status ?? '').toUpperCase()
    if (status && status !== 'COMPLETED') pending = true
    else if (['FAILURE', 'TIMED_OUT', 'CANCELLED', 'ACTION_REQUIRED', 'ERROR'].includes(conclusion)) return 'failing'
    else if (conclusion === '' || conclusion === 'PENDING') pending = true
  }
  return pending ? 'pending' : 'passing'
}

/** Wait until CI has finished, whichever way it went. */
const githubChecks: BlockerDefinition = {
  type: 'github_checks',
  interval: 120,
  describe(_context, config) {
    return `waiting for CI on ${text(config, 'repo') ?? 'the PR'}#${text(config, 'number') ?? '?'}`
  },
  async check(_context, blocker) {
    const repo = text(blocker.config, 'repo')
    const number = text(blocker.config, 'number')
    if (!repo || !number) return { satisfied: true }
    const state = await pullRequest(repo, number)
    if (!state) return { satisfied: false, retryIn: 300, error: 'gh could not read the pull request' }
    const verdict = checksVerdict(state.statusCheckRollup)
    if (verdict === 'pending') return { satisfied: false, retryIn: 120 }
    return { satisfied: true, note: verdict === 'passing' ? 'CI is green' : 'CI failed' }
  },
}

/** Wait until somebody has approved, or asked for changes. */
const githubReview: BlockerDefinition = {
  type: 'github_review',
  interval: 180,
  describe(_context, config) {
    return `waiting for review on ${text(config, 'repo') ?? 'the PR'}#${text(config, 'number') ?? '?'}`
  },
  async check(_context, blocker) {
    const repo = text(blocker.config, 'repo')
    const number = text(blocker.config, 'number')
    if (!repo || !number) return { satisfied: true }
    const state = await pullRequest(repo, number)
    if (!state) return { satisfied: false, retryIn: 300, error: 'gh could not read the pull request' }
    if (!state.reviewDecision || state.reviewDecision === 'REVIEW_REQUIRED') return { satisfied: false, retryIn: 180 }
    return { satisfied: true, note: `review: ${state.reviewDecision.toLowerCase().replace(/_/g, ' ')}` }
  },
}

/** Wait until the PR is merged or closed. */
const githubMerged: BlockerDefinition = {
  type: 'github_merged',
  interval: 180,
  describe(_context, config) {
    return `waiting for ${text(config, 'repo') ?? 'the PR'}#${text(config, 'number') ?? '?'} to merge`
  },
  async check(_context, blocker) {
    const repo = text(blocker.config, 'repo')
    const number = text(blocker.config, 'number')
    if (!repo || !number) return { satisfied: true }
    const state = await pullRequest(repo, number)
    if (!state) return { satisfied: false, retryIn: 300, error: 'gh could not read the pull request' }
    if (state.state === 'OPEN') return { satisfied: false, retryIn: 180 }
    return { satisfied: true, note: state.mergedAt ? 'merged' : 'closed without merging' }
  },
}

/** Wait until a command succeeds. The escape hatch for anything else. */
const command: BlockerDefinition = {
  type: 'command',
  interval: 60,
  describe(_context, config) {
    return `waiting for \`${text(config, 'command') ?? 'a command'}\``
  },
  async check(_context, blocker) {
    const line = text(blocker.config, 'command')
    if (!line) return { satisfied: true }
    const interval = typeof blocker.config.interval === 'number' ? blocker.config.interval : 60
    const result = await run('bash', ['-lc', line], {
      cwd: text(blocker.config, 'cwd') ?? undefined,
      timeoutMs: 120_000,
    })
    if (result.code === 0) return { satisfied: true, note: `\`${line}\` succeeded` }
    return { satisfied: false, retryIn: interval }
  },
}

/** Wait until an agent says the condition has been met. The expensive escape hatch. */
const agent: BlockerDefinition = {
  type: 'agent',
  interval: 300,
  describe(_context, config) {
    return `waiting until ${text(config, 'question') ?? 'an agent says so'}`
  },
  async check(context, blocker) {
    const question = text(blocker.config, 'question')
    if (!question) return { satisfied: true }
    const interval = typeof blocker.config.interval === 'number' ? blocker.config.interval : 300
    const prompt = [
      'Answer with a single word, YES or NO, and nothing else.',
      'Investigate using the tools available to you, then decide.',
      '',
      `Has this happened yet? ${question}`,
    ].join('\n')
    const result = await run(
      context.config.claude.binary,
      ['-p', prompt, '--model', context.config.claude.model],
      { cwd: text(blocker.config, 'cwd') ?? undefined, timeoutMs: 600_000 },
    )
    if (result.code !== 0) return { satisfied: false, retryIn: interval, error: result.stderr.slice(0, 400) }
    if (/\bYES\b/i.test(result.stdout)) return { satisfied: true, note: `agent says yes: ${question}` }
    return { satisfied: false, retryIn: interval }
  },
}

export const definitions: BlockerDefinition[] = [
  snooze,
  activity,
  slackReply,
  claudeRun,
  subtopics,
  githubChecks,
  githubReview,
  githubMerged,
  command,
  agent,
]

export const definitionsByType = new Map(definitions.map((definition) => [definition.type, definition]))

export function definitionFor(blocker: Pick<Blocker, 'type'>): BlockerDefinition | null {
  return definitionsByType.get(blocker.type) ?? null
}
