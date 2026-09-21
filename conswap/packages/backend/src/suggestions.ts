import type { BlockerSuggestion, TopicId } from '@conswap/common/types'
import type { Context } from './context.js'
import { childIds, getTopic } from './topics.js'

function inMinutes(minutes: number): string {
  return new Date(Date.now() + minutes * 60_000).toISOString()
}

interface SlackHint {
  channelId: string
  channelName: string
  threadTs?: string
}

interface GithubHint {
  repo: string
  number: string
}

/**
 * Reads the topic's own feed for the things it is plausibly waiting on. Anything
 * found in the content comes first; the generic options come last.
 */
export function suggestBlockers(context: Context, topicId: TopicId): BlockerSuggestion[] {
  const topic = getTopic(context.db, topicId)
  if (!topic) return []

  const slack = new Map<string, SlackHint>()
  const github = new Map<string, GithubHint>()
  let runningClaude: string | null = null
  const openChildren: { id: string; text: string }[] = []

  const metadata = topic.metadata as Record<string, unknown>
  if (typeof metadata.repo === 'string' && typeof metadata.number !== 'undefined') {
    github.set(`${metadata.repo}#${String(metadata.number)}`, {
      repo: metadata.repo,
      number: String(metadata.number),
    })
  }
  if (typeof metadata.channelId === 'string') {
    slack.set(metadata.channelId, {
      channelId: metadata.channelId,
      channelName: typeof metadata.channelName === 'string' ? metadata.channelName : metadata.channelId,
    })
  }

  // Walks the whole subtree, not just what the feed happens to be showing.
  const visit = (id: string, depth: number, seen: Set<string>): void => {
    if (depth === 0 || seen.has(id)) return
    seen.add(id)
    for (const childId of childIds(context.db, id)) {
      const child = getTopic(context.db, childId)
      if (!child) continue
      const meta = child.metadata as Record<string, unknown>
      if (child.type === 'slack_message' && typeof meta.channelId === 'string') {
        const key = typeof meta.threadTs === 'string' ? `${meta.channelId}:${meta.threadTs}` : meta.channelId
        slack.set(key, {
          channelId: meta.channelId,
          channelName: typeof meta.channelName === 'string' ? meta.channelName : meta.channelId,
          ...(typeof meta.threadTs === 'string' ? { threadTs: meta.threadTs } : {}),
        })
      }
      if (typeof meta.repo === 'string' && typeof meta.number !== 'undefined') {
        github.set(`${meta.repo}#${String(meta.number)}`, { repo: meta.repo, number: String(meta.number) })
      }
      if (child.type === 'claude_prompt' && typeof meta.runId === 'string') {
        const row = context.db.prepare('SELECT status FROM runs WHERE id = ?').get(meta.runId) as
          | { status: string }
          | undefined
        if (row?.status === 'running') runningClaude = meta.runId
      }
      if (!child.open && !child.resolved && child.type === 'topic') {
        openChildren.push({ id: child.id, text: child.text })
      }
      visit(child.id, depth - 1, seen)
    }
  }
  visit(topicId, 4, new Set())

  const suggestions: BlockerSuggestion[] = []

  if (runningClaude) {
    suggestions.push({ type: 'claude', label: 'Claude finishes this run', config: { runId: runningClaude } })
  }

  for (const hint of slack.values()) {
    suggestions.push({
      type: 'slack_reply',
      label: hint.threadTs ? `a reply in the ${hint.channelName} thread` : `a message in ${hint.channelName}`,
      config: hint.threadTs
        ? { channelId: hint.channelId, channelName: hint.channelName, threadTs: hint.threadTs }
        : { channelId: hint.channelId, channelName: hint.channelName },
    })
  }

  for (const hint of github.values()) {
    suggestions.push({
      type: 'github_checks',
      label: `CI finishes on ${hint.repo}#${hint.number}`,
      config: { repo: hint.repo, number: hint.number },
    })
    suggestions.push({
      type: 'github_review',
      label: `${hint.repo}#${hint.number} is reviewed`,
      config: { repo: hint.repo, number: hint.number },
    })
    suggestions.push({
      type: 'github_merged',
      label: `${hint.repo}#${hint.number} is merged`,
      config: { repo: hint.repo, number: hint.number },
    })
  }

  suggestions.push({ type: 'activity', label: 'anything new happens here', config: {} })
  suggestions.push({ type: 'snooze', label: 'an hour passes', config: { until: inMinutes(60) } })
  suggestions.push({ type: 'snooze', label: 'tomorrow morning', config: { until: tomorrowMorning() } })
  suggestions.push({
    type: 'command',
    label: 'a command succeeds',
    config: { cwd: typeof metadata.repository === 'string' ? metadata.repository : undefined },
    prompt: { field: 'command', label: 'command', placeholder: 'gh pr checks 42 --watch' },
  })
  suggestions.push({
    type: 'agent',
    label: 'an agent says it has happened',
    config: { cwd: typeof metadata.repository === 'string' ? metadata.repository : undefined },
    prompt: { field: 'question', label: 'condition', placeholder: 'the staging deploy is live' },
  })

  return suggestions
}

function tomorrowMorning(): string {
  const date = new Date()
  date.setDate(date.getDate() + 1)
  date.setHours(9, 0, 0, 0)
  return date.toISOString()
}
