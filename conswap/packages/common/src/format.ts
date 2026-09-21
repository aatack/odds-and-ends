import type { Topic } from './types'

/** Short enough to sit in a corner without pulling the eye. */
export function ago(iso: string): string {
  const seconds = Math.max(Math.round((Date.now() - new Date(iso).getTime()) / 1000), 0)
  if (seconds < 45) return 'now'
  if (seconds < 3600) return `${Math.round(seconds / 60)}m`
  if (seconds < 86_400) return `${Math.round(seconds / 3600)}h`
  if (seconds < 604_800) return `${Math.round(seconds / 86_400)}d`
  return new Date(iso).toLocaleDateString(undefined, { month: 'short', day: 'numeric' })
}

export function until(iso: string): string {
  const seconds = Math.round((new Date(iso).getTime() - Date.now()) / 1000)
  if (seconds <= 0) return 'any moment'
  if (seconds < 90) return `${seconds}s`
  if (seconds < 5400) return `${Math.round(seconds / 60)}m`
  if (seconds < 172_800) return `${Math.round(seconds / 3600)}h`
  return `${Math.round(seconds / 86_400)}d`
}

export function clock(iso: string): string {
  return new Date(iso).toLocaleTimeString(undefined, { hour: '2-digit', minute: '2-digit' })
}

export const typeNames: Record<string, string> = {
  topic: 'topic',
  note: 'note',
  system: 'app',
  slack_message: 'slack',
  slack_channel: 'slack channel',
  claude_prompt: 'to claude',
  claude_response: 'from claude',
  github_event: 'github',
  github_pull_request: 'pull request',
}

/** One colour per source, used for the thin rule beside an event. */
export const typeAccents: Record<string, string> = {
  note: 'var(--accent-note)',
  system: 'var(--accent-system)',
  slack_message: 'var(--accent-slack)',
  slack_channel: 'var(--accent-slack)',
  claude_prompt: 'var(--accent-claude)',
  claude_response: 'var(--accent-claude)',
  github_event: 'var(--accent-github)',
  github_pull_request: 'var(--accent-github)',
  topic: 'var(--accent-topic)',
}

export function accentFor(type: string): string {
  return typeAccents[type] ?? 'var(--accent-topic)'
}

/** The line above an event saying where it came from. */
export function attribution(topic: Topic): string {
  const metadata = topic.metadata as { userName?: string; channelName?: string; reason?: string; status?: string }
  if (topic.type === 'slack_message') {
    const who = metadata.userName ?? 'someone'
    return metadata.channelName ? `${who} in ${metadata.channelName}` : who
  }
  if (topic.type === 'claude_response') return metadata.status === 'failed' ? 'Claude, unhappily' : 'Claude'
  if (topic.type === 'claude_prompt') return 'you, to Claude'
  return typeNames[topic.type] ?? topic.type
}

export function metadataPairs(topic: Topic): [string, string][] {
  const skip = new Set(['pending'])
  return Object.entries(topic.metadata)
    .filter(([key, value]) => !skip.has(key) && value !== null && value !== undefined && value !== '')
    .map(([key, value]) => [key, typeof value === 'string' ? value : JSON.stringify(value)] as [string, string])
}

/** Where a Slack message written here would land. */
export function slackTarget(detail: { topic: Topic; children: { topic: Topic }[] } | null): string | null {
  if (!detail) return null
  const own = detail.topic.metadata as { channelName?: string }
  if (typeof own.channelName === 'string') return own.channelName
  for (const child of detail.children) {
    const metadata = child.topic.metadata as { channelName?: string }
    if (child.topic.type === 'slack_message' && typeof metadata.channelName === 'string') return metadata.channelName
  }
  return null
}
