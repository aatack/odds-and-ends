// What an item is, from its id and what it holds, when it says nothing (no `type`) or might say wrong: a Slack link a
// Slack message or channel, a GitHub pull request's a pull request, any other web address a link; values that only one
// type has (a script's `source`, a bot's `instructions`) that type. The engine pulls URLs in by it, renders an id with no
// item by it, and the debug view offers to set an item's type to it when they differ.

import { parseSlackLink } from './slack'

export type Inferred = { type: string; params: Record<string, unknown> }

const SLACK_CHANNEL = /^https:\/\/[\w-]+(?:\.enterprise)?\.slack\.com\/archives\/([CGD][A-Z0-9]+)\/?(?:[?#].*)?$/
export const GITHUB_PR = /^https:\/\/github\.com\/([\w.-]+\/[\w.-]+)\/pull\/(\d+)/

/** What a web address is: a Slack message or channel, a pull request, or a plain link. Null if it is not one. */
export function inferUrl(url: string, now = Date.now()): Inferred | null {
  if (!/^https?:\/\//.test(url)) return null
  if (parseSlackLink(url)) return { type: 'slack_message', params: { link: url } }
  const channel = SLACK_CHANNEL.exec(url)
  if (channel) return { type: 'slack_channel', params: { channel: channel[1], oldest: new Date(now - 24 * 3_600_000).toISOString() } }
  const pr = GITHUB_PR.exec(url)
  if (pr) return { type: 'github_pr', params: { url, repo: pr[1], number: Number(pr[2]) } }
  return { type: 'link', params: { url } }
}

/**
 * What an item is likeliest to be: by the address it is about (its id, or a `url` or `link` value), else by values only
 * one type has. Its params are what that type needs, from what it holds (what it already has wins). Null: no guess.
 */
export function inferType(id: string, values: Record<string, unknown> = {}): Inferred | null {
  const url = [values.url, values.link, id].find((v) => typeof v === 'string' && /^https?:\/\//.test(v)) as string | undefined
  const byUrl = url ? inferUrl(url) : null
  const keep = Object.fromEntries(Object.entries(values).filter(([k]) => k !== 'type'))
  if (byUrl) return { type: byUrl.type, params: { ...byUrl.params, ...keep } }
  if (typeof values.source === 'string') return { type: 'script', params: keep }
  if (typeof values.instructions === 'string' || typeof values.model === 'string') return { type: 'bot', params: keep }
  if (typeof values.username === 'string') return { type: 'user', params: keep }
  if (typeof values.path === 'string' || typeof values.hash === 'string') return { type: 'file', params: keep }
  if (typeof values.text === 'string') return { type: 'topic', params: keep }
  return null
}

/**
 * What something pasted (or dropped) becomes, wherever it lands: a web address alone what `inferUrl` says, with the
 * address as its id (the one item for it); a kept file (its name, hash and size) a file; any other text a topic.
 */
export function inferPasted(p: { text: string } | { file: { name: string; hash: string; size: number } }, now = Date.now()): Inferred & { id?: string } {
  if ('file' in p) return inferType('', p.file)!
  const url = p.text.trim()
  const byUrl = /^\S+$/.test(url) ? inferUrl(url, now) : null
  return byUrl ? { ...byUrl, id: url } : inferType('', { text: p.text })!
}
