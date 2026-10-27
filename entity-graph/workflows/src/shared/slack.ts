// Slack message links: what the engine pulls in, and what the chat input recognises when one is pasted.

/** A message's link, as Slack's Copy link gives it. */
export const SLACK_LINK = /https:\/\/([\w-]+(?:\.enterprise)?)\.slack\.com\/archives\/([A-Z0-9]+)\/p(\d{10})(\d{6})(?:\?[^\s<>]*)?/

/** Where a message's link points: its workspace's host, channel, ts, and the thread it is in, if any. */
export function parseSlackLink(link: string): { host: string; channel: string; ts: string; thread_ts?: string } | null {
  const m = SLACK_LINK.exec(link)
  if (!m) return null
  const thread = /[?&]thread_ts=(\d+\.\d+)/.exec(m[0])?.[1]
  return { host: `${m[1]}.slack.com`, channel: m[2], ts: `${m[3]}.${m[4]}`, ...(thread && thread !== `${m[3]}.${m[4]}` ? { thread_ts: thread } : {}) }
}
