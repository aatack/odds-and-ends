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

/** The script that waits for something new on Slack after a message: in its thread, and (if it is not a reply) its channel. */
export const slackWait = (link: string, me: string) => `const s = context.state ?? { step: 0 }
const m = await tools.slack_message({ link: ${JSON.stringify(link)} })
const top = !m.message.thread_ts || m.message.thread_ts === m.message.ts
const thread = await tools.slack_thread({ channel: m.channel, ts: m.message.thread_ts ?? m.message.ts })
const channel = top ? await tools.slack_channel_history({ channel: m.channel, oldest: m.message.ts, limit: 50 }) : []
// The newest seen in each, kept apart: a reply can be older than the channel's newest message.
const newest = (xs) => xs.reduce((a, x) => (Number(x.ts) > Number(a) ? x.ts : a), m.message.ts)
const since = s.since ?? { thread: newest(thread), channel: newest(channel) }
const newer = [...thread.filter((x) => Number(x.ts) > Number(since.thread)), ...channel.filter((x) => Number(x.ts) > Number(since.channel))]
  .sort((a, b) => Number(a.ts) - Number(b.ts))
if (!newer.length) return { state: { step: s.step + 1, since }, next: ['+5m', '+15m', '+60m'][Math.min(s.step, 2)] }
return {
  messages: [{ text: 'New on Slack, in ' + m.channel_name + ':\\n\\n' + newer.map((x) => '**' + x.author + '**: ' + x.text).join('\\n\\n') }],
  assign: context.tasks.map((t) => ({ item: t.id, to: [${JSON.stringify(me)}] })),
  done: true,
}`

