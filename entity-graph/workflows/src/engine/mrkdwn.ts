// Slack's mrkdwn, as markdown: what a Slack message says, ready for the app's
// Markdown. Pure, so the tests drive it directly.

/** How to name what a message mentions. Left unanswered, a mention keeps its label or its id. */
export interface Names {
  user: (id: string) => string | undefined
  channel: (id: string) => string | undefined
}

const decode = (s: string) => s.replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&')

/** One `<…>` token: a mention, a special, or a link. */
function token(inner: string, names: Names): string {
  const [target, label] = inner.split('|', 2) as [string, string | undefined]
  if (target.startsWith('@')) {
    const id = target.slice(1)
    return `@${names.user(id) ?? label ?? id}`
  }
  if (target.startsWith('#')) {
    const id = target.slice(1)
    return `#${label || names.channel(id) || id}`
  }
  if (target.startsWith('!')) {
    const what = target.slice(1)
    if (['here', 'channel', 'everyone'].includes(what)) return `@${what}`
    if (what.startsWith('subteam^')) return label ?? '@group'
    return label ?? ''
  }
  const url = decode(target)
  if (!label) return `<${url}>`
  return `[${decode(label).replace(/[[\]]/g, '\\$&')}](${url.replace(/[()]/g, encodeURIComponent)})`
}

/** Prose (no code in it): mentions, links and formatting. */
function prose(text: string, names: Names): string {
  const kept: string[] = []
  const keep = (s: string) => `\u0000${kept.push(s) - 1}\u0000`
  let out = text.replace(/<([^<>\n]+)>/g, (_m, inner: string) => keep(token(inner, names)))
  // Bold first, to a marker, so the italics that follow don't take its stars.
  out = out
    .replace(/(^|[\s(>~_])\*(?=\S)([^*\n]*?\S)\*(?=$|[\s).,!?:;~_])/gm, '$1\u0001$2\u0001')
    .replace(/(^|[\s(>~\u0001])_(?=\S)([^_\n]*?\S)_(?=$|[\s).,!?:;~\u0001])/gm, '$1*$2*')
    .replace(/(^|[\s(>*\u0001])~(?=\S)([^~\n]*?\S)~(?=$|[\s).,!?:;*\u0001])/gm, '$1~~$2~~')
    .replace(/\u0001/g, '**')
    .replace(/^([ \t]*)[•◦▪] ?/gm, '$1- ')
  out = decode(out)
  // A single line break in Slack is a break, where in markdown it is a space.
  out = out.replace(/([^\n])\n(?!\n)/g, '$1  \n')
  return out.replace(/\u0000(\d+)\u0000/g, (_m, i: string) => kept[Number(i)])
}

/** A Slack message's text as markdown. Code (``` blocks and `spans`) is kept as it is. */
export function mrkdwnToMarkdown(text: string, names: Names): string {
  const parts = text.split(/(```[\s\S]*?```|`[^`\n]+`)/)
  return parts
    .map((part, i) => {
      if (i % 2 === 0) return prose(part, names)
      if (part.startsWith('```')) return `\n\`\`\`\n${decode(part.slice(3, -3)).replace(/^\n|\n$/g, '')}\n\`\`\`\n`
      return decode(part)
    })
    .join('')
    .trim()
}

/** The users and channels a message's text mentions by id. */
export function mentioned(text: string): { users: string[]; channels: string[] } {
  const users = [...text.matchAll(/<@([UW][A-Z0-9]+)(?:\|[^>]*)?>/g)].map((m) => m[1])
  const channels = [...text.matchAll(/<#([CGD][A-Z0-9]+)>/g)].map((m) => m[1])
  return { users: [...new Set(users)], channels: [...new Set(channels)] }
}
