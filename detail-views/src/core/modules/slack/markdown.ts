import { mentionScheme } from '../../types.ts'
import { emojify } from './emoji.ts'

export interface Resolve {
  /** A user's display name, and the entity a mention of them opens. */
  user(id: string): { name: string; target: string | null }
  /** The entity a channel link opens. */
  channel(id: string): string | null
}


function mention(label: string, key: string, target: string | null): string {
  return `[${escapeLabel(label)}](${mentionScheme}${key}/${target ?? ''})`
}

function escapeLabel(label: string): string {
  return label.replace(/[[\]\\]/g, '\\$&')
}

function decode(text: string): string {
  return text.replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&')
}

/** Everything outside code: Slack's markup rewritten as markdown. */
function convertProse(text: string, resolve: Resolve): string {
  const converted = text
    .replace(/<@([UW][A-Z0-9]+)(?:\|[^>]*)?>/g, (_, id: string) => {
      const user = resolve.user(id)
      return mention(user.name, id, user.target)
    })
    .replace(/<#([CG][A-Z0-9]+)(?:\|([^>]*))?>/g, (_, id: string, name?: string) =>
      mention(`#${name || id}`, id, resolve.channel(id)),
    )
    .replace(/<!(here|channel|everyone)[^>]*>/g, '**@$1**')
    .replace(/<!subteam\^[A-Z0-9]+(?:\|([^>]*))?>/g, (_, name?: string) => `**${name ?? '@team'}**`)
    .replace(/<!date\^[^|>]*\|([^>]*)>/g, '$1')
    .replace(/<((?:https?|mailto):[^>|]+)\|([^>]+)>/g, (_, url: string, label: string) => `[${escapeLabel(decode(label))}](${url})`)
    .replace(/<((?:https?|mailto):[^>]+)>/g, '<$1>')
  return emojify(
    converted
      .split('\n')
      .map((line) =>
        line
          // Slack has no headings; a leading # is just a character.
          .replace(/^(\s*)#/, '$1\\#')
          .replace(/^(\s*)[•◦▪]\s+/, '$1- ')
          .replace(/^&gt;\s?/, '> '),
      )
      .join('\n')
      .replace(/(^|[\s([{"'])\*(?=\S)([^*\n]*?\S)\*(?=$|[\s.,!?;:)\]}"'])/gm, '$1**$2**')
      .replace(/(^|[\s([{"'])~(?=\S)([^~\n]*?\S)~(?=$|[\s.,!?;:)\]}"'])/gm, '$1~~$2~~')
      .replace(/&lt;/g, '\\<')
      .replace(/&gt;/g, '>')
      .replace(/&amp;/g, '&'),
  )
}

/** Slack mrkdwn to CommonMark (with GFM strikethrough). Code is left alone. */
export function slackToMarkdown(text: string, resolve: Resolve): string {
  const out: string[] = []
  // Code blocks first, then inline code; neither is touched inside.
  const pattern = /```([\s\S]*?)```|`([^`\n]+)`/g
  let last = 0
  for (const match of text.matchAll(pattern)) {
    out.push(convertProse(text.slice(last, match.index), resolve))
    if (match[1] !== undefined) {
      const body = decode(match[1]).replace(/^\n|\n$/g, '')
      const prefix = out.length && !out[out.length - 1].endsWith('\n') && out[out.length - 1] !== '' ? '\n' : ''
      out.push(`${prefix}\`\`\`\n${body}\n\`\`\`\n`)
    } else {
      out.push(`\`${decode(match[2]!)}\``)
    }
    last = match.index + match[0].length
  }
  out.push(convertProse(text.slice(last), resolve))
  return out.join('')
}
