import { gemoji } from 'gemoji'

const byName = new Map<string, string>()
for (const entry of gemoji) for (const name of entry.names) byName.set(name, entry.emoji)

/** Slack names a few differently from GitHub. */
const aliases: Record<string, string> = {
  thumbsup: '+1',
  thumbsdown: '-1',
  simple_smile: 'slightly_smiling_face',
  white_check_mark: 'white_check_mark',
}

/** The character for a Slack emoji name, or `:name:` for custom ones. */
export function emoji(name: string): string {
  const base = name.replace(/::skin-tone-\d$/, '')
  return byName.get(aliases[base] ?? base) ?? `:${base}:`
}

/** Replaces `:name:` in text, leaving unknown names as they are. */
export function emojify(text: string): string {
  return text.replace(/:([a-z0-9_+-]+(?:::skin-tone-\d)?):/g, (whole, name: string) => {
    const found = emoji(name)
    return found.startsWith(':') ? whole : found
  })
}
