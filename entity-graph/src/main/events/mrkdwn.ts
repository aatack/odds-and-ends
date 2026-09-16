// Slack's mrkdwn, turned into the markdown this app renders.
//
// They look alike and are not the same language, which is the worst kind of
// difference: `*bold*` is bold in one and italic in the other, a link is written
// inside angle brackets rather than square ones, and a mention of somebody is a
// raw id that nothing outside Slack can read. So a message pasted through
// untouched comes out subtly wrong rather than obviously wrong.
//
// **A mention becomes an entity mention.** `<@U0123ABCD>` is the one form that
// cannot be translated into markdown at all — there is no markdown for "this is
// a person" — so it becomes `[@entity:U0123ABCD](@alex)`, which the outliner
// draws as a pill for that entity. The id Slack uses *is* the id the entity has,
// so the pill points at the note about that person, and the label is only what
// shows until there is one.
//
// Everything here is pure: the names come in already looked up, because looking
// one up is a call to Slack and this has no business making one.

/** What a mentioned id is called, for the label on its pill. */
export type MentionNames = Record<string, string>

/**
 * Code, and everything that is not code. Nothing inside a code span or a fence
 * is rewritten — a message explaining Slack's syntax must survive being read.
 */
const CODE = /(```[\s\S]*?```|`[^`\n]*`)/

/** One `<…>` form: a mention, a channel, a special, or a link. */
const ANGLE = /<([^<>\n]+)>/g

/** The two kinds of id worth pointing an entity mention at. */
const USER = /^[UW][A-Z0-9]+$/
const CHANNEL = /^[CDG][A-Z0-9]+$/

/**
 * A label safe to put in the parens of `[@entity:id](label)`. Parentheses would
 * close the form early, and a newline would end it — and a display name is
 * somebody's to choose, so it really can contain either.
 */
const label = (text: string): string => text.replace(/[()\n]/g, ' ').trim()

/** Split a `<…>` form into the id or URL and the label Slack put after the bar. */
const halves = (inner: string): [string, string | null] => {
  const bar = inner.indexOf('|')
  return bar === -1 ? [inner, null] : [inner.slice(0, bar), inner.slice(bar + 1)]
}

/** `&amp;`, `&lt;` and `&gt;` — the only three Slack escapes, undone. */
const unescape = (text: string): string =>
  text.replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/&amp;/g, '&')

/**
 * The ids a message mentions: everybody and everywhere named in it. What the
 * feed asks Slack about before converting, and what it makes a note for.
 */
export function mentionsIn(text: string): string[] {
  const found = new Set<string>()
  for (const [part, isCode] of segments(text)) {
    if (isCode) continue
    for (const match of part.matchAll(ANGLE)) {
      const inner = match[1]
      if (!inner.startsWith('@') && !inner.startsWith('#')) continue
      const [id] = halves(inner.slice(1))
      if (USER.test(id) || CHANNEL.test(id)) found.add(id)
    }
  }
  return [...found]
}

/** The message as markdown. `names` says what each mentioned id is called. */
export function slackToMarkdown(text: string, names: MentionNames = {}): string {
  return segments(text)
    .map(([part, isCode]) => (isCode ? part : convert(part, names)))
    .join('')
}

/** The text in order, each piece flagged as code or not. */
function segments(text: string): [string, boolean][] {
  // Captured split, so the delimiters are kept: odd indices are the code.
  return text.split(new RegExp(CODE.source, 'g')).map((part, at) => [part, at % 2 === 1])
}

function convert(text: string, names: MentionNames): string {
  // The angle forms first, and the unescaping after: Slack writes a literal `<`
  // as `&lt;`, so undoing that first would turn `&lt;@U1&gt;` — somebody typing
  // out a mention rather than making one — into a mention.
  const linked = text.replace(ANGLE, (whole, inner: string) => form(inner, names) ?? whole)
  return emphasis(unescape(linked))
}

/** One `<…>` form, or null for a shape this does not recognise. */
function form(inner: string, names: MentionNames): string | null {
  if (inner.startsWith('@') || inner.startsWith('#')) {
    const at = inner.startsWith('@')
    const [id, said] = halves(inner.slice(1))
    if (!USER.test(id) && !CHANNEL.test(id)) return null
    const sigil = at ? '@' : '#'
    // The name Slack was asked for, then whatever it wrote in the message, then
    // the bare id — which at least says *something*, and is what the pill would
    // otherwise show on its own.
    const name = names[id] ?? (said ? `${sigil}${said}` : id)
    return `[@entity:${id}](${label(name)})`
  }

  if (inner.startsWith('!')) {
    const [what, said] = halves(inner.slice(1))
    // `@here`, `@channel`, `@everyone`: an audience rather than a person, so
    // there is nothing to point at and the word itself is the whole meaning.
    if (['here', 'channel', 'everyone'].includes(what)) return `@${what}`
    // A user group, and a date Slack formats for the reader — both carry the
    // text they should have shown after the bar, which is all that is wanted.
    return said ? label(said) : `@${what.split('^')[0]}`
  }

  const [url, said] = halves(inner)
  if (!/^[a-z][a-z0-9+.-]*:/i.test(url)) return null
  // A destination with a space or a bracket in it cannot go in the parens of a
  // markdown link, and a URL is more use whole than linked, so it goes bare.
  if (/[\s()]/.test(url)) return url
  return said ? `[${label(said)}](${url})` : url
}

/**
 * The two marks that differ. `*one star*` is bold in Slack and italic in
 * markdown, and `~tilde~` is a strikethrough with one rather than two — while
 * `_underscore_` and backticks already mean the same in both and are left alone.
 *
 * Bounded by a non-word character on the outside and a non-space on the inside,
 * which is Slack's own rule and is what keeps `2 * 3 * 4` arithmetic rather than
 * turning the middle of it bold.
 */
function emphasis(text: string): string {
  return text
    .replace(/(?<![\w*])\*(?![\s*])([^*\n]*?)(?<![\s*])\*(?![\w*])/g, '**$1**')
    .replace(/(?<![\w~])~(?![\s~])([^~\n]*?)(?<![\s~])~(?![\w~])/g, '~~$1~~')
}
