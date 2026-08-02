import { cn } from '../helpers/cn'
import { DOT_COLORS, type BadgeColor } from './ui/Badge'

// An entity's `type`, said out loud. A type is otherwise invisible unless the app
// happens to know it: `code` and `file` change how a row draws itself, and
// anything else the user invents — `changeset`, say — reads as an ordinary
// bullet. This is what tells them apart, at the head of the row and at a glance.
//
// It is chrome and not text, so it is set in the app's sans rather than the serif
// of the row it sits in, and it is out of a selection's way: the type is a value
// on the entity, not part of what the user typed.
//
// A row's type is the reason it exists, but not the only thing it draws: the
// `[@pill](text)` field puts the same shape in the middle of a sentence, for a
// word the text wants to set apart when there is no value on the entity saying
// so, and `[@tool:callId]` puts a call's status in one. Hence `label` rather than
// `type` — the pill draws a word, and the type is one of them.

/**
 * A word, as a small pill: the tone of a secondary button with nothing to press.
 * Exactly one line of row text tall, which is load-bearing where it is floated
 * into prose — a line box only moves aside for a float it overlaps, so a pill the
 * height of one line indents the first line and leaves every line under it at the
 * full width. It also means the pill never opens a row up.
 *
 * `dot` marks it as a status: a coloured dot against the same neutral surface,
 * which is how a badge says one too. Colour is the dot's and never the pill's —
 * a chip that turns red in a line of prose shouts, and the dot is enough to
 * follow at a glance.
 */
export function TypePill({
  label,
  dot,
  title,
  className,
}: {
  label: string
  dot?: BadgeColor
  /** Hover text, for the pill that has more to say than it can fit. */
  title?: string
  className?: string
}): React.JSX.Element {
  return (
    <span
      title={title}
      className={cn(
        'inline-flex h-5 shrink-0 items-center gap-1 rounded-md bg-gray-100 px-1.5',
        'font-sans text-[12px] font-medium text-gray-700 select-none',
        className,
      )}
    >
      {dot && <span className={cn('size-1.5 shrink-0 rounded-full', DOT_COLORS[dot])} />}
      {label}
    </span>
  )
}
