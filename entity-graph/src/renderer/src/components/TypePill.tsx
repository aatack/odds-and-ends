import { cn } from '../helpers/cn'

// An entity's `type`, said out loud. A type is otherwise invisible unless the app
// happens to know it: `code` and `file` change how a row draws itself, and
// anything else the user invents — `changeset`, say — reads as an ordinary
// bullet. This is what tells them apart, at the head of the row and at a glance.
//
// It is chrome and not text, so it is set in the app's sans rather than the serif
// of the row it sits in, and it is out of a selection's way: the type is a value
// on the entity, not part of what the user typed.

/**
 * The type, as a small pill: the tone of a secondary button with nothing to
 * press. Exactly one line of row text tall, so it sits on the first line wherever
 * it is put and never opens the row up.
 */
export function TypePill({
  type,
  className,
}: {
  type: string
  className?: string
}): React.JSX.Element {
  return (
    <span
      className={cn(
        'inline-flex h-5 shrink-0 items-center rounded-md bg-gray-100 px-1.5',
        'font-sans text-[12px] font-medium text-gray-700 select-none',
        className,
      )}
    >
      {type}
    </span>
  )
}
