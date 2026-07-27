import { cn } from '../../helpers/cn'

/**
 * A two-state switch for a setting that takes effect immediately, as opposed to
 * a checkbox in a form that is saved later. The knob doesn't slide — it is where
 * it is, like every other state change in this app.
 */
export function Switch({
  checked,
  onChange,
  disabled = false,
  label,
  className,
}: {
  checked: boolean
  onChange: (next: boolean) => void
  disabled?: boolean
  /** What the switch controls, for anyone not looking at the label beside it. */
  label: string
  className?: string
}): React.JSX.Element {
  return (
    <button
      type="button"
      role="switch"
      aria-checked={checked}
      aria-label={label}
      disabled={disabled}
      onClick={() => onChange(!checked)}
      className={cn(
        'inline-flex h-5 w-9 shrink-0 items-center rounded-full p-0.5 focus:outline-none',
        'focus-visible:ring-2 focus-visible:ring-brand-500/40 disabled:opacity-40',
        checked ? 'bg-brand-600' : 'bg-gray-200',
        className,
      )}
    >
      <span className={cn('block size-4 rounded-full bg-white shadow-xs', checked && 'ml-auto')} />
    </button>
  )
}
