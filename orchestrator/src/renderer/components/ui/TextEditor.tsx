import { useEffect, useLayoutEffect, useRef, useState } from "react";
import { cn } from "../../helpers/cn";

// The single text-editing primitive for the whole app. Autosizes: fills its
// width, then grows onto new lines. With `eager`, every keystroke calls
// setValue; otherwise setValue fires on blur or Enter (Shift+Enter = newline).
export function TextEditor({
  value,
  setValue,
  eager = false,
  placeholder,
  className,
  autoFocus,
}: {
  value: string;
  setValue: (value: string) => void;
  eager?: boolean;
  placeholder?: string;
  className?: string;
  autoFocus?: boolean;
}) {
  const [draft, setDraft] = useState(value);
  const ref = useRef<HTMLTextAreaElement>(null);
  // Tracks the last value we handed to setValue. `value` can't do this job: it
  // only catches up after the async action round-trips, so a commit followed by
  // the blur it triggers would both see the stale prop and fire twice.
  const committed = useRef(value);

  useEffect(() => {
    setDraft(value);
    committed.current = value;
  }, [value]);

  useLayoutEffect(() => {
    const el = ref.current;
    if (!el) return;
    el.style.height = "auto";
    el.style.height = `${el.scrollHeight}px`;
  }, [draft]);

  const commit = (next: string) => {
    if (next !== committed.current) {
      committed.current = next;
      setValue(next);
    }
  };

  return (
    <textarea
      ref={ref}
      rows={1}
      value={draft}
      placeholder={placeholder}
      autoFocus={autoFocus}
      onChange={(e) => {
        setDraft(e.target.value);
        if (eager) setValue(e.target.value);
      }}
      onBlur={() => {
        if (!eager) commit(draft);
      }}
      onKeyDown={(e) => {
        if (e.key === "Enter" && !e.shiftKey) {
          e.preventDefault();
          if (!eager) commit(draft);
          e.currentTarget.blur();
        }
      }}
      className={cn(
        "w-full resize-none bg-transparent outline-none placeholder:text-gray-400",
        className,
      )}
    />
  );
}
