import { useEffect, useMemo, useRef, useState } from "react";
import { useAtomValue, useSetAtom } from "jotai";
import { logCancelled, paletteActions, runAction, type ActionDef } from "../actions";
import { paletteResumeAtom, selectedRefAtom } from "../state/store";
import { cn } from "../helpers/cn";

// Write `value` into `obj` at a dot-path (e.g. "ref.id"), creating objects along
// the way. Mutates `obj` in place.
function setPath(obj: Record<string, unknown>, path: string, value: unknown): void {
  const parts = path.split(".");
  let cur = obj;
  for (let i = 0; i < parts.length - 1; i++) {
    if (typeof cur[parts[i]] !== "object" || cur[parts[i]] === null) cur[parts[i]] = {};
    cur = cur[parts[i]] as Record<string, unknown>;
  }
  cur[parts[parts.length - 1]] = value;
}

// Merge the collected string values into a copy of the action's default params,
// coercing and skipping blank optional fields.
function buildParams(action: ActionDef<unknown>, values: Record<string, string>): unknown {
  const params = structuredClone(action.palette!.defaultParams) as Record<string, unknown>;
  for (const field of action.palette!.fields ?? []) {
    const raw = values[field.path] ?? "";
    if (field.optional && raw.trim() === "") continue;
    setPath(params, field.path, field.kind === "number" ? Number(raw) : raw);
  }
  return params;
}

// Seed the wizard: id fields marked `focusDefault` start from the focused item.
function initialValues(
  action: ActionDef<unknown>,
  focusedId: string,
): Record<string, string> {
  const values: Record<string, string> = {};
  for (const field of action.palette!.fields ?? []) {
    values[field.path] = field.focusDefault && focusedId ? focusedId : "";
  }
  return values;
}

// Ctrl+P launcher. The registry is the single source of truth: anything with a
// `palette` entry is runnable here. Actions that need arguments collect them one
// at a time through the very same input, so nothing pops up underneath — the
// input becomes the field, its placeholder names the argument, and the action
// name sits on the right.
export function CommandPalette({
  open,
  onClose,
}: {
  open: boolean;
  onClose: () => void;
}) {
  const [query, setQuery] = useState("");
  // The action being configured, or null while browsing the list.
  const [selected, setSelected] = useState<ActionDef<unknown> | null>(null);
  const [values, setValues] = useState<Record<string, string>>({});
  // The id minted when this wizard opened, and the pristine defaults it started
  // from — used to log a cancellation under a stable id, and only when the
  // arguments were actually touched.
  const [wizardKey, setWizardKey] = useState<string | null>(null);
  const [defaults, setDefaults] = useState<Record<string, string>>({});
  const [step, setStep] = useState(0);
  const [error, setError] = useState<string | null>(null);
  // Which list row the arrow keys have highlighted.
  const [activeIndex, setActiveIndex] = useState(0);
  const activeRef = useRef<HTMLButtonElement>(null);
  const inputRef = useRef<HTMLInputElement>(null);
  const actions = useMemo(() => paletteActions(), []);
  const focusedId = useAtomValue(selectedRefAtom)?.id ?? "";
  const resume = useAtomValue(paletteResumeAtom);
  const setResume = useSetAtom(paletteResumeAtom);

  const matches = actions.filter((a) =>
    a.palette!.label.toLowerCase().includes(query.toLowerCase()),
  );
  // Clamp so the highlight stays valid as the filtered list shrinks.
  const active = matches.length ? Math.min(activeIndex, matches.length - 1) : 0;

  const fields = selected?.palette!.fields ?? [];
  const stepIndex = Math.min(step, Math.max(0, fields.length - 1));
  const field = selected ? fields[stepIndex] : null;
  const isLast = stepIndex >= fields.length - 1;

  useEffect(() => {
    if (!open) {
      setQuery("");
      setSelected(null);
      setValues({});
      setWizardKey(null);
      setDefaults({});
      setStep(0);
      setError(null);
      setActiveIndex(0);
    }
  }, [open]);

  // Resuming a cancelled action: jump straight into its wizard, prefilled, and
  // reuse its log id so re-cancelling updates that entry instead of adding one.
  useEffect(() => {
    if (!open || !resume) return;
    const action = actions.find((a) => a.name === resume.name);
    if (action) {
      setSelected(action);
      setValues(resume.values);
      setDefaults(initialValues(action, focusedId));
      setWizardKey(resume.key);
      setStep(0);
      setError(null);
    }
    setResume(null);
  }, [open, resume, actions, focusedId, setResume]);

  // Keep focus in the one input as we swap between browsing and each step, and
  // keep the highlighted list row scrolled into view.
  useEffect(() => {
    if (open) inputRef.current?.focus();
  }, [open, selected, step]);
  useEffect(() => {
    if (!selected) activeRef.current?.scrollIntoView({ block: "nearest" });
  }, [active, selected]);

  if (!open) return null;

  const run = (name: string, params: unknown) => {
    onClose();
    runAction(name, params);
  };

  // Escape or an outside click abandons the whole thing. If arguments were being
  // entered and any differ from their defaults, it's logged (or the resumed
  // entry updated) as cancelled so it can be resumed from the activity log.
  const cancel = () => {
    if (selected && wizardKey) {
      const touched = fields.some((f) => (values[f.path] ?? "") !== (defaults[f.path] ?? ""));
      if (touched) logCancelled(wizardKey, selected.name, values);
    }
    onClose();
  };

  // Enter the wizard (or run immediately when the action takes no arguments).
  const pick = (a: ActionDef<unknown>) => {
    if (a.palette!.fields?.length) {
      const init = initialValues(a, focusedId);
      setSelected(a);
      setValues(init);
      setDefaults(init);
      setWizardKey(crypto.randomUUID());
      setStep(0);
      setError(null);
    } else {
      run(a.name, a.palette!.defaultParams);
    }
  };

  const submit = () => {
    if (!selected) return;
    const parsed = selected.params.safeParse(buildParams(selected, values));
    if (!parsed.success) {
      setError(parsed.error.issues[0]?.message ?? "Invalid input");
      return;
    }
    run(selected.name, parsed.data);
  };

  const forward = () => {
    if (isLast) submit();
    else {
      setStep(stepIndex + 1);
      setError(null);
    }
  };
  const backward = () => {
    if (stepIndex === 0) {
      // Step back off the first field returns to browsing the action list.
      setSelected(null);
      setError(null);
    } else {
      setStep(stepIndex - 1);
      setError(null);
    }
  };

  const onKeyDown = (e: React.KeyboardEvent) => {
    if (e.key === "Escape") {
      e.preventDefault();
      cancel();
      return;
    }
    if (selected) {
      // Argument entry. Enter always advances (running on the last step); Tab
      // advances too, except on the last step where only Enter may run it.
      if (e.key === "Enter") {
        e.preventDefault();
        forward();
      } else if (e.key === "Tab" && e.shiftKey) {
        e.preventDefault();
        backward();
      } else if (e.key === "Tab") {
        e.preventDefault();
        if (!isLast) forward();
      }
      return;
    }
    // Browsing the list. Shift+Tab is swallowed so focus never escapes the
    // palette to the page behind it.
    if (e.key === "Tab" && e.shiftKey) {
      e.preventDefault();
      return;
    }
    const target = matches[active];
    // Enter selects the highlighted action, running it if it takes no arguments.
    if (e.key === "Enter" && target) {
      e.preventDefault();
      pick(target);
    }
    // Tab advances into an action's arguments, but never executes on its own:
    // an action with no arguments still needs Enter for the final confirmation.
    if (e.key === "Tab" && target) {
      e.preventDefault();
      if (target.palette!.fields?.length) pick(target);
    }
    if (e.key === "ArrowDown" && matches.length) {
      e.preventDefault();
      setActiveIndex((i) => (Math.min(i, matches.length - 1) + 1) % matches.length);
    }
    if (e.key === "ArrowUp" && matches.length) {
      e.preventDefault();
      setActiveIndex(
        (i) => (Math.min(i, matches.length - 1) + matches.length - 1) % matches.length,
      );
    }
  };

  // The one input serves both modes: a search box while browsing, the current
  // argument's field while entering one.
  const placeholder = field
    ? field.kind === "select"
      ? `${field.label} (${(field.options ?? []).join(" / ")})`
      : field.label
    : "Run an action…";
  const value = field ? values[field.path] ?? "" : query;
  const onChange = (next: string) => {
    if (field) {
      setValues((v) => ({ ...v, [field.path]: next }));
      setError(null);
    } else {
      setQuery(next);
      setActiveIndex(0);
    }
  };

  return (
    <div
      className="fixed inset-0 z-50 flex items-start justify-center bg-black/10 pt-32 backdrop-blur-xs animate-fade-in"
      onClick={cancel}
    >
      <div
        className="w-full max-w-lg overflow-hidden rounded-xl bg-white shadow-lg animate-pop-in"
        onClick={(e) => e.stopPropagation()}
      >
        <div className="flex items-center border-b border-gray-100">
          <input
            ref={inputRef}
            autoFocus
            value={value}
            onChange={(e) => onChange(e.target.value)}
            onKeyDown={onKeyDown}
            placeholder={placeholder}
            className="min-w-0 flex-1 bg-transparent px-4 py-3.5 text-sm outline-none placeholder:text-gray-400"
          />
          {selected && (
            <span className="whitespace-nowrap px-4 text-xs font-medium text-gray-400">
              {selected.palette!.label}
            </span>
          )}
        </div>

        {selected ? (
          error && <div className="px-4 py-2.5 text-xs text-red-500">{error}</div>
        ) : (
          <ul className="max-h-80 overflow-y-auto py-1">
            {matches.length === 0 ? (
              <li className="px-4 py-3 text-sm text-gray-400">No matching actions.</li>
            ) : (
              matches.map((a, i) => (
                <li key={a.name}>
                  <button
                    ref={i === active ? activeRef : undefined}
                    onClick={() => pick(a)}
                    onMouseMove={() => setActiveIndex(i)}
                    className={cn(
                      "flex w-full items-center justify-between px-4 py-2.5 text-left text-sm transition-colors",
                      i === active && "bg-gray-100/70",
                    )}
                  >
                    <span className="font-medium text-gray-800">{a.palette!.label}</span>
                    <span className="text-xs text-gray-400">{a.name}</span>
                  </button>
                </li>
              ))
            )}
          </ul>
        )}
      </div>
    </div>
  );
}
