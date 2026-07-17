import { useAtomValue, useSetAtom } from "jotai";
import {
  actionLogAtom,
  paletteOpenAtom,
  paletteResumeAtom,
  type ActionLogEntry,
} from "../state/store";
import { rerunAction } from "../actions";
import { cn } from "../helpers/cn";
import { relativeTime } from "../helpers/time";
import { Badge, type BadgeColor } from "./ui/Badge";
import { Button } from "./ui/Button";

const STATUS: Record<ActionLogEntry["status"], { label: string; color: BadgeColor }> = {
  pending: { label: "Running", color: "blue" },
  success: { label: "Done", color: "success" },
  error: { label: "Failed", color: "error" },
  cancelled: { label: "Cancelled", color: "gray" },
};

// The audit trail: every logged action, newest first. Failed ones can be
// re-run with their original arguments; cancelled ones reopen their wizard.
export function ActionLog() {
  const log = useAtomValue(actionLogAtom);
  const setResume = useSetAtom(paletteResumeAtom);
  const setPaletteOpen = useSetAtom(paletteOpenAtom);
  const pending = log.filter((e) => e.status === "pending").length;
  const failed = log.filter((e) => e.status === "error").length;

  const resume = (entry: ActionLogEntry) => {
    setResume({ key: entry.key, name: entry.name, values: entry.formValues ?? {} });
    setPaletteOpen(true);
  };

  return (
    <div className="flex h-full flex-col">
      <div className="flex items-center gap-2 border-b border-gray-100 px-4 py-3">
        <h2 className="text-sm font-semibold text-gray-900">Activity</h2>
        <div className="flex-1" />
        {pending > 0 && <Badge color="blue">{pending} running</Badge>}
        {failed > 0 && <Badge color="error">{failed} failed</Badge>}
      </div>

      <div className="flex-1 overflow-y-auto">
        {log.length === 0 ? (
          <p className="px-4 py-6 text-sm text-gray-400">No actions yet.</p>
        ) : (
          <ul className="py-1">
            {log.map((entry) => {
              const cancelled = entry.status === "cancelled";
              return (
                <li
                  key={entry.key}
                  onClick={cancelled ? () => resume(entry) : undefined}
                  title={cancelled ? "Click to resume" : undefined}
                  className={cn(
                    "flex items-start gap-2 px-4 py-2",
                    cancelled && "cursor-pointer transition-colors hover:bg-gray-100/70",
                  )}
                >
                  <div className="min-w-0 flex-1">
                    <div className="truncate text-sm text-gray-800">{entry.title}</div>
                    <div className="mt-0.5 text-xs text-gray-400">
                      {relativeTime(entry.startedAt)}
                      {entry.error && (
                        <span className="text-error-600"> · {entry.error}</span>
                      )}
                    </div>
                  </div>
                  <Badge dot color={STATUS[entry.status].color}>
                    {STATUS[entry.status].label}
                  </Badge>
                  {entry.status === "error" && (
                    <Button size="sm" variant="secondary" onClick={() => rerunAction(entry)}>
                      Retry
                    </Button>
                  )}
                  {cancelled && (
                    <Button
                      size="sm"
                      variant="secondary"
                      onClick={(e) => {
                        e.stopPropagation();
                        resume(entry);
                      }}
                    >
                      Resume
                    </Button>
                  )}
                </li>
              );
            })}
          </ul>
        )}
      </div>
    </div>
  );
}
