import { atom, createStore } from "jotai";
import { atomWithStorage } from "jotai/utils";
import type { ItemRef, Snapshot } from "../../shared/types";

export const emptySnapshot: Snapshot = { items: [], links: [], tasks: [] };

// Persisted (SQLite) state, mirrored into the renderer. Never mutated directly:
// actions call the DB over IPC, then replace this with a fresh snapshot.
export const snapshotAtom = atom<Snapshot>(emptySnapshot);

export const itemsAtom = atom((get) => get(snapshotAtom).items);
export const linksAtom = atom((get) => get(snapshotAtom).links);
export const tasksAtom = atom((get) => get(snapshotAtom).tasks);

// User-specific state we can afford to lose: which item is open. Persisted to
// localStorage, mutated through the (unlogged) selection action.
export const selectedRefAtom = atomWithStorage<ItemRef | null>(
  "orchestrator:selected",
  null,
);

// Colour theme, persisted to localStorage. Defaults to the OS preference on a
// first run. App applies it by toggling the `dark` class on <html>.
export type Theme = "light" | "dark";
export const themeAtom = atomWithStorage<Theme>(
  "orchestrator:theme",
  typeof window !== "undefined" &&
    window.matchMedia?.("(prefers-color-scheme: dark)").matches
    ? "dark"
    : "light",
);

// Command-palette visibility, shared so the activity log can reopen it.
export const paletteOpenAtom = atom(false);

// A pending request to reopen the palette on a specific action's wizard,
// prefilled with previously entered values (used to resume a cancelled action).
export interface PaletteResume {
  // The log entry's id, carried back so re-cancelling updates it in place.
  key: string;
  name: string;
  values: Record<string, string>;
}
export const paletteResumeAtom = atom<PaletteResume | null>(null);

export type ActionStatus = "pending" | "success" | "error" | "cancelled";

export interface ActionLogEntry {
  key: string;
  name: string;
  title: string;
  params: unknown;
  status: ActionStatus;
  startedAt: number;
  finishedAt: number | null;
  error: string | null;
  // The half-entered command-palette values for a cancelled action, so its
  // wizard can be reopened and resumed from where it was abandoned.
  formValues?: Record<string, string>;
}

export const MAX_LOG = 200;

export const actionLogAtom = atom<ActionLogEntry[]>([]);

export const pendingActionsAtom = atom((get) =>
  get(actionLogAtom).filter((e) => e.status === "pending"),
);
export const failedActionsAtom = atom((get) =>
  get(actionLogAtom).filter((e) => e.status === "error"),
);

// A single store instance shared by the React tree (via <Provider>) and by the
// action runner, so actions work identically from the UI, hotkeys, or elsewhere.
export const store = createStore();
