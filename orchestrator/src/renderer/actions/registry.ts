import type { ZodType } from "zod";
import type { OrchestratorApi } from "../../shared/api";
import {
  actionLogAtom,
  MAX_LOG,
  snapshotAtom,
  store,
  type ActionLogEntry,
} from "../state/store";

export interface ActionContext {
  store: typeof store;
  api: OrchestratorApi;
}

// A mutating action applies its change to local state synchronously, then
// returns this thunk to persist that change to the DB in the background.
export type Persist = () => Promise<void>;

// One argument the command palette prompts for before running an action. Its
// collected (string) value is written into a copy of `defaultParams` at `path`.
export interface PaletteField {
  // Dot-path into the params object, e.g. "ref.id" or "patch.status".
  path: string;
  label: string;
  // How the value is entered and coerced. Defaults to "text".
  kind?: "text" | "number" | "select";
  // Choices for a "select" field.
  options?: readonly string[];
  // Optional fields are omitted from the params when left blank.
  optional?: boolean;
  // Prefill with the currently focused item's id (an item-id field).
  focusDefault?: boolean;
  placeholder?: string;
}

export interface ActionDef<P> {
  name: string;
  params: ZodType<P>;
  // The body runs synchronously and applies the optimistic state change; the
  // `Persist` thunk it returns saves that change in the background, so no loading
  // state is ever shown. A body that instead returns a Promise is awaited with a
  // visible pending state — for actions with nothing to show until data arrives
  // (the initial load, a remote fetch).
  run: (params: P, ctx: ActionContext) => Persist | Promise<void> | void;
  // Logged to the audit trail unless `log` is false (e.g. changing selection).
  log?: boolean;
  describe?: (params: P) => string;
  // Present iff the action is safe to launch straight from the command palette.
  // `fields`, when present, are prompted for in a small form before running;
  // their values are merged into `defaultParams`.
  palette?: { label: string; defaultParams: P; fields?: PaletteField[] };
}

const registry = new Map<string, ActionDef<unknown>>();

export function defineAction<P>(def: ActionDef<P>): ActionDef<P> {
  registry.set(def.name, def as ActionDef<unknown>);
  return def;
}

export function getAction(name: string): ActionDef<unknown> | undefined {
  return registry.get(name);
}

export function allActions(): ActionDef<unknown>[] {
  return [...registry.values()];
}

export function paletteActions(): ActionDef<unknown>[] {
  return allActions().filter((a) => a.palette);
}

function appendLog(entry: ActionLogEntry): void {
  store.set(actionLogAtom, (prev) => [entry, ...prev].slice(0, MAX_LOG));
}

// Record an action abandoned from the command palette, keyed by the id its
// wizard was given when it opened. Resuming then re-cancelling reuses that id,
// so the existing entry is updated in place rather than duplicated. Unlogged
// actions (e.g. selection) leave no trace, cancelled or not.
export function logCancelled(
  key: string,
  name: string,
  formValues: Record<string, string>,
): void {
  const def = registry.get(name);
  if (!def || def.log === false) return;
  store.set(actionLogAtom, (prev) => {
    if (prev.some((e) => e.key === key)) {
      return prev.map((e) =>
        e.key === key
          ? { ...e, status: "cancelled", formValues, finishedAt: Date.now() }
          : e,
      );
    }
    const entry: ActionLogEntry = {
      key,
      name,
      title: def.palette?.label ?? name,
      params: null,
      status: "cancelled",
      startedAt: Date.now(),
      finishedAt: Date.now(),
      error: null,
      formValues,
    };
    return [entry, ...prev].slice(0, MAX_LOG);
  });
}

function patchLog(key: string, patch: Partial<ActionLogEntry>): void {
  store.set(actionLogAtom, (prev) =>
    prev.map((e) => (e.key === key ? { ...e, ...patch } : e)),
  );
}

// Replace the mirrored snapshot with the DB's authoritative state. Used as the
// body of the explicit refresh, and to roll a failed optimistic action back.
export async function reconcile(ctx: ActionContext): Promise<void> {
  ctx.store.set(snapshotAtom, await ctx.api.snapshot());
}

export async function runAction(name: string, rawParams: unknown): Promise<void> {
  const def = registry.get(name);
  if (!def) throw new Error(`Unknown action: ${name}`);

  const params = def.params.parse(rawParams);
  const logged = def.log !== false;
  const ctx: ActionContext = { store, api: window.api };
  const key = crypto.randomUUID();
  const entry: ActionLogEntry = {
    key,
    name,
    title: def.describe ? def.describe(params) : name,
    params,
    status: "pending",
    startedAt: Date.now(),
    finishedAt: null,
    error: null,
  };

  const fail = (err: unknown): void => {
    const message = err instanceof Error ? err.message : String(err);
    if (!logged) throw err;
    patchLog(key, { status: "error", finishedAt: Date.now(), error: message });
  };

  let outcome: Persist | Promise<void> | void;
  try {
    outcome = def.run(params, ctx);
  } catch (err) {
    if (logged)
      appendLog({
        ...entry,
        status: "error",
        finishedAt: Date.now(),
        error: err instanceof Error ? err.message : String(err),
      });
    if (!logged) throw err;
    return;
  }

  // Optimistic action: the change is already applied, so it's logged as done
  // straight away — no pending flicker. Persistence runs in the background; a
  // failure moves the entry to the failed list and reconciles from the DB.
  if (typeof outcome === "function") {
    if (logged) appendLog({ ...entry, status: "success", finishedAt: Date.now() });
    try {
      await outcome();
    } catch (err) {
      await reconcile(ctx).catch(() => {});
      fail(err);
    }
    return;
  }

  // Async action: nothing to show yet, so surface a pending state until it settles.
  if (logged) appendLog(entry);
  try {
    await outcome;
    if (logged) patchLog(key, { status: "success", finishedAt: Date.now() });
  } catch (err) {
    fail(err);
  }
}

// Re-run a failed action with the exact arguments it was called with.
export function rerunAction(entry: ActionLogEntry): Promise<void> {
  return runAction(entry.name, entry.params);
}
