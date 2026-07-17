import type { ItemRef, Snapshot, TaskInput, TaskPatch } from "./types";

// The surface exposed to the renderer over IPC (see src/preload/index.ts).
// Every persisted-state mutation the app performs goes through one of these.
export interface OrchestratorApi {
  snapshot: () => Promise<Snapshot>;
  task: {
    create: (input: TaskInput) => Promise<void>;
    update: (id: string, patch: TaskPatch) => Promise<void>;
  };
  item: {
    setArchived: (ref: ItemRef, at: number | null) => Promise<void>;
  };
  link: {
    // Resolves to the id of the created (or already-existing) link.
    create: (source: ItemRef, dest: ItemRef) => Promise<number>;
    delete: (id: number) => Promise<void>;
  };
}
