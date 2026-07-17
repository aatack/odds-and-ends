import { z } from "zod";
import type { ItemRef } from "../../shared/types";
import { selectedRefAtom } from "../state/store";
import { defineAction } from "./registry";

// Selection is user-specific throwaway state, so it is applied synchronously and
// deliberately kept out of the audit log.
export const selectAction = defineAction({
  name: "selection.select",
  params: z.object({
    ref: z.object({ type: z.enum(["task"]), id: z.string() }).nullable(),
  }),
  log: false,
  palette: {
    label: "Select item",
    defaultParams: { ref: { type: "task", id: "" } },
    fields: [{ path: "ref.id", label: "Task ID" }],
  },
  run: (p, ctx) => {
    ctx.store.set(selectedRefAtom, p.ref as ItemRef | null);
  },
});
