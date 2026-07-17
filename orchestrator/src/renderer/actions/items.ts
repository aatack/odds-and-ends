import { z } from "zod";
import {
  sameRef,
  type Item,
  type ItemRef,
  type Link,
  type Snapshot,
  type Task,
} from "../../shared/types";
import { selectedRefAtom, snapshotAtom } from "../state/store";
import { defineAction, reconcile, type ActionContext, type Persist } from "./registry";

const itemRef = z.object({ type: z.enum(["task"]), id: z.string() });
const taskStatus = z.enum(["todo", "in_progress", "done"]);

function updateSnapshot(ctx: ActionContext, fn: (s: Snapshot) => Snapshot): void {
  ctx.store.set(snapshotAtom, fn(ctx.store.get(snapshotAtom)));
}

export const refreshAction = defineAction({
  name: "snapshot.refresh",
  params: z.object({}),
  log: false,
  palette: { label: "Refresh", defaultParams: {} },
  run: (_p, ctx) => reconcile(ctx),
});

export const createTaskAction = defineAction({
  name: "task.create",
  params: z.object({ title: z.string().optional() }),
  describe: (p) => `Create task “${p.title ?? "New task"}”`,
  palette: {
    label: "New task",
    defaultParams: {},
    fields: [{ path: "title", label: "Title", optional: true, placeholder: "New task" }],
  },
  run: (p, ctx) => {
    const id = crypto.randomUUID();
    const title = p.title ?? "New task";
    const item: Item = { type: "task", id, createdAt: Date.now(), archivedAt: null };
    const task: Task = { itemId: id, title, status: "todo" };
    updateSnapshot(ctx, (s) => ({
      ...s,
      items: [item, ...s.items],
      tasks: [task, ...s.tasks],
    }));
    ctx.store.set(selectedRefAtom, { type: "task", id });
    return () => ctx.api.task.create({ id, title, status: "todo" });
  },
});

export const updateTaskAction = defineAction({
  name: "task.update",
  params: z.object({
    id: z.string(),
    patch: z.object({
      title: z.string().optional(),
      status: taskStatus.optional(),
    }),
  }),
  describe: () => "Update task",
  palette: {
    label: "Update task",
    defaultParams: { id: "", patch: {} },
    fields: [
      { path: "id", label: "Task ID", focusDefault: true },
      { path: "patch.title", label: "Title", optional: true },
      {
        path: "patch.status",
        label: "Status",
        kind: "select",
        options: ["todo", "in_progress", "done"],
        optional: true,
      },
    ],
  },
  run: (p, ctx) => {
    updateSnapshot(ctx, (s) => ({
      ...s,
      tasks: s.tasks.map((t) => (t.itemId === p.id ? { ...t, ...p.patch } : t)),
    }));
    return () => ctx.api.task.update(p.id, p.patch);
  },
});

function setArchived(ctx: ActionContext, ref: ItemRef, at: number | null): Persist {
  updateSnapshot(ctx, (s) => ({
    ...s,
    items: s.items.map((i) => (sameRef(i, ref) ? { ...i, archivedAt: at } : i)),
  }));
  return () => ctx.api.item.setArchived(ref, at);
}

export const archiveItemAction = defineAction({
  name: "item.archive",
  params: z.object({ ref: itemRef }),
  describe: (p) => `Archive ${p.ref.type}`,
  palette: {
    label: "Archive item",
    defaultParams: { ref: { type: "task", id: "" } },
    fields: [{ path: "ref.id", label: "Task ID", focusDefault: true }],
  },
  run: (p, ctx) => setArchived(ctx, p.ref as ItemRef, Date.now()),
});

export const unarchiveItemAction = defineAction({
  name: "item.unarchive",
  params: z.object({ ref: itemRef }),
  describe: (p) => `Restore ${p.ref.type}`,
  palette: {
    label: "Restore item",
    defaultParams: { ref: { type: "task", id: "" } },
    fields: [{ path: "ref.id", label: "Task ID", focusDefault: true }],
  },
  run: (p, ctx) => setArchived(ctx, p.ref as ItemRef, null),
});

export const linkItemsAction = defineAction({
  name: "link.create",
  params: z.object({ source: itemRef, dest: itemRef }),
  describe: (p) => `Link ${p.source.type} → ${p.dest.type}`,
  palette: {
    label: "Link items",
    defaultParams: { source: { type: "task", id: "" }, dest: { type: "task", id: "" } },
    fields: [
      { path: "source.id", label: "Source task ID", focusDefault: true },
      { path: "dest.id", label: "Destination task ID" },
    ],
  },
  run: (p, ctx) => {
    const source = p.source as ItemRef;
    const dest = p.dest as ItemRef;
    const { links } = ctx.store.get(snapshotAtom);
    if (links.some((l) => sameRef(l.source, source) && sameRef(l.dest, dest))) return;
    // Negative sentinel id until the DB assigns the real (positive) one.
    const tempId = -Date.now();
    const link: Link = { id: tempId, source, dest, createdAt: Date.now() };
    updateSnapshot(ctx, (s) => ({ ...s, links: [link, ...s.links] }));
    return async () => {
      const id = await ctx.api.link.create(source, dest);
      updateSnapshot(ctx, (s) => ({
        ...s,
        links: s.links.map((l) => (l.id === tempId ? { ...l, id } : l)),
      }));
    };
  },
});

export const unlinkAction = defineAction({
  name: "link.delete",
  // Identified by its endpoints rather than the internal link id, so the same
  // arguments work from the palette, a hotkey, or the UI without knowing the id.
  params: z.object({ source: itemRef, dest: itemRef }),
  describe: (p) => `Unlink ${p.source.type} → ${p.dest.type}`,
  palette: {
    label: "Remove link",
    defaultParams: { source: { type: "task", id: "" }, dest: { type: "task", id: "" } },
    fields: [
      { path: "source.id", label: "Source task ID", focusDefault: true },
      { path: "dest.id", label: "Destination task ID" },
    ],
  },
  run: (p, ctx) => {
    const source = p.source as ItemRef;
    const dest = p.dest as ItemRef;
    const link = ctx.store
      .get(snapshotAtom)
      .links.find((l) => sameRef(l.source, source) && sameRef(l.dest, dest));
    if (!link) return;
    const { id } = link;
    updateSnapshot(ctx, (s) => ({ ...s, links: s.links.filter((l) => l.id !== id) }));
    return () => ctx.api.link.delete(id);
  },
});
