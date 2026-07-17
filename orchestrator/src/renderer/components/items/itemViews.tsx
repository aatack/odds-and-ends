import type { FC } from "react";
import type { ItemType, Snapshot, Task, TaskStatus } from "../../../shared/types";
import type { BadgeColor } from "../ui/Badge";
import { TextEditor } from "../ui/TextEditor";
import { runAction } from "../../actions";
import { cn } from "../../helpers/cn";

export interface ItemSummary {
  title: string;
  status?: { label: string; color: BadgeColor };
}

// Everything type-specific about an item lives behind this interface, so the
// Pill / Card / Detail shells stay generic. Register a new item type by adding
// an entry to `itemViews` below.
//
// `CardBody` renders the compact, type-specific overview shown when the item
// appears as a single (possibly nested) bullet in a list — a few lines at most,
// reflecting the item's current state. `DetailBody` is the full editor.
export interface ItemView<Data = unknown> {
  label: string;
  color: BadgeColor;
  data: (snapshot: Snapshot, id: string) => Data | undefined;
  summary: (data: Data) => ItemSummary;
  CardBody?: FC<{ data: Data }>;
  DetailBody?: FC<{ data: Data }>;
}

const TASK_STATUS: Record<TaskStatus, { label: string; color: BadgeColor }> = {
  todo: { label: "To do", color: "gray" },
  in_progress: { label: "In progress", color: "blue" },
  done: { label: "Done", color: "success" },
};

const TASK_STATUSES: TaskStatus[] = ["todo", "in_progress", "done"];

// The card shows only the task's name — a single bullet's worth of overview.
const TaskCardBody: FC<{ data: Task }> = ({ data }) => (
  <div className="font-serif text-[15px] text-gray-900">
    {data.title || "Untitled task"}
  </div>
);

const TaskDetailBody: FC<{ data: Task }> = ({ data }) => (
  <div className="flex flex-col gap-4">
    <TextEditor
      value={data.title}
      setValue={(title) => runAction("task.update", { id: data.itemId, patch: { title } })}
      placeholder="Task title"
      className="font-serif text-2xl font-semibold tracking-tightish text-gray-900"
    />

    <div className="inline-flex items-center gap-0.5 self-start rounded-lg bg-gray-100 p-0.5">
      {TASK_STATUSES.map((status) => {
        const active = data.status === status;
        return (
          <button
            key={status}
            onClick={() => runAction("task.update", { id: data.itemId, patch: { status } })}
            className={cn(
              "rounded-md px-2.5 py-1 text-[13px] font-medium transition-colors",
              active
                ? "bg-white text-gray-900 shadow-xs"
                : "text-gray-500 hover:text-gray-700",
            )}
          >
            {TASK_STATUS[status].label}
          </button>
        );
      })}
    </div>
  </div>
);

const taskView: ItemView<Task> = {
  label: "Task",
  color: "brand",
  data: (snapshot, id) => snapshot.tasks.find((t) => t.itemId === id),
  summary: (task) => ({
    title: task.title || "Untitled task",
    status: TASK_STATUS[task.status],
  }),
  CardBody: TaskCardBody,
  DetailBody: TaskDetailBody,
};

export const itemViews: Record<ItemType, ItemView> = {
  task: taskView as ItemView,
};
