export type ItemType = "task";

export interface ItemRef {
  type: ItemType;
  id: string;
}

export interface Item extends ItemRef {
  createdAt: number;
  archivedAt: number | null;
}

export type TaskStatus = "todo" | "in_progress" | "done";

export interface Task {
  itemId: string;
  title: string;
  status: TaskStatus;
}

export interface TaskInput {
  id: string;
  title: string;
  status: TaskStatus;
}

export interface TaskPatch {
  title?: string;
  status?: TaskStatus;
}

export interface Link {
  id: number;
  source: ItemRef;
  dest: ItemRef;
  createdAt: number;
}

export interface Snapshot {
  items: Item[];
  links: Link[];
  tasks: Task[];
}

export function refKey(ref: ItemRef): string {
  return `${ref.type}:${ref.id}`;
}

export function sameRef(a: ItemRef, b: ItemRef): boolean {
  return a.type === b.type && a.id === b.id;
}
