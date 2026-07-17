import { getDb } from "./db";
import type {
  Item,
  ItemRef,
  Link,
  Snapshot,
  Task,
  TaskInput,
  TaskPatch,
} from "../shared/types";

interface ItemRow {
  type: string;
  id: string;
  created_at: number;
  archived_at: number | null;
}

interface LinkRow {
  id: number;
  source_type: string;
  source_id: string;
  dest_type: string;
  dest_id: string;
  created_at: number;
}

interface TaskRow {
  item_id: string;
  title: string;
  status: string;
}

function toItem(row: ItemRow): Item {
  return {
    type: row.type as Item["type"],
    id: row.id,
    createdAt: row.created_at,
    archivedAt: row.archived_at,
  };
}

function toLink(row: LinkRow): Link {
  return {
    id: row.id,
    source: { type: row.source_type as ItemRef["type"], id: row.source_id },
    dest: { type: row.dest_type as ItemRef["type"], id: row.dest_id },
    createdAt: row.created_at,
  };
}

function toTask(row: TaskRow): Task {
  return {
    itemId: row.item_id,
    title: row.title,
    status: row.status as Task["status"],
  };
}

export function snapshot(): Snapshot {
  const db = getDb();
  const items = (db.prepare("SELECT * FROM items ORDER BY created_at DESC").all() as ItemRow[]).map(toItem);
  const links = (db.prepare("SELECT * FROM links ORDER BY created_at DESC").all() as LinkRow[]).map(toLink);
  const tasks = (db.prepare("SELECT * FROM tasks").all() as TaskRow[]).map(toTask);
  return { items, links, tasks };
}

export function createTask(input: TaskInput): void {
  const db = getDb();
  const tx = db.transaction(() => {
    db.prepare(
      "INSERT INTO items (type, id, created_at, archived_at) VALUES ('task', ?, ?, NULL)",
    ).run(input.id, Date.now());
    db.prepare(
      "INSERT INTO tasks (item_id, title, status) VALUES (?, ?, ?)",
    ).run(input.id, input.title, input.status);
  });
  tx();
}

export function updateTask(id: string, patch: TaskPatch): void {
  const db = getDb();
  const sets: string[] = [];
  const values: unknown[] = [];
  if (patch.title !== undefined) {
    sets.push("title = ?");
    values.push(patch.title);
  }
  if (patch.status !== undefined) {
    sets.push("status = ?");
    values.push(patch.status);
  }
  if (sets.length === 0) return;
  values.push(id);
  db.prepare(`UPDATE tasks SET ${sets.join(", ")} WHERE item_id = ?`).run(...values);
}

export function setItemArchived(ref: ItemRef, at: number | null): void {
  getDb()
    .prepare("UPDATE items SET archived_at = ? WHERE type = ? AND id = ?")
    .run(at, ref.type, ref.id);
}

export function createLink(source: ItemRef, dest: ItemRef): number {
  const db = getDb();
  const info = db
    .prepare(
      `INSERT OR IGNORE INTO links
         (source_type, source_id, dest_type, dest_id, created_at)
       VALUES (?, ?, ?, ?, ?)`,
    )
    .run(source.type, source.id, dest.type, dest.id, Date.now());
  if (info.changes > 0) return Number(info.lastInsertRowid);
  // The link already existed (INSERT OR IGNORE was a no-op); return its id so
  // the caller can reconcile its optimistic placeholder either way.
  const row = db
    .prepare(
      `SELECT id FROM links
       WHERE source_type = ? AND source_id = ? AND dest_type = ? AND dest_id = ?`,
    )
    .get(source.type, source.id, dest.type, dest.id) as { id: number };
  return row.id;
}

export function deleteLink(id: number): void {
  getDb().prepare("DELETE FROM links WHERE id = ?").run(id);
}
