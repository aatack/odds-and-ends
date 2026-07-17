import { join } from "path";
import { randomUUID } from "crypto";
import { app } from "electron";
import Database from "better-sqlite3";

let handle: Database.Database | null = null;

export function getDb(): Database.Database {
  if (!handle) {
    handle = new Database(join(app.getPath("userData"), "orchestrator.db"));
    handle.pragma("journal_mode = WAL");
    handle.pragma("foreign_keys = ON");
    migrate(handle);
    seed(handle);
  }
  return handle;
}

function migrate(db: Database.Database): void {
  db.exec(`
    CREATE TABLE IF NOT EXISTS items (
      type        TEXT    NOT NULL,
      id          TEXT    NOT NULL,
      created_at  INTEGER NOT NULL,
      archived_at INTEGER,
      PRIMARY KEY (type, id)
    );

    CREATE TABLE IF NOT EXISTS links (
      id          INTEGER PRIMARY KEY AUTOINCREMENT,
      source_type TEXT    NOT NULL,
      source_id   TEXT    NOT NULL,
      dest_type   TEXT    NOT NULL,
      dest_id     TEXT    NOT NULL,
      created_at  INTEGER NOT NULL,
      UNIQUE (source_type, source_id, dest_type, dest_id)
    );

    CREATE TABLE IF NOT EXISTS tasks (
      item_id     TEXT PRIMARY KEY,
      title       TEXT NOT NULL DEFAULT '',
      status      TEXT NOT NULL DEFAULT 'todo'
    );
  `);

  // Tasks no longer carry a description. Databases created before that change
  // still have the (NOT NULL) column, which breaks inserts that omit it, so
  // drop it once. Guarded by a table_info check to stay idempotent.
  const taskColumns = db.prepare("PRAGMA table_info(tasks)").all() as {
    name: string;
  }[];
  if (taskColumns.some((c) => c.name === "description")) {
    db.exec("ALTER TABLE tasks DROP COLUMN description");
  }
}

// Give a fresh database some example content so the landing page has something
// to render on first launch.
function seed(db: Database.Database): void {
  const count = db.prepare("SELECT COUNT(*) AS n FROM items").get() as { n: number };
  if (count.n > 0) return;

  const now = Date.now();
  const insertItem = db.prepare(
    "INSERT INTO items (type, id, created_at, archived_at) VALUES ('task', ?, ?, NULL)",
  );
  const insertTask = db.prepare(
    "INSERT INTO tasks (item_id, title, status) VALUES (?, ?, ?)",
  );
  const insertLink = db.prepare(
    `INSERT INTO links (source_type, source_id, dest_type, dest_id, created_at)
     VALUES ('task', ?, 'task', ?, ?)`,
  );

  const demo: Array<[string, string]> = [
    ["Wire up the items table", "done"],
    ["Design the item card", "in_progress"],
    ["Command palette", "todo"],
    ["Hook up the links table", "todo"],
  ];

  const ids = demo.map(() => randomUUID());
  const tx = db.transaction(() => {
    demo.forEach(([title, status], i) => {
      insertItem.run(ids[i], now - (demo.length - i) * 60_000);
      insertTask.run(ids[i], title, status);
    });
    insertLink.run(ids[1], ids[3], now);
  });
  tx();
}
