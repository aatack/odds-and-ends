# Workflows — agent & contributor guide

An Electron + React + TypeScript app for managing a handful of recurring
workflows: meetings, programming, reviewing and Slack. What each workflow needs
to achieve is written in [`workflow-descriptions/`](./workflow-descriptions) —
read the relevant one before building anything for it.

## Data store: the Rust database

All data lives in **`egdb`**, the Rust crate in [`../database`](../database).
Don't add another store (no separate SQLite schema, JSON files or
localStorage for real data).

- **Use it in place.** Depend on the crate by path (`../database`); don't copy or
  fork it. If it lacks something, change it there, with its tests.
- **Reach it from the main process only.** The renderer talks to the main process
  over IPC; the main process calls `egdb` through a native binding. Keep the
  binding thin: it exposes store operations, not workflow logic.
- **Read its docs first:** `README.md` (the model and API in brief), `DESIGN.md`
  (the detail), `ARCHITECTURE.md` (the reasons).

### The model in brief

- A store is one SQLite file: `Store::open(path)`.
- Everything is an **entity** with an id. An entity has **values**
  (`key → JSON`) and **links** to other entities (`source → destination`, with a
  link type and a sort order).
- Nothing is changed in place: every write is an event. Write with
  `store.write(WriteBatch::new(branch, author, ops))`, using `Op::set(id, key,
  value)` and `Op::link(src, dst, type, SortOrder::…)`. Setting a value to null
  clears it; `SortOrder::Removed` removes a link.
- An entity exists once it has events. Give every entity a `type` value;
  `archive_entity` sets it to null, and archived entities drop out of reads.
- **Branches.** Every read and write names a branch. Use one main branch for
  the user's data; let agents work on a `fork` and `merge` it back when accepted.
- **Reads.** `values` / `links` take a batch of ids; `children` / `parents` for
  one entity; `find` for a filtered, paginated list (e.g. all entities of
  `type: meeting`); `walk` for a paginated depth-first traversal.
- **Undo** is per branch and per author, for batches from the last five minutes.

## Develop

Check the database with `cargo test` in `../database`.
