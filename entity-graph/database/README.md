# entity-graph database (`egdb`)

A standalone, event-sourced store of entities, values and links, with branches.
It runs on SQLite (`rusqlite` with the `bundled` feature), so a store is one file and needs no server.
The TypeScript app does not call it.

The design is in [`DESIGN.md`](DESIGN.md).

## Model in brief

- **Events.** There are two kinds:
  - A value event sets `(entity, key) = value`. Writing `Value::Null` clears a field.
  - A link event writes the state of `(source, destination, link_type)`. The write API takes a `SortOrder`:
    - `Timestamp`: stored as the event's timestamp in ms.
    - `Explicit(f64)`: stored as is.
    - `Removed`: stored as NULL.
  - The tables keep one nullable `sort REAL` column. Reads sort live links by it.
- **Entities.** There are no create or delete events. An entity exists when it has events. To archive an entity, set its `type` value to null (`archive_entity`). To restore it, pass the type to `restore_entity`. An index on `(key, value)` serves "entities of type X". `children`, `parents` and `links` show only entities whose `type` is not null. Entities that never had a type are hidden too. `links_including_archived` skips this filter.
- **Sequence IDs.** Every event has a per-branch sequence ID, and `(branch, seq)` is unique.
- **Current and history tables.**
  - `value_current` and `link_current` hold only the last event per target.
  - When an event is superseded, it moves to `value_history` or `link_history` in the same transaction.
- **Branches.**
  - Each branch has a fixed UUIDv7 id.
  - Each branch has a parent list of `(branch, seq range)` entries. An upper bound freezes that parent.
  - `fork` copies the source's whole stack, frozen.
  - `merge` puts another branch's whole stack, frozen, on top of what the branch has now.
  - `pull` adds one raw range.
  - `set_parents` is rebase.
  - Branches are archived, never deleted.
- **Batch reads.** `values`, `links`, `links_including_archived`, `exists` and `is_archived` take a batch of entity IDs and return results keyed by ID (one SQL query per view layer, IDs passed with `json_each`). `value`, `children` and `parents` are thin one-entity helpers.
- **Merged views.** The parent list is a stack. Per target, the winner is ordered by `(layer, seq)`: a later entry wins, and the branch's own events are on top. Timestamps do not order events. Every read flattens the parent graph first (depth-first expansion; each branch kept once, at its first place, with its greatest range; open ranges chain, frozen ranges do not), loads each layer from the current table (open) or current + history (bounded), and rolls up with one shared function.
- **Undo.** Undo works on one branch, for one author, on batches from the last five minutes. It removes the newest batch and moves the rows it replaced back from history. It refuses when a later event changed a target, or when another branch has a frozen range over the batch. Sequence IDs are never reused.

## Use

```rust
use egdb::{Op, SortOrder, Store, WriteBatch};

let mut store = Store::open("notes.db")?;
let main = store.create_branch("main", &[])?;
store.write(WriteBatch::new(main, "alex", vec![
    Op::set("@index", "text", "Index"),
    Op::link("@index", "note-1", "child", SortOrder::Timestamp),
]))?;
let agent = store.fork(main, "agent sandbox")?;
store.write(WriteBatch::new(agent, "agent", vec![Op::set("note-1", "text", "draft")]))?;
store.merge(main, agent)?;                       // or skip it, or archive the branch
let children = store.children(main, "@index", "child")?;
store.undo(main, "alex")?;

use egdb::{Filter, FindOptions, Value};
let open_tasks = Filter::And(vec![
    Filter::Eq("type".into(), Value::Text("task".into())),
    Filter::Contains("title".into(), "report".into()),
    Filter::Gt("score".into(), Value::Int(50)),
]);
let mut cursor = None;
loop {
    let page = store.find(main, &open_tasks, FindOptions { cursor, ..Default::default() })?;
    for row in &page.rows { println!("{} {:?}", row.id, row.values.get("title")); }
    match page.next { Some(next) => cursor = Some(next), None => break }
}

// Top 20 tasks by score, highest first (the cursor of the next page is in `next`).
let top = store.find(main, &Filter::Eq("type".into(), Value::Text("task".into())),
    FindOptions { limit: 20, order: egdb::FindOrder::ValueDesc("score".into()), ..Default::default() })?;
```

### Write middleware

Register middleware to transform the ops of every write batch. It runs inside the write transaction: it can change, drop or add ops, or reject the whole batch. See DESIGN.md section 8.1.1.

```rust
use egdb::{Error, Op, WriteContext};

// Stamp who last touched each entity.
store.add_middleware(|ctx: &WriteContext, ops: &mut Vec<Op>| {
    let touched: Vec<String> = ops.iter()
        .filter_map(|op| match op { Op::SetValue { entity, .. } => Some(entity.clone()), _ => None })
        .collect();
    for e in touched {
        ops.push(Op::set(e, "updated_by", ctx.author));
    }
    Ok(())
});

// Reject every write by an agent author.
store.add_middleware(|ctx: &WriteContext, _ops: &mut Vec<Op>| {
    if ctx.author.starts_with("agent-") {
        return Err(Error::Rejected("agents may not write through this handle".into()));
    }
    Ok(())
});
```

## Status

- **Implemented:**
  - Schema with migrations (`PRAGMA user_version`)
  - Writes
  - Current/history split
  - Values
  - Links grouped by type, with sorting
  - Entity existence and archiving
  - Branches: create, fork, pull, merge, rebase, archive
  - Undo
  - Write middleware
- **Walk:** `walk` is a paginated depth-first walk (type-based follow map, continue/include predicates, step limit, `next` path to resume, read-ahead, optional cache and visited set). See DESIGN.md section 11.1.
- **Find:** `find` gives a paginated flat list of entities that match a `Filter` (Eq, ranges, contains, prefix, in, null, and/or/not, custom), ordered by entity id, insertion, or a value. See DESIGN.md section 11.2.
- **Prove:** `prove` shows that an item is reachable from one of a set of roots through a type-keyed relationship map (for example, permissions). It does a backward BFS from the item, returns a shortest path with its links, and answers `Proved`, `NoPath` or `Unknown` (budget). It works on every view. See DESIGN.md section 11.3.

## Develop

```sh
cargo test
cargo clippy --all-targets -- -D warnings
cargo fmt
```
