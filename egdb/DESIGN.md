# egdb: design

Status: draft 3, for review.
Drafts 2 and 3 add Alexander's decisions of 2026-10-12 and 2026-10-13.
This document is the design.
The implementation is in this folder (Rust crate `egdb`).

Writing rule:

- The prose uses ASD-STE100 Simplified Technical English.
- Code, SQL and the verbatim quote in Appendix A do not.

Marks:

- **[RULE]**: a decision from Alexander. Do not change it.
- **[CHOSEN]**: an interpretation that we chose where a rule is not clear. Section 13 lists all of them.
- **[TBD: Alexander]**: a placeholder. Alexander will give the semantics later.

---

## 1. Rules from Alexander

Each rule below is a restatement of Alexander's words in Simplified Technical English.
Appendix A gives his words verbatim.
If the two are different, Appendix A is correct.

1. **R1 [RULE]. Sequence IDs.**
   - Each event has a sequence ID for its branch.
   - The pair (branch ID, sequence ID) is unique.
   - Sequence IDs give the order of events, also when a merge brings the events in out of order.
2. **R2 [RULE] (B1). Forks.** After a fork, a branch does not see new events of its parent.
3. **R3 [RULE]. Branches and parents.**
   - Each branch has a branch ID.
   - Each branch has an optional list of parents.
   - Each entry in the list is a branch ID and a range of sequence IDs.
   - An upper bound on the range freezes that parent.
   - Any branch can pull from any number of other branches.
4. **R4 [RULE]. Merge.**
   - Merge is necessary.
   - Merge has no conflict resolution.
   - The data is a conflict-free type, so stacking always gives a valid result.
5. **R5 [RULE]. Rebase.** Rebase means: change the parent list so that it points to other targets.
6. **R6 [RULE]. No branch deletion.** A user can only archive a branch.
7. **R7 [RULE]. Child branches.**
   - Child branches are permitted, with no limit on depth.
   - The model uses parent lists. It does not use child pointers.
8. **R8 [RULE]. Undo.** Undo operates on one branch, for one user, on events from the last five minutes.
9. **R9 [RULE] (B7). Branch IDs.** A branch ID is fixed when the branch is created. This is for sync in the future.
10. **R10 [RULE]. Writes.** Frozen and merged branches can accept writes, for now.
11. **R11 [RULE]. Purpose.**
    - Branches are sandboxes for agents.
    - Agents can make errors freely.
    - The user then reverts the branch, or does not merge it.
12. **R12 [RULE]. Entities.**
    - There are no events that create or delete an entity.
    - An entity exists canonically when it has events.
    - Deletion is archiving.
13. **R13 [RULE]. Fields.**
    - There are no events that clear a field.
    - To clear a field, write null.
14. **R14 [RULE]. Links.**
    - Links carry no data.
    - Each link has a link type and a sort order that can be null.
    - A null sort order means "use the timestamp of the add".
    - An explicit float sorts all links of that type.
15. **R15 [RULE]. Children.** To find the children of an entity for one link type, do these steps:
    1. Take the events with that source ID and that type.
    2. Group them by type. The defaults are inbound and outbound.
    3. Remove duplicates: keep the last event for each (source, destination, type).
    4. Sort the result by sort order.
16. **R16 [RULE]. Sort states.**
    - There are three sort states:
      - Use the timestamp (the default).
      - An explicit float.
      - Removed.
    - This is the same pattern as "null clears a field".
17. **R17 [RULE]. Open question to record.** Must the API force an explicit value in place of null for the default? **Resolved (2026-10-12):** the store keeps one nullable float. See section 8.4 and Q1.
18. **R18 [RULE]. Diffs.** Diffs are out of scope for now.

Later rules (2026-10-12). These rules replace parts of the rules above and of draft 2:

19. **R19 [RULE]. Undo removes events.** Undo deletes the events from the current tables. It has a time limit of five minutes. Normal reads must never load history.
20. **R20 [RULE]. Order across branches.** Order by parent branch first, in parent-list order. Then order by seq in each branch. A later entry in the list wins. The branch's own events are last, so they always win.
21. **R21 [RULE]. Archived entities.** To archive an entity, set its `type` to null. To restore it, the caller supplies the old type. An index must make "all entities of type X" fast.
22. **R22 [RULE]. One sort column.** A link row stores one nullable float, `sort`. The write API keeps the three states. Before a write, the default becomes the event's timestamp in ms. Null means removed.
23. **R23 [RULE]. Hide entities with no type.** `children`, `parents`, `links`, `walk` and `find` apply an implicit filter. They return only entities whose winning `type` value is not null.
24. **R24 [RULE]. Do not copy events.** Do not copy events unless there is a very good reason. Use parent ranges to show events of other branches. Keep one copy of each event.
25. **R25 [RULE]. One roll-up for every read.** Every read can roll up historical events. A read points at the current table most of the time, so it rolls up few events. A read that is not at the head of a branch loads events from both tables. It rolls them up with the same logic. No other logic is necessary.
26. **R26 [RULE]. Flatten the parent chain first.** Before a read loads events, the store resolves the branch's parent chain into one flat list. The store dedupes branches. Each branch takes the greatest sequence range that is covered. Each branch then appears where it first appeared in the list. (This replaces the earlier C3 rule.)
27. **R27 [RULE]. Open wins over frozen.** If the flat list has an open range and a frozen range of the same branch, the open range keeps the events after the dedupe. Alexander accepted this.
28. **R28 [RULE]. Flattening effects are accepted.** Alexander accepted both effects of R26 (2026-10-13):
    - A branch that the list reaches low and high stays at its first (low) position.
    - The greatest range spans the gaps between appearances.
29. **R29 [RULE]. Batch reads.** Every low-level read takes a batch of entity IDs. It returns the results by ID. `values` returns all values of each entity.
30. **R30 [RULE]. Walk.** `walk` takes a starting path, a map from the source entity's type to the links to follow (each a link type, a direction and a child type), a "continue" predicate, an "include" predicate and a step limit. It returns the paths of the emitted nodes, the cache of the entities on those paths, and the path of the next node to visit (`None` when exhausted). It never allows cycles: a node already on the current path is skipped completely. Option: a global visited set.
31. **R31 [RULE]. Read-ahead.** On a cache miss, `walk` loads the needed entities plus about two layers of their followed links, in batches. The cache lives for one walk.

---

## 2. Goals

1. Supply a Rust library that keeps entities, values and links as events.
2. Keep one store in one file. Do not use a separate server.
3. Make setup easy. `cargo add` must be sufficient.
4. Make reads and writes fast.
5. Keep the event model small: value events and link events only.
6. Put only current events in the hot tables. Move superseded events to history tables, one for each event type. Keep history queryable.
7. Support branches as agent sandboxes (R2 to R11).
8. Make each write batch one transaction.
9. Make undo remove the events of a recent batch (R19). Normal reads never use history.
10. Supply dedicated reads: `walk` (done, R30) and `find` (done, section 11.2).

## 3. Non-goals (for now)

1. Native bindings (napi-rs and the like).
   - The crate is a standalone database.
   - Other languages reach it through the HTTP API (section 15) and the TypeScript client in `client/`.
2. Diffs between branches (R18).
3. Conflict resolution (R4).
4. Sync between two store files. R9 prepares for it.
5. A query language.
6. Full-text search.
7. Resources (blobs). See Q9.

---

## 4. Storage choice

| Option | Setup | For | Against |
| --- | --- | --- | --- |
| SQLite via `rusqlite` (`bundled`) | `cargo add rusqlite --features bundled` | One file. ACID. B-tree, covering and partial indexes. WAL readers. SQL for history. Very mature. | One writer at a time. |
| `redb` | `cargo add redb` | One file. Pure Rust. Fast. | No query planner. We must write all indexes and queries by hand. |
| DuckDB (`bundled`) | `cargo add duckdb --features bundled` | Fast scans over history. | Slow for many small writes and point reads. Large build. |

**Decision: SQLite via `rusqlite` with `bundled`.**

- The workload is many small writes and many point and prefix reads. SQLite does this well.
- One writer is acceptable for a local, embedded store.

---

## 5. Concepts

- **Store**: one SQLite file. It holds all branches.
- **Entity**: a string ID.
  - An entity exists on a branch when the branch sees an event that names it (R12).
  - The event can be a value event, or a link event at either end.
- **Value event**: sets `(entity, key)` to a typed value. Null clears the field (R13).
- **Link event**: writes the state of the link `(source, destination, link_type)`.
  - The state is one of the three sort states (R16).
  - A link carries no other data (R14).
- **Target**: the thing that an event changes.
  - For a value event, the target is `(entity, key)`.
  - For a link event, the target is `(source, link_type, destination)`.
- **Sequence ID (seq)**: an integer for each event on a branch.
  - The first event on a branch has seq 1.
  - `(branch, seq)` is unique (R1).
  - `(branch ID, seq)` is also a globally unique event ID, because branch IDs are UUIDs (R9).
- **Batch**: the events of one write call. A batch is one transaction and one unit of undo.
- **Branch**: a fixed UUIDv7 ID, a name, an archived flag, and a parent list (R3, R9).
- **View**: the events that a branch can see: its own events, plus the parent ranges in its parent list.

---

## 6. Data model (Rust types)

These are the types in `egdb` (`src/types.rs`).

```rust
pub type Seq = i64;            // per-branch sequence ID, starts at 1
pub type Timestamp = i64;      // Unix ms
pub const TYPE_KEY: &str = "type";   // null type = archived (R21)
pub const UNDO_WINDOW_MS: Timestamp = 5 * 60 * 1000;

pub struct BranchId(pub Uuid); // UUIDv7, fixed at creation (R9)

pub struct SeqRange { pub start: Seq, pub end: Option<Seq> } // inclusive; end = None is open (R3)
pub struct ParentRef { pub branch: BranchId, pub range: SeqRange }
pub struct BranchInfo { pub id: BranchId, pub name: String, pub archived: bool,
                        pub head: Seq, pub parents: Vec<ParentRef>, pub created_at: Timestamp }

pub enum Value { Null, Bool(bool), Int(i64), Float(f64), Text(String), Json(serde_json::Value) }

pub enum SortOrder { Timestamp, Explicit(f64), Removed }   // R16: write API only; stored as REAL NULL (R22)

pub enum Op {
    SetValue { entity: String, key: String, value: Value },                         // Null clears (R13)
    Link { source: String, destination: String, link_type: String, sort: SortOrder }, // R14
}

pub struct WriteBatch { pub branch: BranchId, pub author: String,
                        pub timestamp: Option<Timestamp>, pub ops: Vec<Op> }
pub struct WriteResult { pub branch: BranchId, pub first_seq: Seq, pub last_seq: Seq, pub timestamp: Timestamp }

pub struct EventRef { pub branch: BranchId, pub seq: Seq, pub timestamp: Timestamp, pub author: String }
pub struct Link { pub source: String, pub destination: String, pub link_type: String,
                  pub sort: f64 /* stored key; removed links are not returned */, pub event: EventRef }
pub enum Direction { Outbound, Inbound }
pub struct UndoOutcome { pub removed: (Seq, Seq), pub events: usize, pub restored: usize }
```

Removed from draft 1, because the rules now forbid them:

- `NodeOp` (create and delete) is removed (R12).
- `ValueOp::Remove` is removed (R13).
- Link IDs, positions, `Placement` and link values are removed (R14).
- The global sequence counter is removed (R1).

### 6.1 Definition of "superseded"

Event E2 supersedes event E1 when all of these are true:

- E1 and E2 are on the same branch.
- E1 and E2 have the same target.
- E2 has a higher seq.

| Event type | Target (supersede key) |
| --- | --- |
| Value event | `(branch, entity, key)` |
| Link event | `(branch, source, link_type, destination)` |

Rules:

1. The store gives each event the next seq of its branch. A new event therefore always supersedes the current row for its target.
2. In one batch, a later op on the same target supersedes an earlier op.
3. A null value (R13) and a `Removed` link (R16) stay in the current table. They are normal events. Thus each target that has events has exactly one current row on each branch.
4. An event on one branch never supersedes an event on another branch. Across branches, the view order decides which event wins (section 8.2).
5. History rows never change. Only undo deletes them, or moves them back to the current table (section 10).

---

## 7. SQL schema (version 1)

This is migration 1 in `src/schema.rs`.

- All tables are `STRICT`.
- The current and history tables are `WITHOUT ROWID`.
- Branch IDs and entity IDs are interned to integers (`bid`, `eid`). This makes keys and indexes small.

```sql
CREATE TABLE branches (
  bid        INTEGER PRIMARY KEY,          -- local key, compact
  id         BLOB    NOT NULL UNIQUE,      -- BranchId (UUIDv7), fixed at creation
  name       TEXT    NOT NULL,
  archived   INTEGER NOT NULL DEFAULT 0,
  head_seq   INTEGER NOT NULL DEFAULT 0,   -- last sequence ID written on this branch
  last_ts    INTEGER NOT NULL DEFAULT 0,   -- last event timestamp (timestamps never go down)
  created_at INTEGER NOT NULL
) STRICT;

-- The parent list. Each entry reads the parent's own events in [seq_start, seq_end].
-- seq_end NULL = open; a value freezes the parent.
CREATE TABLE branch_parents (
  bid        INTEGER NOT NULL REFERENCES branches(bid),
  pos        INTEGER NOT NULL,
  parent_bid INTEGER NOT NULL REFERENCES branches(bid),
  seq_start  INTEGER NOT NULL,
  seq_end    INTEGER,
  PRIMARY KEY (bid, pos)
) STRICT, WITHOUT ROWID;

-- Interned entity IDs. An entity exists when it has events; this is only a dictionary.
CREATE TABLE entities (
  eid    INTEGER PRIMARY KEY,
  ext_id TEXT    NOT NULL UNIQUE
) STRICT;

-- One row per write batch: the unit of undo.
CREATE TABLE batches (
  bid        INTEGER NOT NULL REFERENCES branches(bid),
  first_seq  INTEGER NOT NULL,
  last_seq   INTEGER NOT NULL,
  author     TEXT    NOT NULL,
  created_at INTEGER NOT NULL,             -- store clock, used for the undo window
  PRIMARY KEY (bid, first_seq)
) STRICT, WITHOUT ROWID;
CREATE INDEX batches_author ON batches(bid, author, first_seq);

-- Values: one current row per (branch, entity, key). Superseded rows move to history.
CREATE TABLE value_current (
  bid    INTEGER NOT NULL,
  eid    INTEGER NOT NULL,
  key    TEXT    NOT NULL,
  seq    INTEGER NOT NULL,
  ts     INTEGER NOT NULL,
  author TEXT    NOT NULL,
  vtype  INTEGER NOT NULL,                 -- 0 null, 1 bool, 2 int, 3 float, 4 text, 5 json
  value  ANY,
  PRIMARY KEY (bid, eid, key),
  UNIQUE (bid, seq)
) STRICT, WITHOUT ROWID;

CREATE TABLE value_history (
  bid           INTEGER NOT NULL,
  seq           INTEGER NOT NULL,
  eid           INTEGER NOT NULL,
  key           TEXT    NOT NULL,
  ts            INTEGER NOT NULL,
  author        TEXT    NOT NULL,
  vtype         INTEGER NOT NULL,
  value         ANY,
  superseded_by INTEGER NOT NULL,          -- seq of the event that replaced it
  PRIMARY KEY (bid, seq)
) STRICT, WITHOUT ROWID;
-- History has the same lookups as current, plus seq (R25).
CREATE INDEX value_history_target     ON value_history(bid, eid, key, seq);
CREATE INDEX value_history_value      ON value_history(bid, key, value, eid, seq, superseded_by);  -- V2
CREATE INDEX value_history_superseded ON value_history(bid, superseded_by);  -- undo

-- Find by value, including "all entities of type X" (key = 'type').
CREATE INDEX value_current_value ON value_current(bid, key, value, eid);

-- Links: one current row per (branch, source, type, destination).
CREATE TABLE link_current (
  bid         INTEGER NOT NULL,
  source      INTEGER NOT NULL,
  link_type   TEXT    NOT NULL,
  destination INTEGER NOT NULL,
  seq         INTEGER NOT NULL,
  ts          INTEGER NOT NULL,
  author      TEXT    NOT NULL,
  sort        REAL,                        -- sort key (R22); NULL = removed
  PRIMARY KEY (bid, source, link_type, destination),
  UNIQUE (bid, seq)
) STRICT, WITHOUT ROWID;
CREATE INDEX link_current_in ON link_current(bid, destination, link_type);

CREATE TABLE link_history (
  bid           INTEGER NOT NULL,
  seq           INTEGER NOT NULL,
  source        INTEGER NOT NULL,
  link_type     TEXT    NOT NULL,
  destination   INTEGER NOT NULL,
  ts            INTEGER NOT NULL,
  author        TEXT    NOT NULL,
  sort          REAL,
  superseded_by INTEGER NOT NULL,
  PRIMARY KEY (bid, seq)
) STRICT, WITHOUT ROWID;
CREATE INDEX link_history_target ON link_history(bid, source, link_type, destination, seq);
CREATE INDEX link_history_in         ON link_history(bid, destination, link_type, seq);
CREATE INDEX link_history_superseded ON link_history(bid, superseded_by);  -- undo
```

| Index | Used by |
| --- | --- |
| PK `value_current (bid, eid, key)` | All values of an entity (prefix). One value. Supersede. |
| PK `link_current (bid, source, link_type, destination)` | Outbound links of one type (prefix). One link. Supersede. |
| `link_current_in (bid, destination, link_type)` | Inbound links (parents). |
| `UNIQUE (bid, seq)` on current tables, PK `(bid, seq)` on history | R1 uniqueness. The events of one batch (undo). |
| `value_current_value (bid, key, value, eid, seq)` | Find by value. `seq` (schema V3) lets a value-order scan tell the winning row from the index alone. "All entities of type X" (R21): a null type does not match, so archived entities drop out. |
| `value_history_target (bid, eid, key, seq)` | Bounded roll-up of values (section 8.3). The same lookup as the current PK, plus seq. |
| `value_history_value (bid, key, value, eid, seq, superseded_by)` | `find` in a bounded range. `superseded_by` (added in schema V2) lets the "still the winner at the range end" check use the index alone. |
| `link_history_target (bid, source, link_type, destination, seq)` | Bounded roll-up of outbound links. |
| `link_history_in (bid, destination, link_type, seq)` | Bounded roll-up of inbound links. Entity existence. |
| `batches_author (bid, author, first_seq)` | Undo: the newest batch of one author. |
| `value_history_superseded`, `link_history_superseded` | Undo: find the rows that a batch replaced. |

**Index decisions.**

- The new `value_current_value` index makes the partial `value_current_type` index redundant. `EXPLAIN QUERY PLAN` for `key = 'type' AND value = ?` uses `value_current_value`. We removed the partial index.
- The old `link_history_in (bid, destination)` is replaced by the longer index. No other index became redundant.
- Each history table has the same lookups as its current table, with seq last.

**Schema version and migrations.**

The crate is not released. All changes so far are in version 1. There is no version 2 yet.


1. `PRAGMA user_version` holds the version.
2. `MIGRATIONS` is an ordered, append-only list.
3. `Store::open` applies each missing migration in its own transaction.
4. If the file is newer than the crate, `open` returns `SchemaTooNew`. It does not change the file.

---

## 8. Write path and read path

### 8.1 Write path (`Store::write`)

All steps run in one `BEGIN IMMEDIATE` transaction. If a step fails, the store writes nothing.

1. **Batch check.** A batch must have at least one op.
2. **Branch check.** The branch must exist and must not be archived.
3. **Timestamp.**
   - `ts = max(batch.timestamp or store clock, branch.last_ts)`.
   - Timestamps thus never go down on a branch [CHOSEN, I13]. The timestamp gives the default sort key. It does not order events.
4. **Middleware.** Any registered write middleware transforms the ops (see 8.1.1). It runs here, inside the transaction, before validation.
5. **Validate** the ops (the transformed ops).
   - IDs, keys and link types must not be empty.
   - Floats must be finite.
   - Validation runs after the branch check now, not before it. So an invalid op on an unknown branch reports the unknown branch first.
   - If middleware dropped every op, the store writes nothing and stops (see 8.1.1).
6. **For each op:**
   1. Increment the branch's seq.
   2. Intern the entity IDs. For a link, replace `SortOrder` with the stored key: `Timestamp` becomes `ts` as `f64`, `Explicit(f)` becomes `f`, and `Removed` becomes NULL (R22).
   3. Copy the current row of the target to its history table, with `superseded_by = seq`.
   4. Upsert the new current row.
7. **Finish.**
   - Update `branches.head_seq` and `last_ts`.
   - Insert the `batches` row.
8. **COMMIT.** A reader sees the state before the commit or after it, never between.

### 8.1.1 Write middleware

Write middleware transforms the ops of a batch before the store writes them. It is a hook for the caller.

- A middleware takes the batch context and the ops. It can change, drop or add ops, or return an error.
- The context gives the branch, the author, the resolved timestamp, the store clock reading, and the branch head before the batch. It does not give the branch name. Middleware cannot read the store.
- Middleware runs inside the write transaction, after the branch check and the timestamp resolution, and before validation. Validation and the write path then use the transformed ops.
- Each store handle keeps its own list of middleware. They run in registration order. Each one sees the ops that the one before it left.
- An error rejects the whole batch. The transaction rolls back, so the store writes nothing, the branch head does not move, and there is no `batches` row. `Error::Rejected` is the error for a plain refusal; any other error passes through.
- If middleware drops every op, the write does nothing: no sequence ID is used and there is no `batches` row. The result has an empty range: `first_seq = head + 1` and `last_seq = head`.
- Only `Store::write` runs middleware. `archive_entity` and `restore_entity` call `write`, so they run it too. Branch operations (`fork`, `merge`, `pull`, `set_parents`) write no events, and `undo` removes events, so middleware does not run for them.
- When no middleware is registered, the write path does no extra work: it builds no context and does not touch the ops.

### 8.2 Order in a view [RULE R20, I1]

A view is a stack of layers. For each target, the event with the largest key wins:

```text
(layer, seq)      larger wins
```

- Each parent entry is a layer, in parent-list order. The first entry is the bottom layer.
- Consecutive entries for the same branch become one layer.
- The branch's own events that no entry covers are the top layer. They always win.
- In one layer, seq decides (R1).
- Timestamps do not order events.
- The order of entries thus has an effect. A later entry wins over an earlier entry.

### 8.3 The read path [RULE R25, R26]

Every read does the same three steps.

**Step 1. Flatten the view (R26).**

1. **Expand.** The store expands the parent graph depth-first. The order is the stack order: the lowest priority first.
   - It reads the parent list in list order. A later entry has a higher priority.
   - An **open** entry `(P, lo..)` expands to P's own expansion: P's parents first, then P's own events from `lo`. Thus a branch is always after its parents.
   - A **frozen** entry (an upper bound) is a leaf. It gives only P's own events in the range. A self entry is also a leaf.
   - The result is a list of `(branch, range)` entries.
2. **Dedupe by branch (R26).**
   - Each branch stays only at the position of its **first** appearance in the expanded list.
   - Its range is the greatest covered range of all its appearances: the lowest start and the highest end.
   - An open end is higher than all frozen ends. Thus an open range wins over a frozen range of the same branch (R27).
   - Example: `X → [A, B] → M` expands to `M, A, M, B`. The dedupe gives `M, A, B`. A and B win over M.
3. **Top layer.** The branch's own events that no entry covers are the last layer. They always win (R20).
4. The store does not check for a consistent order. Every parent graph gives a valid flat list.

**Step 2. Load events per layer.** Each interval of a layer has one event source:

| Interval | Event source |
| --- | --- |
| Open (no upper bound) | The current table only. The current row is the newest event of its target, so it is in the interval if any event is. |
| Bounded (frozen, or "as of") | The current table `UNION ALL` the history table, filtered to the interval. |

The choice of source is the only difference between the two paths.

**Step 3. Roll up.** One function (`roll_up`) takes the events of all layers. For each target, it keeps the event with the largest `(layer, seq)` (section 8.2). Values and links use the same function.

**Results.**

- A branch with no frozen parents reads only the current tables, with the target indexes.
- A frozen range reads history through the history target indexes. The roll-up logic is the same. No event is copied (R24).
- Q14 is resolved: this replaces options (a), (b) and (c).

**Rules for the parent graph.**

- **B1 still holds.** A frozen entry is a leaf, so it never chains. `fork` and `merge` record the flat list of the source, frozen at the current heads. They record sequence bounds only, not events. Later events on any branch of that chain are above the bounds. A later change to a parent list of that chain has no effect on the frozen list.
- **Cycles are cut, not refused [CHOSEN].** The expansion does not expand a branch that is already on the current path. Thus a cycle through open entries ends there. Example: A and B pull each other with open ranges. Each one sees the other's events, and its own events win. Frozen entries cannot make a cycle, because they do not chain.
- **A wide range.** The greatest range spans the gaps between appearances. Example: `2..=2` and `5..=5` give `2..=5`. This is an accepted rule (R28).

**View cache.** The store keeps the flat list for each branch in memory.

- A parent-list change on this handle clears the cache.
- A commit from another connection changes `PRAGMA data_version`. The store then clears the cache.
- Writes of events do not change a flat list, because open ranges have no bound.

**Query plans (checked with `EXPLAIN QUERY PLAN`).**

- Current-table queries write `+seq`. This stops SQLite from using the `(bid, seq)` key in place of the target key.
- The history half names its index with `INDEXED BY`.
- Inbound links name `link_current_in`.

| Read | Current half | History half |
| --- | --- | --- |
| Value | PK `(bid, eid, key)` | `value_history_target (bid, eid, key, seq)` |
| Outbound links | PK `(bid, source, link_type)` | `link_history_target (bid, source, link_type)` |
| Inbound links | `link_current_in (bid, destination, link_type)` | `link_history_in (bid, destination, link_type, seq)` |

### 8.4 Children and parents (R15, R16) [CHOSEN, I8, I9]

`links(branch, entities, groups)` does these steps for each entity of the batch. `groups` is a list of `(link type, direction)` pairs:

1. **Find the rows.** Read the current rows where the entity is the source (`Outbound`) or the destination (`Inbound`). If a type is given, read only that type.
2. **Remove duplicates.** Resolve each `(source, destination, type)` with section 8.3. One event is left for each.
3. **Remove links.** Drop the links whose stored `sort` is NULL (removed).
4. **Group by type.**
5. **Sort each group ascending** by the stored `sort` key. Ties sort by the ID of the other entity.

**One sort column (R22).** The store keeps one nullable float:

- The write API keeps three states: `Timestamp`, `Explicit(f64)` and `Removed`.
- The store writes `Timestamp` as the event's timestamp in ms.
- A read thus sees `Some(f64)` (a live link) or `None` (a removed link).

Results of this rule:

- A stored row does not record if its key came from the default or from an explicit value. This is acceptable.
- If you add a link again with the default, it gets the timestamp of that new event. It thus moves to the end of the default order.

6. **Filter by type (R23) [CHOSEN, I16].** Drop each link whose other entity has no winning `type`, or has a null `type`.
   - The store reads the `type` of each other entity once. It uses the primary key of `value_current`, one point search for each layer.
   - An entity that never had a `type` is hidden too. This is the literal meaning of "type is not null".
   - The filter applies to the other entity only. The entity that you ask about can have no type.

Shortcuts:

- `children(branch, entity, type)` is `Outbound` for one type.
- `parents(branch, entity, type)` is `Inbound` for one type.
- `links_including_archived(..)` is `links` without the filter. Use it for repair tools and tests.

### 8.5 Other reads

- `values(branch, entities)`: all keys of each entity. Null values are not returned.
- `value(branch, entity, key)`: one key of one entity. It is a thin helper. Null and "never set" both give `None`.
- `exists(branch, entities)`: true when the view has an event for the entity. The event can be a value event, or a link event at either end, in any state.
- `is_archived(branch, entities)`: true when the winning `type` event of the entity is null. An entity that never had a type is not archived.
- `archive_entity(branch, author, entity)` writes `type = null`. `restore_entity(branch, author, entity, type)` writes the type again. The caller supplies the type, because the store does not read history to find it (R21).

### 8.6 Batch reads (R29)

1. A read resolves the flat view once for the call.
2. The store sends the IDs to SQLite as one JSON array (`json_each`). It does not send one query for each ID.
3. Each layer interval is one query for the whole batch. The query plans stay on the same indexes (section 8.3). `json_each` gives the `IN` list.
4. The roll-up key includes the entity. Thus one roll-up serves the whole batch.
5. The type filter reads the `type` of all other entities in one batch.
6. Each asked-for ID is a key of the result. An ID that the store does not know gives an empty result.

---

## 9. Branches (R2 to R11)

### 9.1 Model

1. A branch has a fixed UUIDv7 ID (R9).
   - The table also has a local integer `bid`.
   - The `bid` is for compact keys only. It never leaves the store.
2. A branch has a parent list of `(branch ID, seq range)` entries (R3).
   - `end = None` is open. The entry then follows the parent as it grows.
   - An upper bound freezes the parent.
3. **Open entries chain; frozen entries do not [RULE R26, I2].**
   - An open entry gives the parent's whole flat list, live (section 8.3).
   - A frozen entry gives only the parent's own events in the range.
   - `fork` and `merge` record frozen flat lists. Thus R2 holds.
4. A branch always sees all of its own events [CHOSEN, I3].
   - An entry can name the branch itself. It puts those own events at that place in the stack.
   - The own events that no entry covers are the top layer.
5. The store dedupes a stored parent list with the same rule as reads (R26). Each branch keeps its first position and the greatest range of its entries.
6. Child branches can have any depth (R7). A child stores only its parent list. No branch stores pointers to its children.

### 9.2 Operations

```rust
fn create_branch(&mut self, name: &str, parents: &[ParentRef]) -> Result<BranchId>;
fn fork(&mut self, from: BranchId, name: &str) -> Result<BranchId>;
fn pull(&mut self, into: BranchId, from: BranchId, range: SeqRange) -> Result<()>;
fn merge(&mut self, into: BranchId, from: BranchId) -> Result<()>;
fn set_parents(&mut self, branch: BranchId, parents: &[ParentRef]) -> Result<()>; // rebase (R5)
fn archive_branch(&mut self, branch: BranchId, archived: bool) -> Result<()>;     // R6
fn branch(&self, branch: BranchId) -> Result<BranchInfo>;
fn branches(&self) -> Result<Vec<BranchInfo>>;
```

| Operation | Effect |
| --- | --- |
| `create_branch` | Makes a branch with the given raw parent list. |
| `fork` | Makes a branch whose parent list is the whole stack of `from`, frozen at the current heads (R2). This means each of `from`'s entries capped at that parent's current head, then `from`'s own uncovered events on top. The child keeps the stack order. |
| `pull` | Adds one entry `(from, range)` at the top of the parent list (R3). The entry is below the branch's own uncovered events. An open range follows `from` and its whole chain live. A bounded range gives only `from`'s own events. |
| `merge` | Puts the whole stack of `from`, frozen at the current heads, on top of what `into` has now (R4, R20). Steps: (1) freeze `into`'s own uncovered events into a self entry; (2) add `from`'s entries and own events on top; (3) leave out `from`'s entries that name `into`. Step 3 is necessary: a stale copy of `into` on top would hide the concurrent edits of `into`. `into`'s later writes sit on top again. Agent work after the merge needs a new merge. To follow `from` live, use `pull` with an open range [CHOSEN, I15]. |
| `set_parents` | Replaces the parent list (R5). |
| `archive_branch` | Sets or clears the flag. Branches are never deleted (R6). |

**Writes to branches (R10) [CHOSEN, I7]:**

- Frozen branches and merged branches accept writes.
- An archived branch refuses writes and undo. Users can still read it, and can restore it.

**Agent sandbox flow (R11):**

1. `fork(main)`.
2. The agent writes to its branch.
3. Do one of these:
   - `merge(main, agent)` to accept the work.
   - `undo` on the agent branch.
   - Do not merge.
4. `archive_branch(agent)`.

### 9.3 Branch questions from draft 1

| # | Question | Status |
| --- | --- | --- |
| B1 | Live or frozen fork | **Resolved: frozen (R2).** |
| B2 | Merge | **Resolved: necessary, with no conflict resolution (R4).** |
| B3 | Rebase | **Resolved: change the parent lists (R5).** |
| B4 | Delete branch | **Resolved: archive only (R6).** |
| B5 | Nesting and depth | **Resolved: no limit, through parent lists (R7).** |
| B6 | Undo across branches | **Resolved: one branch (R8).** |
| B7 | Branch identity | **Resolved: fixed at creation (R9). We chose UUIDv7.** |
| B8 | Diff between branches | **Resolved: out of scope (R18).** |
| B9 | Writes to frozen or merged branches | **Resolved: permitted for now (R10).** |
| B10 | Purpose | **Resolved: agent sandboxes (R11).** |

---

## 10. Undo (R8, R19)

```rust
fn undo(&mut self, branch: BranchId, author: &str) -> Result<Option<UndoOutcome>>;
```

[CHOSEN, I5] The rules:

- The **user** is the batch's `author` string.
- The **unit** is a batch.
- The **window** is the batch's store-clock time. It must be within the last 5 minutes. The store never removes an older event.
- Undo **removes** events. It does not write new events.

Steps, all in one `BEGIN IMMEDIATE` transaction:

1. **Select the batch.**
   - Find the newest batch B on the branch with the given author and `created_at >= now − 5 min`.
   - If there is no batch, return `None`.
   - If the branch is archived, return `Archived`.
2. **Check hazard 1: another branch froze the events.**
   - Look for another branch with a frozen parent range on this branch that includes a seq of B.
   - If one exists, refuse with `UndoRefused`. Nothing changes.
   - An open range does not block undo. The live follower sees the removal.
   - A self entry of the same branch does not block undo.
3. **Check hazard 2: a later event replaced B.**
   - Look for a history row of B whose `superseded_by` is after B.
   - Such a row means that a later batch (of any author) changed a target of B.
   - If one exists, refuse with `UndoRefused`. Nothing changes.
4. **Remove and restore,** for the value tables and then the link tables:
   1. Delete the current rows with a seq in B.
   2. For each target, move back the row that B's first event on that target replaced. Copy it from history to the current table.
   3. Delete from history the restored rows, and the rows of B that a later op in B replaced.
   4. If no prior row exists, the target has no current row.
5. **Finish.** Delete B's row in `batches`. Return the removed range and the counts.

Hazard rules [CHOSEN]:

- **Changed since:** refuse, do not skip. Partial undo is harder to explain and to test. A refusal changes nothing.
- **Pulled or merged:** refuse when another branch has a frozen range over B. A fork and a merge make frozen ranges. Thus a fork or a merge of the branch blocks undo of the earlier batches. Allow it for open ranges.
- **Seq gaps:** the branch counter never goes back. Removed seqs are never used again. `(branch, seq)` stays unique, and a sync peer can never see two events with one ID. A gap in seq is normal.

Results:

- A second call removes the next older batch of the author, within the window.
- No current-state read uses history after an undo.
- There is no redo. See Q5.

## 11. Read API [PROVISIONAL, TBD: Alexander]

Implemented. All low-level reads take a batch (R29):

```rust
pub type LinkGroups = BTreeMap<(String, Direction), Vec<Link>>;

fn values(&self, branch: BranchId, entities: &[&str]) -> Result<HashMap<String, BTreeMap<String, Value>>>;
fn links(&self, branch: BranchId, entities: &[&str], groups: &[(&str, Direction)]) -> Result<HashMap<String, LinkGroups>>;
fn links_including_archived(&self, branch: BranchId, entities: &[&str], groups: &[(&str, Direction)]) -> Result<HashMap<String, LinkGroups>>;
fn exists(&self, branch: BranchId, entities: &[&str]) -> Result<HashMap<String, bool>>;
fn is_archived(&self, branch: BranchId, entities: &[&str]) -> Result<HashMap<String, bool>>;
```

Thin helpers for one entity [CHOSEN]. We keep them, because callers often read one entity. They call the batch reads:

```rust
fn value(&self, branch: BranchId, entity: &str, key: &str) -> Result<Option<Value>>;
fn children(&self, branch: BranchId, entity: &str, link_type: &str) -> Result<Vec<Link>>; // (type, Outbound)
fn parents(&self, branch: BranchId, entity: &str, link_type: &str) -> Result<Vec<Link>>;  // (type, Inbound)
```

Other reads: `branch`, `branches`.

`links` no longer has an "all types" mode. The caller names the `(type, direction)` pairs.

`find` is in section 11.2.

### 11.1 Walk (R30)

```rust
pub type Follow = HashMap<String, Vec<FollowRule>>; // source entity's `type` -> rules
pub struct FollowRule { pub link_type: String, pub direction: Direction, pub child_type: Option<String> } // None = any
pub struct WalkNode { pub id: String, pub values: BTreeMap<String, Value> }
pub struct ReadAhead { pub depth: usize, pub width: usize }             // default 2, 32
pub enum WalkDirection { Forward /* default */, Backward }
pub struct WalkOptions { pub steps: usize,                              // default 1000
                         pub read_ahead: ReadAhead,
                         pub visited: Option<HashSet<String>>,          // default None
                         pub direction: WalkDirection }                 // default Forward
pub struct WalkCache { pub nodes: HashMap<String, WalkNode>, pub links: HashMap<String, LinkGroups> }
pub struct Walk { pub paths: Vec<Vec<String>>,    // paths of the emitted nodes
                  pub cache: WalkCache,            // the entities on `paths` only
                  pub next: Option<Vec<String>>,   // path of the next node to visit; None = exhausted
                  pub steps: usize, pub loads: usize,
                  pub visited: Option<HashSet<String>> }

fn walk(&self, branch: BranchId, start: &[&str], follow: &Follow,
        descend: impl FnMut(&[String], &WalkNode) -> bool,
        include: impl FnMut(&[String], &WalkNode) -> bool,
        options: WalkOptions) -> Result<Walk>;
```

The rules [CHOSEN, I19]:

1. **Start path.** The last ID of `start` is the first node to visit. The earlier IDs are its ancestors. The walk never goes above `start[0]`.
2. **Order.** The walk is depth-first. The next path is the first child. If there is no child, it is the next sibling of the deepest ancestor that has one.
3. **Children.** The store reads the `type` of the node. It follows the rules of that type in `follow`. A rule `(link type, direction, child type)` follows the live links of that type and direction to entities whose `type` is the child type; a linked entity of another type is not a child. A child type of `None` is the wildcard: any typed child. The children come link group by link group, in the order that the rules first name each `(link type, direction)`, and each group keeps its sort order: two rules on one group with different child types give one list, in link order (so manually sorted children stay in order). A child reached twice is kept once, at its first place. A type with no entry follows nothing. There is no wildcard parent key (Q17, resolved). The types of linked entities that the walk has not loaded are read in one batch per load round.
4. **Filter.** Links use the archived filter of `links` (R23). The start node is visited even if it has no type.
5. **Steps.** Each visited node is one step. The start node is a step too, and `include` can emit it. A node that `include` refuses still costs a step.
6. **Predicates.** `include` decides if the node is emitted. `descend` decides if the walk goes below it. Both get the path and the node's values. They do not get links. Each runs once for each path. A resumed walk also runs `descend` on the ancestors of the start, to find their later children.
7. **Paths.** The result is the list of the paths of the emitted nodes. The same entity on two paths gives two paths. The values of each are in the cache (rule 11).
8. **Next.** When the page has used `steps` nodes, `next` is the path of the next node to visit. Pass it back as `start` to resume there. `next` is `None` when the walk is exhausted. With `steps = 0`, nothing is visited and `next` is `start`.
9. **Cycles (always).** A node that is already on the current path is skipped completely. It is not visited, emitted, expanded or counted.
10. **Visited set (option).** With `visited = Some(set)`, an entity in the set is skipped in the same way, on every path. "Visited" means visited (a step), not emitted. The walk adds each visited node to the set and returns it. Pass `next` and the set back together to resume. The start node is never skipped.
11. **Cache.** The walk returns the values of each entity on the returned paths (ancestors of the start included), and its links for the followed `(link type, direction)` groups. A group holds every live link, whatever the child's type, so it means the same as a group from `links`. Entities that the walk loaded but that are on no returned path (skipped by `include`, or loaded by read-ahead) are left out. A page that emits nothing returns an empty cache.
12. **Backward (option).** With `direction = Backward`, the walk visits the nodes of the forward order in reverse order. From a path, the next node is the last descendant of the previous sibling. If there is no previous sibling, it is the parent. The walk ends after `start[0]`. A path whose last ID is no longer a child of its parent goes to the parent. `descend`, the cycle rule and the step limit apply as in a forward walk. Read-ahead loads the last children first. A backward walk cannot use a visited set (`Error::Invalid`). Why: a client that shows rows from a point in the middle of a tree must also find the rows above that point (see `client/PARADIGM.md`).

**Loading (R31).**

- The walk resolves the flat view once.
- A cache miss loads the missing entities. Then it loads up to `read_ahead.depth` layers of their followed targets. Each layer has at most `min(read_ahead.width, steps left)` entities.
- When the walk finds the children of a node, it loads them as one batch, with the same cap.
- Each layer is one load round: one batch `values` read, and one batch `links` read for each type in the layer.
- `Walk::loads` counts the rounds. In the test tree (121 nodes, depth 4, fan-out 3), the walk needs 121 rounds with no read-ahead and 21 with the defaults.
- The cache lives for one call only.

Proposals for later (not built):

- History reads: `value_history(branch, entity, key)` and `link_history(branch, source, type, destination)`.
- A per-branch event log `events(branch, range)`. This prepares for sync.

### 11.2 Find [CHOSEN, I20]

```rust
pub enum Filter {
    Eq(String, Value), Ne(String, Value),
    Lt(String, Value), Le(String, Value), Gt(String, Value), Ge(String, Value),
    Contains(String, String), ContainsIgnoreCase(String, String), Prefix(String, String),
    In(String, Vec<Value>), IsNull(String), NotNull(String),
    And(Vec<Filter>), Or(Vec<Filter>), Not(Box<Filter>),
    Custom(Predicate),   // Arc<dyn Fn(&str, &BTreeMap<String, Value>) -> bool + Send + Sync>
}
pub enum FindOrder { EntityId /* default */, Insertion, ValueAsc(String), ValueDesc(String) }
pub struct FindOptions { pub limit: usize,              // default 1000 rows
                         pub cursor: Option<String>,    // `next` of the previous page
                         pub order: FindOrder,
                         pub values: bool,              // default true: all values; false = ids only
                         pub include_archived: bool,    // default false (R23)
                         pub scan_limit: usize }        // default unlimited: candidates per call
pub struct FindRow { pub id: String, pub values: BTreeMap<String, Value> }
pub struct Find { pub rows: Vec<FindRow>, pub next: Option<String>, pub scanned: usize }

fn find(&self, branch: BranchId, filter: &Filter, options: FindOptions) -> Result<Find>;
```

**Result.** The result is the same as this: roll up every entity in the view (R5, R6), apply the filter to the final values, sort, and cut a page. Indexes only choose candidates. Every candidate is rolled up and checked again with the exact filter.

**Semantics.**

1. **Two-valued logic.** A key is *absent* when it was never set or its winning event is a null clear. A comparison with an absent key is false. `Ne` and `Not` are true for it.
2. **Eq is typed.** `Int(1)`, `Float(1.0)` and `Bool(true)` are all different. `Eq(k, Null)` is `IsNull(k)`. `In` is an `Or` of `Eq`.
3. **Ranges.** A number bound (`Int` or `Float`) compares with `Int` and `Float` values only, exactly (no rounding of large `i64`). A `Text` bound compares with `Text` values only, in byte order (SQLite `BINARY`). Other bound types are `Error::Invalid`. `Bool` and `Json` values never match a range.
4. **Text.** `Contains` and `Prefix` match `Text` values only. They are case-sensitive: a substring (or prefix) test on the UTF-8 bytes. `ContainsIgnoreCase` folds ASCII letters only: `A`-`Z` become `a`-`z` in the value and in the needle, then the same byte substring test. Every other character must match exactly: no Unicode case folding and no normalisation. So `Ä` does not match `ä`, `ß` and `ẞ` do not match `ss`, Turkish `İ` and `ı` match only themselves (`I` and `i` match each other), `É` does not match `é`, `é` (U+00E9) does not match `e` + U+0301, the ligature `ﬁ` does not match `fi`, and the Kelvin sign does not match `k`. A NUL byte in a value is an ordinary byte. These were also the semantics of the old SQL (`instr(lower(value), needle)`: SQLite's `lower()` without ICU folds ASCII only); `find_tests::contains_ignore_case_folds_ascii_only_on_every_path` checks them. Full Unicode folding would need a folded copy of the value in the index (see BENCHMARKS.md, trigram prototype).
5. **Archived (R23).** By default, an entity matches only if its winning `type` is not null. With `include_archived`, every entity with at least one event in the view is a candidate (the same as `links_including_archived`).
6. **Order and paging.** `EntityId` sorts by the external id (byte order). `Insertion` sorts by the store's internal entity number (first write anywhere in the store). `next` is the id of the last row when the page is full, `None` when nothing is left. Pass it back as `cursor`. `ValueAsc(key)` and `ValueDesc(key)` sort by the final value of `key`: numbers (`Int`, `Float`, `Bool` as 0/1) by value before text (`Text`, `Json` as its text) by bytes, ties by the internal entity number (reversed for `ValueDesc`). They add `NotNull(key)` to the filter. Their cursor is opaque (the last row's value and id). Pages are stateless and stable: a store that does not change gives the same rows, and the pages put together are the result of one big call. `scan_limit` caps the candidates checked by one call. When it stops a page early, `next` is the last candidate checked.
7. `limit = 0` is `Error::Invalid`.

**Plan.** These steps are the optimisations (BENCHMARKS.md, "Find").

1. **Candidates from the value index.** `Eq`, `In`, ranges, `Prefix`, `Contains` and `NotNull` turn into a range of `value_current_value (bid, key, value, eid)` for each layer. For a bounded range, the same range of `value_history_value (bid, key, value, eid, seq, superseded_by)` with `seq BETWEEN lo AND hi AND superseded_by > hi` is added; schema V2 put `superseded_by` in that index so the check needs no table lookup. `Eq` is one index seek, so it is the fastest path.
2. **Ranges without a schema change.** In an `ANY` column, SQLite sorts numbers (INTEGER and REAL compared by value) before text. A number range adds `value < ''`, a text range adds `value >= ''`. So no type column is needed in the index.
3. **Contains** is `instr(value, ?)` on the index range `value >= ''` of the key. `ContainsIgnoreCase` is `egdb_contains(value, ?, 1)`, a Rust SQL function registered on every connection, which folds ASCII while it compares (`instr(lower(value), ?)` copied every value first: 1.3–1.6× slower; `LIKE` is faster still but stops at a NUL byte in the value). No FTS index (see BENCHMARKS.md).
4. **And** estimates the candidates of each term (up to 2048) in rank order (`Eq` first). The first term under 2048 is used alone, and the other terms are checked after the roll-up. If all terms are large, the two first are intersected in SQL (measured faster than the smallest large term alone). **Or** is a union, if every part has an index plan. Other filters (`Not`, `Ne`, `IsNull`, `Custom`) have no plan.
5. **No plan.** The store reads the entity dictionary in page order and checks each entity after the roll-up. With the default archived filter this is also the plan of a lone `type` filter that has no index term.
6. **Keyset pages.** Candidates come after the cursor, in chunks of `limit` (min 64, max 2048). A sparse plan sorts all its candidates in SQL on every query, so when the check can reject candidates (several layers or history: a name overridden in a child layer; or a filter that is more than its one indexed term, such as an `And` or a `Custom` behind a pre-filter) it fetches two chunks at once (max 4096) and keeps the second for the next chunk of the same call: a page that rejects candidates then costs one scan, not two. One indexed term on one current layer fetches one chunk, since it rejects almost nothing. The extra candidates are dropped when the page is full; the cursor is still the last row returned. A plan is *dense* when its estimated candidates reach `sqrt(limit × entities)` (10 000 for pages of 1000 in 100 000 entities): the cost of sorting all candidates on each page equals the cost of reading `limit / density` entities. A lone `Eq` is dense only from `sqrt(6 × limit × entities)` (24 495 in the same store): its candidates are one index seek per layer, cheaper to sort than a range or an `instr` scan, and the sorted plan measured faster up to about 2.5× the general cap (BENCHMARKS.md, "Search"). Sparse candidates are sorted in SQL. Dense ones are read in dictionary order: with an `EXISTS` probe of each entity when the view reads no history (no frozen range) or the plan is one `Eq`, else by checking every entity after the roll-up (a probe that must also search history costs more than the roll-up). The estimate counts the current rows of each layer separately (each count streams from the index and stops at the cap) and never decides a result.
7. **Value order.** A sparse filter collects all its candidates, rolls them up and sorts them by `(value, eid)` in memory on each page. A dense filter reads the rows of the order key in `(value, eid)` order from `value_current_value` (and `value_history_value` for frozen ranges) of every layer, merged by SQLite, and emits an entity at the row that holds its final value. A bound of the filter on the order key (for example `score > 50`) limits that scan. Schema V3 put `seq` in `value_current_value`, so the "is this row the winner" check of the merge needs no table lookup.
8. **Batched roll-up.** Each chunk is rolled up in one `values` read for each layer. With `values = false`, only the keys of the filter are read.


---

### 11.3 Prove [CHOSEN, I21]

"Would this walk show this entity?" Given a walk query (a start path, a follow map, `descend` and `include`) and an item, prove that the walk from the start emits the item, and return the path. For permissions: the walk from a user is "what this user can see", and `prove` checks one object without walking everything.

```rust
pub struct ProveOptions { pub max_depth: Option<usize>,   // most links below the start; default None
                          pub max_nodes: Option<usize> }  // budget of expanded entities; default None
pub struct Proof { pub path: Vec<String>,   // the walk's path to the item: the start path, then the steps
                   pub links: Vec<Link> }   // the link of each step below the start, as stored
pub enum ProveOutcome { Proved(Proof), NoPath, Unknown }
pub struct ProveStats { pub nodes_visited: usize, pub link_reads: usize, pub value_reads: usize,
                        pub sql_queries: usize, pub depth_reached: usize }
pub struct Prove { pub outcome: ProveOutcome, pub stats: ProveStats }

fn prove(&self, branch: BranchId, start: &[&str], item: &str, follow: &Follow,
         descend: impl FnMut(&[String], &WalkNode) -> bool,
         include: impl FnMut(&[String], &WalkNode) -> bool,
         options: ProveOptions) -> Result<Prove>;
```

**Semantics.**

1. **Same query as walk.** `Proved` means that `walk` with the same `start`, `follow`, `descend` and `include` emits the item on `proof.path`. Every rule of 11.1 applies: steps follow the rules of the parent's type to children of the rule's child type (or any type, for the wildcard); links to archived or untyped entities are not followed; a node already on the path is not visited again; `descend` must accept every node above the item on the path, the start and its ancestors' paths included, and `include` must accept the item. The item may be the start (a path with no links). Paging (`steps`, `next`), the visited set and the direction do not change which paths exist, so they are not options.
2. **Backward, then forward.** The search reads the item's ancestors one layer at a time: a layered BFS over the reversed rules, one batch link read and one batch type read per layer. A candidate parent `u`, found by a rule's link reversed, counts only if a rule of `type(u)` has that link type and direction and accepts the child's type. Each ancestor gets its exact distance to the item. When the start is among the ancestors, a forward depth-first search runs from the start with the walk's loader (`Walker`: its cache, read-ahead and child-type checks) and the predicates, and goes only into ancestors, within `max_depth`. If it finds no path, the backward search reads the next layer and the forward search runs again on the larger set, until the layers run out. Why: the predicates depend on the whole path from the start, so the backward search cannot test them; the forward search can, and the ancestors keep it small. Stopping the backward search as soon as it reaches the start keeps short proofs cheap.
3. **Complete.** The forward search is the walk restricted to entities that can reach the item, so a walk that emits the item gives `Proved`, and `NoPath` means the walk does not emit it (within `max_depth`). The proof is one of the walk's paths to the item, short paths first; it is not always the walk's first one.
4. **Edge cases.** An unknown start is visited, as by the walk, so it proves only itself. An empty start gives `NoPath`. An item with no (non-null) type has no ancestors, so only `item == start` can prove it.
5. **Views.** The view is resolved once per call. All reads are the shared batch reads and the shared roll-up, so prove works on every view, as in every other read.
6. **Limits.** `max_depth`: paths with more links below the start are not found; the forward search uses the distances to prune. `max_nodes`: the most entities that the two phases may expand, in total. When the budget stops the search before it decides, the answer is `Unknown`, which is different from `NoPath`.
7. **Cost.** The forward search tries simple paths, so with path-dependent predicates on a graph with many paths between the start and the item it can repeat work; `max_nodes` bounds it.
8. **Stats.** `nodes_visited` = entities expanded in both phases; `link_reads` / `value_reads` = batch rounds (the forward loader's rounds count in both); `sql_queries` = roll-up SQL statements; `depth_reached` = the deepest layer of either phase.

The earlier version took a set of roots and a pair-only relationship map, and had three strategies (Backward, Bidirectional, Walk). It was replaced so that prove answers exactly the question a walk answers (Alexander, 2026-10-25). BENCHMARKS.md, "Prove", still holds the measurements of that version.

## 12. Performance notes

1. **Hot tables stay small.** They hold one row per target per branch. History does not slow normal reads. Undo removes events, so it does not add rows.
2. **One B-tree search per point read.**
   - Primary keys are the natural keys, `WITHOUT ROWID`.
   - Integer `bid` and `eid` make keys small.
3. **Prepared statements.** All statements use `prepare_cached`. The cache holds 64 statements.
4. **Write cost.**
   - Each event costs one upsert. A current value row updates 3 B-trees: the PK, `UNIQUE (bid, seq)` and `value_current_value`. A current link row also updates 3: the PK, `UNIQUE (bid, seq)` and `link_current_in`.
   - If a current row exists, the event also costs one history copy. A history row updates 4 B-trees: the PK, two lookup indexes and the `superseded_by` index.
   - The new history indexes thus add one B-tree insert per superseded event. The value index adds one per value write. This is the price for indexed reads of history.
   - Each batch is one transaction. The store uses WAL with `synchronous = NORMAL`.
5. **View cost.** The read cost grows with the number of branches in a view.
   - It is one prefix query for each branch.
   - A bounded interval adds one indexed history search to its query. It rolls up the history rows of the target in that interval.
   - Step 1 is cached, so the expansion runs once per branch until a parent list changes.
   - A long chain of frozen parents costs more. Measure this before you optimise it (Q7).
6. **Parent lists grow with merges.** Normalisation merges adjacent ranges, so repeated merges of the same branch stay one entry.
7. **Next steps:**
   - Add `criterion` benchmarks.
   - Add a reader pool for parallel reads in WAL mode.
   - Batch many-entity reads with `rarray`.

---

## 13. Interpretations we chose [CHOSEN]

| # | Where | Choice |
| --- | --- | --- |
| I1 | R1, R4, R20: order across branches | **Replaced by R20.** `(layer, seq)`. A layer is a parent entry in list order. Consecutive entries for one branch are one layer. The branch's own uncovered events are the top layer. Timestamps do not order events. |
| I2 | R2, R3, R7, R26: transitivity | **Changed by R26.** Open entries chain through a depth-first expansion, deduped by first appearance (R26). Frozen and self entries are leaves. `fork` and `merge` record the flat list frozen at the heads (bounds only, no events). |
| I3 | R3: self entries | A branch always sees all of its own events. An entry that names the branch itself puts those events at that place in the stack. Uncovered own events are on top. `merge` uses this to put merged work over `into`'s earlier events. |
| I4 | R4: "stacking" | The view is the stack of entries. A later entry wins (R20). Normalisation removes the parts of earlier entries that later entries cover. |
| I5 | R8, R19: undo | User = the `author` string. Unit = a write batch. Window = the batch's store-clock time, 5 min. Undo removes the batch's events and moves the replaced rows back from history. It refuses when a later event replaced a target, or when another branch froze the batch. Seqs are not used again. Repeated undo steps back. |
| I6 | R12, R21: archiving an entity | **Replaced by R21.** Archive = `type = null`. Restore = write the type again; the caller supplies it. A partial index on live text types serves "entities of type X". `children` and `values` do not hide archived entities. |
| I7 | R6, R10: archived branches | Archiving is a flag. An archived branch refuses writes and undo. Users can read it and can restore it. |
| I8 | R15: "defaults inbound/outbound" | The direction defaults: `children` reads outbound links (entity = source), and `parents` reads inbound links (entity = destination). `links` takes explicit `(type, direction)` pairs. |
| I9 | R14, R16, R22: "timestamp-at-add" and mixed sorting | The store writes the default as the event's timestamp in ms, in the one `sort` column. If you write `Timestamp` again, the link gets the new time. Explicit floats and default timestamps are on one numeric axis, ascending. Ties sort by the other entity's ID. |
| I10 | R16: re-add | A `Removed` link becomes live again when a later event sets `Timestamp` or `Explicit`. |
| I11 | R12: "has any events" | Any visible value event, or link event at either end, in any state. It is per branch view. |
| I12 | R9: ID form | UUIDv7. A local integer `bid` is for keys only. |
| I13 | Timestamps | The store clock is the default. A client can supply a timestamp. The store clamps it so that it never goes down on a branch. Timestamps give default sort keys only. |
| I14 | R8: undo and archives | Undo on an archived branch is refused (I7). |
| I17 | R26: order and cycles | Expansion is depth-first in stack order (parents before the branch, list order, lowest priority first). "First appeared" means the lowest position in that order. There is no order error. A cycle through open entries is cut where the branch repeats on the path. |
| I18 | R26, R27: same branch twice | One entry per branch, at its first position. Range = lowest start to highest end. Open beats frozen. |
| I19 | R30: walk details | See section 11.1: start counts as a step and can be emitted, predicates see values only, no wildcard follow key (Q17, resolved), skipped nodes cost no step. |
| I20 | `find` (11.2) | Typed `Eq`, numbers and text compared within their kind, case-sensitive text, two-valued null, archived hidden, keyset pages by id. |
| I16 | R23: entities with no type | Hidden, the same as archived entities. The filter checks the other end of each link. `links_including_archived` skips the filter. |
| I15 | R4: live merges | `merge` is a frozen snapshot. To follow a branch live, use `pull` with an open range. |
| I21 | Prove (11.3) | Takes a walk query and an item: proved when the walk emits the item. Backward layers of ancestors from the item, then a forward search from the start through them with the walk's predicates, repeated until a proof or no more layers. A budget gives `Unknown`, never a false `NoPath`. |
| I22 | Write middleware (8.1.1) | A hook transforms a batch's ops inside the write transaction, after the branch check and the timestamp resolution and before validation. It may change, drop or add ops, or reject the batch. Only `write`, `archive_entity` and `restore_entity` run it. Middleware cannot read the store. Registration is per store handle, in order. A batch dropped to empty writes nothing and returns an empty range. |

---

## 14. Open questions

Resolved from draft 1:

- **Q2** (implicit node create): not applicable. There are no create events (R12).
- **Q3** (delete cascade): not applicable (R12).
- **Q4** (edge properties): no. Links carry no data (R14).
- **Q5 of draft 1** (link uniqueness): one link for each `(source, destination, type)` (R15).
- **Q10** (undo scope): R8.
- **Q12** (callers): standalone. Clients use the HTTP API (section 15).
- **Q16** (first appearance and wide ranges): **resolved by R28.**
- **Q17** (follow wildcard): **resolved: no wildcard.** An entity whose type has no entry in the follow map follows nothing (Alexander, 2026-10-14).
- **Q14 of draft 3** (history in frozen reads): **resolved by R25.** All reads use one roll-up. A bounded interval reads current and history.
- **Q14** (timestamps): yes. They give the default sort (R14). They no longer order events across branches (R20).
- **Q1 of draft 2** (explicit default, R17): **resolved by R22.** The API keeps `SortOrder::Timestamp`. The store writes it as a float. The database thus has no "default" null: null means removed.
- **Q8 of draft 2** (clock skew across branches): **resolved by R20.** Timestamps no longer order events. Skew can only change default sort keys.
- **Q6 of draft 2** (archived entities): **resolved by R21 and R23.** Archive = null type. Link reads, `walk` and `find` hide entities with no type.
- **Q1 of draft 3** (merge order): **confirmed.** `merge` leaves out `from`'s stale copy of `into`. R24 records the guiding rule.

Still open:

- **Q2. Walk and find**: **resolved** by R30 (walk) and 11.2 (find). `find` can also be ordered by a value (`FindOrder::ValueAsc/ValueDesc`).

- **Q3. Interpretations.** Do I2 to I18 match the intent? I2 (open entries chain, frozen entries do not) is the most important.
- **Q13. Undo after a fork.** A fork or a merge freezes a range, so it blocks undo of earlier batches. Must undo be allowed and the frozen follower lose the events? Or must open forks be used?
- **Q15. Archived type.** Must `restore_entity` find the old type itself? It would then read history, against R19.
- **Q4. Mixed sort keys.** Small explicit floats sort before all default (timestamp) links. Is this correct? Or must explicit links and default links be separate groups?
- **Q5. Redo.** Is redo necessary? Must undo of a batch that is not the latest be permitted?
- **Q7. Deep frozen chains.** Is a cache necessary for each branch view? An example is a materialised current table for a branch that has many frozen parents.
- **Q9. Resources and builtins.** Must the store keep blobs (content-addressed, with a value reference)? Must builtin entities move into the store?
- **Q10. Value types.** Are the six `Value` variants sufficient?
- **Q11. Live merge.** Must `merge` follow the other branch live, as an open range, and not freeze it (I15)?
- **Q12. Authors.** Is a free author string sufficient for "one user" (I5)? Or is a user ID table necessary?

- **Q18. Middleware store reads.** Write middleware (8.1.1) cannot read the store this version. Must it be able to, for example to look up the current value of a field it edits?

- **Q19. Branch name in the context.** The middleware context does not give the branch name, only the branch ID, to keep the write path cheap. Must the name be added?

- **Q20. Dropped-to-empty batch.** A middleware that drops every op writes nothing and returns an empty range. Must this be an error instead?

---

## 15. HTTP API

An optional HTTP API wraps the store. It is behind the `server` cargo feature, so the library has no web dependencies by default.

- It is a thin wrapper. There is one endpoint for each public store operation. The server holds no logic of its own: it parses JSON, calls one store method, and returns JSON.
- The default bind address is loopback (`127.0.0.1`). A bearer token can protect it.
- The store is not thread safe, so the server keeps it behind one mutex and runs each call on a blocking task. Calls do not run in parallel.
- The library types keep no serde derives. The server has its own wire types that convert to and from them.
- Closures cannot cross JSON, so `Filter::Custom` is not exposed. A `walk` takes its `descend` and `include` predicates as filters.
- See `API.md` for every endpoint and `ARCHITECTURE.md` for the "why".

---

## Appendix A. Alexander's rules, verbatim (2026-10-12)

> - Each event has a per-branch sequence ID; unique on (branch ID, sequence ID). Sequence IDs order events even when merged out of order.
> - B1: after a fork, a branch does not see new parent events.
> - Each branch has a branch ID plus an optional list of parents, each entry being a branch ID and a sequence-ID range; an upper bound freezes that parent. Any branch may pull from any number of others.
> - Merge is needed, with no conflict resolution: data is a conflict-free type, stacking always yields a valid result.
> - Rebase = retarget parent lists.
> - No branch deletion; archive only.
> - Child branches allowed, unlimited depth, modeled via parent lists, not child pointers.
> - Undo: one branch, one user, events in the last five minutes.
> - B7: branch IDs are fixed at creation for future sync.
> - Frozen/merged branches may accept writes for now.
> - Branches are agent sandboxes: agents may err freely; revert or skip merging.
> - No create/delete entity events; an entity exists canonically if it has any events; deletion is archiving.
> - No field-clear events; write null to clear.
> - Links carry no data: each has a link type and a nullable sort order. Null sort order means timestamp-at-add; an explicit float sorts all links of that type.
> - To find children of an entity for one link type: take events with that source ID and type, grouped by type (defaults inbound/outbound), de-duplicated by last event per (source, destination, type), sorted by sort order.
> - Distinguish three sort states: inherit-timestamp default, explicit float, and removed — same pattern as null-clears-a-field.
> - Open question to note in the doc: whether to force an explicit value instead of null for the default.
> - Diffs are out of scope for now.
