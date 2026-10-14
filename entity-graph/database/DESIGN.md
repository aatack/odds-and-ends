# egdb (entity-graph database): design

Status: draft 3, for review.
Drafts 2 and 3 add Alexander's decisions of 2026-10-12 and 2026-10-13.
This document is the design.
The implementation is in this folder (Rust crate `egdb`).
Source analysis of the old store: not in the repository.

Writing rule:

- The prose uses ASD-STE100 Simplified Technical English.
- Code, SQL and the verbatim quote in Appendix A do not.

Marks:

- **[RULE]**: a decision from Alexander. Do not change it.
- **[CHOSEN]**: an interpretation that we chose where a rule is not clear. Section 14 lists all of them.
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
30. **R30 [RULE]. Walk.** `walk` takes a starting path, a map from the source entity's type to the links to follow, a "continue" predicate, an "include" predicate and a step limit. It returns rows and the path of the next node to visit (`None` when exhausted). It never allows cycles: a node already on the current path is skipped completely. Options: return the cache, and a global visited set.
31. **R31 [RULE]. Read-ahead.** On a cache miss, `walk` loads the needed entities plus about two layers of their followed links, in batches. The cache lives for one walk.

---

## 2. Goals

1. Supply a Rust library that keeps entities, values and links as events.
2. Keep one store in one file. Do not use a separate server.
3. Make setup easy. `cargo add` must be sufficient.
4. Make reads and writes fast.
5. Keep the event model close to entity-graph. Value events and link events stay. Section 12 gives the migration.
6. Put only current events in the hot tables. Move superseded events to history tables, one for each event type. Keep history queryable.
7. Support branches as agent sandboxes (R2 to R11).
8. Make each write batch one transaction.
9. Make undo remove the events of a recent batch (R19). Normal reads never use history.
10. Supply dedicated reads: `walk` (done, R30) and `find` [TBD: Alexander].

## 3. Non-goals (for now)

1. Bindings for the TypeScript app.
   - The crate is a standalone database.
   - The app does not call it.
   - There is no napi-rs and no HTTP layer.
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
- The old store is SQLite, so migration is simple.
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
CREATE INDEX value_history_value      ON value_history(bid, key, value, eid, seq);
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
| `value_current_value (bid, key, value, eid)` | Find by value. "All entities of type X" (R21): a null type does not match, so archived entities drop out. |
| `value_history_target (bid, eid, key, seq)` | Bounded roll-up of values (section 8.3). The same lookup as the current PK, plus seq. |
| `value_history_value (bid, key, value, eid, seq)` | Find by value "as of" a bounded range (future `find`). |
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

1. **Validate.**
   - A batch must have at least one op.
   - IDs, keys and link types must not be empty.
   - Floats must be finite.
2. **Branch check.** The branch must exist and must not be archived.
3. **Timestamp.**
   - `ts = max(batch.timestamp or store clock, branch.last_ts)`.
   - Timestamps thus never go down on a branch [CHOSEN, I13]. The timestamp gives the default sort key. It does not order events.
4. **For each op:**
   1. Increment the branch's seq.
   2. Intern the entity IDs. For a link, replace `SortOrder` with the stored key: `Timestamp` becomes `ts` as `f64`, `Explicit(f)` becomes `f`, and `Removed` becomes NULL (R22).
   3. Copy the current row of the target to its history table, with `superseded_by = seq`.
   4. Upsert the new current row.
5. **Finish.**
   - Update `branches.head_seq` and `last_ts`.
   - Insert the `batches` row.
6. **COMMIT.** A reader sees the state before the commit or after it, never between.

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

Placeholder (it returns `Error::NotImplemented`). Alexander will give the signature:

```rust
/// TODO(Alexander): predicates, indexes, paging.
fn find(&self, branch: BranchId, key: &str, value: &Value) -> Result<Vec<String>>;
```

### 11.1 Walk (R30)

```rust
pub type Follow = HashMap<String, Vec<(String, Direction)>>; // source entity's `type` -> pairs
pub struct WalkNode { pub id: String, pub values: BTreeMap<String, Value> }
pub struct WalkRow { pub path: Vec<String>, pub node: WalkNode }
pub struct ReadAhead { pub depth: usize, pub width: usize }             // default 2, 32
pub struct WalkOptions { pub steps: usize,                              // default 1000
                         pub read_ahead: ReadAhead,
                         pub return_cache: bool,                        // default false
                         pub visited: Option<HashSet<String>> }         // default None
pub struct WalkCache { pub nodes: HashMap<String, WalkNode>, pub links: HashMap<String, LinkGroups> }
pub struct Walk { pub rows: Vec<WalkRow>,
                  pub next: Option<Vec<String>>,   // path of the next node to visit; None = exhausted
                  pub steps: usize, pub loads: usize,
                  pub cache: Option<WalkCache>, pub visited: Option<HashSet<String>> }

fn walk(&self, branch: BranchId, start: &[&str], follow: &Follow,
        descend: impl FnMut(&[String], &WalkNode) -> bool,
        include: impl FnMut(&[String], &WalkNode) -> bool,
        options: WalkOptions) -> Result<Walk>;
```

The rules [CHOSEN, I19]. They follow the TS query walk (`src/core/query.ts`):

1. **Start path.** The last ID of `start` is the first node to visit. The earlier IDs are its ancestors. The walk never goes above `start[0]`.
2. **Order.** The walk is depth-first. The next path is the first child. If there is no child, it is the next sibling of the deepest ancestor that has one.
3. **Children.** The store reads the `type` of the node. It follows the pairs of that type in `follow`, in the listed order. Each link group keeps its sort order. A child that two pairs reach is kept once, at its first place. A type with no entry follows nothing. There is no wildcard key (Q17, resolved).
4. **Filter.** Links use the archived filter of `links` (R23). The start node is visited even if it has no type.
5. **Steps.** Each visited node is one step. The start node is a step too, and `include` can emit it. A node that `include` refuses still costs a step.
6. **Predicates.** `include` decides if the node is emitted. `descend` decides if the walk goes below it. Both get the path and the node's values. They do not get links. Each runs once for each path. A resumed walk also runs `descend` on the ancestors of the start, to find their later children.
7. **Rows.** A row is `(path, node)`. The same entity on two paths gives two rows.
8. **Next.** When the page has used `steps` nodes, `next` is the path of the next node to visit. Pass it back as `start` to resume there. `next` is `None` when the walk is exhausted. With `steps = 0`, nothing is visited and `next` is `start`.
9. **Cycles (always).** A node that is already on the current path is skipped completely. It is not visited, emitted, expanded or counted.
10. **Visited set (option).** With `visited = Some(set)`, an entity in the set is skipped in the same way, on every path. "Visited" means visited (a step), not emitted. The walk adds each visited node to the set and returns it. Pass `next` and the set back together to resume. The start node is never skipped.
11. **Cache (option).** With `return_cache`, the walk returns its cache: the values of each loaded entity, and its links for the followed pairs. Entities that read-ahead loaded are in it too.

**Loading (R31).** This follows `scanEvents` in `src/core/pensive/tools.ts`:

- The walk resolves the flat view once.
- A cache miss loads the missing entities. Then it loads up to `read_ahead.depth` layers of their followed targets. Each layer has at most `min(read_ahead.width, steps left)` entities.
- When the walk finds the children of a node, it loads them as one batch, with the same cap.
- Each layer is one load round: one batch `values` read, and one batch `links` read for each type in the layer.
- `Walk::loads` counts the rounds. In the test tree (121 nodes, depth 4, fan-out 3), the walk needs 121 rounds with no read-ahead and 21 with the defaults.
- The cache lives for one call only.

Proposals for later (not built):

- History reads: `value_history(branch, entity, key)` and `link_history(branch, source, type, destination)`.
- A per-branch event log `events(branch, range)`. This prepares for sync.
- `find` can use `value_current_value (bid, key, value, eid)`, also for types (R21). A bounded range can use `value_history_value`.

---

## 12. Migration from entity-graph SQLite

1. Open the old file read-only. Write a new file. Do not overwrite a file.
2. Make one branch, `main`.
3. Read the old `value_events` and `link_events` in order `(timestamp, table, id)`.
4. Make one batch for each `(author, timestamp)` group.
   - Keep the author.
   - Use the old timestamp as `WriteBatch.timestamp`.
5. **Values.**
   - An old value event becomes `SetValue` with a typed value.
   - An old `null` stays `Null`. This is now "clear" (R13), the same meaning as before.
6. **Links.**
   - Use `link_type = "child"`.
   - Replay the old ordering logic (`rollupEntity`) for each source.
   - An old add with no moves becomes `SortOrder::Timestamp`. This gives the same order as the old append.
   - An old remove becomes `Removed`.
   - An old move (actions 2 and 3) becomes `Explicit(f)`. `f` is the midpoint of the new neighbours' sort keys.
   - When a source gets its first explicit key, its links that use `Timestamp` keep their timestamp keys. Mixed keys therefore still sort correctly.
7. **Duplicates.** Identical old events stay separate events. Each gets its own seq.
8. **Builtins and resources.** These are not migrated (Q9).
9. **Verify.**
   - Compare the old rollup of each entity with the new `values` and `children`.
   - Stop on the first difference. Write nothing.

---

## 13. Performance notes

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

## 14. Interpretations we chose [CHOSEN]

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
| I16 | R23: entities with no type | Hidden, the same as archived entities. The filter checks the other end of each link. `links_including_archived` skips the filter. |
| I15 | R4: live merges | `merge` is a frozen snapshot. To follow a branch live, use `pull` with an open range. |

---

## 15. Open questions

Resolved from draft 1:

- **Q2** (implicit node create): not applicable. There are no create events (R12).
- **Q3** (delete cascade): not applicable (R12).
- **Q4** (edge properties): no. Links carry no data (R14).
- **Q5 of draft 1** (link uniqueness): one link for each `(source, destination, type)` (R15).
- **Q10** (undo scope): R8.
- **Q12** (callers): standalone. The app does not call it.
- **Q16** (first appearance and wide ranges): **resolved by R28.**
- **Q17** (follow wildcard): **resolved: no wildcard.** An entity whose type has no entry in the follow map follows nothing (Alexander, 2026-10-14).
- **Q14 of draft 3** (history in frozen reads): **resolved by R25.** All reads use one roll-up. A bounded interval reads current and history.
- **Q14** (timestamps): yes. They give the default sort (R14). They no longer order events across branches (R20).
- **Q1 of draft 2** (explicit default, R17): **resolved by R22.** The API keeps `SortOrder::Timestamp`. The store writes it as a float. The database thus has no "default" null: null means removed.
- **Q8 of draft 2** (clock skew across branches): **resolved by R20.** Timestamps no longer order events. Skew can only change default sort keys.
- **Q6 of draft 2** (archived entities): **resolved by R21 and R23.** Archive = null type. Link reads, `walk` and `find` hide entities with no type.
- **Q1 of draft 3** (merge order): **confirmed.** `merge` leaves out `from`'s stale copy of `into`. R24 records the guiding rule.

Still open:

- **Q2. Walk and find** [TBD: Alexander]. Filters, order, depth, cycles and paging.

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
