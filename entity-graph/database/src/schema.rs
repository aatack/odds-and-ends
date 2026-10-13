//! Schema and migrations. `PRAGMA user_version` holds the applied version.

use rusqlite::Connection;

use crate::error::{Error, Result};

/// Migration `i` takes the file from version `i` to version `i + 1`.
/// Append only: never edit a migration that has shipped.
const MIGRATIONS: &[&str] = &[V1];

/// The schema version this build writes.
pub const SCHEMA_VERSION: u32 = MIGRATIONS.len() as u32;

const V1: &str = r#"
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
-- History has the same lookups as current, plus seq, so bounded roll-ups stay indexed.
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
  sort        REAL,                        -- sort key; NULL = removed
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
"#;

/// Bring the file up to [`SCHEMA_VERSION`]. Each migration runs in its own
/// transaction. A file newer than this build is refused and left unchanged.
pub(crate) fn migrate(conn: &mut Connection) -> Result<()> {
    let found: u32 = conn.pragma_query_value(None, "user_version", |r| r.get(0))?;
    if found > SCHEMA_VERSION {
        return Err(Error::SchemaTooNew {
            found,
            supported: SCHEMA_VERSION,
        });
    }
    for (index, sql) in MIGRATIONS.iter().enumerate().skip(found as usize) {
        let tx = conn.transaction()?;
        tx.execute_batch(sql)?;
        tx.pragma_update(None, "user_version", index as u32 + 1)?;
        tx.commit()?;
    }
    Ok(())
}
