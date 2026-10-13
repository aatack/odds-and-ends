//! The write path. One batch is one SQLite transaction. Each event that
//! replaces a current row moves that row to the history table of its type
//! in the same transaction.

use rusqlite::{params, Connection, TransactionBehavior};

use crate::codec::{encode_sort, encode_value};
use crate::error::{Error, Result};
use crate::store::{branch_row, intern_entity, Store};
use crate::types::{
    BranchId, Op, Seq, SortOrder, Timestamp, Value, WriteBatch, WriteResult, TYPE_KEY,
};

impl Store {
    /// Write a batch of events to one branch. All or nothing.
    pub fn write(&mut self, batch: WriteBatch) -> Result<WriteResult> {
        let now = self.now();
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let result = write_in(&tx, &batch, now)?;
        tx.commit()?;
        Ok(result)
    }

    /// Archive an entity: write null to its [`TYPE_KEY`]. There are no delete
    /// events. The entity still exists; a search by type no longer finds it.
    pub fn archive_entity(
        &mut self,
        branch: BranchId,
        author: &str,
        entity: &str,
    ) -> Result<WriteResult> {
        self.write(WriteBatch::new(
            branch,
            author,
            vec![Op::clear(entity, TYPE_KEY)],
        ))
    }

    /// Restore an archived entity by writing its type again. The caller
    /// supplies the type: the store does not read history to find the old one.
    pub fn restore_entity(
        &mut self,
        branch: BranchId,
        author: &str,
        entity: &str,
        entity_type: &str,
    ) -> Result<WriteResult> {
        self.write(WriteBatch::new(
            branch,
            author,
            vec![Op::set(entity, TYPE_KEY, entity_type)],
        ))
    }
}

fn validate(op: &Op) -> Result<()> {
    let empty = |what: &str| Err(Error::Invalid(format!("{what} must not be empty")));
    match op {
        Op::SetValue { entity, key, value } => {
            if entity.is_empty() {
                return empty("entity");
            }
            if key.is_empty() {
                return empty("key");
            }
            if let Value::Float(f) = value {
                if !f.is_finite() {
                    return Err(Error::Invalid(format!("value {f} is not finite")));
                }
            }
        }
        Op::Link {
            source,
            destination,
            link_type,
            sort,
        } => {
            if source.is_empty() || destination.is_empty() {
                return empty("link endpoint");
            }
            if link_type.is_empty() {
                return empty("link type");
            }
            if let SortOrder::Explicit(f) = sort {
                if !f.is_finite() {
                    return Err(Error::Invalid(format!("sort order {f} is not finite")));
                }
            }
        }
    }
    Ok(())
}

/// The write path, inside a transaction the caller owns.
pub(crate) fn write_in(
    conn: &Connection,
    batch: &WriteBatch,
    now: Timestamp,
) -> Result<WriteResult> {
    if batch.ops.is_empty() {
        return Err(Error::Invalid("a batch needs at least one op".into()));
    }
    batch.ops.iter().try_for_each(validate)?;

    let row = branch_row(conn, batch.branch)?;
    if row.archived {
        return Err(Error::BranchArchived(batch.branch));
    }
    // Timestamps never go down within a branch, so the merged-view order
    // (timestamp first) agrees with sequence-ID order on every branch.
    let ts = batch.timestamp.unwrap_or(now).max(row.last_ts);
    let first_seq = row.head + 1;
    let mut seq = row.head;

    for op in &batch.ops {
        seq += 1;
        match op {
            Op::SetValue { entity, key, value } => {
                let eid = intern_entity(conn, entity)?;
                write_value(conn, row.bid, seq, ts, &batch.author, eid, key, value)?;
            }
            Op::Link {
                source,
                destination,
                link_type,
                sort,
            } => {
                let src = intern_entity(conn, source)?;
                let dst = intern_entity(conn, destination)?;
                write_link(
                    conn,
                    row.bid,
                    seq,
                    ts,
                    &batch.author,
                    src,
                    link_type,
                    dst,
                    *sort,
                )?;
            }
        }
    }

    conn.prepare_cached("UPDATE branches SET head_seq = ?2, last_ts = ?3 WHERE bid = ?1")?
        .execute(params![row.bid, seq, ts])?;
    conn.prepare_cached(
        "INSERT INTO batches (bid, first_seq, last_seq, author, created_at)
         VALUES (?1, ?2, ?3, ?4, ?5)",
    )?
    .execute(params![row.bid, first_seq, seq, batch.author, now])?;

    Ok(WriteResult {
        branch: batch.branch,
        first_seq,
        last_seq: seq,
        timestamp: ts,
    })
}

#[allow(clippy::too_many_arguments)]
fn write_value(
    conn: &Connection,
    bid: i64,
    seq: Seq,
    ts: Timestamp,
    author: &str,
    eid: i64,
    key: &str,
    value: &Value,
) -> Result<()> {
    conn.prepare_cached(
        "INSERT INTO value_history (bid, seq, eid, key, ts, author, vtype, value, superseded_by)
         SELECT bid, seq, eid, key, ts, author, vtype, value, ?4
         FROM value_current WHERE bid = ?1 AND eid = ?2 AND key = ?3",
    )?
    .execute(params![bid, eid, key, seq])?;

    let (vtype, sql_value) = encode_value(value);
    conn.prepare_cached(
        "INSERT INTO value_current (bid, eid, key, seq, ts, author, vtype, value)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)
         ON CONFLICT (bid, eid, key) DO UPDATE SET
           seq = excluded.seq, ts = excluded.ts, author = excluded.author,
           vtype = excluded.vtype, value = excluded.value",
    )?
    .execute(params![bid, eid, key, seq, ts, author, vtype, sql_value])?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn write_link(
    conn: &Connection,
    bid: i64,
    seq: Seq,
    ts: Timestamp,
    author: &str,
    source: i64,
    link_type: &str,
    destination: i64,
    sort: SortOrder,
) -> Result<()> {
    conn.prepare_cached(
        "INSERT INTO link_history
           (bid, seq, source, link_type, destination, ts, author, sort, superseded_by)
         SELECT bid, seq, source, link_type, destination, ts, author, sort, ?5
         FROM link_current
         WHERE bid = ?1 AND source = ?2 AND link_type = ?3 AND destination = ?4",
    )?
    .execute(params![bid, source, link_type, destination, seq])?;

    let sort = encode_sort(sort, ts);
    conn.prepare_cached(
        "INSERT INTO link_current
           (bid, source, link_type, destination, seq, ts, author, sort)
         VALUES (?1, ?2, ?3, ?4, ?5, ?6, ?7, ?8)
         ON CONFLICT (bid, source, link_type, destination) DO UPDATE SET
           seq = excluded.seq, ts = excluded.ts, author = excluded.author,
           sort = excluded.sort",
    )?
    .execute(params![
        bid,
        source,
        link_type,
        destination,
        seq,
        ts,
        author,
        sort
    ])?;
    Ok(())
}
