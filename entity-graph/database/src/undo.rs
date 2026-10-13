//! Undo: one branch, one author, batches from the last five minutes.
//!
//! Undo removes the batch's events. For each target whose current row goes,
//! the row that it superseded moves back from history into the current table.
//! Reads therefore never need history to find the current state.

use rusqlite::{params, Connection, OptionalExtension, TransactionBehavior};

use crate::error::{Error, Result};
use crate::store::{branch_row, Store};
use crate::types::{BranchId, Seq, UndoOutcome, UNDO_WINDOW_MS};
use crate::view::branch_uuid;

/// The two event types, as (current table, history table, columns).
const TABLES: [(&str, &str, &str); 2] = [
    (
        "value_current",
        "value_history",
        "bid, eid, key, seq, ts, author, vtype, value",
    ),
    (
        "link_current",
        "link_history",
        "bid, source, link_type, destination, seq, ts, author, sort",
    ),
];

impl Store {
    /// Remove the newest batch that `author` wrote on `branch` in the last
    /// [`UNDO_WINDOW_MS`]. Call it again to step further back. Returns `None`
    /// when there is nothing to undo.
    ///
    /// Refused (nothing changes) when:
    /// - a later event replaced one of the batch's events, or
    /// - another branch has a frozen parent range that includes the batch.
    ///
    /// Removed sequence IDs are never reused: the branch counter does not go back.
    pub fn undo(&mut self, branch: BranchId, author: &str) -> Result<Option<UndoOutcome>> {
        let now = self.now();
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let row = branch_row(&tx, branch)?;
        if row.archived {
            return Err(Error::BranchArchived(branch));
        }

        let found: Option<(Seq, Seq)> = tx
            .prepare_cached(
                "SELECT first_seq, last_seq FROM batches
                 WHERE bid = ?1 AND author = ?2 AND created_at >= ?3
                 ORDER BY first_seq DESC LIMIT 1",
            )?
            .query_row(params![row.bid, author, now - UNDO_WINDOW_MS], |r| {
                Ok((r.get(0)?, r.get(1)?))
            })
            .optional()?;
        let Some((first, last)) = found else {
            return Ok(None);
        };

        if let Some(follower) = frozen_follower(&tx, row.bid, first, last)? {
            return Err(Error::UndoRefused(format!(
                "branch {follower} has a frozen range over events {first}..={last}"
            )));
        }
        if replaced_since(&tx, row.bid, first, last)? {
            return Err(Error::UndoRefused(format!(
                "a later event replaced one of events {first}..={last}"
            )));
        }

        let mut events = 0;
        let mut restored = 0;
        for (current, history, columns) in TABLES {
            // The batch's last event per target is the current row. Remove it.
            events += tx
                .prepare_cached(&format!(
                    "DELETE FROM {current} WHERE bid = ?1 AND seq BETWEEN ?2 AND ?3"
                ))?
                .execute(params![row.bid, first, last])?;
            // The row that the batch superseded comes back. One per target at most:
            // it is the row that the batch's first event on that target replaced.
            restored += tx
                .prepare_cached(&format!(
                    "INSERT INTO {current} ({columns})
                     SELECT {columns} FROM {history}
                     WHERE bid = ?1 AND superseded_by BETWEEN ?2 AND ?3 AND seq < ?2"
                ))?
                .execute(params![row.bid, first, last])?;
            // Drop the restored rows from history, and the batch's own
            // superseded events (a target written twice in the batch).
            let gone = tx
                .prepare_cached(&format!(
                    "DELETE FROM {history}
                     WHERE bid = ?1 AND (superseded_by BETWEEN ?2 AND ?3 OR seq BETWEEN ?2 AND ?3)"
                ))?
                .execute(params![row.bid, first, last])?;
            events += gone;
        }
        // `gone` above counted the restored rows too; they were moved, not removed.
        events -= restored;

        tx.prepare_cached("DELETE FROM batches WHERE bid = ?1 AND first_seq = ?2")?
            .execute(params![row.bid, first])?;
        tx.commit()?;
        Ok(Some(UndoOutcome {
            removed: (first, last),
            events,
            restored,
        }))
    }
}

/// Another branch whose parent list has a frozen range over `first..=last`.
/// Open ranges do not block undo: a live follower sees the removal. A branch's
/// own self entries do not block it either.
fn frozen_follower(conn: &Connection, bid: i64, first: Seq, last: Seq) -> Result<Option<BranchId>> {
    let id: Option<Vec<u8>> = conn
        .prepare_cached(
            "SELECT b.id FROM branch_parents p JOIN branches b ON b.bid = p.bid
             WHERE p.parent_bid = ?1 AND p.bid <> ?1 AND p.seq_end IS NOT NULL
               AND p.seq_start <= ?3 AND p.seq_end >= ?2
             LIMIT 1",
        )?
        .query_row(params![bid, first, last], |r| r.get(0))
        .optional()?;
    id.map(|id| branch_uuid(&id)).transpose()
}

/// Whether an event after the batch replaced one of the batch's events.
fn replaced_since(conn: &Connection, bid: i64, first: Seq, last: Seq) -> Result<bool> {
    Ok(conn
        .prepare_cached(
            "SELECT EXISTS (SELECT 1 FROM value_history
                            WHERE bid = ?1 AND seq BETWEEN ?2 AND ?3 AND superseded_by > ?3)
                 OR EXISTS (SELECT 1 FROM link_history
                            WHERE bid = ?1 AND seq BETWEEN ?2 AND ?3 AND superseded_by > ?3)",
        )?
        .query_row(params![bid, first, last], |r| r.get(0))?)
}
