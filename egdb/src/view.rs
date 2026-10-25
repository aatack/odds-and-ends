//! What a branch can see, and in which order.
//!
//! Every read starts here. Step 1 flattens the branch's parent graph into one
//! ordered list with one entry per branch ([`dedupe`]). Step 2 loads the
//! events of each entry from its event source. Step 3 rolls them up with
//! [`roll_up`]. Later entries win; within an entry, the higher sequence ID
//! wins. The branch's own uncovered events are on top.
//!
//! Chaining: an *open* entry `(P, lo..)` brings in P's whole expansion, live.
//! A *frozen* entry (upper bound) and a self entry are leaves: they give only
//! that branch's own events in the range. `fork` and `merge` record the flat
//! list frozen at the current heads, so a frozen view never grows.

use std::collections::HashMap;
use std::hash::Hash;

use rusqlite::{Connection, OptionalExtension};

use crate::error::{Error, Result};
use crate::types::{BranchId, Seq, Timestamp};

/// The upper bound used for an open range.
pub(crate) const OPEN: Seq = Seq::MAX;

/// One branch in a view, with the sequence intervals the view reads from it.
/// Intervals are inclusive, sorted and disjoint.
#[derive(Clone, Debug)]
pub(crate) struct Source {
    /// Position in the stack. Higher wins.
    pub layer: usize,
    pub bid: i64,
    pub id: BranchId,
    pub intervals: Vec<(Seq, Seq)>,
}

#[derive(Clone, Debug)]
pub(crate) struct View {
    pub sources: Vec<Source>,
}

/// Merge overlapping and adjacent intervals.
pub(crate) fn normalize(mut intervals: Vec<(Seq, Seq)>) -> Vec<(Seq, Seq)> {
    intervals.retain(|&(lo, hi)| lo <= hi);
    intervals.sort_unstable();
    let mut out: Vec<(Seq, Seq)> = Vec::with_capacity(intervals.len());
    for (lo, hi) in intervals {
        match out.last_mut() {
            Some(last) if lo <= last.1.saturating_add(1) => last.1 = last.1.max(hi),
            _ => out.push((lo, hi)),
        }
    }
    out
}

/// Remove the inclusive intervals `cuts` (sorted, disjoint) from `iv`.
pub(crate) fn subtract(iv: (Seq, Seq), cuts: &[(Seq, Seq)]) -> Vec<(Seq, Seq)> {
    let mut out = Vec::new();
    let mut lo = iv.0;
    for &(clo, chi) in cuts {
        if chi < lo || clo > iv.1 {
            continue;
        }
        if clo > lo {
            out.push((lo, clo - 1));
        }
        if chi >= iv.1 {
            return out;
        }
        lo = chi + 1;
    }
    if lo <= iv.1 {
        out.push((lo, iv.1));
    }
    out
}

/// The order of events in a view: layer first, then sequence ID. The larger
/// wins. `(layer, seq)` is unique in a view, so `branch` and `ts` never decide;
/// they only travel with the order. Timestamps do not order events.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Order {
    pub layer: usize,
    pub seq: Seq,
    pub branch: BranchId,
    pub ts: Timestamp,
}

/// One candidate event for a target, and its payload.
#[derive(Clone, Debug)]
pub(crate) struct Cand<T> {
    pub order: Order,
    pub author: String,
    pub data: T,
}

impl<T> Cand<T> {
    /// Keep the newer of `slot` and `self`.
    pub fn offer(self, slot: &mut Option<Cand<T>>) {
        if slot.as_ref().is_none_or(|c| c.order < self.order) {
            *slot = Some(self);
        }
    }
}

/// Step 3: the one roll-up for every read. Keep the newest event per key,
/// by [`Order`]. The events can come from any event source.
pub(crate) fn roll_up<K: Eq + Hash, T>(
    events: impl IntoIterator<Item = (K, Cand<T>)>,
) -> HashMap<K, Cand<T>> {
    let mut best: HashMap<K, Option<Cand<T>>> = HashMap::new();
    for (k, c) in events {
        c.offer(best.entry(k).or_default());
    }
    best.into_iter()
        .filter_map(|(k, c)| c.map(|c| (k, c)))
        .collect()
}

/// Where a layer's interval loads its events from.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum EventSource {
    /// An open interval: the current table holds the newest event of each
    /// target, and that event is inside the interval if any event is.
    Current,
    /// A bounded (frozen or "as of") interval: current UNION ALL history,
    /// filtered to the interval.
    CurrentAndHistory,
}

impl EventSource {
    pub fn of(interval: (Seq, Seq)) -> Self {
        if interval.1 == OPEN {
            Self::Current
        } else {
            Self::CurrentAndHistory
        }
    }
}

pub(crate) fn branch_uuid(bytes: &[u8]) -> Result<BranchId> {
    uuid::Uuid::from_slice(bytes)
        .map(BranchId)
        .map_err(|e| Error::Corrupt(format!("branch id: {e}")))
}

/// The raw parent list of `bid` in stack order: `(parent bid, parent id, lo, hi)`.
pub(crate) fn parent_rows(conn: &Connection, bid: i64) -> Result<Vec<(i64, BranchId, Seq, Seq)>> {
    let mut stmt = conn.prepare_cached(
        "SELECT p.parent_bid, b.id, p.seq_start, p.seq_end
         FROM branch_parents p JOIN branches b ON b.bid = p.parent_bid
         WHERE p.bid = ?1 ORDER BY p.pos",
    )?;
    let mut rows = stmt.query([bid])?;
    let mut out = Vec::new();
    while let Some(r) = rows.next()? {
        let id = branch_uuid(&r.get::<_, Vec<u8>>(1)?)?;
        let hi: Option<Seq> = r.get(3)?;
        out.push((r.get(0)?, id, r.get(2)?, hi.unwrap_or(OPEN)));
    }
    Ok(out)
}

/// The branch's own events that no entry naming the branch covers: the top layer.
pub(crate) fn own_tail(bid: i64, rows: &[(i64, BranchId, Seq, Seq)]) -> Vec<(Seq, Seq)> {
    let covered = normalize(
        rows.iter()
            .filter(|r| r.0 == bid)
            .map(|r| (r.2, r.3))
            .collect(),
    );
    subtract((1, OPEN), &covered)
}

/// One `(branch, range)` entry of an expansion.
type Entry = (i64, BranchId, Seq, Seq);

fn branch_id(conn: &Connection, bid: i64) -> Result<BranchId> {
    let raw: Vec<u8> = conn
        .prepare_cached("SELECT id FROM branches WHERE bid = ?1")?
        .query_row([bid], |r| r.get(0))
        .optional()?
        .ok_or_else(|| Error::Corrupt(format!("no branch row {bid}")))?;
    branch_uuid(&raw)
}

/// Depth-first expansion of `bid` in stack order (lowest priority first):
/// for each entry in parent-list order, an open entry on another branch
/// expands to that branch's own expansion (its own events clamped to the
/// entry's start); a frozen entry or a self entry is a leaf. The branch's own
/// events come last. A branch already on the current path is cut, so a cycle
/// through open entries ends there (no error).
fn expand(
    conn: &Connection,
    bid: i64,
    id: BranchId,
    lo: Seq,
    path: &mut Vec<i64>,
    out: &mut Vec<Entry>,
) -> Result<()> {
    path.push(bid);
    for (parent, pid, plo, phi) in parent_rows(conn, bid)? {
        if phi != OPEN || parent == bid {
            out.push((parent, pid, plo, phi));
        } else if !path.contains(&parent) {
            expand(conn, parent, pid, plo, path, out)?;
        }
    }
    path.pop();
    out.push((bid, id, lo, OPEN));
    Ok(())
}

/// Step 1, dedupe: keep each branch once, at the position of its first
/// appearance, with the greatest covered range of all its appearances: the
/// lowest start and the highest end (open beats frozen).
pub(crate) fn dedupe(entries: impl IntoIterator<Item = Entry>) -> Vec<Entry> {
    let mut at: HashMap<i64, usize> = HashMap::new();
    let mut out: Vec<Entry> = Vec::new();
    for (b, id, lo, hi) in entries {
        match at.get(&b) {
            Some(&i) => {
                out[i].2 = out[i].2.min(lo);
                out[i].3 = out[i].3.max(hi);
            }
            None => {
                at.insert(b, out.len());
                out.push((b, id, lo, hi));
            }
        }
    }
    out
}

/// The view of branch `bid`: its flat list as layers, and its own events
/// that the list does not cover as the top layer (R20).
pub(crate) fn load_view(conn: &Connection, bid: i64) -> Result<View> {
    let mut all = Vec::new();
    let id = branch_id(conn, bid)?;
    expand(conn, bid, id, 1, &mut Vec::new(), &mut all)?;
    all.pop(); // the root's own events: handled as the top layer below
    let flat = dedupe(all);
    let mut sources: Vec<Source> = Vec::with_capacity(flat.len() + 1);
    let mut covered = Vec::new();
    for (b, bid_id, lo, hi) in flat {
        if b == bid {
            covered.push((lo, hi));
        }
        sources.push(Source {
            layer: sources.len(),
            bid: b,
            id: bid_id,
            intervals: vec![(lo, hi)],
        });
    }
    let tail = subtract((1, OPEN), &normalize(covered));
    if !tail.is_empty() {
        sources.push(Source {
            layer: sources.len(),
            bid,
            id,
            intervals: tail,
        });
    }
    Ok(View { sources })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn normalize_merges_overlap_and_adjacency() {
        assert_eq!(
            normalize(vec![(5, 9), (1, 3), (4, 4), (12, OPEN), (20, 30), (11, 10)]),
            vec![(1, 9), (12, OPEN)]
        );
    }

    #[test]
    fn subtract_cuts_holes() {
        assert_eq!(
            subtract((1, OPEN), &[(1, 2), (5, 6)]),
            vec![(3, 4), (7, OPEN)]
        );
        assert_eq!(subtract((3, 4), &[(1, 10)]), vec![]);
        assert_eq!(subtract((3, 9), &[(1, 2), (12, 14)]), vec![(3, 9)]);
    }
}
