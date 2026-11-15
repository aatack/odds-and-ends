//! The log: every value and link event a branch can see, current and
//! superseded alike, filtered by entity, author, key (or link type), kind and
//! time. Where [`Store::value_events`] says which event set each value now,
//! the log says everything that was done, and when. Read with `after`, it is
//! the change feed: the branch's own events after a sequence ID, oldest first.

use std::collections::HashMap;

use rusqlite::params;

use crate::codec::decode_value;
use crate::error::Result;
use crate::store::{branch_row, Store};
use crate::types::{BranchId, EventRef, Seq, Timestamp, Value};

/// What a log read keeps. Each filter left out keeps everything.
#[derive(Clone, Debug, Default)]
pub struct LogFilter {
    /// Only events on these entities: for links, either end.
    pub entities: Option<Vec<String>>,
    /// Only events by these authors.
    pub authors: Option<Vec<String>>,
    /// Only these value keys, or these link types.
    pub keys: Option<Vec<String>>,
    /// Only events at or after this time.
    pub since: Option<Timestamp>,
    /// Only events before this time.
    pub until: Option<Timestamp>,
    /// At most this many: the newest kept, or with `after` the oldest.
    pub limit: Option<usize>,
    /// Only value events, or only link events; both when left out.
    pub kind: Option<LogKind>,
    /// Only clears (a value set to null) and link removals.
    pub removed: bool,
    /// Only the branch's own events after this sequence ID, oldest first: the
    /// change feed. Events the branch sees through its parents are left out.
    pub after: Option<Seq>,
}

/// Which events a log read keeps.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LogKind {
    Values,
    Links,
}

/// One value event: `value` is null for a clear.
#[derive(Clone, Debug, PartialEq)]
pub struct ValueChange {
    pub entity: String,
    pub key: String,
    pub value: Value,
    pub event: EventRef,
}

/// One link event: `sort` is `None` for a removal.
#[derive(Clone, Debug, PartialEq)]
pub struct LinkChange {
    pub source: String,
    pub destination: String,
    pub link_type: String,
    pub sort: Option<f64>,
    pub event: EventRef,
}

/// One event in the log.
#[derive(Clone, Debug, PartialEq)]
pub enum Change {
    Value(ValueChange),
    Link(LinkChange),
}

impl Change {
    pub fn event(&self) -> &EventRef {
        match self {
            Change::Value(c) => &c.event,
            Change::Link(c) => &c.event,
        }
    }
}

/// A log read: the events, and the sequence ID to read `after` next time.
/// Without `after`, `head` is the branch's head. With it, it is the last
/// event's when `limit` cut the answer short (so nothing is skipped), else the
/// head. `reset` says the cursor is past the head: it came from another store
/// (or the store was replaced), so what a client holds must be read again.
#[derive(Clone, Debug, PartialEq)]
pub struct Log {
    pub events: Vec<Change>,
    pub head: Seq,
    pub reset: bool,
}

const VALUE_LOG_SQL: &str = "
  SELECT eid, key, seq, ts, author, vtype, value FROM (
    SELECT bid, eid, key, seq, ts, author, vtype, value FROM value_current
    UNION ALL
    SELECT bid, eid, key, seq, ts, author, vtype, value FROM value_history
  )
  WHERE bid = ?1 AND seq BETWEEN ?2 AND ?3
    AND (?4 IS NULL OR eid IN (SELECT value FROM json_each(?4)))
    AND (?5 IS NULL OR author IN (SELECT value FROM json_each(?5)))
    AND (?6 IS NULL OR key IN (SELECT value FROM json_each(?6)))
    AND (?7 IS NULL OR ts >= ?7)
    AND (?8 IS NULL OR ts < ?8)";

const LINK_LOG_SQL: &str = "
  SELECT source, destination, link_type, seq, ts, author, sort FROM (
    SELECT bid, source, destination, link_type, seq, ts, author, sort FROM link_current
    UNION ALL
    SELECT bid, source, destination, link_type, seq, ts, author, sort FROM link_history
  )
  WHERE bid = ?1 AND seq BETWEEN ?2 AND ?3
    AND (?4 IS NULL OR source IN (SELECT value FROM json_each(?4)) OR destination IN (SELECT value FROM json_each(?4)))
    AND (?5 IS NULL OR author IN (SELECT value FROM json_each(?5)))
    AND (?6 IS NULL OR link_type IN (SELECT value FROM json_each(?6)))
    AND (?7 IS NULL OR ts >= ?7)
    AND (?8 IS NULL OR ts < ?8)";

/// The filter's lists as SQL parameters: JSON arrays, or null for no filter.
struct Bound {
    eids: Option<String>,
    authors: Option<String>,
    keys: Option<String>,
}


/// An event as read, before its entities are named.
enum Raw {
    Value(i64, String, Value, EventRef),
    Link(i64, i64, String, Option<f64>, EventRef),
}

impl Store {
    /// Every event the branch sees that `filter` keeps: the newest first (by
    /// time, then by layer and seq), or with `after` the branch's own events
    /// after it, oldest first.
    pub fn log(&self, branch: BranchId, filter: &LogFilter) -> Result<Log> {
        let row = branch_row(&self.conn, branch)?;
        let head = row.head;
        if filter.after.is_some_and(|a| a > head) {
            return Ok(Log { events: Vec::new(), head, reset: true });
        }
        let Some((bound, mut names)) = self.bind(filter)? else {
            return Ok(Log { events: Vec::new(), head, reset: false });
        };
        let view = self.branch_view(branch)?;
        let mut raw: Vec<(usize, Raw)> = Vec::new();
        for src in &view.sources {
            if filter.after.is_some() && src.bid != row.bid {
                continue;
            }
            for &(lo, hi) in &src.intervals {
                let lo = filter.after.map_or(lo, |a| lo.max(a + 1));
                if lo > hi {
                    continue;
                }
                let bind = params![src.bid, lo, hi, bound.eids, bound.authors, bound.keys, filter.since, filter.until];
                if filter.kind != Some(LogKind::Links) {
                    self.note_sql(1);
                    let mut stmt = self.conn.prepare_cached(VALUE_LOG_SQL)?;
                    let mut rows = stmt.query(bind)?;
                    while let Some(r) = rows.next()? {
                        let value = decode_value(r.get(5)?, r.get_ref(6)?)?;
                        if filter.removed && value != Value::Null {
                            continue;
                        }
                        let event = EventRef { branch: src.id, seq: r.get(2)?, timestamp: r.get(3)?, author: r.get(4)? };
                        raw.push((src.layer, Raw::Value(r.get(0)?, r.get(1)?, value, event)));
                    }
                }
                if filter.kind != Some(LogKind::Values) {
                    self.note_sql(1);
                    let mut stmt = self.conn.prepare_cached(LINK_LOG_SQL)?;
                    let mut rows = stmt.query(bind)?;
                    while let Some(r) = rows.next()? {
                        let sort: Option<f64> = r.get(6)?;
                        if filter.removed && sort.is_some() {
                            continue;
                        }
                        let event = EventRef { branch: src.id, seq: r.get(3)?, timestamp: r.get(4)?, author: r.get(5)? };
                        raw.push((src.layer, Raw::Link(r.get(0)?, r.get(1)?, r.get(2)?, sort, event)));
                    }
                }
            }
        }
        // Ordered, and cut, before naming the entities: only what is kept is looked up.
        let key = |r: &Raw| match r {
            Raw::Value(.., e) | Raw::Link(.., e) => (e.timestamp, e.seq),
        };
        let cut = filter.limit.is_some_and(|n| raw.len() > n);
        let kept = match filter.after {
            Some(_) => {
                raw.sort_by_key(|(_, r)| key(r).1);
                let mut out: Vec<Raw> = raw.into_iter().map(|(_, r)| r).collect();
                if let Some(n) = filter.limit {
                    out.truncate(n);
                }
                out
            }
            None => newest(raw, filter.limit, key),
        };
        let mut name = |eid: i64| -> Result<String> {
            if let Some(n) = names.get(&eid) {
                return Ok(n.clone());
            }
            let n = self.entity_name(eid)?;
            names.insert(eid, n.clone());
            Ok(n)
        };
        let mut events = Vec::with_capacity(kept.len());
        for r in kept {
            events.push(match r {
                Raw::Value(eid, key, value, event) => Change::Value(ValueChange { entity: name(eid)?, key, value, event }),
                Raw::Link(s, d, link_type, sort, event) => {
                    Change::Link(LinkChange { source: name(s)?, destination: name(d)?, link_type, sort, event })
                }
            });
        }
        let head = match (filter.after, cut, events.last()) {
            (Some(_), true, Some(last)) => last.event().seq,
            _ => head,
        };
        Ok(Log { events, head, reset: false })
    }

    /// The filter's lists bound for SQL, and the entities' interned keys; None
    /// when it names entities none of which exist (nothing can match).
    fn bind(&self, filter: &LogFilter) -> Result<Option<(Bound, HashMap<i64, String>)>> {
        let json = |v: &Option<Vec<String>>| v.as_ref().map(|v| serde_json::to_string(v).expect("strings serialise"));
        let names = match &filter.entities {
            Some(ids) => {
                let refs: Vec<&str> = ids.iter().map(String::as_str).collect();
                let names = self.lookup(&refs)?;
                if names.is_empty() {
                    return Ok(None);
                }
                names
            }
            None => HashMap::new(),
        };
        let eids = filter
            .entities
            .as_ref()
            .map(|_| serde_json::to_string(&names.keys().collect::<Vec<_>>()).expect("ints serialise"));
        Ok(Some((Bound { eids, authors: json(&filter.authors), keys: json(&filter.keys) }, names)))
    }

    fn entity_name(&self, eid: i64) -> Result<String> {
        Ok(self
            .conn
            .prepare_cached("SELECT ext_id FROM entities WHERE eid = ?1")?
            .query_row([eid], |r| r.get(0))?)
    }
}

/// Newest first: by time, then the higher layer, then the higher seq; cut to `limit`.
fn newest<T>(mut rows: Vec<(usize, T)>, limit: Option<usize>, key: impl Fn(&T) -> (Timestamp, i64)) -> Vec<T> {
    rows.sort_by(|(la, a), (lb, b)| {
        let (ta, sa) = key(a);
        let (tb, sb) = key(b);
        (tb, lb, sb).cmp(&(ta, la, sa))
    });
    let mut out: Vec<T> = rows.into_iter().map(|(_, t)| t).collect();
    if let Some(n) = limit {
        out.truncate(n);
    }
    out
}
