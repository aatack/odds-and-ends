//! The edit log: every value and link event a branch can see, current and
//! superseded alike, filtered by entity, author, key (or link type) and time.
//! Where [`Store::value_events`] says which event set each value now, the log
//! says everything that was done, and when.

use std::collections::HashMap;

use rusqlite::params;

use crate::codec::decode_value;
use crate::error::Result;
use crate::store::Store;
use crate::types::{EventRef, Timestamp, Value};

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
    /// At most this many, the newest kept.
    pub limit: Option<usize>,
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

impl Store {
    /// Every value event the branch sees that `filter` keeps, the newest first
    /// (by time, then by layer and seq).
    pub fn value_log(&self, branch: crate::types::BranchId, filter: &LogFilter) -> Result<Vec<ValueChange>> {
        let Some((bound, names)) = self.bind(filter)? else {
            return Ok(Vec::new());
        };
        let view = self.branch_view(branch)?;
        let mut out = Vec::new();
        for src in &view.sources {
            for &(lo, hi) in &src.intervals {
                self.note_sql(1);
                let mut stmt = self.conn.prepare_cached(VALUE_LOG_SQL)?;
                let mut rows = stmt.query(params![
                    src.bid, lo, hi, bound.eids, bound.authors, bound.keys, filter.since, filter.until
                ])?;
                while let Some(r) = rows.next()? {
                    let eid: i64 = r.get(0)?;
                    let entity = match names.get(&eid) {
                        Some(n) => n.clone(),
                        None => self.entity_name(eid)?,
                    };
                    out.push((
                        src.layer,
                        ValueChange {
                            entity,
                            key: r.get(1)?,
                            value: decode_value(r.get(5)?, r.get_ref(6)?)?,
                            event: EventRef { branch: src.id, seq: r.get(2)?, timestamp: r.get(3)?, author: r.get(4)? },
                        },
                    ));
                }
            }
        }
        Ok(newest(out, filter.limit, |c| (c.event.timestamp, c.event.seq)))
    }

    /// Every link event the branch sees that `filter` keeps (its entities
    /// matching either end), the newest first.
    pub fn link_log(&self, branch: crate::types::BranchId, filter: &LogFilter) -> Result<Vec<LinkChange>> {
        let Some((bound, _)) = self.bind(filter)? else {
            return Ok(Vec::new());
        };
        let view = self.branch_view(branch)?;
        let mut raw = Vec::new();
        for src in &view.sources {
            for &(lo, hi) in &src.intervals {
                self.note_sql(1);
                let mut stmt = self.conn.prepare_cached(LINK_LOG_SQL)?;
                let mut rows = stmt.query(params![
                    src.bid, lo, hi, bound.eids, bound.authors, bound.keys, filter.since, filter.until
                ])?;
                while let Some(r) = rows.next()? {
                    raw.push((
                        src.layer,
                        (r.get::<_, i64>(0)?, r.get::<_, i64>(1)?, r.get::<_, String>(2)?, r.get::<_, Option<f64>>(6)?),
                        EventRef { branch: src.id, seq: r.get(3)?, timestamp: r.get(4)?, author: r.get(5)? },
                    ));
                }
            }
        }
        let mut names: HashMap<i64, String> = HashMap::new();
        let mut out = Vec::with_capacity(raw.len());
        for (layer, (s, d, link_type, sort), event) in raw {
            for eid in [s, d] {
                if !names.contains_key(&eid) {
                    let n = self.entity_name(eid)?;
                    names.insert(eid, n);
                }
            }
            out.push((
                layer,
                LinkChange { source: names[&s].clone(), destination: names[&d].clone(), link_type, sort, event },
            ));
        }
        Ok(newest(out, filter.limit, |c| (c.event.timestamp, c.event.seq)))
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
