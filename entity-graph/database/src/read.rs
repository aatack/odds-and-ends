//! The read path. Every read runs the same three steps: flatten the view
//! (`view::load_view`, cached per branch), load each layer's events from its
//! event source, and roll them up (`view::roll_up`). Open intervals read the
//! current tables only; bounded intervals read current UNION ALL history.

use std::collections::{BTreeMap, HashMap};

use rusqlite::{params, Connection};

use crate::codec::decode_value;
use crate::error::{Error, Result};
use crate::store::{branch_row, lookup_entity, Store};
use crate::types::{BranchId, Direction, EventRef, Link, Seq, Timestamp, Value, TYPE_KEY};
use crate::view::{roll_up, Cand, EventSource, Order, Source, View};

impl Store {
    /// All values of an entity on a branch. Null (cleared) values are left out.
    pub fn values(&self, branch: BranchId, entity: &str) -> Result<BTreeMap<String, Value>> {
        let Some((view, eid)) = self.prepare(branch, entity)? else {
            return Ok(BTreeMap::new());
        };
        Ok(resolve_values(&self.conn, &view, eid, None)?
            .into_iter()
            .filter(|(_, c)| !c.data.is_null())
            .map(|(k, c)| (k, c.data))
            .collect())
    }

    /// One value. `None` when the key was never set or was cleared with null.
    pub fn value(&self, branch: BranchId, entity: &str, key: &str) -> Result<Option<Value>> {
        let Some((view, eid)) = self.prepare(branch, entity)? else {
            return Ok(None);
        };
        Ok(resolve_values(&self.conn, &view, eid, Some(key))?
            .remove(key)
            .map(|c| c.data)
            .filter(|v| !v.is_null()))
    }

    /// Live links of an entity, grouped by link type, each group sorted.
    ///
    /// The last event per `(source, destination, type)` wins. Removed links
    /// (NULL sort) are left out. Each group is sorted ascending by the stored
    /// sort key. Ties sort by the other entity's ID.
    ///
    /// Implicit filter: the other entity must have a winning `type` value that
    /// is not null. Archived entities (null type) and entities that never had
    /// a type are left out. Use [`Store::links_including_archived`] to skip
    /// the filter.
    pub fn links(
        &self,
        branch: BranchId,
        entity: &str,
        direction: Direction,
        link_type: Option<&str>,
    ) -> Result<BTreeMap<String, Vec<Link>>> {
        self.links_filtered(branch, entity, direction, link_type, true)
    }

    /// [`Store::links`] without the type filter: every live link, whatever
    /// the type of the other entity.
    pub fn links_including_archived(
        &self,
        branch: BranchId,
        entity: &str,
        direction: Direction,
        link_type: Option<&str>,
    ) -> Result<BTreeMap<String, Vec<Link>>> {
        self.links_filtered(branch, entity, direction, link_type, false)
    }

    fn links_filtered(
        &self,
        branch: BranchId,
        entity: &str,
        direction: Direction,
        link_type: Option<&str>,
        typed_only: bool,
    ) -> Result<BTreeMap<String, Vec<Link>>> {
        let Some((view, eid)) = self.prepare(branch, entity)? else {
            return Ok(BTreeMap::new());
        };
        // One PK lookup per layer for each distinct other entity.
        let mut typed: HashMap<i64, bool> = HashMap::new();
        let mut groups: BTreeMap<String, Vec<Link>> = BTreeMap::new();
        for ((link_type, other_eid), cand) in
            resolve_links(&self.conn, &view, eid, direction, link_type)?
        {
            let (other, sort) = cand.data;
            let Some(sort) = sort else { continue };
            if typed_only {
                let ok = match typed.get(&other_eid) {
                    Some(&ok) => ok,
                    None => {
                        let ok = has_type(&self.conn, &view, other_eid)?;
                        typed.insert(other_eid, ok);
                        ok
                    }
                };
                if !ok {
                    continue;
                }
            }
            let (source, destination) = match direction {
                Direction::Outbound => (entity.to_owned(), other),
                Direction::Inbound => (other, entity.to_owned()),
            };
            groups.entry(link_type.clone()).or_default().push(Link {
                source,
                destination,
                link_type,
                sort,
                event: EventRef {
                    branch: cand.order.branch,
                    seq: cand.order.seq,
                    timestamp: cand.order.ts,
                    author: cand.author,
                },
            });
        }
        for links in groups.values_mut() {
            links.sort_by(|a, b| {
                a.sort.total_cmp(&b.sort).then_with(|| match direction {
                    Direction::Outbound => a.destination.cmp(&b.destination),
                    Direction::Inbound => a.source.cmp(&b.source),
                })
            });
        }
        Ok(groups)
    }

    /// The children of an entity for one link type, sorted. Applies the
    /// type filter of [`Store::links`].
    pub fn children(&self, branch: BranchId, entity: &str, link_type: &str) -> Result<Vec<Link>> {
        Ok(self
            .links(branch, entity, Direction::Outbound, Some(link_type))?
            .remove(link_type)
            .unwrap_or_default())
    }

    /// The parents of an entity for one link type, sorted. Applies the
    /// type filter of [`Store::links`].
    pub fn parents(&self, branch: BranchId, entity: &str, link_type: &str) -> Result<Vec<Link>> {
        Ok(self
            .links(branch, entity, Direction::Inbound, Some(link_type))?
            .remove(link_type)
            .unwrap_or_default())
    }

    /// An entity exists on a branch when the branch sees any event that
    /// names it: a value, or a link at either end.
    pub fn exists(&self, branch: BranchId, entity: &str) -> Result<bool> {
        let Some((view, eid)) = self.prepare(branch, entity)? else {
            return Ok(false);
        };
        any_event(&self.conn, &view, eid)
    }

    /// Whether the entity is archived on this branch: its newest visible
    /// `type` event wrote null. An entity that never had a type is not archived.
    pub fn is_archived(&self, branch: BranchId, entity: &str) -> Result<bool> {
        let Some((view, eid)) = self.prepare(branch, entity)? else {
            return Ok(false);
        };
        Ok(resolve_values(&self.conn, &view, eid, Some(TYPE_KEY))?
            .remove(TYPE_KEY)
            .is_some_and(|c| c.data.is_null()))
    }

    /// Walk the graph from a start entity.
    ///
    /// TODO(Alexander): semantics to come (order, depth, filters, cycles, paging).
    /// Must apply the type filter of [`Store::links`] (R23).
    pub fn walk(&self, _branch: BranchId, _start: &str) -> Result<Vec<Link>> {
        Err(Error::NotImplemented("walk"))
    }

    /// Find entities by their values.
    ///
    /// TODO(Alexander): semantics to come (predicates, indexes, paging).
    /// Must return only entities whose `type` is not null (R23).
    pub fn find(&self, _branch: BranchId, _key: &str, _value: &Value) -> Result<Vec<String>> {
        Err(Error::NotImplemented("find"))
    }

    /// The view of a branch and the interned entity key, or `None` when the
    /// entity was never written anywhere in the store.
    fn prepare(&self, branch: BranchId, entity: &str) -> Result<Option<(View, i64)>> {
        let bid = branch_row(&self.conn, branch)?.bid;
        let Some(eid) = lookup_entity(&self.conn, entity)? else {
            return Ok(None);
        };
        Ok(Some((self.view(bid)?, eid)))
    }
}

/// The SQL for one event source: the current-table query, plus a `UNION ALL`
/// of the same query on history for a bounded interval.
/// Current-table queries write `+seq`, so that SQLite searches by target and
/// not by the `(bid, seq)` key.
///
/// The history half names its index, because the planner cannot know that
/// `(bid, seq)` ranges are wide and target lookups are narrow.
pub(crate) fn with_source(current_sql: &str, src: EventSource, history_index: &str) -> String {
    match src {
        EventSource::Current => current_sql.to_owned(),
        EventSource::CurrentAndHistory => {
            // `+seq` keeps the current half on its target key. History may
            // use seq inside its target index, so drop the `+` there.
            let history = current_sql
                .replace(" INDEXED BY link_current_in", "")
                .replace("+seq", "seq")
                .replace("+l.seq", "l.seq")
                .replace(
                    "value_current",
                    &format!("value_history INDEXED BY {history_index}"),
                )
                .replace(
                    "link_current l",
                    &format!("link_history l INDEXED BY {history_index}"),
                )
                .replace(
                    "link_current",
                    &format!("link_history INDEXED BY {history_index}"),
                );
            format!("{current_sql} UNION ALL {history}")
        }
    }
}

/// Whether the entity's winning `type` value in the view is not null.
fn has_type(conn: &Connection, view: &View, eid: i64) -> Result<bool> {
    Ok(resolve_values(conn, view, eid, Some(TYPE_KEY))?
        .remove(TYPE_KEY)
        .is_some_and(|c| !c.data.is_null()))
}

fn order(src: &Source, seq: Seq, ts: Timestamp) -> Order {
    Order {
        layer: src.layer,
        seq,
        branch: src.id,
        ts,
    }
}

/// The newest visible event per key of one entity (or of one key).
/// Step 2 loads each layer's value events from its event source; step 3 is
/// the shared [`roll_up`].
pub(crate) fn resolve_values(
    conn: &Connection,
    view: &View,
    eid: i64,
    key: Option<&str>,
) -> Result<HashMap<String, Cand<Value>>> {
    let base = if key.is_some() {
        "SELECT key, seq, ts, author, vtype, value FROM value_current
         WHERE bid = ?1 AND eid = ?2 AND key = ?3 AND +seq BETWEEN ?4 AND ?5"
    } else {
        "SELECT key, seq, ts, author, vtype, value FROM value_current
         WHERE bid = ?1 AND eid = ?2 AND ?3 IS NULL AND +seq BETWEEN ?4 AND ?5"
    };
    let mut events = Vec::new();
    for src in &view.sources {
        for &(lo, hi) in &src.intervals {
            let mut stmt = conn.prepare_cached(&with_source(
                base,
                EventSource::of((lo, hi)),
                "value_history_target",
            ))?;
            let mut rows = stmt.query(params![src.bid, eid, key, lo, hi])?;
            while let Some(r) = rows.next()? {
                let value = decode_value(r.get(4)?, r.get_ref(5)?)?;
                events.push((
                    r.get::<_, String>(0)?,
                    Cand {
                        order: order(src, r.get(1)?, r.get(2)?),
                        author: r.get(3)?,
                        data: value,
                    },
                ));
            }
        }
    }
    Ok(roll_up(events))
}

/// `(link type, other entity key)`.
type LinkKey = (String, i64);
/// The other entity's ID and the stored sort key (`None` = removed).
type LinkCand = Cand<(String, Option<f64>)>;

/// The newest visible event per `(link type, other entity)` around one entity.
/// Same steps as [`resolve_values`]: per-layer event source, shared roll-up.
pub(crate) fn resolve_links(
    conn: &Connection,
    view: &View,
    eid: i64,
    direction: Direction,
    link_type: Option<&str>,
) -> Result<HashMap<LinkKey, LinkCand>> {
    let (me, other, hint, index) = match direction {
        Direction::Outbound => ("source", "destination", "", "link_history_target"),
        Direction::Inbound => (
            "destination",
            "source",
            " INDEXED BY link_current_in",
            "link_history_in",
        ),
    };
    let ty = if link_type.is_some() {
        "l.link_type = ?3"
    } else {
        "?3 IS NULL"
    };
    let base = format!(
        "SELECT l.link_type, l.{other}, e.ext_id, l.seq, l.ts, l.author, l.sort
         FROM link_current l{hint} JOIN entities e ON e.eid = l.{other}
         WHERE l.bid = ?1 AND l.{me} = ?2 AND {ty} AND +l.seq BETWEEN ?4 AND ?5"
    );
    let mut events = Vec::new();
    for src in &view.sources {
        for &(lo, hi) in &src.intervals {
            let mut stmt =
                conn.prepare_cached(&with_source(&base, EventSource::of((lo, hi)), index))?;
            let mut rows = stmt.query(params![src.bid, eid, link_type, lo, hi])?;
            while let Some(r) = rows.next()? {
                events.push((
                    (r.get::<_, String>(0)?, r.get::<_, i64>(1)?),
                    Cand {
                        order: order(src, r.get(3)?, r.get(4)?),
                        author: r.get(5)?,
                        data: (r.get::<_, String>(2)?, r.get::<_, Option<f64>>(6)?),
                    },
                ));
            }
        }
    }
    Ok(roll_up(events))
}

/// Whether any event of the view names the entity. Uses the same per-layer
/// event sources as the roll-ups.
fn any_event(conn: &Connection, view: &View, eid: i64) -> Result<bool> {
    const PROBES: [(&str, &str); 3] = [
        ("SELECT 1 FROM value_current WHERE bid = ?1 AND eid = ?2 AND +seq BETWEEN ?3 AND ?4", "value_history_target"),
        ("SELECT 1 FROM link_current WHERE bid = ?1 AND source = ?2 AND +seq BETWEEN ?3 AND ?4", "link_history_target"),
        ("SELECT 1 FROM link_current INDEXED BY link_current_in WHERE bid = ?1 AND destination = ?2 AND +seq BETWEEN ?3 AND ?4", "link_history_in"),
    ];
    for src in &view.sources {
        for &(lo, hi) in &src.intervals {
            for (probe, index) in PROBES {
                let sql = format!(
                    "SELECT EXISTS ({})",
                    with_source(probe, EventSource::of((lo, hi)), index)
                );
                let found: bool = conn
                    .prepare_cached(&sql)?
                    .query_row(params![src.bid, eid, lo, hi], |r| r.get(0))?;
                if found {
                    return Ok(true);
                }
            }
        }
    }
    Ok(false)
}
