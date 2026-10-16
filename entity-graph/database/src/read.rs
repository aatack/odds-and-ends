//! The read path. Every read takes a batch of entity IDs and runs the same
//! three steps once per call: flatten the view (`view::load_view`, cached per
//! branch), load each layer's events from its event source for the whole
//! batch, and roll them up (`view::roll_up`). Open intervals read the current
//! tables only; bounded intervals read current UNION ALL history. A batch is
//! passed to SQLite as one JSON array (`json_each`), not as one query per ID.

use std::collections::{BTreeMap, HashMap, HashSet};

use rusqlite::{params, Connection};

use crate::codec::decode_value;
use crate::error::{Error, Result};
use crate::store::{branch_row, Store};
use crate::types::{BranchId, Direction, EventRef, Link, Seq, Timestamp, Value, TYPE_KEY};
use crate::view::{roll_up, Cand, EventSource, Order, Source, View};

/// Links of one entity, grouped by the asked-for `(link type, direction)`.
pub type LinkGroups = BTreeMap<(String, Direction), Vec<Link>>;

impl Store {
    /// All values of each entity. Null (cleared) values are left out. Every
    /// asked-for ID is a key of the result; an unknown ID maps to no values.
    pub fn values(
        &self,
        branch: BranchId,
        entities: &[&str],
    ) -> Result<HashMap<String, BTreeMap<String, Value>>> {
        self.values_in(&self.branch_view(branch)?, entities)
    }

    /// [`Store::values`] on a view that the caller resolved.
    pub(crate) fn values_in(
        &self,
        view: &View,
        entities: &[&str],
    ) -> Result<HashMap<String, BTreeMap<String, Value>>> {
        let eids = self.lookup(entities)?;
        self.values_with(view, entities, &eids)
    }

    /// [`Store::values_in`] with the interned keys already looked up.
    pub(crate) fn values_with(
        &self,
        view: &View,
        entities: &[&str],
        eids: &HashMap<i64, String>,
    ) -> Result<HashMap<String, BTreeMap<String, Value>>> {
        let mut out: HashMap<String, BTreeMap<String, Value>> = entities
            .iter()
            .map(|e| ((*e).to_owned(), BTreeMap::new()))
            .collect();
        let keys: Vec<i64> = eids.keys().copied().collect();
        for ((eid, key), c) in resolve_values(&self.conn, view, &keys, None)? {
            if !c.data.is_null() {
                out.get_mut(&eids[&eid])
                    .expect("asked for")
                    .insert(key, c.data);
            }
        }
        Ok(out)
    }

    /// One value of one entity: a thin helper. `None` when the key was never
    /// set or was cleared with null.
    pub fn value(&self, branch: BranchId, entity: &str, key: &str) -> Result<Option<Value>> {
        let (view, eids) = self.prepare(branch, &[entity])?;
        let keys: Vec<i64> = eids.keys().copied().collect();
        Ok(resolve_values(&self.conn, &view, &keys, Some(key))?
            .into_values()
            .next()
            .map(|c| c.data)
            .filter(|v| !v.is_null()))
    }

    /// Live links of each entity, grouped by the asked-for
    /// `(link type, direction)` pairs. Every asked-for ID and pair is a key.
    ///
    /// The last event per `(source, destination, type)` wins. Removed links
    /// (NULL sort) are left out. Each group is sorted ascending by the stored
    /// sort key; ties sort by the other entity's ID.
    ///
    /// Implicit filter: the other entity must have a winning `type` value that
    /// is not null. Archived entities (null type) and entities that never had
    /// a type are left out. Use [`Store::links_including_archived`] to skip
    /// the filter.
    pub fn links(
        &self,
        branch: BranchId,
        entities: &[&str],
        groups: &[(&str, Direction)],
    ) -> Result<HashMap<String, LinkGroups>> {
        self.links_filtered(branch, entities, groups, true)
    }

    /// [`Store::links`] without the type filter.
    pub fn links_including_archived(
        &self,
        branch: BranchId,
        entities: &[&str],
        groups: &[(&str, Direction)],
    ) -> Result<HashMap<String, LinkGroups>> {
        self.links_filtered(branch, entities, groups, false)
    }

    fn links_filtered(
        &self,
        branch: BranchId,
        entities: &[&str],
        groups: &[(&str, Direction)],
        typed_only: bool,
    ) -> Result<HashMap<String, LinkGroups>> {
        self.links_in(&self.branch_view(branch)?, entities, groups, typed_only)
    }

    /// [`Store::links`] on a view that the caller resolved.
    pub(crate) fn links_in(
        &self,
        view: &View,
        entities: &[&str],
        groups: &[(&str, Direction)],
        typed_only: bool,
    ) -> Result<HashMap<String, LinkGroups>> {
        let eids = self.lookup(entities)?;
        self.links_with(view, entities, &eids, groups, typed_only)
    }

    /// [`Store::links_in`] with the interned keys already looked up.
    pub(crate) fn links_with(
        &self,
        view: &View,
        entities: &[&str],
        eids: &HashMap<i64, String>,
        groups: &[(&str, Direction)],
        typed_only: bool,
    ) -> Result<HashMap<String, LinkGroups>> {
        let empty: LinkGroups = groups
            .iter()
            .map(|&(t, d)| ((t.to_owned(), d), Vec::new()))
            .collect();
        let mut out: HashMap<String, LinkGroups> = entities
            .iter()
            .map(|e| ((*e).to_owned(), empty.clone()))
            .collect();
        let keys: Vec<i64> = eids.keys().copied().collect();
        let mut found = Vec::new();
        for direction in [Direction::Outbound, Direction::Inbound] {
            let types: Vec<&str> = groups
                .iter()
                .filter(|g| g.1 == direction)
                .map(|g| g.0)
                .collect();
            if types.is_empty() || keys.is_empty() {
                continue;
            }
            for ((eid, ty, other_eid), c) in
                resolve_links(&self.conn, view, &keys, direction, &types)?
            {
                if c.data.1.is_some() {
                    found.push((direction, eid, ty, other_eid, c));
                }
            }
        }
        let typed = if typed_only {
            let others: Vec<i64> = found.iter().map(|f| f.3).collect();
            Some(typed_entities(&self.conn, view, &others)?)
        } else {
            None
        };
        for (direction, eid, link_type, other_eid, cand) in found {
            if typed.as_ref().is_some_and(|t| !t.contains(&other_eid)) {
                continue;
            }
            let me = eids[&eid].clone();
            let (other, sort) = cand.data;
            let sort = sort.expect("removed links filtered above");
            let (source, destination) = match direction {
                Direction::Outbound => (me.clone(), other),
                Direction::Inbound => (other, me.clone()),
            };
            out.get_mut(&me)
                .expect("asked for")
                .get_mut(&(link_type.clone(), direction))
                .expect("asked for")
                .push(Link {
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
        for groups in out.values_mut() {
            for ((_, direction), links) in groups.iter_mut() {
                links.sort_by(|a, b| {
                    a.sort.total_cmp(&b.sort).then_with(|| match direction {
                        Direction::Outbound => a.destination.cmp(&b.destination),
                        Direction::Inbound => a.source.cmp(&b.source),
                    })
                });
            }
        }
        Ok(out)
    }

    /// The children of one entity for one link type: a thin helper over
    /// [`Store::links`] (same filter and order).
    pub fn children(&self, branch: BranchId, entity: &str, link_type: &str) -> Result<Vec<Link>> {
        self.one_group(branch, entity, link_type, Direction::Outbound)
    }

    /// The parents of one entity for one link type: a thin helper over
    /// [`Store::links`] (same filter and order).
    pub fn parents(&self, branch: BranchId, entity: &str, link_type: &str) -> Result<Vec<Link>> {
        self.one_group(branch, entity, link_type, Direction::Inbound)
    }

    fn one_group(&self, b: BranchId, e: &str, t: &str, d: Direction) -> Result<Vec<Link>> {
        Ok(self
            .links(b, &[e], &[(t, d)])?
            .remove(e)
            .and_then(|mut g| g.remove(&(t.to_owned(), d)))
            .unwrap_or_default())
    }

    /// Whether each entity exists on the branch: the view has any event that
    /// names it (a value, or a link at either end, in any state).
    pub fn exists(&self, branch: BranchId, entities: &[&str]) -> Result<HashMap<String, bool>> {
        let (view, eids) = self.prepare(branch, entities)?;
        let keys: Vec<i64> = eids.keys().copied().collect();
        let found = any_event(&self.conn, &view, &keys)?;
        Ok(entities
            .iter()
            .map(|e| {
                let hit = eids.iter().any(|(k, v)| v == e && found.contains(k));
                ((*e).to_owned(), hit)
            })
            .collect())
    }

    /// Whether each entity is archived: its winning `type` event wrote null.
    /// An entity that never had a type is not archived.
    pub fn is_archived(
        &self,
        branch: BranchId,
        entities: &[&str],
    ) -> Result<HashMap<String, bool>> {
        let (view, eids) = self.prepare(branch, entities)?;
        let keys: Vec<i64> = eids.keys().copied().collect();
        let archived: HashSet<i64> = resolve_values(&self.conn, &view, &keys, Some(TYPE_KEY))?
            .into_iter()
            .filter(|(_, c)| c.data.is_null())
            .map(|((eid, _), _)| eid)
            .collect();
        Ok(entities
            .iter()
            .map(|e| {
                let hit = eids.iter().any(|(k, v)| v == e && archived.contains(k));
                ((*e).to_owned(), hit)
            })
            .collect())
    }

    /// Find entities by their values.
    ///
    /// TODO(Alexander): signatures to come (predicates, indexes, paging).
    /// Must return only entities whose `type` is not null (R23).
    pub fn find(&self, _branch: BranchId, _key: &str, _value: &Value) -> Result<Vec<String>> {
        Err(Error::NotImplemented("find"))
    }

    /// The view of a branch (resolved once per call) and the interned keys of
    /// the asked-for IDs. IDs never written anywhere in the store are absent.
    fn prepare(&self, branch: BranchId, entities: &[&str]) -> Result<(View, HashMap<i64, String>)> {
        Ok((self.branch_view(branch)?, self.lookup(entities)?))
    }

    /// The flattened view of a branch, resolved once (and cached).
    pub(crate) fn branch_view(&self, branch: BranchId) -> Result<View> {
        self.view(branch_row(&self.conn, branch)?.bid)
    }

    /// The interned keys of the IDs that the store knows.
    pub(crate) fn lookup(&self, entities: &[&str]) -> Result<HashMap<i64, String>> {
        let json = serde_json::to_string(entities).expect("strings serialise");
        let mut stmt = self.conn.prepare_cached(
            "SELECT eid, ext_id FROM entities WHERE ext_id IN (SELECT value FROM json_each(?1))",
        )?;
        let eids = stmt
            .query_map([json], |r| Ok((r.get(0)?, r.get(1)?)))?
            .collect::<rusqlite::Result<_>>()?;
        Ok(eids)
    }
}

/// A JSON array of interned keys, bound as one parameter.
fn json_keys(keys: &[i64]) -> String {
    serde_json::to_string(keys).expect("integers serialise")
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

/// The entities of `eids` whose winning `type` value in the view is not null.
fn typed_entities(conn: &Connection, view: &View, eids: &[i64]) -> Result<HashSet<i64>> {
    let mut unique = eids.to_vec();
    unique.sort_unstable();
    unique.dedup();
    if unique.is_empty() {
        return Ok(HashSet::new());
    }
    Ok(resolve_values(conn, view, &unique, Some(TYPE_KEY))?
        .into_iter()
        .filter(|(_, c)| !c.data.is_null())
        .map(|((eid, _), _)| eid)
        .collect())
}

fn order(src: &Source, seq: Seq, ts: Timestamp) -> Order {
    Order {
        layer: src.layer,
        seq,
        branch: src.id,
        ts,
    }
}

pub(crate) const VALUES_SQL: &str =
    "SELECT eid, key, seq, ts, author, vtype, value FROM value_current
     WHERE bid = ?1 AND eid IN (SELECT value FROM json_each(?2))
       AND (?3 IS NULL OR key = ?3) AND +seq BETWEEN ?4 AND ?5";

/// The newest visible event per `(entity, key)` for a batch of entities (all
/// keys, or one key). Step 2 loads each layer's value events from its event
/// source; step 3 is the shared [`roll_up`].
pub(crate) fn resolve_values(
    conn: &Connection,
    view: &View,
    eids: &[i64],
    key: Option<&str>,
) -> Result<HashMap<(i64, String), Cand<Value>>> {
    if eids.is_empty() {
        return Ok(HashMap::new());
    }
    let batch = json_keys(eids);
    let mut events = Vec::new();
    for src in &view.sources {
        for &(lo, hi) in &src.intervals {
            let sql = with_source(
                VALUES_SQL,
                EventSource::of((lo, hi)),
                "value_history_target",
            );
            let mut stmt = conn.prepare_cached(&sql)?;
            let mut rows = stmt.query(params![src.bid, batch, key, lo, hi])?;
            while let Some(r) = rows.next()? {
                let value = decode_value(r.get(5)?, r.get_ref(6)?)?;
                events.push((
                    (r.get::<_, i64>(0)?, r.get::<_, String>(1)?),
                    Cand {
                        order: order(src, r.get(2)?, r.get(3)?),
                        author: r.get(4)?,
                        data: value,
                    },
                ));
            }
        }
    }
    Ok(roll_up(events))
}

/// `(asked-for entity, link type, other entity)`.
type LinkKey = (i64, String, i64);
/// The other entity's ID and the stored sort key (`None` = removed).
type LinkCand = Cand<(String, Option<f64>)>;

/// The SQL that reads links of a batch of entities for a set of link types.
pub(crate) fn links_sql(direction: Direction) -> (String, &'static str) {
    let (me, other, hint, index) = match direction {
        Direction::Outbound => ("source", "destination", "", "link_history_target"),
        Direction::Inbound => (
            "destination",
            "source",
            " INDEXED BY link_current_in",
            "link_history_in",
        ),
    };
    (
        format!(
            "SELECT l.{me}, l.link_type, l.{other}, e.ext_id, l.seq, l.ts, l.author, l.sort
             FROM link_current l{hint} JOIN entities e ON e.eid = l.{other}
             WHERE l.bid = ?1 AND l.{me} IN (SELECT value FROM json_each(?2))
               AND l.link_type IN (SELECT value FROM json_each(?3))
               AND +l.seq BETWEEN ?4 AND ?5"
        ),
        index,
    )
}

/// The newest visible link event per `(entity, type, other entity)` for a
/// batch of entities. Same steps as [`resolve_values`].
pub(crate) fn resolve_links(
    conn: &Connection,
    view: &View,
    eids: &[i64],
    direction: Direction,
    types: &[&str],
) -> Result<HashMap<LinkKey, LinkCand>> {
    let (base, index) = links_sql(direction);
    let batch = json_keys(eids);
    let types = serde_json::to_string(types).expect("strings serialise");
    let mut events = Vec::new();
    for src in &view.sources {
        for &(lo, hi) in &src.intervals {
            let sql = with_source(&base, EventSource::of((lo, hi)), index);
            let mut stmt = conn.prepare_cached(&sql)?;
            let mut rows = stmt.query(params![src.bid, batch, types, lo, hi])?;
            while let Some(r) = rows.next()? {
                events.push((
                    (r.get(0)?, r.get(1)?, r.get(2)?),
                    Cand {
                        order: order(src, r.get(4)?, r.get(5)?),
                        author: r.get(6)?,
                        data: (r.get(3)?, r.get(7)?),
                    },
                ));
            }
        }
    }
    Ok(roll_up(events))
}

/// The entities of the batch that some event of the view names. Uses the
/// same per-layer event sources as the roll-ups.
fn any_event(conn: &Connection, view: &View, eids: &[i64]) -> Result<HashSet<i64>> {
    const PROBES: [(&str, &str); 3] = [
        (
            "SELECT eid FROM value_current WHERE bid = ?1 AND eid IN (SELECT value FROM json_each(?2)) AND +seq BETWEEN ?3 AND ?4",
            "value_history_target",
        ),
        (
            "SELECT source FROM link_current WHERE bid = ?1 AND source IN (SELECT value FROM json_each(?2)) AND +seq BETWEEN ?3 AND ?4",
            "link_history_target",
        ),
        (
            "SELECT destination FROM link_current INDEXED BY link_current_in WHERE bid = ?1 AND destination IN (SELECT value FROM json_each(?2)) AND +seq BETWEEN ?3 AND ?4",
            "link_history_in",
        ),
    ];
    let mut found = HashSet::new();
    if eids.is_empty() {
        return Ok(found);
    }
    let batch = json_keys(eids);
    for src in &view.sources {
        for &(lo, hi) in &src.intervals {
            for (probe, index) in PROBES {
                let sql = with_source(probe, EventSource::of((lo, hi)), index);
                let mut stmt = conn.prepare_cached(&sql)?;
                let rows =
                    stmt.query_map(params![src.bid, batch, lo, hi], |r| r.get::<_, i64>(0))?;
                for eid in rows {
                    found.insert(eid?);
                }
            }
            if found.len() == eids.len() {
                return Ok(found);
            }
        }
    }
    Ok(found)
}
