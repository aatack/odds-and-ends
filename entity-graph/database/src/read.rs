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
        let mut out: HashMap<String, BTreeMap<String, Value>> = entities
            .iter()
            .map(|e| ((*e).to_owned(), BTreeMap::new()))
            .collect();
        for ((entity, key), c) in resolve_values(&self.conn, view, entities, None)? {
            if !c.data.is_null() {
                out.get_mut(&entity)
                    .expect("asked for")
                    .insert(key, c.data);
            }
        }
        Ok(out)
    }

    /// One value of one entity: a thin helper. `None` when the key was never
    /// set or was cleared with null.
    pub fn value(&self, branch: BranchId, entity: &str, key: &str) -> Result<Option<Value>> {
        let view = self.branch_view(branch)?;
        Ok(resolve_values(&self.conn, &view, &[entity], Some(key))?
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
        let empty: LinkGroups = groups
            .iter()
            .map(|&(t, d)| ((t.to_owned(), d), Vec::new()))
            .collect();
        let mut out: HashMap<String, LinkGroups> = entities
            .iter()
            .map(|e| ((*e).to_owned(), empty.clone()))
            .collect();
        let mut found = Vec::new();
        for direction in [Direction::Outbound, Direction::Inbound] {
            let types: Vec<&str> = groups
                .iter()
                .filter(|g| g.1 == direction)
                .map(|g| g.0)
                .collect();
            if types.is_empty() || entities.is_empty() {
                continue;
            }
            for ((entity, ty, other), c) in
                resolve_links(&self.conn, view, entities, direction, &types)?
            {
                if c.data.1.is_some() {
                    found.push((direction, entity, ty, other, c));
                }
            }
        }
        let typed = if typed_only {
            let others: Vec<&str> = found.iter().map(|f: &(Direction, String, String, String, LinkCand)| f.3.as_str()).collect();
            Some(typed_entities(&self.conn, view, &others)?)
        } else {
            None
        };
        for (direction, me, link_type, other_id, cand) in found {
            if typed.as_ref().is_some_and(|t| !t.contains(&other_id)) {
                continue;
            }
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
        let view = self.branch_view(branch)?;
        let found = any_event(&self.conn, &view, entities)?;
        Ok(entities
            .iter()
            .map(|e| ((*e).to_owned(), found.contains(*e)))
            .collect())
    }

    /// Whether each entity is archived: its winning `type` event wrote null.
    /// An entity that never had a type is not archived.
    pub fn is_archived(
        &self,
        branch: BranchId,
        entities: &[&str],
    ) -> Result<HashMap<String, bool>> {
        let view = self.branch_view(branch)?;
        let archived: HashSet<String> =
            resolve_values(&self.conn, &view, entities, Some(TYPE_KEY))?
                .into_iter()
                .filter(|(_, c)| c.data.is_null())
                .map(|((entity, _), _)| entity)
                .collect();
        Ok(entities
            .iter()
            .map(|e| ((*e).to_owned(), archived.contains(*e)))
            .collect())
    }

    /// Find entities by their values.
    ///
    /// TODO(Alexander): signatures to come (predicates, indexes, paging).
    /// Must return only entities whose `type` is not null (R23).
    pub fn find(&self, _branch: BranchId, _key: &str, _value: &Value) -> Result<Vec<String>> {
        Err(Error::NotImplemented("find"))
    }

    /// The flattened view of a branch, resolved once (and cached).
    pub(crate) fn branch_view(&self, branch: BranchId) -> Result<View> {
        self.view(branch_row(&self.conn, branch)?.bid)
    }
}

/// A JSON array of entity IDs, bound as one parameter.
fn json_keys(keys: &[&str]) -> String {
    serde_json::to_string(keys).expect("strings serialise")
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
fn typed_entities(conn: &Connection, view: &View, eids: &[&str]) -> Result<HashSet<String>> {
    let mut unique = eids.to_vec();
    unique.sort_unstable();
    unique.dedup();
    if unique.is_empty() {
        return Ok(HashSet::new());
    }
    Ok(resolve_values(conn, view, &unique, Some(TYPE_KEY))?
        .into_iter()
        .filter(|(_, c)| !c.data.is_null())
        .map(|((entity, _), _)| entity)
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
    "SELECT entity, key, seq, ts, author, vtype, value FROM value_current
     WHERE bid = ?1 AND entity IN (SELECT value FROM json_each(?2))
       AND (?3 IS NULL OR key = ?3) AND +seq BETWEEN ?4 AND ?5";

/// The newest visible event per `(entity, key)` for a batch of entities (all
/// keys, or one key). Step 2 loads each layer's value events from its event
/// source; step 3 is the shared [`roll_up`].
pub(crate) fn resolve_values(
    conn: &Connection,
    view: &View,
    eids: &[&str],
    key: Option<&str>,
) -> Result<HashMap<(String, String), Cand<Value>>> {
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
                    (r.get::<_, String>(0)?, r.get::<_, String>(1)?),
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
type LinkKey = (String, String, String);
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
            "SELECT l.{me}, l.link_type, l.{other}, l.seq, l.ts, l.author, l.sort
             FROM link_current l{hint}
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
    eids: &[&str],
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
                    (r.get(0)?, r.get(1)?, r.get::<_, String>(2)?),
                    Cand {
                        order: order(src, r.get(3)?, r.get(4)?),
                        author: r.get(5)?,
                        data: (r.get(2)?, r.get(6)?),
                    },
                ));
            }
        }
    }
    Ok(roll_up(events))
}

/// The entities of the batch that some event of the view names. Uses the
/// same per-layer event sources as the roll-ups.
fn any_event(conn: &Connection, view: &View, eids: &[&str]) -> Result<HashSet<String>> {
    const PROBES: [(&str, &str); 3] = [
        (
            "SELECT entity FROM value_current WHERE bid = ?1 AND entity IN (SELECT value FROM json_each(?2)) AND +seq BETWEEN ?3 AND ?4",
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
                    stmt.query_map(params![src.bid, batch, lo, hi], |r| r.get::<_, String>(0))?;
                for entity in rows {
                    found.insert(entity?);
                }
            }
            if found.len() == eids.len() {
                return Ok(found);
            }
        }
    }
    Ok(found)
}
