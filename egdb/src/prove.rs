//! `prove`: decide whether a walk would emit an entity, and return the path
//! that shows it.
//!
//! The query is a [`Store::walk`] query: a start path, a follow map, and the
//! `descend` and `include` predicates. The item is proved when the walk from
//! the start would emit it on some path. The search has two phases:
//!
//! 1. **Backward.** A layered breadth-first search from the item over the
//!    reversed follow rules finds every entity that can reach the item by
//!    valid steps (parent type, link type, direction, child type), with its
//!    distance to the item. It reads only the item's ancestors, in batches:
//!    one link read and one type read per layer. It does not run the
//!    predicates: they depend on the whole path from the start, which is not
//!    known yet.
//! 2. **Forward.** A depth-first search from the start with the walk's own
//!    loader, rules and predicates, that goes only into entities that phase 1
//!    found (within `max_depth`). It stops at the first path to the item that
//!    `include` accepts.
//!
//! Phase 2 is the walk cut down to the entities that can lead to the item, so
//! the answer is the walk's answer. Both phases use the shared batch reads
//! and the roll-up, so prove works on every view. See `DESIGN.md` section
//! 11.3.

use std::collections::HashMap;

use crate::error::Result;
use crate::store::Store;
use crate::types::{BranchId, Direction, Link, Value};
use crate::view::View;
use crate::walk::{Follow, ReadAhead, WalkNode, Walker};

/// Options of [`Store::prove`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ProveOptions {
    /// The most links below the start that a proof may have. `None` is
    /// unlimited. If no proof is within the limit, the answer is
    /// [`ProveOutcome::NoPath`]: "no path of at most `max_depth` links".
    pub max_depth: Option<usize>,
    /// The budget: the most entities the search may expand, in both phases
    /// together. `None` is unlimited. If the budget runs out before the
    /// search decides, the answer is [`ProveOutcome::Unknown`].
    pub max_nodes: Option<usize>,
}

/// A proof that the walk emits the item.
#[derive(Clone, Debug, PartialEq)]
pub struct Proof {
    /// The path on which the walk emits the item: the start path, then the
    /// steps below it, ending at the item. It is the path that
    /// [`Walk::paths`](crate::Walk::paths) would hold.
    pub path: Vec<String>,
    /// The links of the steps below the start, in order: `links[i]` connects
    /// `path[s + i]` and `path[s + i + 1]`, where `s + 1` is the length of
    /// the start path. Empty when the item is the start.
    pub links: Vec<Link>,
}

/// The result of a [`Store::prove`] call.
#[derive(Clone, Debug, PartialEq)]
pub enum ProveOutcome {
    /// The walk emits the item.
    Proved(Proof),
    /// The walk does not emit the item (within `max_depth`).
    NoPath,
    /// The budget ([`ProveOptions::max_nodes`]) ran out before the search
    /// could decide.
    Unknown,
}

/// Counts of the work a [`Store::prove`] call did.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ProveStats {
    /// Entities expanded, in both phases.
    pub nodes_visited: usize,
    /// Batch link-read rounds.
    pub link_reads: usize,
    /// Batch value- or type-read rounds.
    pub value_reads: usize,
    /// SQL statements the call ran (one per layer/interval query).
    pub sql_queries: usize,
    /// The greatest distance (in links) that either phase reached.
    pub depth_reached: usize,
}

/// The outcome and the stats of a [`Store::prove`] call.
#[derive(Clone, Debug, PartialEq)]
pub struct Prove {
    pub outcome: ProveOutcome,
    pub stats: ProveStats,
}

/// `Outbound` becomes `Inbound` and the reverse: the direction to read a
/// link from the other end.
fn reverse(d: Direction) -> Direction {
    match d {
        Direction::Outbound => Direction::Inbound,
        Direction::Inbound => Direction::Outbound,
    }
}

/// The entity at the other end of `link`, read in direction `d`.
fn other_end(link: &Link, d: Direction) -> &String {
    match d {
        Direction::Outbound => &link.destination,
        Direction::Inbound => &link.source,
    }
}

/// The text of a `type` value.
fn text(v: Option<&Value>) -> Option<&str> {
    match v {
        Some(Value::Text(t)) => Some(t),
        _ => None,
    }
}

/// The node budget and the stats, shared by both phases.
struct Budget {
    max_nodes: Option<usize>,
    stats: ProveStats,
    /// The budget ran out.
    out: bool,
}

impl Budget {
    /// Spend `n` expansions; false (and `out` set) when that goes over.
    fn spend(&mut self, n: usize) -> bool {
        if self
            .max_nodes
            .is_some_and(|m| self.stats.nodes_visited + n > m)
        {
            self.out = true;
            return false;
        }
        self.stats.nodes_visited += n;
        true
    }
}

/// Phase 1's state: the entities known to reach the item, layer by layer.
struct Ancestors {
    /// Distance to the item of each entity found so far. Exact: a layer is
    /// complete before the next one starts.
    dist: HashMap<String, usize>,
    /// The last layer found, each entity with its type text.
    frontier: Vec<(String, Option<String>)>,
    /// The distance of the frontier.
    depth: usize,
    eid_of: HashMap<String, i64>,
}

impl Store {
    /// Prove that [`Store::walk`] from `start`, with the same `follow`,
    /// `descend` and `include`, would emit `item`, and return the path.
    ///
    /// The walk's rules apply unchanged: `start` is a path whose last ID is
    /// the first node visited and whose earlier IDs are its ancestors; each
    /// step follows a rule of the parent's type to a child of the rule's
    /// child type; a node already on the path is not visited again; links to
    /// archived or untyped entities are not followed; `descend` must accept
    /// every node above the item on the path (the start included), and
    /// `include` must accept the item. The item may be the start. The walk's
    /// paging (`steps`, `next`), visited set and direction do not change
    /// which paths exist, so they are not options here.
    ///
    /// The search goes backward from the item, one layer of ancestors at a
    /// time. Once the start is among them, it searches forward from the start
    /// through the ancestors found so far; if that finds no path, the
    /// backward search goes on and the forward search runs again, until the
    /// ancestors are exhausted (see the [module docs](crate::prove)). The
    /// proof is one path on which the walk emits the item, the shortest
    /// such paths first; it is not always the walk's first such path.
    #[allow(clippy::too_many_arguments)]
    pub fn prove(
        &self,
        branch: BranchId,
        start: &[&str],
        item: &str,
        follow: &Follow,
        mut descend: impl FnMut(&[String], &WalkNode) -> bool,
        mut include: impl FnMut(&[String], &WalkNode) -> bool,
        options: ProveOptions,
    ) -> Result<Prove> {
        let sql0 = self.sql_queries();
        let mut budget = Budget {
            max_nodes: options.max_nodes,
            stats: ProveStats::default(),
            out: false,
        };
        let outcome = match start.last() {
            None => ProveOutcome::NoPath,
            Some(first) => {
                let view = self.branch_view(branch)?;
                let mut back = self.ancestors_start(&view, item, *first == item, &mut budget)?;
                let mut ld = self.walk_loader(view.clone(), follow, ReadAhead::default());
                let start_path: Vec<String> = start.iter().map(|s| (*s).to_owned()).collect();
                let mut searched = 0;
                let outcome = loop {
                    // Search forward when the start is reached and the
                    // ancestors grew since the last search.
                    if back.dist.contains_key(*first) && back.dist.len() > searched {
                        if searched == 0 {
                            // The start's ancestors, as the walk loads them.
                            ld.ensure(&start_path, usize::MAX)?;
                        }
                        searched = back.dist.len();
                        let mut path = start_path.clone();
                        let mut fwd = Forward {
                            item,
                            dist: &back.dist,
                            max_depth: options.max_depth,
                            base: path.len(),
                            descend: &mut descend,
                            include: &mut include,
                            budget: &mut budget,
                        };
                        if let Some(path) = fwd.search(&mut ld, &mut path)? {
                            let links = proof_links(&ld, follow, &path[start.len() - 1..]);
                            break ProveOutcome::Proved(Proof { path, links });
                        }
                    }
                    if budget.out {
                        break ProveOutcome::Unknown;
                    }
                    let more = !back.frontier.is_empty()
                        && options.max_depth.is_none_or(|m| back.depth < m);
                    if !more {
                        break ProveOutcome::NoPath;
                    }
                    self.ancestors_layer(&view, follow, &mut back, &mut budget)?;
                };
                budget.stats.link_reads += ld.loads;
                budget.stats.value_reads += ld.loads;
                outcome
            }
        };
        budget.stats.sql_queries = (self.sql_queries() - sql0) as usize;
        Ok(Prove {
            outcome,
            stats: budget.stats,
        })
    }

    /// Phase 1's first layer: the item. It is reached only through links, so
    /// it needs a (non-null) type to have ancestors; its type text picks the
    /// rules that can step to it. An item that is the start has no
    /// ancestors to look for: no path reaches the start again.
    fn ancestors_start(
        &self,
        view: &View,
        item: &str,
        is_start: bool,
        budget: &mut Budget,
    ) -> Result<Ancestors> {
        let mut back = Ancestors {
            dist: HashMap::from([(item.to_owned(), 0)]),
            frontier: Vec::new(),
            depth: 0,
            eid_of: HashMap::new(),
        };
        if !is_start {
            let types = self.prove_types(view, &[item.to_owned()], &mut back.eid_of, budget)?;
            if let Some(t) = types.get(item) {
                back.frontier = vec![(item.to_owned(), text(Some(t)).map(str::to_owned))];
            }
        }
        Ok(back)
    }

    /// Phase 1's next layer: the entities that step to the frontier by a
    /// valid step and are not known yet. Sets `budget.out` instead when the
    /// budget cannot pay for the frontier.
    fn ancestors_layer(
        &self,
        view: &View,
        follow: &Follow,
        back: &mut Ancestors,
        budget: &mut Budget,
    ) -> Result<()> {
        let frontier = std::mem::take(&mut back.frontier);
        if !budget.spend(frontier.len()) {
            return Ok(());
        }
        // The reversed pairs of every rule that accepts some frontier
        // entity's type: read them all in one batch.
        let mut pairs: Vec<(String, Direction)> = Vec::new();
        for rules in follow.values() {
            for r in rules {
                let pair = (r.link_type.clone(), reverse(r.direction));
                if !pairs.contains(&pair) && frontier.iter().any(|(_, t)| r.accepts(t.as_deref())) {
                    pairs.push(pair);
                }
            }
        }
        let ids: Vec<&str> = frontier.iter().map(|(id, _)| id.as_str()).collect();
        let links = self.prove_links(view, &ids, &pairs, &mut back.eid_of, budget)?;
        // Candidate parents: (parent, link type, forward direction, the
        // child's type text).
        let mut cands: Vec<(String, &str, Direction, Option<&str>)> = Vec::new();
        for (id, ty) in &frontier {
            for (lt, rd) in &pairs {
                for link in &links[id][&(lt.clone(), *rd)] {
                    let u = other_end(link, *rd);
                    if !back.dist.contains_key(u) {
                        cands.push((u.clone(), lt, reverse(*rd), ty.as_deref()));
                    }
                }
            }
        }
        let mut fresh: Vec<String> = cands.iter().map(|c| c.0.clone()).collect();
        fresh.sort();
        fresh.dedup();
        let types = self.prove_types(view, &fresh, &mut back.eid_of, budget)?;
        let mut next: Vec<(String, Option<String>)> = Vec::new();
        for (u, lt, d, child) in cands {
            if back.dist.contains_key(&u) {
                continue;
            }
            let tu = text(types.get(&u));
            let steps = tu.and_then(|t| follow.get(t)).is_some_and(|rules| {
                rules
                    .iter()
                    .any(|r| r.link_type == lt && r.direction == d && r.accepts(child))
            });
            if steps {
                back.dist.insert(u.clone(), back.depth + 1);
                next.push((u, tu.map(str::to_owned)));
            }
        }
        back.depth += 1;
        if !next.is_empty() {
            budget.stats.depth_reached = budget.stats.depth_reached.max(back.depth);
        }
        back.frontier = next;
        Ok(())
    }

    /// The non-null `type` of each ID, in one batched read.
    fn prove_types(
        &self,
        view: &View,
        ids: &[String],
        eid_of: &mut HashMap<String, i64>,
        budget: &mut Budget,
    ) -> Result<HashMap<String, Value>> {
        let eids = self.prove_eids(ids.iter().map(String::as_str), eid_of)?;
        if eids.is_empty() {
            return Ok(HashMap::new());
        }
        let keys: Vec<i64> = eids.keys().copied().collect();
        budget.stats.value_reads += 1;
        Ok(self
            .resolve_types(view, &keys)?
            .into_iter()
            .map(|(eid, v)| (eids[&eid].clone(), v))
            .collect())
    }

    /// The live links of `ids` to typed entities, for `pairs`, in one batched
    /// read.
    fn prove_links(
        &self,
        view: &View,
        ids: &[&str],
        pairs: &[(String, Direction)],
        eid_of: &mut HashMap<String, i64>,
        budget: &mut Budget,
    ) -> Result<HashMap<String, crate::read::LinkGroups>> {
        let eids = self.prove_eids(ids.iter().copied(), eid_of)?;
        let groups: Vec<(&str, Direction)> = pairs.iter().map(|(t, d)| (t.as_str(), *d)).collect();
        budget.stats.link_reads += 1;
        self.links_with(view, ids, &eids, &groups, true, Some(eid_of))
    }

    /// The interned keys of the IDs that the store knows, `eid -> ID`,
    /// looking up only the ones not seen before.
    fn prove_eids<'s>(
        &self,
        ids: impl Iterator<Item = &'s str> + Clone,
        eid_of: &mut HashMap<String, i64>,
    ) -> Result<HashMap<i64, String>> {
        let unknown: Vec<&str> = ids.clone().filter(|id| !eid_of.contains_key(*id)).collect();
        if !unknown.is_empty() {
            for (eid, ext) in self.lookup(&unknown)? {
                eid_of.insert(ext, eid);
            }
        }
        Ok(ids
            .filter_map(|id| eid_of.get(id).map(|e| (*e, id.to_owned())))
            .collect())
    }
}

/// Phase 2: the walk's depth-first search from the start, cut down to the
/// entities that can reach the item.
struct Forward<'f> {
    item: &'f str,
    /// Distance to the item of each entity that can reach it (phase 1).
    dist: &'f HashMap<String, usize>,
    max_depth: Option<usize>,
    /// Length of the start path: depth is counted from its last entity.
    base: usize,
    descend: &'f mut dyn FnMut(&[String], &WalkNode) -> bool,
    include: &'f mut dyn FnMut(&[String], &WalkNode) -> bool,
    budget: &'f mut Budget,
}

impl Forward<'_> {
    /// Visit the node at the end of `path` as the walk does, and search below
    /// it. Returns the first path to the item that `include` accepts.
    fn search<D, I>(
        &mut self,
        ld: &mut Walker<'_, D, I>,
        path: &mut Vec<String>,
    ) -> Result<Option<Vec<String>>>
    where
        D: FnMut(&[String], &WalkNode) -> bool,
        I: FnMut(&[String], &WalkNode) -> bool,
    {
        if !self.budget.spend(1) {
            return Ok(None);
        }
        let depth = path.len() - self.base;
        self.budget.stats.depth_reached = self.budget.stats.depth_reached.max(depth);
        let id = path.last().expect("paths are not empty").clone();
        ld.ensure(std::slice::from_ref(&id), usize::MAX)?;
        let node = &ld.nodes[&id];
        if id == self.item {
            // Below the item, no path can reach it again (cycles).
            return Ok((self.include)(path, node).then(|| path.clone()));
        }
        if !(self.descend)(path, node) {
            return Ok(None);
        }
        let kids: Vec<String> = ld
            .targets_of(&id)
            .iter()
            .filter(|t| {
                !path.contains(t)
                    && self
                        .dist
                        .get(*t)
                        .is_some_and(|d| self.max_depth.is_none_or(|m| depth + 1 + d <= m))
            })
            .cloned()
            .collect();
        ld.ensure(&kids, usize::MAX)?;
        for kid in kids {
            path.push(kid);
            if let Some(found) = self.search(ld, path)? {
                return Ok(Some(found));
            }
            path.pop();
            if self.budget.out {
                return Ok(None);
            }
        }
        Ok(None)
    }
}

/// The link of each step of `steps` (the start's last entity, then the path
/// below it), as the walk followed it.
fn proof_links<D, I>(ld: &Walker<'_, D, I>, follow: &Follow, steps: &[String]) -> Vec<Link>
where
    D: FnMut(&[String], &WalkNode) -> bool,
    I: FnMut(&[String], &WalkNode) -> bool,
{
    let type_of = |id: &str| text(ld.nodes.get(id).and_then(|n| n.values.get(crate::TYPE_KEY)));
    steps
        .windows(2)
        .map(|w| {
            let (u, v) = (&w[0], &w[1]);
            let rules = type_of(u)
                .and_then(|t| follow.get(t))
                .expect("a step follows a rule");
            let groups = &ld.links[u];
            rules
                .iter()
                .filter(|r| r.accepts(type_of(v)))
                .find_map(|r| {
                    groups[&(r.link_type.clone(), r.direction)]
                        .iter()
                        .find(|l| other_end(l, r.direction) == v)
                })
                .expect("a step has a link")
                .clone()
        })
        .collect()
}
