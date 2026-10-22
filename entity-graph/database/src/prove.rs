//! `prove`: find whether an entity is reachable from one of a set of roots by
//! following a type-keyed relationship map downward, and return one proof
//! (the path and the links that make it).
//!
//! It reuses the shared read path exactly like [`Store::walk`]: the view is
//! resolved once ([`Store::branch_view`]), every link read is one batched
//! [`Store::links_with`] over a whole frontier, and every type read is one
//! batched [`Store::resolve_types`]. No new SQL bypasses the roll-up, so prove
//! works unchanged at branch heads, on stacks of open and frozen parent
//! ranges, on frozen views, and after archive, undo, merge, pull and rebase.
//!
//! See `DESIGN.md` section 11.3 for the semantics.

use std::collections::{HashMap, HashSet};

use crate::error::Result;
use crate::store::Store;
use crate::types::{BranchId, Direction, Link, Value};
use crate::view::View;
use crate::walk::Follow;

/// Which search a [`Store::prove`] call uses. All three return the same
/// reachability answer; `Backward` and `Bidirectional` return a shortest
/// proof (fewest links), `Walk` returns the first proof its depth-first
/// search finds.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub enum ProveStrategy {
    /// Layered breadth-first search from the item over reversed relationships,
    /// stopping at the first layer that contains a root. Shortest.
    #[default]
    Backward,
    /// Breadth-first search from both ends, expanding the smaller frontier,
    /// stopping when the frontiers meet. Shortest.
    Bidirectional,
    /// Depth-first search from the item over reversed relationships. Returns
    /// the first proof found, not necessarily the shortest.
    Walk,
}

/// Options of [`Store::prove`].
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ProveOptions {
    /// The most links a proof may have. `None` is unlimited. A proof longer
    /// than this is not found; if none is within the limit the answer is
    /// [`ProveOutcome::NoPath`] (a definite "no path of length <= max_depth").
    pub max_depth: Option<usize>,
    /// The budget: the most entities the search may expand (read links for) in
    /// total. `None` is unlimited. If the budget runs out before the search
    /// can decide, the answer is [`ProveOutcome::Unknown`].
    pub max_nodes: Option<usize>,
    /// Include archived (null-typed) and never-typed entities. Default false:
    /// the same type filter as [`Store::links`], so archived entities and
    /// links to them cannot form a proof.
    pub include_archived: bool,
    pub strategy: ProveStrategy,
}

/// A proof that the item is reachable: the path from a root to the item, and
/// the links along it.
#[derive(Clone, Debug, PartialEq)]
pub struct Proof {
    /// `path[0]` is one of the roots; the last element is the item. A proof of
    /// depth 0 (the item is itself a root) is a single-element path.
    pub path: Vec<String>,
    /// `links[i]` connects `path[i]` and `path[i + 1]`, as stored
    /// (source/destination/type/sort/event). Empty for a depth-0 proof.
    pub links: Vec<Link>,
}

/// The result of a [`Store::prove`] call.
#[derive(Clone, Debug, PartialEq)]
pub enum ProveOutcome {
    /// A proof was found.
    Proved(Proof),
    /// The search finished and found no proof within the limits.
    NoPath,
    /// A limit ([`ProveOptions::max_nodes`]) stopped the search before it
    /// could decide.
    Unknown,
}

/// Counts of the work a [`Store::prove`] call did.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ProveStats {
    /// Entities expanded (their links read).
    pub nodes_visited: usize,
    /// Batch link-read rounds, each covering a whole frontier.
    pub link_reads: usize,
    /// Batch type-read rounds.
    pub value_reads: usize,
    /// SQL statements the call ran (one per layer/interval query).
    pub sql_queries: usize,
    /// The greatest distance (in links) the search reached from its start.
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

/// The distinct `(link type, direction)` pairs that occur anywhere in the
/// relationship map, sorted for determinism.
fn forward_pairs(relationships: &Follow) -> Vec<(String, Direction)> {
    let mut set: HashSet<(String, Direction)> = HashSet::new();
    for pairs in relationships.values() {
        for p in pairs {
            set.insert(p.clone());
        }
    }
    let mut out: Vec<(String, Direction)> = set.into_iter().collect();
    out.sort();
    out
}

/// Shared state of one prove call: the resolved view, the relationship map
/// and the running stats. Every read goes through here so the stats and the
/// interned-key cache are maintained in one place.
struct Ctx<'a> {
    store: &'a Store,
    view: View,
    relationships: &'a Follow,
    /// The distinct forward pairs, and the reversed pairs (for backward reads).
    forward: Vec<(String, Direction)>,
    reversed: Vec<(String, Direction)>,
    typed_only: bool,
    roots: HashSet<String>,
    /// Interned key per external ID, learned as reads see entities.
    eid_of: HashMap<String, i64>,
    stats: ProveStats,
}

impl<'a> Ctx<'a> {
    fn new(
        store: &'a Store,
        branch: BranchId,
        roots: &[&str],
        relationships: &'a Follow,
        include_archived: bool,
    ) -> Result<Self> {
        let forward = forward_pairs(relationships);
        let reversed: Vec<(String, Direction)> = forward
            .iter()
            .map(|(t, d)| (t.clone(), reverse(*d)))
            .collect();
        Ok(Self {
            view: store.branch_view(branch)?,
            store,
            relationships,
            forward,
            reversed,
            typed_only: !include_archived,
            roots: roots.iter().map(|r| (*r).to_owned()).collect(),
            eid_of: HashMap::new(),
            stats: ProveStats::default(),
        })
    }

    /// Intern the IDs we do not yet know, and return the interned keys of the
    /// ones the store knows, keyed `eid -> ext`.
    fn eids_of(&mut self, ids: &[&str]) -> Result<HashMap<i64, String>> {
        let unknown: Vec<&str> = ids
            .iter()
            .copied()
            .filter(|id| !self.eid_of.contains_key(*id))
            .collect();
        if !unknown.is_empty() {
            for (eid, ext) in self.store.lookup(&unknown)? {
                self.eid_of.insert(ext, eid);
            }
        }
        Ok(ids
            .iter()
            .filter_map(|id| self.eid_of.get(*id).map(|e| (*e, (*id).to_owned())))
            .collect())
    }

    /// Read the non-null type of each ID (one batched type read).
    fn types_of(&mut self, ids: &[&str]) -> Result<HashMap<String, Value>> {
        let eids = self.eids_of(ids)?;
        let keys: Vec<i64> = eids.keys().copied().collect();
        self.stats.value_reads += 1;
        let by_eid = self.store.resolve_types(&self.view, &keys)?;
        Ok(by_eid
            .into_iter()
            .filter_map(|(eid, v)| eids.get(&eid).map(|ext| (ext.clone(), v)))
            .collect())
    }

    /// Read the links of a whole `frontier` for `groups` in one batched round,
    /// learning the interned keys of the other ends. Counts the round and the
    /// entities expanded.
    fn read_links(
        &mut self,
        frontier: &[String],
        groups: &[(String, Direction)],
    ) -> Result<HashMap<String, crate::read::LinkGroups>> {
        let refs: Vec<&str> = frontier.iter().map(String::as_str).collect();
        let eids = self.eids_of(&refs)?;
        let groups: Vec<(&str, Direction)> = groups.iter().map(|(t, d)| (t.as_str(), *d)).collect();
        self.stats.link_reads += 1;
        self.stats.nodes_visited += frontier.len();
        self.store.links_with(
            &self.view,
            &refs,
            &eids,
            &groups,
            self.typed_only,
            Some(&mut self.eid_of),
        )
    }
}

/// The text of a `type` value, if it is text (the key used in the map).
fn type_text(types: &HashMap<String, Value>, id: &str) -> Option<String> {
    match types.get(id) {
        Some(Value::Text(t)) => Some(t.clone()),
        _ => None,
    }
}

/// The other end of `link` relative to `me`, given the direction it was read
/// in (`rd`): the end that is not `me`.
fn other_end(link: &Link, rd: Direction) -> String {
    match rd {
        Direction::Outbound => link.destination.clone(),
        Direction::Inbound => link.source.clone(),
    }
}

/// Rebuild a proof from a root down to the item by following the `pred` map,
/// which points each node at the next node closer to the item.
fn reconstruct_down(root: &str, item: &str, pred: &HashMap<String, (String, Link)>) -> Proof {
    let mut path = vec![root.to_owned()];
    let mut links = Vec::new();
    let mut cur = root.to_owned();
    while cur != item {
        let (lower, link) = &pred[&cur];
        path.push(lower.clone());
        links.push(link.clone());
        cur = lower.clone();
    }
    Proof { path, links }
}

/// Shared start logic: empty roots, the archived rule on the item, and the
/// depth-0 (item is itself a root) case. `Some` means the answer is already
/// decided.
fn preamble(ctx: &mut Ctx, item: &str) -> Result<Option<ProveOutcome>> {
    if ctx.roots.is_empty() {
        return Ok(Some(ProveOutcome::NoPath));
    }
    let types = ctx.types_of(&[item])?;
    let item_typed = types.contains_key(item);
    let item_known = ctx.eid_of.contains_key(item);
    // Default mode: every entity on a path, the item included, must be typed.
    if ctx.typed_only && !item_typed {
        return Ok(Some(ProveOutcome::NoPath));
    }
    if ctx.roots.contains(item) && (item_typed || item_known) {
        return Ok(Some(ProveOutcome::Proved(Proof {
            path: vec![item.to_owned()],
            links: vec![],
        })));
    }
    Ok(None)
}

/// Layered breadth-first search from the item over reversed relationships.
/// The first layer that reaches a root gives a shortest proof.
fn backward(
    ctx: &mut Ctx,
    item: &str,
    max_depth: Option<usize>,
    max_nodes: Option<usize>,
) -> Result<ProveOutcome> {
    let mut pred: HashMap<String, (String, Link)> = HashMap::new();
    let mut visited: HashSet<String> = HashSet::from([item.to_owned()]);
    let mut frontier: Vec<String> = vec![item.to_owned()];
    let mut depth = 0usize;
    let reversed = ctx.reversed.clone();
    loop {
        if frontier.is_empty() {
            return Ok(ProveOutcome::NoPath);
        }
        if max_depth.is_some_and(|m| depth >= m) {
            return Ok(ProveOutcome::NoPath);
        }
        if max_nodes.is_some_and(|m| ctx.stats.nodes_visited + frontier.len() > m) {
            return Ok(ProveOutcome::Unknown);
        }
        let links = ctx.read_links(&frontier, &reversed)?;
        // Candidate predecessors, each with the lower node, the link and the
        // original (downward) pair it was reached by.
        let mut cands: Vec<(String, String, Link, (String, Direction))> = Vec::new();
        for lower in &frontier {
            let groups = &links[lower];
            for (t, rd) in &reversed {
                let orig = (t.clone(), reverse(*rd));
                for link in &groups[&(t.clone(), *rd)] {
                    cands.push((
                        other_end(link, *rd),
                        lower.clone(),
                        link.clone(),
                        orig.clone(),
                    ));
                }
            }
        }
        let mut fresh: Vec<&str> = cands
            .iter()
            .map(|c| c.0.as_str())
            .filter(|u| !visited.contains(*u))
            .collect();
        fresh.sort();
        fresh.dedup();
        let types = ctx.types_of(&fresh)?;
        depth += 1;
        ctx.stats.depth_reached = ctx.stats.depth_reached.max(depth);
        cands.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        let mut next: Vec<String> = Vec::new();
        for (u, lower, link, orig) in cands {
            if visited.contains(&u) {
                continue;
            }
            let Some(ty) = type_text(&types, &u) else {
                continue;
            };
            if !ctx
                .relationships
                .get(&ty)
                .is_some_and(|ps| ps.contains(&orig))
            {
                continue;
            }
            visited.insert(u.clone());
            pred.insert(u.clone(), (lower, link));
            next.push(u);
        }
        if let Some(root) = next
            .iter()
            .filter(|u| ctx.roots.contains(*u))
            .min()
            .cloned()
        {
            return Ok(ProveOutcome::Proved(reconstruct_down(&root, item, &pred)));
        }
        frontier = next;
    }
}

/// One side of a bidirectional search.
#[derive(Default)]
struct Side {
    /// Node -> its shortest depth from this side's start.
    visited: HashMap<String, usize>,
    /// Node -> (the neighbour one step toward this side's start, the link).
    parent: HashMap<String, (String, Link)>,
    /// Node -> its type text (needed to expand a forward node).
    types: HashMap<String, String>,
    frontier: Vec<String>,
    depth: usize,
}

/// Expand the forward side one layer: from each frontier node `u` of type `T`,
/// step to the children given by `relationships[T]`. Returns the new nodes.
fn expand_fwd(ctx: &mut Ctx, fwd: &mut Side, max_depth: Option<usize>) -> Result<Vec<String>> {
    let frontier = std::mem::take(&mut fwd.frontier);
    if frontier.is_empty() {
        return Ok(vec![]);
    }
    let forward = ctx.forward.clone();
    let links = ctx.read_links(&frontier, &forward)?;
    let nd = fwd.depth + 1;
    let mut adds: Vec<(String, String, Link)> = Vec::new();
    for u in &frontier {
        let Some(ty) = fwd.types.get(u).cloned() else {
            continue;
        };
        let Some(pairs) = ctx.relationships.get(&ty) else {
            continue;
        };
        let groups = &links[u];
        for (lt, d) in pairs {
            for link in &groups[&(lt.clone(), *d)] {
                adds.push((other_end(link, *d), u.clone(), link.clone()));
            }
        }
    }
    adds.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
    let mut new_nodes = Vec::new();
    for (v, u, link) in adds {
        if fwd.visited.contains_key(&v) {
            continue;
        }
        fwd.visited.insert(v.clone(), nd);
        fwd.parent.insert(v.clone(), (u, link));
        new_nodes.push(v);
    }
    fwd.depth = nd;
    let refs: Vec<&str> = new_nodes.iter().map(String::as_str).collect();
    let vt = ctx.types_of(&refs)?;
    for n in &new_nodes {
        if let Some(t) = type_text(&vt, n) {
            fwd.types.insert(n.clone(), t);
        }
    }
    // A node at the depth cap is kept (it may be a meeting point) but is not
    // expanded again.
    if max_depth != Some(nd) {
        fwd.frontier = new_nodes.clone();
    }
    Ok(new_nodes)
}

/// Expand the backward side one layer, like [`backward`]'s inner step.
/// Returns the new nodes (valid predecessors).
fn expand_bwd(ctx: &mut Ctx, bwd: &mut Side, max_depth: Option<usize>) -> Result<Vec<String>> {
    let frontier = std::mem::take(&mut bwd.frontier);
    if frontier.is_empty() {
        return Ok(vec![]);
    }
    let reversed = ctx.reversed.clone();
    let links = ctx.read_links(&frontier, &reversed)?;
    let nd = bwd.depth + 1;
    let mut cands: Vec<(String, String, Link, (String, Direction))> = Vec::new();
    for lower in &frontier {
        let groups = &links[lower];
        for (t, rd) in &reversed {
            let orig = (t.clone(), reverse(*rd));
            for link in &groups[&(t.clone(), *rd)] {
                cands.push((
                    other_end(link, *rd),
                    lower.clone(),
                    link.clone(),
                    orig.clone(),
                ));
            }
        }
    }
    let mut fresh: Vec<&str> = cands
        .iter()
        .map(|c| c.0.as_str())
        .filter(|u| !bwd.visited.contains_key(*u))
        .collect();
    fresh.sort();
    fresh.dedup();
    let types = ctx.types_of(&fresh)?;
    cands.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
    let mut new_nodes = Vec::new();
    for (u, lower, link, orig) in cands {
        if bwd.visited.contains_key(&u) {
            continue;
        }
        let Some(ty) = type_text(&types, &u) else {
            continue;
        };
        if !ctx
            .relationships
            .get(&ty)
            .is_some_and(|ps| ps.contains(&orig))
        {
            continue;
        }
        bwd.visited.insert(u.clone(), nd);
        bwd.parent.insert(u.clone(), (lower, link));
        new_nodes.push(u);
    }
    bwd.depth = nd;
    if max_depth != Some(nd) {
        bwd.frontier = new_nodes.clone();
    }
    Ok(new_nodes)
}

/// Rebuild the proof through a meeting node `m`: up the forward parents to a
/// root, then down the backward parents to the item.
fn reconstruct_bi(m: &str, fwd: &Side, bwd: &Side, item: &str) -> Proof {
    let mut up_path = vec![m.to_owned()];
    let mut up_links = Vec::new();
    let mut cur = m.to_owned();
    while let Some((p, link)) = fwd.parent.get(&cur) {
        up_links.push(link.clone());
        up_path.push(p.clone());
        cur = p.clone();
    }
    up_path.reverse();
    up_links.reverse();
    let mut cur = m.to_owned();
    while cur != item {
        let (lower, link) = &bwd.parent[&cur];
        up_path.push(lower.clone());
        up_links.push(link.clone());
        cur = lower.clone();
    }
    Proof {
        path: up_path,
        links: up_links,
    }
}

/// Bidirectional search: alternate expanding the shallower side (balanced
/// depths), meeting in the middle. Returns a shortest proof.
fn bidirectional(
    ctx: &mut Ctx,
    item: &str,
    max_depth: Option<usize>,
    max_nodes: Option<usize>,
) -> Result<ProveOutcome> {
    let mut fwd = Side::default();
    let roots: Vec<String> = {
        let mut r: Vec<String> = ctx.roots.iter().cloned().collect();
        r.sort();
        r
    };
    let root_refs: Vec<&str> = roots.iter().map(String::as_str).collect();
    let rtypes = ctx.types_of(&root_refs)?;
    for r in &roots {
        if let Some(ty) = type_text(&rtypes, r) {
            fwd.visited.insert(r.clone(), 0);
            fwd.types.insert(r.clone(), ty);
            fwd.frontier.push(r.clone());
        }
    }
    let mut bwd = Side::default();
    bwd.visited.insert(item.to_owned(), 0);
    bwd.frontier.push(item.to_owned());

    let mut best: Option<(usize, String)> = None;
    let within = |len: usize| max_depth.is_none_or(|m| len <= m);
    loop {
        if let Some((mu, _)) = &best {
            if *mu <= fwd.depth + bwd.depth {
                break;
            }
        }
        let fwd_done = fwd.frontier.is_empty();
        let bwd_done = bwd.frontier.is_empty();
        if fwd_done && bwd_done {
            break;
        }
        // Expand the shallower side (balanced depths); on a tie, the smaller
        // frontier. Skip an exhausted side.
        let do_fwd = if fwd_done {
            false
        } else if bwd_done {
            true
        } else if fwd.depth != bwd.depth {
            fwd.depth < bwd.depth
        } else {
            fwd.frontier.len() <= bwd.frontier.len()
        };
        let frontier_len = if do_fwd {
            fwd.frontier.len()
        } else {
            bwd.frontier.len()
        };
        if max_nodes.is_some_and(|m| ctx.stats.nodes_visited + frontier_len > m) {
            return Ok(ProveOutcome::Unknown);
        }
        let new_nodes = if do_fwd {
            expand_fwd(ctx, &mut fwd, max_depth)?
        } else {
            expand_bwd(ctx, &mut bwd, max_depth)?
        };
        let (near, far) = if do_fwd { (&fwd, &bwd) } else { (&bwd, &fwd) };
        ctx.stats.depth_reached = ctx.stats.depth_reached.max(near.depth);
        for n in &new_nodes {
            if let Some(&od) = far.visited.get(n) {
                let len = near.visited[n] + od;
                if within(len) && best.as_ref().is_none_or(|(b, _)| len < *b) {
                    best = Some((len, n.clone()));
                }
            }
        }
    }
    match best {
        Some((_, m)) => Ok(ProveOutcome::Proved(reconstruct_bi(&m, &fwd, &bwd, item))),
        None => Ok(ProveOutcome::NoPath),
    }
}

/// Depth-first search from the item over reversed relationships, returning the
/// first proof found. The predecessor-type check cannot live in a reversed
/// follow map (which is keyed by the lower node's type), so it is done in this
/// loader: for each candidate `u` reached from the current node by a reversed
/// pair, we read `u`'s type and accept the step only if `relationships[type(u)]`
/// contains the matching downward pair. `seen` memoises the shallowest depth a
/// node was expanded at, so the search still terminates and stays complete for
/// reachability within `max_depth`.
#[allow(clippy::too_many_arguments)]
fn walk_dfs(
    ctx: &mut Ctx,
    reversed: &[(String, Direction)],
    seen: &mut HashMap<String, usize>,
    budget: &mut bool,
    path: &mut Vec<String>,
    links: &mut Vec<Link>,
    depth: usize,
    max_depth: Option<usize>,
    max_nodes: Option<usize>,
) -> Result<Option<Proof>> {
    let cur = path.last().expect("path is never empty").clone();
    if depth > 0 && ctx.roots.contains(&cur) {
        let mut p = path.clone();
        p.reverse();
        let mut l = links.clone();
        l.reverse();
        return Ok(Some(Proof { path: p, links: l }));
    }
    if max_depth.is_some_and(|m| depth >= m) {
        return Ok(None);
    }
    match seen.get(&cur) {
        Some(&sd) if sd <= depth => return Ok(None),
        _ => {}
    }
    if max_nodes.is_some_and(|m| ctx.stats.nodes_visited >= m) {
        *budget = true;
        return Ok(None);
    }
    seen.insert(cur.clone(), depth);
    let read = ctx.read_links(std::slice::from_ref(&cur), reversed)?;
    ctx.stats.depth_reached = ctx.stats.depth_reached.max(depth + 1);
    let mut cands: Vec<(String, Link, (String, Direction))> = Vec::new();
    {
        let groups = &read[&cur];
        for (t, rd) in reversed {
            let orig = (t.clone(), reverse(*rd));
            for link in &groups[&(t.clone(), *rd)] {
                cands.push((other_end(link, *rd), link.clone(), orig.clone()));
            }
        }
    }
    let mut fresh: Vec<&str> = cands
        .iter()
        .map(|c| c.0.as_str())
        .filter(|u| !path.contains(&(*u).to_owned()))
        .collect();
    fresh.sort();
    fresh.dedup();
    let types = ctx.types_of(&fresh)?;
    cands.sort_by(|a, b| a.0.cmp(&b.0));
    for (u, link, orig) in cands {
        if path.contains(&u) {
            continue;
        }
        let Some(ty) = type_text(&types, &u) else {
            continue;
        };
        if !ctx
            .relationships
            .get(&ty)
            .is_some_and(|ps| ps.contains(&orig))
        {
            continue;
        }
        path.push(u.clone());
        links.push(link);
        if let Some(p) = walk_dfs(
            ctx,
            reversed,
            seen,
            budget,
            path,
            links,
            depth + 1,
            max_depth,
            max_nodes,
        )? {
            return Ok(Some(p));
        }
        path.pop();
        links.pop();
    }
    Ok(None)
}

/// Depth-first [`ProveStrategy::Walk`]: first proof found, not necessarily
/// shortest.
fn walk_prove(
    ctx: &mut Ctx,
    item: &str,
    max_depth: Option<usize>,
    max_nodes: Option<usize>,
) -> Result<ProveOutcome> {
    let reversed = ctx.reversed.clone();
    let mut seen: HashMap<String, usize> = HashMap::new();
    let mut budget = false;
    let mut path = vec![item.to_owned()];
    let mut links = Vec::new();
    let proof = walk_dfs(
        ctx,
        &reversed,
        &mut seen,
        &mut budget,
        &mut path,
        &mut links,
        0,
        max_depth,
        max_nodes,
    )?;
    Ok(match proof {
        Some(p) => ProveOutcome::Proved(p),
        None if budget => ProveOutcome::Unknown,
        None => ProveOutcome::NoPath,
    })
}

impl Store {
    /// Prove that `item` is reachable from one of `roots` by following
    /// `relationships` downward, and return one proof.
    ///
    /// `relationships` has the shape of a [`walk`](Store::walk) follow map,
    /// read downward: an entity `u` of type `T` may step to `v` by each
    /// `(link type, direction)` in `relationships[T]` (`Outbound`: a link
    /// `u -> v`; `Inbound`: a link `v -> u`). A proof is a sequence
    /// `root = p0, p1, ..., pk = item` where each `p(i) -> p(i+1)` is such a
    /// step from `p(i)`'s type. The map key is the type of the upper entity.
    ///
    /// Semantics are in `DESIGN.md` section 11.3. In brief: every entity on
    /// the path needs a live non-null type unless
    /// [`ProveOptions::include_archived`]; a removed link does not count;
    /// `item == root` is a depth-0 proof; unknown IDs and empty roots give
    /// [`ProveOutcome::NoPath`]; [`ProveStrategy::Backward`] and
    /// [`ProveStrategy::Bidirectional`] return a shortest proof.
    pub fn prove(
        &self,
        branch: BranchId,
        item: &str,
        roots: &[&str],
        relationships: &Follow,
        options: ProveOptions,
    ) -> Result<Prove> {
        let sql0 = self.sql_queries();
        let mut ctx = Ctx::new(self, branch, roots, relationships, options.include_archived)?;
        let outcome = match preamble(&mut ctx, item)? {
            Some(decided) => decided,
            None => match options.strategy {
                ProveStrategy::Backward => {
                    backward(&mut ctx, item, options.max_depth, options.max_nodes)?
                }
                ProveStrategy::Bidirectional => {
                    bidirectional(&mut ctx, item, options.max_depth, options.max_nodes)?
                }
                ProveStrategy::Walk => {
                    walk_prove(&mut ctx, item, options.max_depth, options.max_nodes)?
                }
            },
        };
        ctx.stats.sql_queries = (self.sql_queries() - sql0) as usize;
        Ok(Prove {
            outcome,
            stats: ctx.stats,
        })
    }
}
