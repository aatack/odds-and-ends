//! `walk`: a depth-first reading of the graph from a starting path, one page
//! at a time: the next path is the first child, else the next sibling of the
//! deepest ancestor that has one; a page stops after a number of steps and
//! returns the path to resume from.
//!
//! A cache miss loads the missing entities plus a few layers of the entities
//! they link to, in batches, into a cache that lives for one walk.

use std::collections::{BTreeMap, HashMap, HashSet};

use crate::error::{Error, Result};
use crate::read::LinkGroups;
use crate::store::Store;
use crate::types::{BranchId, Direction, Value, TYPE_KEY};
use crate::view::View;

/// Which links to follow out of an entity, keyed by that entity's `type`
/// value. The pairs are followed in the listed order. An entity whose type
/// has no entry (or that has no type) is visited, but nothing is followed.
pub type Follow = HashMap<String, Vec<(String, Direction)>>;

/// What a walk knows of one entity: its ID and all its (non-null) values.
#[derive(Clone, Debug, PartialEq)]
pub struct WalkNode {
    pub id: String,
    pub values: BTreeMap<String, Value>,
}

/// One emitted row. The path runs from the walk's first starting entity to
/// this one; it is the row's identity, because one entity can be reached by
/// many paths.
#[derive(Clone, Debug, PartialEq)]
pub struct WalkRow {
    pub path: Vec<String>,
    pub node: WalkNode,
}

/// The data that a walk loaded: the values of each entity, and the links
/// of the pairs that its type follows. Entities loaded by read-ahead are in
/// it too.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct WalkCache {
    pub nodes: HashMap<String, WalkNode>,
    /// Links per entity, grouped by the followed `(link type, direction)`
    /// pairs, with the same filter and order as [`Store::links`]. An entity
    /// whose type follows nothing has no entry.
    pub links: HashMap<String, LinkGroups>,
}

/// Options of [`Store::walk`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct WalkOptions {
    /// Most nodes to visit on this page. Default 1000.
    pub steps: usize,
    pub read_ahead: ReadAhead,
    /// Return the walk's cache in [`Walk::cache`]. Default false.
    pub return_cache: bool,
    /// `Some(set)` turns on the global visited-set dedupe: an entity in the
    /// set is not visited again through any path. Pass back
    /// [`Walk::visited`] with [`Walk::next`] to resume. Default `None`.
    /// Only a forward walk can use it.
    pub visited: Option<HashSet<String>>,
    /// Which way the walk reads the tree. Default [`WalkDirection::Forward`].
    pub direction: WalkDirection,
}

/// The order in which [`Store::walk`] visits nodes.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub enum WalkDirection {
    /// Depth-first: a node, then its children, then its next sibling.
    #[default]
    Forward,
    /// The forward order read in reverse: the previous sibling's last
    /// descendant, else the parent. A backward walk from the path of a row
    /// gives the rows above it, nearest first.
    Backward,
}

impl Default for WalkOptions {
    fn default() -> Self {
        Self {
            steps: 1000,
            read_ahead: ReadAhead::default(),
            return_cache: false,
            visited: None,
            direction: WalkDirection::Forward,
        }
    }
}

/// One page of a walk.
#[derive(Clone, Debug, PartialEq)]
pub struct Walk {
    /// The rows that `include` accepted, in depth-first order.
    pub rows: Vec<WalkRow>,
    /// The path of the next node to visit. Pass it back as the starting path
    /// to resume exactly here. `None` when the walk is exhausted.
    pub next: Option<Vec<String>>,
    /// The nodes visited on this page, emitted or not.
    pub steps: usize,
    /// Database load rounds on this page: one per read-ahead layer.
    pub loads: usize,
    /// The cache, when [`WalkOptions::return_cache`] is set.
    pub cache: Option<WalkCache>,
    /// The updated visited set, when [`WalkOptions::visited`] is set.
    pub visited: Option<HashSet<String>>,
}

/// How far a cache miss reads ahead.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ReadAhead {
    /// Layers of followed links to load below the missed entities.
    /// 0 loads only what was asked for.
    pub depth: usize,
    /// Most entities to load per read-ahead layer. Also capped by the steps
    /// left on the page.
    pub width: usize,
}

impl Default for ReadAhead {
    /// Two layers of 32 entities.
    fn default() -> Self {
        Self {
            depth: 2,
            width: 32,
        }
    }
}

/// The loading engine of a walk, shared with [`Store::prove`]'s `Walk`
/// strategy: the per-call cache of values and followed links, with the batched
/// read-ahead ([`Walker::ensure`]/`load`). It is `pub(crate)` so that `prove`
/// can drive the same read-ahead over a reversed follow map; `prove` reads
/// [`Walker::nodes`] and [`Walker::links`] after an [`Walker::ensure`], and
/// does its own traversal and predecessor-type check.
pub(crate) struct Walker<'a, D, I> {
    store: &'a Store,
    view: View,
    follow: &'a Follow,
    read_ahead: ReadAhead,
    /// Read ahead from the end of each child list, not its start: a backward
    /// walk goes to the last children first.
    backward: bool,
    /// Link reads use the archived filter of [`Store::links`] when set (always
    /// for `walk`; `!include_archived` for `prove`).
    typed_only: bool,
    descend: D,
    include: I,
    /// Values of each loaded entity (its `type` included).
    pub(crate) nodes: HashMap<String, WalkNode>,
    /// Followed links of each loaded entity, grouped by `(link type, dir)`.
    pub(crate) links: HashMap<String, LinkGroups>,
    visited: Option<HashSet<String>>,
    /// Followed link targets per entity, in order, before path filtering.
    targets: HashMap<String, Vec<String>>,
    /// Children of each prefix of the current path, so that each predicate
    /// runs once per path. A stack, not a map: memory stays O(depth).
    stack_ids: Vec<String>,
    stack_kids: Vec<Option<Vec<String>>>,
    /// Database load rounds so far: one per read-ahead layer.
    pub(crate) loads: usize,
    /// Interned keys that this walk has seen, by entity ID.
    eid_of: HashMap<String, i64>,
}

impl<D, I> Walker<'_, D, I>
where
    D: FnMut(&[String], &WalkNode) -> bool,
    I: FnMut(&[String], &WalkNode) -> bool,
{
    /// Make sure `ids` are cached. A miss loads them, then up to
    /// `read_ahead.depth` layers of their followed targets.
    pub(crate) fn ensure(&mut self, ids: &[String], remaining: usize) -> Result<()> {
        let mut layer: Vec<String> = ids
            .iter()
            .filter(|id| !self.nodes.contains_key(*id))
            .cloned()
            .collect::<HashSet<_>>()
            .into_iter()
            .collect();
        layer.sort();
        let cap = self.read_ahead.width.min(remaining.max(1));
        for depth in 0..=self.read_ahead.depth {
            if layer.is_empty() {
                break;
            }
            self.load(&layer)?;
            if depth == self.read_ahead.depth {
                break;
            }
            let mut next = Vec::new();
            let mut seen = HashSet::new();
            'outer: for id in &layer {
                let targets = &self.targets[id];
                let ordered: Box<dyn Iterator<Item = &String>> = if self.backward {
                    Box::new(targets.iter().rev())
                } else {
                    Box::new(targets.iter())
                };
                for t in ordered {
                    if next.len() >= cap {
                        break 'outer;
                    }
                    if !self.nodes.contains_key(t) && seen.insert(t.clone()) {
                        next.push(t.clone());
                    }
                }
            }
            layer = next;
        }
        Ok(())
    }

    /// Make sure the links of `id` for `pairs` are cached, even when `id`'s
    /// type selects no follow pairs. [`Store::prove`]'s `Walk` strategy needs
    /// this for an untyped item (its type keys nothing in the reversed follow
    /// map), so that the walk still reads its links. For a node whose type
    /// does follow these pairs, [`Walker::ensure`] already cached them and this
    /// is a no-op; otherwise it is one extra load round.
    pub(crate) fn ensure_links(&mut self, id: &str, pairs: &[(String, Direction)]) -> Result<()> {
        let owned = id.to_owned();
        self.ensure(std::slice::from_ref(&owned), usize::MAX)?;
        if self.links.contains_key(id) {
            return Ok(());
        }
        self.loads += 1;
        let eids = self.store.lookup(&[id])?;
        let groups: Vec<(&str, Direction)> = pairs.iter().map(|(t, d)| (t.as_str(), *d)).collect();
        let links = self.store.links_with(
            &self.view,
            &[id],
            &eids,
            &groups,
            self.typed_only,
            Some(&mut self.eid_of),
        )?;
        self.links.insert(owned, links[id].clone());
        Ok(())
    }

    /// One load round: values of `ids`, then their followed links, grouped by
    /// type so that each type reads only its own link pairs.
    fn load(&mut self, ids: &[String]) -> Result<()> {
        self.loads += 1;
        let refs: Vec<&str> = ids.iter().map(String::as_str).collect();
        // Interned keys: reuse the ones that earlier link reads showed; look
        // up only the rest, once for the whole batch.
        let mut eids: HashMap<i64, String> = HashMap::with_capacity(refs.len());
        let mut unknown: Vec<&str> = Vec::new();
        for id in &refs {
            match self.eid_of.get(*id) {
                Some(eid) => {
                    eids.insert(*eid, (*id).to_owned());
                }
                None => unknown.push(id),
            }
        }
        if !unknown.is_empty() {
            for (eid, ext) in self.store.lookup(&unknown)? {
                self.eid_of.insert(ext.clone(), eid);
                eids.insert(eid, ext);
            }
        }
        let mut values = self.store.values_with(&self.view, &refs, &eids)?;
        // Group by the followed pairs, not by type: types that follow the
        // same pairs share one batch read.
        let mut by_pairs: HashMap<&[(String, Direction)], Vec<&str>> = HashMap::new();
        for id in &refs {
            let values = values.remove(*id).unwrap_or_default();
            if let Some(pairs) = match values.get(TYPE_KEY) {
                Some(Value::Text(t)) => self.follow.get(t),
                _ => None,
            } {
                by_pairs.entry(pairs.as_slice()).or_default().push(id);
            }
            self.targets.insert((*id).to_owned(), Vec::new());
            self.nodes.insert(
                (*id).to_owned(),
                WalkNode {
                    id: (*id).to_owned(),
                    values,
                },
            );
        }
        for (pairs, members) in by_pairs {
            let groups: Vec<(&str, Direction)> =
                pairs.iter().map(|(t, d)| (t.as_str(), *d)).collect();
            let mine: HashMap<i64, String> = eids
                .iter()
                .filter(|(_, ext)| members.contains(&ext.as_str()))
                .map(|(k, v)| (*k, v.clone()))
                .collect();
            let links = self.store.links_with(
                &self.view,
                &members,
                &mine,
                &groups,
                self.typed_only,
                Some(&mut self.eid_of),
            )?;
            for id in members {
                self.links.insert(id.to_owned(), links[id].clone());
                let mut out = Vec::new();
                let mut seen = HashSet::new();
                let mine = &links[id];
                for (t, d) in pairs {
                    for link in &mine[&(t.clone(), *d)] {
                        let other = match d {
                            Direction::Outbound => &link.destination,
                            Direction::Inbound => &link.source,
                        };
                        if seen.insert(other.clone()) {
                            out.push(other.clone());
                        }
                    }
                }
                self.targets.insert(id.to_owned(), out);
            }
        }
        Ok(())
    }

    /// The children of the node at the end of `path`: none when `descend`
    /// says no; never an entity already on the path (cycles).
    fn children_of(&mut self, path: &[String], remaining: usize) -> Result<Vec<String>> {
        let common = self
            .stack_ids
            .iter()
            .zip(path)
            .take_while(|(a, b)| a == b)
            .count();
        self.stack_ids.truncate(common);
        self.stack_kids.truncate(common);
        self.stack_ids.extend_from_slice(&path[common..]);
        self.stack_kids.resize(path.len(), None);
        if let Some(c) = &self.stack_kids[path.len() - 1] {
            return Ok(c.clone());
        }
        let id = path.last().expect("paths are not empty").clone();
        self.ensure(std::slice::from_ref(&id), remaining)?;
        let node = &self.nodes[&id];
        let kids = if (self.descend)(path, node) {
            self.targets[&id]
                .iter()
                .filter(|t| !path.contains(t))
                .cloned()
                .collect()
        } else {
            Vec::new()
        };
        // Load the new frontier as one batch, capped by
        // the steps left, so that each child is not a miss of its own.
        let take = kids.len().min(self.read_ahead.width.min(remaining.max(1)));
        if self.read_ahead.depth > 0 {
            let batch = if self.backward {
                &kids[kids.len() - take..]
            } else {
                &kids[..take]
            };
            self.ensure(batch, remaining)?;
        }
        self.stack_kids[path.len() - 1] = Some(kids.clone());
        Ok(kids)
    }

    /// Whether the visited-set dedupe skips this entity.
    fn seen(&self, id: &str) -> bool {
        self.visited.as_ref().is_some_and(|v| v.contains(id))
    }

    /// The next path in a depth-first reading, or `None`. Never climbs above
    /// `path[0]`.
    fn step(&mut self, path: &[String], remaining: usize) -> Result<Option<Vec<String>>> {
        let below = self.children_of(path, remaining)?;
        if let Some(first) = below.iter().find(|c| !self.seen(c)) {
            let mut next = path.to_vec();
            next.push(first.clone());
            return Ok(Some(next));
        }
        for i in (1..path.len()).rev() {
            let parent = &path[..i];
            let siblings = self.children_of(parent, remaining)?;
            if let Some(at) = siblings.iter().position(|s| *s == path[i]) {
                if let Some(sibling) = siblings[at + 1..].iter().find(|c| !self.seen(c)) {
                    let mut next = parent.to_vec();
                    next.push(sibling.clone());
                    return Ok(Some(next));
                }
            }
        }
        Ok(None)
    }

    /// The previous path in a depth-first reading, or `None` at `path[0]`:
    /// the last descendant of the previous sibling, else the parent. A path
    /// whose last ID is no longer a child of its parent goes to the parent.
    fn step_back(&mut self, path: &[String], remaining: usize) -> Result<Option<Vec<String>>> {
        let Some((last, parent)) = path.split_last() else {
            return Ok(None);
        };
        if parent.is_empty() {
            return Ok(None);
        }
        let siblings = self.children_of(parent, remaining)?;
        let previous = match siblings.iter().position(|s| s == last) {
            Some(at) if at > 0 => siblings[at - 1].clone(),
            _ => return Ok(Some(parent.to_vec())),
        };
        let mut at = parent.to_vec();
        at.push(previous);
        loop {
            let below = self.children_of(&at, remaining)?;
            match below.last() {
                Some(child) => at.push(child.clone()),
                None => return Ok(Some(at)),
            }
        }
    }
}

impl Store {
    /// Walk the graph depth-first from `start`, for at most `options.steps`
    /// nodes.
    ///
    /// - `start` is a path: its last ID is the first node to visit, and the
    ///   earlier IDs are its ancestors. A path of one ID walks that entity's
    ///   subtree. A longer path (for example a returned `next`) resumes in the
    ///   middle of the tree; the walk then goes on to the later siblings of
    ///   the start and of its ancestors, but never above `start[0]`.
    /// - `follow` maps the visited entity's `type` value to the
    ///   `(link type, direction)` pairs to follow, in order. Children keep the
    ///   sort order of each link group. A child reached by two pairs is kept
    ///   once, at its first place.
    /// - Each visited node is a step, the start node too. `include` decides if
    ///   the node is emitted; a node that is not emitted still costs a step.
    ///   `descend` decides if the walk goes below the node. Both get the path
    ///   and the node's values (not its links), and run once per path. A
    ///   resumed walk also runs `descend` on the start's ancestors, to find
    ///   their later children.
    /// - Links follow the archived filter of [`Store::links`]: entities with a
    ///   null type, or no type, are not reached through links.
    /// - An entity already on the current path is not visited again (cycles).
    ///   With [`WalkOptions::visited`], an entity visited before (on any path,
    ///   on this page or an earlier one) is skipped: it is not visited, not a
    ///   step, not emitted and not expanded. The start node is never skipped.
    ///
    /// Returns the emitted rows and `next`: the path of the next node to
    /// visit, to pass back as `start` (with the returned `visited` set, if
    /// used). `None` when the walk is exhausted. With `steps == 0`, nothing
    /// is visited and `next` is `start`.
    ///
    /// With [`WalkDirection::Backward`], the walk visits the same nodes in the
    /// reverse order: from `start`, it goes to the last descendant of the
    /// previous sibling, else to the parent, and it ends after `start[0]`.
    /// The rows are in visiting order (nearest first). A backward walk cannot
    /// use a visited set (`Error::Invalid`).
    pub fn walk(
        &self,
        branch: BranchId,
        start: &[&str],
        follow: &Follow,
        descend: impl FnMut(&[String], &WalkNode) -> bool,
        include: impl FnMut(&[String], &WalkNode) -> bool,
        options: WalkOptions,
    ) -> Result<Walk> {
        let steps = options.steps;
        let backward = options.direction == WalkDirection::Backward;
        if backward && options.visited.is_some() {
            return Err(Error::Invalid(
                "a backward walk cannot use a visited set".into(),
            ));
        }
        let mut w = Walker {
            store: self,
            view: self.branch_view(branch)?,
            follow,
            read_ahead: options.read_ahead,
            backward,
            typed_only: true,
            descend,
            include,
            nodes: HashMap::new(),
            links: HashMap::new(),
            visited: options.visited,
            targets: HashMap::new(),
            stack_ids: Vec::new(),
            stack_kids: Vec::new(),
            loads: 0,
            eid_of: HashMap::new(),
        };
        let mut rows = Vec::new();
        let mut visited = 0;
        let mut path: Option<Vec<String>> = if start.is_empty() {
            None
        } else {
            Some(start.iter().map(|s| (*s).to_owned()).collect())
        };
        while let Some(p) = path {
            if visited == steps {
                path = Some(p);
                break;
            }
            let remaining = steps - visited;
            // The ancestors are needed to find later siblings.
            w.ensure(&p, remaining)?;
            visited += 1;
            let id = p.last().expect("not empty");
            if let Some(v) = &mut w.visited {
                v.insert(id.clone());
            }
            let node = &w.nodes[id];
            let emit = (w.include)(&p, node);
            let node = emit.then(|| node.clone());
            path = if backward {
                w.step_back(&p, remaining - 1)?
            } else {
                w.step(&p, remaining - 1)?
            };
            if let Some(node) = node {
                rows.push(WalkRow { path: p, node });
            }
        }
        let cache = options.return_cache.then(|| WalkCache {
            nodes: std::mem::take(&mut w.nodes),
            links: std::mem::take(&mut w.links),
        });
        Ok(Walk {
            rows,
            next: path,
            steps: visited,
            loads: w.loads,
            cache,
            visited: w.visited,
        })
    }

    /// A bare [`Walker`] for [`Store::prove`]'s `Walk` strategy: the walk
    /// cache and read-ahead over `follow`, with no descend/include predicate
    /// and no visited set. `prove` calls [`Walker::ensure`] and reads the
    /// loaded [`Walker::nodes`] and [`Walker::links`] itself.
    #[allow(clippy::type_complexity)]
    pub(crate) fn walk_loader<'a>(
        &'a self,
        view: View,
        follow: &'a Follow,
        read_ahead: ReadAhead,
        typed_only: bool,
    ) -> Walker<
        'a,
        impl FnMut(&[String], &WalkNode) -> bool,
        impl FnMut(&[String], &WalkNode) -> bool,
    > {
        Walker {
            store: self,
            view,
            follow,
            read_ahead,
            backward: false,
            typed_only,
            descend: |_: &[String], _: &WalkNode| true,
            include: |_: &[String], _: &WalkNode| true,
            nodes: HashMap::new(),
            links: HashMap::new(),
            visited: None,
            targets: HashMap::new(),
            stack_ids: Vec::new(),
            stack_kids: Vec::new(),
            loads: 0,
            eid_of: HashMap::new(),
        }
    }
}
