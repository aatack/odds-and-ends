//! Named unit tests for [`Store::prove`]: it proves that a walk emits an item.

use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

use crate::*;

const T0: Timestamp = 1_700_000_000_000;

struct Fix {
    store: Store,
    clock: Arc<AtomicI64>,
    main: BranchId,
}

impl Fix {
    fn new() -> Self {
        let mut store = Store::open_in_memory().unwrap();
        let clock = Arc::new(AtomicI64::new(T0));
        let c = clock.clone();
        store.set_clock(move || c.load(Ordering::SeqCst));
        let main = store.create_branch("main", &[]).unwrap();
        Self { store, clock, main }
    }

    fn write(&mut self, branch: BranchId, ops: Vec<Op>) -> WriteResult {
        self.clock.fetch_add(1, Ordering::SeqCst);
        self.store.write(WriteBatch::new(branch, "a", ops)).unwrap()
    }

    /// Set the type of an entity.
    fn typ(&mut self, branch: BranchId, entity: &str, ty: &str) {
        self.write(branch, vec![Op::set(entity, TYPE_KEY, ty)]);
    }

    /// Archive an entity (clear its type).
    fn archive(&mut self, branch: BranchId, entity: &str) {
        self.write(branch, vec![Op::clear(entity, TYPE_KEY)]);
    }

    /// A live link `source -> destination` of `link_type`.
    fn link(&mut self, branch: BranchId, source: &str, destination: &str, link_type: &str) {
        self.write(
            branch,
            vec![Op::link(
                source,
                destination,
                link_type,
                SortOrder::Timestamp,
            )],
        );
    }
}

/// A follow map from `(parent type, link type, direction, child type)`;
/// a child type of `None` is the wildcard.
fn fol(rules: &[(&str, &str, Direction, Option<&str>)]) -> Follow {
    let mut m: Follow = HashMap::new();
    for (p, lt, d, c) in rules {
        m.entry((*p).to_owned()).or_default().push(FollowRule {
            link_type: (*lt).to_owned(),
            direction: *d,
            child_type: c.map(str::to_owned),
        });
    }
    m
}

use Direction::{Inbound, Outbound};

/// Prove with predicates that accept everything.
fn prove(f: &Fix, b: BranchId, start: &[&str], item: &str, follow: &Follow) -> ProveOutcome {
    prove_opts(f, b, start, item, follow, ProveOptions::default())
}

fn prove_opts(
    f: &Fix,
    b: BranchId,
    start: &[&str],
    item: &str,
    follow: &Follow,
    options: ProveOptions,
) -> ProveOutcome {
    let o = f
        .store
        .prove(b, start, item, follow, |_, _| true, |_, _| true, options)
        .unwrap()
        .outcome;
    if let ProveOutcome::Proved(p) = &o {
        assert_valid(f, b, start.len(), p);
    }
    o
}

/// The paths of a full walk that end at `item`.
fn walk_hits(
    f: &Fix,
    b: BranchId,
    start: &[&str],
    item: &str,
    follow: &Follow,
    descend: impl FnMut(&[String], &WalkNode) -> bool,
    include: impl FnMut(&[String], &WalkNode) -> bool,
) -> Vec<Vec<String>> {
    let w = f
        .store
        .walk(
            b,
            start,
            follow,
            descend,
            include,
            WalkOptions {
                steps: usize::MAX,
                ..Default::default()
            },
        )
        .unwrap();
    w.paths
        .into_iter()
        .filter(|p| p.last().map(String::as_str) == Some(item))
        .collect()
}

/// Prove with predicates, and check the answer against a full walk: proved
/// exactly when the walk emits the item, on a path that the walk emits.
fn prove_vs_walk<D, I>(
    f: &Fix,
    b: BranchId,
    start: &[&str],
    item: &str,
    follow: &Follow,
    descend: D,
    include: I,
) -> ProveOutcome
where
    D: FnMut(&[String], &WalkNode) -> bool + Clone,
    I: FnMut(&[String], &WalkNode) -> bool + Clone,
{
    let hits = walk_hits(f, b, start, item, follow, descend.clone(), include.clone());
    let o = f
        .store
        .prove(
            b,
            start,
            item,
            follow,
            descend,
            include,
            ProveOptions::default(),
        )
        .unwrap()
        .outcome;
    match &o {
        ProveOutcome::Proved(p) => {
            assert!(
                hits.contains(&p.path),
                "proof {:?} is not a walk hit {hits:?}",
                p.path
            );
            assert_valid(f, b, start.len(), p);
        }
        other => assert!(hits.is_empty(), "{other:?} but the walk emits {hits:?}"),
    }
    o
}

/// Each returned link is live and joins the consecutive path entries below
/// the start.
fn assert_valid(f: &Fix, b: BranchId, start_len: usize, p: &Proof) {
    let steps = &p.path[start_len - 1..];
    assert_eq!(p.links.len(), steps.len() - 1, "one link per step");
    for (w, link) in steps.windows(2).zip(&p.links) {
        let (u, v) = (&w[0], &w[1]);
        let d = if &link.source == u && &link.destination == v {
            Outbound
        } else if &link.source == v && &link.destination == u {
            Inbound
        } else {
            panic!("link {link:?} does not join {u} and {v}");
        };
        let live = f
            .store
            .links(b, &[u.as_str()], &[(link.link_type.as_str(), d)])
            .unwrap();
        assert!(live[u.as_str()][&(link.link_type.clone(), d)].contains(link));
    }
}

fn path_of(o: &ProveOutcome) -> Vec<&str> {
    match o {
        ProveOutcome::Proved(p) => p.path.iter().map(String::as_str).collect(),
        other => panic!("expected a proof, got {other:?}"),
    }
}

/// org R -> folder M -> doc I, all by `child`.
fn chain(f: &mut Fix) -> Follow {
    let b = f.main;
    f.typ(b, "R", "org");
    f.typ(b, "M", "folder");
    f.typ(b, "I", "doc");
    f.link(b, "R", "M", "child");
    f.link(b, "M", "I", "child");
    fol(&[
        ("org", "child", Outbound, Some("folder")),
        ("folder", "child", Outbound, Some("doc")),
    ])
}

// --- the walk's rules -------------------------------------------------------

#[test]
fn direct_child_and_multi_hop_chain() {
    let mut f = Fix::new();
    let r = chain(&mut f);
    let b = f.main;
    assert_eq!(path_of(&prove(&f, b, &["R"], "M", &r)), ["R", "M"]);
    let o = prove(&f, b, &["R"], "I", &r);
    assert_eq!(path_of(&o), ["R", "M", "I"]);
    let ProveOutcome::Proved(p) = o else {
        unreachable!()
    };
    assert_eq!(p.links.len(), 2);
    // Links point down only: nothing proves R from I.
    assert_eq!(prove(&f, b, &["I"], "R", &r), ProveOutcome::NoPath);
}

#[test]
fn the_child_type_must_match_unless_it_is_the_wildcard() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "S", "system");
    f.typ(b, "Q", "requirement");
    f.typ(b, "T", "test");
    f.link(b, "S", "Q", "contains");
    f.link(b, "S", "T", "contains");
    f.link(b, "S", "T", "verified by");
    let r = fol(&[
        ("system", "contains", Outbound, Some("requirement")),
        ("system", "verified by", Outbound, Some("test")),
    ]);
    assert_eq!(path_of(&prove(&f, b, &["S"], "Q", &r)), ["S", "Q"]);
    // T is reached by `verified by`, not by `contains` (wrong child type).
    let ProveOutcome::Proved(p) = prove(&f, b, &["S"], "T", &r) else {
        panic!("T is verified by S")
    };
    assert_eq!(p.links[0].link_type, "verified by");
    let only_contains = fol(&[("system", "contains", Outbound, Some("requirement"))]);
    assert_eq!(
        prove(&f, b, &["S"], "T", &only_contains),
        ProveOutcome::NoPath
    );
    let any = fol(&[("system", "contains", Outbound, None)]);
    assert_eq!(path_of(&prove(&f, b, &["S"], "T", &any)), ["S", "T"]);
}

#[test]
fn the_parent_type_must_have_the_rule() {
    let mut f = Fix::new();
    let r = chain(&mut f);
    let b = f.main;
    f.typ(b, "M", "group"); // groups follow nothing
    assert_eq!(prove(&f, b, &["R"], "I", &r), ProveOutcome::NoPath);
}

#[test]
fn inbound_rules_step_against_the_link() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "G", "group");
    f.typ(b, "U", "user");
    f.link(b, "U", "G", "member"); // U -> G
    let r = fol(&[("group", "member", Inbound, Some("user"))]);
    assert_eq!(path_of(&prove(&f, b, &["G"], "U", &r)), ["G", "U"]);
}

#[test]
fn the_item_may_be_the_start_if_include_accepts_it() {
    let mut f = Fix::new();
    let r = chain(&mut f);
    let b = f.main;
    let o = prove(&f, b, &["R"], "R", &r);
    assert_eq!(path_of(&o), ["R"]);
    // An unknown, untyped start is still visited by the walk.
    assert_eq!(path_of(&prove(&f, b, &["ghost"], "ghost", &r)), ["ghost"]);
    let none = prove_vs_walk(&f, b, &["R"], "R", &r, |_, _| true, |_, _| false);
    assert_eq!(none, ProveOutcome::NoPath);
}

#[test]
fn unknown_and_disconnected_ids_are_no_path() {
    let mut f = Fix::new();
    let r = chain(&mut f);
    let b = f.main;
    f.typ(b, "X", "doc");
    assert_eq!(prove(&f, b, &["R"], "X", &r), ProveOutcome::NoPath);
    assert_eq!(prove(&f, b, &["R"], "ghost", &r), ProveOutcome::NoPath);
    assert_eq!(prove(&f, b, &["ghost"], "I", &r), ProveOutcome::NoPath);
    assert_eq!(prove(&f, b, &[], "I", &r), ProveOutcome::NoPath);
}

#[test]
fn a_longer_start_path_counts_its_ancestors() {
    let mut f = Fix::new();
    let r = chain(&mut f);
    let b = f.main;
    assert_eq!(
        path_of(&prove(&f, b, &["R", "M"], "I", &r)),
        ["R", "M", "I"]
    );
    // The walk never goes above the start, and never back onto the path.
    assert_eq!(prove(&f, b, &["R", "M"], "R", &r), ProveOutcome::NoPath);
    // `descend` sees the whole path, ancestors included.
    let o = prove_vs_walk(&f, b, &["R", "M"], "I", &r, |p, _| p.len() < 2, |_, _| true);
    assert_eq!(o, ProveOutcome::NoPath);
}

// --- predicates -------------------------------------------------------------

/// R -> M -> I and R -> N -> I, where M and N are folders.
fn diamond(f: &mut Fix) -> Follow {
    let r = chain(f);
    let b = f.main;
    f.typ(b, "N", "folder");
    f.link(b, "R", "N", "child");
    f.link(b, "N", "I", "child");
    r
}

#[test]
fn descend_blocks_one_path_and_another_still_proves() {
    let mut f = Fix::new();
    let r = diamond(&mut f);
    let b = f.main;
    assert_eq!(path_of(&prove(&f, b, &["R"], "I", &r)), ["R", "M", "I"]);
    let not_m = |_: &[String], n: &WalkNode| n.id != "M";
    let o = prove_vs_walk(&f, b, &["R"], "I", &r, not_m, |_, _| true);
    assert_eq!(path_of(&o), ["R", "N", "I"]);
    let neither = |_: &[String], n: &WalkNode| n.id != "M" && n.id != "N";
    let o = prove_vs_walk(&f, b, &["R"], "I", &r, neither, |_, _| true);
    assert_eq!(o, ProveOutcome::NoPath);
    // `descend` does not apply to the item itself.
    let not_i = |_: &[String], n: &WalkNode| n.id != "I";
    assert!(matches!(
        prove_vs_walk(&f, b, &["R"], "I", &r, not_i, |_, _| true),
        ProveOutcome::Proved(_)
    ));
}

#[test]
fn include_is_tested_on_the_whole_path() {
    let mut f = Fix::new();
    let r = diamond(&mut f);
    let b = f.main;
    let via_n = |p: &[String], _: &WalkNode| p.iter().any(|x| x == "N");
    let o = prove_vs_walk(&f, b, &["R"], "I", &r, |_, _| true, via_n);
    assert_eq!(path_of(&o), ["R", "N", "I"]);
    // A filter on values, as the server's filters are.
    let titled = |_: &[String], n: &WalkNode| n.values.contains_key("title");
    assert_eq!(
        prove_vs_walk(&f, b, &["R"], "I", &r, |_, _| true, titled),
        ProveOutcome::NoPath
    );
    f.write(b, vec![Op::set("I", "title", "doc")]);
    assert!(matches!(
        prove_vs_walk(&f, b, &["R"], "I", &r, |_, _| true, titled),
        ProveOutcome::Proved(_)
    ));
}

#[test]
fn cycles_do_not_trap_the_search() {
    let mut f = Fix::new();
    let b = f.main;
    for x in ["A", "B", "C"] {
        f.typ(b, x, "folder");
    }
    f.typ(b, "D", "doc");
    f.link(b, "A", "B", "child");
    f.link(b, "B", "C", "child");
    f.link(b, "C", "A", "child");
    f.link(b, "C", "D", "child");
    let r = fol(&[("folder", "child", Outbound, None)]);
    assert_eq!(
        path_of(&prove(&f, b, &["A"], "D", &r)),
        ["A", "B", "C", "D"]
    );
    assert_eq!(path_of(&prove(&f, b, &["B"], "A", &r)), ["B", "C", "A"]);
}

// --- limits -----------------------------------------------------------------

#[test]
fn max_depth_cuts_long_paths() {
    let mut f = Fix::new();
    let r = diamond(&mut f);
    let b = f.main;
    let depth = |d| ProveOptions {
        max_depth: Some(d),
        ..Default::default()
    };
    assert_eq!(
        prove_opts(&f, b, &["R"], "I", &r, depth(1)),
        ProveOutcome::NoPath
    );
    assert!(matches!(
        prove_opts(&f, b, &["R"], "I", &r, depth(2)),
        ProveOutcome::Proved(_)
    ));
    // A short path that `include` refuses does not hide a longer one, and a
    // depth limit does not let the longer one through.
    f.typ(b, "L", "folder");
    f.link(b, "R", "L", "child");
    f.link(b, "L", "M", "child");
    let mut r = r;
    r.get_mut("folder")
        .unwrap()
        .push(FollowRule::new("child", Outbound, "folder"));
    let long_only = |p: &[String], _: &WalkNode| p.len() == 4;
    let o = prove_vs_walk(&f, b, &["R"], "I", &r, |_, _| true, long_only);
    assert_eq!(path_of(&o), ["R", "L", "M", "I"]);
    let o = f
        .store
        .prove(b, &["R"], "I", &r, |_, _| true, long_only, depth(2))
        .unwrap()
        .outcome;
    assert_eq!(o, ProveOutcome::NoPath);
}

#[test]
fn max_nodes_budget_gives_unknown() {
    let mut f = Fix::new();
    let b = f.main;
    // A chain of 20 folders to a doc.
    for i in 0..20 {
        f.typ(b, &format!("F{i}"), "folder");
        if i > 0 {
            f.link(b, &format!("F{}", i - 1), &format!("F{i}"), "child");
        }
    }
    f.typ(b, "D", "doc");
    f.link(b, "F19", "D", "child");
    let r = fol(&[("folder", "child", Outbound, None)]);
    let budget = |n| ProveOptions {
        max_nodes: Some(n),
        ..Default::default()
    };
    assert_eq!(
        prove_opts(&f, b, &["F0"], "D", &r, budget(5)),
        ProveOutcome::Unknown
    );
    assert!(matches!(
        prove_opts(&f, b, &["F0"], "D", &r, budget(1000)),
        ProveOutcome::Proved(_)
    ));
    // Unknown is not NoPath: a disconnected item with a big budget is NoPath.
    f.typ(b, "X", "doc");
    assert_eq!(
        prove_opts(&f, b, &["F0"], "X", &r, budget(1000)),
        ProveOutcome::NoPath
    );
}

#[test]
fn stats_are_populated() {
    let mut f = Fix::new();
    let r = chain(&mut f);
    let b = f.main;
    let p = f
        .store
        .prove(
            b,
            &["R"],
            "I",
            &r,
            |_, _| true,
            |_, _| true,
            ProveOptions::default(),
        )
        .unwrap();
    assert!(matches!(p.outcome, ProveOutcome::Proved(_)));
    let s = p.stats;
    assert!(s.nodes_visited >= 3, "{s:?}");
    assert!(
        s.link_reads >= 2 && s.value_reads >= 2 && s.sql_queries >= 4,
        "{s:?}"
    );
    assert_eq!(s.depth_reached, 2);
}

// --- archive, undo, merge, views --------------------------------------------

#[test]
fn archived_entities_block_then_restore() {
    let mut f = Fix::new();
    let r = chain(&mut f);
    let b = f.main;
    f.archive(b, "M");
    assert_eq!(prove(&f, b, &["R"], "I", &r), ProveOutcome::NoPath);
    f.typ(b, "M", "folder");
    assert!(matches!(
        prove(&f, b, &["R"], "I", &r),
        ProveOutcome::Proved(_)
    ));
    f.archive(b, "I");
    assert_eq!(prove(&f, b, &["R"], "I", &r), ProveOutcome::NoPath);
    f.typ(b, "I", "doc");
    // An archived start is visited, but its type follows nothing.
    f.archive(b, "R");
    assert_eq!(prove(&f, b, &["R"], "I", &r), ProveOutcome::NoPath);
}

#[test]
fn undo_of_link_batch_removes_proof() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "R", "org");
    f.typ(b, "I", "doc");
    f.link(b, "R", "I", "child");
    let r = fol(&[("org", "child", Outbound, Some("doc"))]);
    assert!(matches!(
        prove(&f, b, &["R"], "I", &r),
        ProveOutcome::Proved(_)
    ));
    f.store.undo(b, "a").unwrap();
    assert_eq!(prove(&f, b, &["R"], "I", &r), ProveOutcome::NoPath);
}

#[test]
fn merge_brings_in_the_link() {
    let mut f = Fix::new();
    let main = f.main;
    f.typ(main, "R", "org");
    f.typ(main, "M", "folder");
    f.link(main, "R", "M", "child");
    let r = fol(&[
        ("org", "child", Outbound, None),
        ("folder", "child", Outbound, None),
    ]);
    let agent = f.store.fork(main, "agent").unwrap();
    f.typ(agent, "I", "doc");
    f.link(agent, "M", "I", "child");
    assert_eq!(prove(&f, main, &["R"], "I", &r), ProveOutcome::NoPath);
    f.store.merge(main, agent).unwrap();
    assert_eq!(path_of(&prove(&f, main, &["R"], "I", &r)), ["R", "M", "I"]);
}

#[test]
fn past_link_absent_at_head_present_on_frozen_view_and_fork() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "O", "org");
    f.typ(b, "D", "doc");
    f.link(b, "O", "D", "child");
    let with_link = f.store.branch(b).unwrap().head;
    let snap = f.store.fork(b, "snap").unwrap();
    let frozen = f
        .store
        .create_branch("frozen", &[ParentRef::new(b, SeqRange::up_to(with_link))])
        .unwrap();
    f.write(b, vec![Op::unlink("O", "D", "child")]);
    let r = fol(&[("org", "child", Outbound, Some("doc"))]);
    assert_eq!(prove(&f, b, &["O"], "D", &r), ProveOutcome::NoPath);
    assert_eq!(path_of(&prove(&f, snap, &["O"], "D", &r)), ["O", "D"]);
    assert_eq!(path_of(&prove(&f, frozen, &["O"], "D", &r)), ["O", "D"]);
}

#[test]
fn three_branch_stack_links_from_different_layers() {
    let mut f = Fix::new();
    let b0 = f.main;
    f.typ(b0, "A", "org");
    f.typ(b0, "B", "folder");
    f.link(b0, "A", "B", "child");
    let head0 = f.store.branch(b0).unwrap().head;
    let b1 = f
        .store
        .create_branch("b1", &[ParentRef::new(b0, SeqRange::up_to(head0))])
        .unwrap();
    f.typ(b1, "C", "folder");
    f.link(b1, "B", "C", "child");
    let b2 = f
        .store
        .create_branch("b2", &[ParentRef::new(b1, SeqRange::ALL)])
        .unwrap();
    f.typ(b2, "D", "doc");
    f.link(b2, "C", "D", "child");
    let r = fol(&[
        ("org", "child", Outbound, Some("folder")),
        ("folder", "child", Outbound, Some("folder")),
        ("folder", "child", Outbound, Some("doc")),
    ]);
    assert_eq!(
        path_of(&prove(&f, b2, &["A"], "D", &r)),
        ["A", "B", "C", "D"]
    );
}

// --- equivalence with walk --------------------------------------------------

/// Small random graphs: for every start and item, prove agrees with a full
/// walk, under several follow maps and predicates.
#[test]
fn prove_agrees_with_walk_on_random_graphs() {
    let mut seed: u64 = 7;
    let mut next = |n: u64| {
        seed = seed
            .wrapping_mul(6364136223846793005)
            .wrapping_add(1442695040888963407);
        (seed >> 33) % n
    };
    let types = ["a", "b", "c"];
    let links = ["x", "y"];
    let ids: Vec<String> = (0..9).map(|i| format!("n{i}")).collect();
    for _ in 0..12 {
        let mut f = Fix::new();
        let b = f.main;
        let mut ops = Vec::new();
        for id in &ids {
            // One in ten untyped.
            if next(10) > 0 {
                ops.push(Op::set(id.as_str(), TYPE_KEY, types[next(3) as usize]));
            }
            ops.push(Op::set(id.as_str(), "n", Value::Int(next(4) as i64)));
        }
        for _ in 0..18 {
            let (s, d) = (next(9) as usize, next(9) as usize);
            ops.push(Op::link(
                ids[s].as_str(),
                ids[d].as_str(),
                links[next(2) as usize],
                SortOrder::Explicit(next(100) as f64),
            ));
        }
        f.write(b, ops);
        let mut rules = Vec::new();
        for p in types {
            for _ in 0..next(3) {
                let child = match next(4) {
                    0 => None,
                    k => Some(types[(k - 1) as usize]),
                };
                let d = if next(3) == 0 { Inbound } else { Outbound };
                rules.push((p, links[next(2) as usize], d, child));
            }
        }
        let follow = fol(&rules);
        for s in &ids {
            for item in &ids {
                let start = [s.as_str()];
                prove_vs_walk(&f, b, &start, item, &follow, |_, _| true, |_, _| true);
                let shallow = |p: &[String], _: &WalkNode| p.len() < 3;
                prove_vs_walk(&f, b, &start, item, &follow, shallow, |_, _| true);
                let odd = |_: &[String], n: &WalkNode| n.values.get("n") != Some(&Value::Int(1));
                let via = |p: &[String], _: &WalkNode| p.len() != 2;
                prove_vs_walk(&f, b, &start, item, &follow, odd, via);
            }
        }
    }
}
