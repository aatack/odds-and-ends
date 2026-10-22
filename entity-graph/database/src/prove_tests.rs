//! Named unit tests for [`Store::prove`], exercising all three strategies.

use std::collections::HashMap;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

use crate::*;

const T0: Timestamp = 1_700_000_000_000;

const STRATEGIES: [ProveStrategy; 3] = [
    ProveStrategy::Backward,
    ProveStrategy::Bidirectional,
    ProveStrategy::Walk,
];

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

    fn tick(&self) {
        self.clock.fetch_add(1, Ordering::SeqCst);
    }

    fn write(&mut self, branch: BranchId, ops: Vec<Op>) -> WriteResult {
        self.tick();
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

/// Build a relationship map from `(upper type, link type, direction)` triples.
fn rel(triples: &[(&str, &str, Direction)]) -> Follow {
    let mut m: Follow = HashMap::new();
    for (ty, lt, d) in triples {
        m.entry((*ty).to_owned())
            .or_default()
            .push(((*lt).to_owned(), *d));
    }
    m
}

/// Run one strategy and return the outcome and stats.
fn prove1(
    fix: &Fix,
    branch: BranchId,
    item: &str,
    roots: &[&str],
    relationships: &Follow,
    strategy: ProveStrategy,
    opts: ProveOptions,
) -> Prove {
    fix.store
        .prove(
            branch,
            item,
            roots,
            relationships,
            ProveOptions { strategy, ..opts },
        )
        .unwrap()
}

/// Whether an outcome is a proof.
fn proved(o: &ProveOutcome) -> bool {
    matches!(o, ProveOutcome::Proved(_))
}

fn len_of(o: &ProveOutcome) -> Option<usize> {
    match o {
        ProveOutcome::Proved(p) => Some(p.links.len()),
        _ => None,
    }
}

/// Run all three strategies with the given options, assert they agree on
/// reachability (ignoring `Unknown`), assert the shortest strategies return
/// equal lengths, and return the `Backward` outcome.
fn prove_all(
    fix: &Fix,
    branch: BranchId,
    item: &str,
    roots: &[&str],
    relationships: &Follow,
    opts: ProveOptions,
) -> ProveOutcome {
    let outs: Vec<(ProveStrategy, ProveOutcome)> = STRATEGIES
        .iter()
        .map(|s| {
            (
                *s,
                prove1(fix, branch, item, roots, relationships, *s, opts.clone()).outcome,
            )
        })
        .collect();
    // No strategy is Unknown here (callers that test budgets check directly).
    for (s, o) in &outs {
        assert!(
            !matches!(o, ProveOutcome::Unknown),
            "{s:?} unexpectedly Unknown"
        );
    }
    let any = proved(&outs[0].1);
    for (s, o) in &outs {
        assert_eq!(proved(o), any, "{s:?} disagrees on reachability: {o:?}");
    }
    // Backward and Bidirectional must return a shortest proof of equal length.
    let bw = len_of(&outs[0].1);
    let bd = len_of(&outs[1].1);
    assert_eq!(bw, bd, "backward and bidirectional lengths differ");
    outs.into_iter().next().unwrap().1
}

/// Assert a proof is exactly this path.
fn assert_path(o: &ProveOutcome, expect: &[&str]) {
    match o {
        ProveOutcome::Proved(p) => assert_eq!(p.path, expect, "unexpected proof path"),
        other => panic!("expected a proof, got {other:?}"),
    }
}

// --- basic reachability ---------------------------------------------------

#[test]
fn direct_link_depth_one() {
    let mut f = Fix::new();
    f.typ(f.main, "org", "org");
    f.typ(f.main, "doc", "doc");
    f.link(f.main, "org", "doc", "child");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    let o = prove_all(&f, f.main, "doc", &["org"], &r, ProveOptions::default());
    assert_path(&o, &["org", "doc"]);
}

#[test]
fn multi_hop_chain() {
    let mut f = Fix::new();
    let b = f.main;
    for (e, t) in [
        ("O", "org"),
        ("T", "team"),
        ("F", "folder"),
        ("S", "folder"),
        ("D", "doc"),
    ] {
        f.typ(b, e, t);
    }
    f.link(b, "O", "T", "child");
    f.link(b, "T", "F", "child");
    f.link(b, "F", "S", "child");
    f.link(b, "S", "D", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("team", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    let o = prove_all(&f, b, "D", &["O"], &r, ProveOptions::default());
    assert_path(&o, &["O", "T", "F", "S", "D"]);
}

#[test]
fn inbound_relationship_member_link() {
    // "group --member--> user" is stored as a user -> group link.
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "G", "group");
    f.typ(b, "U", "user");
    f.link(b, "U", "G", "member");
    let r = rel(&[("group", "member", Direction::Inbound)]);
    let o = prove_all(&f, b, "U", &["G"], &r, ProveOptions::default());
    assert_path(&o, &["G", "U"]);
}

#[test]
fn wrong_type_at_intermediate_breaks_proof() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "O", "org");
    f.typ(b, "X", "note"); // not a type the map can step from
    f.typ(b, "D", "doc");
    f.link(b, "O", "X", "child");
    f.link(b, "X", "D", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("team", "child", Direction::Outbound),
    ]);
    let o = prove_all(&f, b, "D", &["O"], &r, ProveOptions::default());
    assert_eq!(o, ProveOutcome::NoPath);
}

#[test]
fn item_equals_root_is_depth_zero() {
    let mut f = Fix::new();
    f.typ(f.main, "a", "org");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    let o = prove_all(&f, f.main, "a", &["a", "b"], &r, ProveOptions::default());
    match o {
        ProveOutcome::Proved(p) => {
            assert_eq!(p.path, vec!["a".to_owned()]);
            assert!(p.links.is_empty());
        }
        other => panic!("expected depth-0 proof, got {other:?}"),
    }
}

#[test]
fn many_roots_one_reachable() {
    let mut f = Fix::new();
    let b = f.main;
    for (e, t) in [("r1", "org"), ("r2", "org"), ("r3", "org"), ("D", "doc")] {
        f.typ(b, e, t);
    }
    f.link(b, "r2", "D", "child");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    let o = prove_all(&f, b, "D", &["r1", "r2", "r3"], &r, ProveOptions::default());
    assert_path(&o, &["r2", "D"]);
}

#[test]
fn no_path_when_disconnected() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "O", "org");
    f.typ(b, "D", "doc");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    let o = prove_all(&f, b, "D", &["O"], &r, ProveOptions::default());
    assert_eq!(o, ProveOutcome::NoPath);
}

#[test]
fn empty_roots_is_no_path() {
    let mut f = Fix::new();
    f.typ(f.main, "D", "doc");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    let o = prove_all(&f, f.main, "D", &[], &r, ProveOptions::default());
    assert_eq!(o, ProveOutcome::NoPath);
}

#[test]
fn unknown_ids_are_no_path() {
    let mut f = Fix::new();
    f.typ(f.main, "O", "org");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    // Unknown item.
    let o = prove_all(&f, f.main, "ghost", &["O"], &r, ProveOptions::default());
    assert_eq!(o, ProveOutcome::NoPath);
    // Unknown root.
    f.typ(f.main, "D", "doc");
    f.link(f.main, "O", "D", "child");
    let o = prove_all(&f, f.main, "D", &["nobody"], &r, ProveOptions::default());
    assert_eq!(o, ProveOutcome::NoPath);
}

// --- limits, cycles, shortest ---------------------------------------------

#[test]
fn max_depth_cuts_long_paths() {
    let mut f = Fix::new();
    let b = f.main;
    for (e, t) in [("O", "org"), ("T", "team"), ("F", "folder"), ("D", "doc")] {
        f.typ(b, e, t);
    }
    f.link(b, "O", "T", "child");
    f.link(b, "T", "F", "child");
    f.link(b, "F", "D", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("team", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    // The proof has 3 links.
    let tight = ProveOptions {
        max_depth: Some(2),
        ..Default::default()
    };
    assert_eq!(
        prove_all(&f, b, "D", &["O"], &r, tight),
        ProveOutcome::NoPath
    );
    let ok = ProveOptions {
        max_depth: Some(3),
        ..Default::default()
    };
    assert_path(
        &prove_all(&f, b, "D", &["O"], &r, ok),
        &["O", "T", "F", "D"],
    );
}

#[test]
fn max_nodes_budget_gives_unknown() {
    let mut f = Fix::new();
    let b = f.main;
    // A long chain so the search must expand several nodes.
    let n = 8;
    for i in 0..=n {
        f.typ(b, &format!("e{i}"), if i == 0 { "org" } else { "folder" });
    }
    for i in 0..n {
        f.link(b, &format!("e{i}"), &format!("e{}", i + 1), "child");
    }
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    let item = format!("e{n}");
    // A tiny budget cannot reach the decision.
    let tiny = ProveOptions {
        max_nodes: Some(2),
        ..Default::default()
    };
    for s in STRATEGIES {
        let p = prove1(&f, b, &item, &["e0"], &r, s, tiny.clone());
        assert_eq!(p.outcome, ProveOutcome::Unknown, "{s:?} with tiny budget");
    }
    // A generous budget decides (and this is distinct from NoPath).
    let big = ProveOptions {
        max_nodes: Some(1000),
        ..Default::default()
    };
    assert!(proved(&prove_all(&f, b, &item, &["e0"], &r, big)));
}

#[test]
fn unknown_is_distinct_from_no_path() {
    let mut f = Fix::new();
    let b = f.main;
    for i in 0..6 {
        f.typ(b, &format!("e{i}"), if i == 0 { "org" } else { "folder" });
    }
    for i in 0..5 {
        f.link(b, &format!("e{i}"), &format!("e{}", i + 1), "child");
    }
    // A disconnected item: definite NoPath even with a small budget, because
    // the whole (small) ancestor set fits.
    f.typ(b, "lonely", "doc");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    let o = prove1(
        &f,
        b,
        "lonely",
        &["e0"],
        &r,
        ProveStrategy::Backward,
        ProveOptions {
            max_nodes: Some(50),
            ..Default::default()
        },
    );
    assert_eq!(o.outcome, ProveOutcome::NoPath);
}

#[test]
fn cycles_do_not_trap_the_search() {
    let mut f = Fix::new();
    let b = f.main;
    for (e, t) in [("R", "org"), ("A", "folder"), ("B", "folder"), ("D", "doc")] {
        f.typ(b, e, t);
    }
    f.link(b, "R", "A", "child");
    f.link(b, "A", "B", "child");
    f.link(b, "B", "A", "child"); // cycle A <-> B
    f.link(b, "A", "D", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    let o = prove_all(&f, b, "D", &["R"], &r, ProveOptions::default());
    assert_path(&o, &["R", "A", "D"]);
}

#[test]
fn shortest_path_is_chosen() {
    let mut f = Fix::new();
    let b = f.main;
    for (e, t) in [
        ("R", "org"),
        ("A", "folder"),
        ("X1", "folder"),
        ("X2", "folder"),
        ("D", "doc"),
    ] {
        f.typ(b, e, t);
    }
    // short: R -> A -> D (2). long: R -> X1 -> X2 -> D (3).
    f.link(b, "R", "A", "child");
    f.link(b, "A", "D", "child");
    f.link(b, "R", "X1", "child");
    f.link(b, "X1", "X2", "child");
    f.link(b, "X2", "D", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    let o = prove_all(&f, b, "D", &["R"], &r, ProveOptions::default());
    assert_eq!(len_of(&o), Some(2));
}

#[test]
fn removed_link_then_readded() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "O", "org");
    f.typ(b, "D", "doc");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    f.link(b, "O", "D", "child");
    assert!(proved(&prove_all(
        &f,
        b,
        "D",
        &["O"],
        &r,
        ProveOptions::default()
    )));
    // Remove it.
    f.write(b, vec![Op::unlink("O", "D", "child")]);
    assert_eq!(
        prove_all(&f, b, "D", &["O"], &r, ProveOptions::default()),
        ProveOutcome::NoPath
    );
    // Re-add it.
    f.link(b, "O", "D", "child");
    assert!(proved(&prove_all(
        &f,
        b,
        "D",
        &["O"],
        &r,
        ProveOptions::default()
    )));
}

// --- archiving ------------------------------------------------------------

fn chain_rmi(f: &mut Fix) -> Follow {
    let b = f.main;
    f.typ(b, "R", "org");
    f.typ(b, "M", "folder");
    f.typ(b, "I", "doc");
    f.link(b, "R", "M", "child");
    f.link(b, "M", "I", "child");
    rel(&[
        ("org", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ])
}

#[test]
fn archived_intermediate_blocks_then_restores() {
    let mut f = Fix::new();
    let r = chain_rmi(&mut f);
    let b = f.main;
    assert!(proved(&prove_all(
        &f,
        b,
        "I",
        &["R"],
        &r,
        ProveOptions::default()
    )));
    f.archive(b, "M");
    assert_eq!(
        prove_all(&f, b, "I", &["R"], &r, ProveOptions::default()),
        ProveOutcome::NoPath
    );
    f.typ(b, "M", "folder"); // restore
    assert!(proved(&prove_all(
        &f,
        b,
        "I",
        &["R"],
        &r,
        ProveOptions::default()
    )));
}

#[test]
fn archived_intermediate_not_rescued_by_include_archived() {
    let mut f = Fix::new();
    let r = chain_rmi(&mut f);
    let b = f.main;
    f.archive(b, "M");
    let o = prove_all(
        &f,
        b,
        "I",
        &["R"],
        &r,
        ProveOptions {
            include_archived: true,
            ..Default::default()
        },
    );
    assert_eq!(o, ProveOutcome::NoPath);
}

#[test]
fn archived_item_blocks_default_but_include_archived_proves() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "R", "org");
    f.typ(b, "I", "doc");
    f.link(b, "R", "I", "child");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    f.archive(b, "I");
    assert_eq!(
        prove_all(&f, b, "I", &["R"], &r, ProveOptions::default()),
        ProveOutcome::NoPath
    );
    let o = prove_all(
        &f,
        b,
        "I",
        &["R"],
        &r,
        ProveOptions {
            include_archived: true,
            ..Default::default()
        },
    );
    assert_path(&o, &["R", "I"]);
    f.typ(b, "I", "doc"); // restore
    assert!(proved(&prove_all(
        &f,
        b,
        "I",
        &["R"],
        &r,
        ProveOptions::default()
    )));
}

#[test]
fn archived_root_blocks_both_modes_then_restores() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "R", "org");
    f.typ(b, "I", "doc");
    f.link(b, "R", "I", "child");
    let r = rel(&[("org", "child", Direction::Outbound)]);
    f.archive(b, "R");
    assert_eq!(
        prove_all(&f, b, "I", &["R"], &r, ProveOptions::default()),
        ProveOutcome::NoPath
    );
    let incl = ProveOptions {
        include_archived: true,
        ..Default::default()
    };
    assert_eq!(
        prove_all(&f, b, "I", &["R"], &r, incl),
        ProveOutcome::NoPath
    );
    f.typ(b, "R", "org"); // restore
    assert!(proved(&prove_all(
        &f,
        b,
        "I",
        &["R"],
        &r,
        ProveOptions::default()
    )));
}

// --- undo and merge -------------------------------------------------------

#[test]
fn undo_of_link_batch_removes_proof() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "R", "org");
    f.typ(b, "I", "doc");
    f.link(b, "R", "I", "child"); // the newest batch, author "a"
    let r = rel(&[("org", "child", Direction::Outbound)]);
    assert!(proved(&prove_all(
        &f,
        b,
        "I",
        &["R"],
        &r,
        ProveOptions::default()
    )));
    f.store.undo(b, "a").unwrap();
    assert_eq!(
        prove_all(&f, b, "I", &["R"], &r, ProveOptions::default()),
        ProveOutcome::NoPath
    );
}

#[test]
fn merge_brings_in_the_link() {
    let mut f = Fix::new();
    let main = f.main;
    f.typ(main, "R", "org");
    f.typ(main, "M", "folder");
    f.link(main, "R", "M", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    let agent = f.store.fork(main, "agent").unwrap();
    f.typ(agent, "I", "doc");
    f.link(agent, "M", "I", "child");
    // Before merge, main cannot reach I.
    assert_eq!(
        prove_all(&f, main, "I", &["R"], &r, ProveOptions::default()),
        ProveOutcome::NoPath
    );
    f.store.merge(main, agent).unwrap();
    assert_path(
        &prove_all(&f, main, "I", &["R"], &r, ProveOptions::default()),
        &["R", "M", "I"],
    );
}

// --- history: frozen views, forks, stacks ---------------------------------

#[test]
fn past_link_absent_at_head_present_on_frozen_view_and_fork() {
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "O", "org");
    f.typ(b, "D", "doc");
    f.link(b, "O", "D", "child");
    let with_link = f.store.branch(b).unwrap().head;
    // A fork taken while the link is live.
    let snap = f.store.fork(b, "snap").unwrap();
    // A frozen "as of" view at the same point.
    let frozen = f
        .store
        .create_branch("frozen", &[ParentRef::new(b, SeqRange::up_to(with_link))])
        .unwrap();
    // Now remove the link at the head of the main branch.
    f.write(b, vec![Op::unlink("O", "D", "child")]);
    let r = rel(&[("org", "child", Direction::Outbound)]);
    // Head: gone.
    assert_eq!(
        prove_all(&f, b, "D", &["O"], &r, ProveOptions::default()),
        ProveOutcome::NoPath
    );
    // Fork and frozen view: still there, from history.
    assert_path(
        &prove_all(&f, snap, "D", &["O"], &r, ProveOptions::default()),
        &["O", "D"],
    );
    assert_path(
        &prove_all(&f, frozen, "D", &["O"], &r, ProveOptions::default()),
        &["O", "D"],
    );
}

#[test]
fn three_branch_stack_links_from_different_layers() {
    let mut f = Fix::new();
    let b0 = f.main;
    f.typ(b0, "A", "org");
    f.typ(b0, "B", "folder");
    f.link(b0, "A", "B", "child");
    let head0 = f.store.branch(b0).unwrap().head;
    // b1 reads b0 as a frozen range, and adds B -> C.
    let b1 = f
        .store
        .create_branch("b1", &[ParentRef::new(b0, SeqRange::up_to(head0))])
        .unwrap();
    f.typ(b1, "C", "folder");
    f.link(b1, "B", "C", "child");
    // b2 reads b1 open (which chains to b0), and adds C -> D.
    let b2 = f
        .store
        .create_branch("b2", &[ParentRef::new(b1, SeqRange::ALL)])
        .unwrap();
    f.typ(b2, "D", "doc");
    f.link(b2, "C", "D", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    // A -> B from b0 (frozen), B -> C from b1, C -> D from b2 (open).
    assert_path(
        &prove_all(&f, b2, "D", &["A"], &r, ProveOptions::default()),
        &["A", "B", "C", "D"],
    );
}

#[test]
fn link_removed_on_child_still_live_on_parent() {
    let mut f = Fix::new();
    let p = f.main;
    f.typ(p, "A", "org");
    f.typ(p, "B", "doc");
    f.link(p, "A", "B", "child");
    let c = f
        .store
        .create_branch("child", &[ParentRef::new(p, SeqRange::ALL)])
        .unwrap();
    f.write(c, vec![Op::unlink("A", "B", "child")]);
    let r = rel(&[("org", "child", Direction::Outbound)]);
    // Child: removed.
    assert_eq!(
        prove_all(&f, c, "B", &["A"], &r, ProveOptions::default()),
        ProveOutcome::NoPath
    );
    // Parent: still live.
    assert_path(
        &prove_all(&f, p, "B", &["A"], &r, ProveOptions::default()),
        &["A", "B"],
    );
}

#[test]
fn wide_fan_in_still_shortest() {
    // The item is shared with 50 groups; a short path and long paths exist.
    let mut f = Fix::new();
    let b = f.main;
    f.typ(b, "root", "org");
    f.typ(b, "doc", "doc");
    for i in 0..50 {
        let g = format!("g{i}");
        f.typ(b, &g, "group");
        f.link(b, &g, "doc", "shared"); // group -> doc
    }
    // root grants directly to g0 (short), and a long chain via g1..g2..doc too.
    f.link(b, "root", "g0", "grant");
    let r = rel(&[
        ("org", "grant", Direction::Outbound),
        ("group", "shared", Direction::Outbound),
    ]);
    let o = prove_all(&f, b, "doc", &["root"], &r, ProveOptions::default());
    // root -> g0 -> doc : length 2.
    assert_eq!(len_of(&o), Some(2));
}

#[test]
fn stats_are_populated() {
    let mut f = Fix::new();
    let b = f.main;
    for (e, t) in [("O", "org"), ("T", "team"), ("D", "doc")] {
        f.typ(b, e, t);
    }
    f.link(b, "O", "T", "child");
    f.link(b, "T", "D", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("team", "child", Direction::Outbound),
    ]);
    let p = prove1(
        &f,
        b,
        "D",
        &["O"],
        &r,
        ProveStrategy::Backward,
        ProveOptions::default(),
    );
    assert!(proved(&p.outcome));
    assert!(p.stats.nodes_visited >= 1);
    assert!(p.stats.link_reads >= 1);
    assert!(p.stats.value_reads >= 1);
    assert!(p.stats.sql_queries >= 1);
    assert_eq!(p.stats.depth_reached, 2);
}

// --- proof structure ------------------------------------------------------

/// Every link connects consecutive path entities, and is a live stored link.
fn assert_valid(f: &Fix, branch: BranchId, p: &Proof) {
    assert_eq!(p.links.len() + 1, p.path.len(), "links vs path length");
    for (i, link) in p.links.iter().enumerate() {
        let upper = &p.path[i];
        let lower = &p.path[i + 1];
        let ends = [&link.source, &link.destination];
        assert!(
            ends.contains(&upper) && ends.contains(&lower),
            "link does not connect the step"
        );
        // The live link exists in the store, both directions.
        let d = if &link.source == upper {
            Direction::Outbound
        } else {
            Direction::Inbound
        };
        let got = f
            .store
            .links(branch, &[upper.as_str()], &[(link.link_type.as_str(), d)])
            .unwrap();
        let group = &got[upper][&(link.link_type.clone(), d)];
        assert!(group.contains(link), "link is not a live stored link");
    }
}

#[test]
fn returned_proofs_are_valid_for_all_strategies() {
    let mut f = Fix::new();
    let b = f.main;
    for (e, t) in [("O", "org"), ("T", "team"), ("F", "folder"), ("D", "doc")] {
        f.typ(b, e, t);
    }
    f.link(b, "O", "T", "child");
    f.link(b, "T", "F", "child");
    f.link(b, "F", "D", "child");
    let r = rel(&[
        ("org", "child", Direction::Outbound),
        ("team", "child", Direction::Outbound),
        ("folder", "child", Direction::Outbound),
    ]);
    for s in STRATEGIES {
        let p = prove1(&f, b, "D", &["O"], &r, s, ProveOptions::default());
        match p.outcome {
            ProveOutcome::Proved(proof) => assert_valid(&f, b, &proof),
            other => panic!("{s:?} did not prove: {other:?}"),
        }
    }
}
