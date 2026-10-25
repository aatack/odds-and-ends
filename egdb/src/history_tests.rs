//! Correctness of every read on historical data: names changed many times,
//! values cleared and restored, frozen ranges that must read history,
//! 3-branch stacks, merges, undo, and paging. Speed does not matter.
//!
//! Each test keeps the expected final state of every branch by hand (an
//! overlay of maps, no SQL) and [`check`] compares every read against it:
//! `find` with every filter kind and order at several page sizes and both
//! archive settings, `values`, `value`, `exists`, `is_archived`, `links`,
//! `links_including_archived` and `walk`.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

use crate::*;

/// The expected events that win in one view: `Value::Null` is a clear, a
/// `None` sort is a removed link. An entity exists when it has a value
/// entry or is an end of a link entry.
#[derive(Clone, Default, Debug)]
struct Exp {
    vals: BTreeMap<(String, String), Value>,
    links: BTreeMap<(String, String, String), Option<f64>>,
}

impl Exp {
    fn apply(&mut self, ops: &[Op]) {
        for op in ops {
            match op {
                Op::SetValue { entity, key, value } => {
                    self.vals
                        .insert((entity.clone(), key.clone()), value.clone());
                }
                Op::Link {
                    source,
                    destination,
                    link_type,
                    sort,
                } => {
                    let s = match sort {
                        SortOrder::Explicit(x) => Some(*x),
                        SortOrder::Removed => None,
                        SortOrder::Timestamp => panic!("tests use explicit sorts"),
                    };
                    self.links
                        .insert((source.clone(), link_type.clone(), destination.clone()), s);
                }
            }
        }
    }
    /// `top` over `self`.
    fn under(&self, top: &Exp) -> Exp {
        let mut e = self.clone();
        e.vals.extend(top.vals.clone());
        e.links.extend(top.links.clone());
        e
    }
    fn values(&self, ent: &str) -> BTreeMap<String, Value> {
        self.vals
            .iter()
            .filter(|((e, _), v)| e == ent && !v.is_null())
            .map(|((_, k), v)| (k.clone(), v.clone()))
            .collect()
    }
    fn exists(&self, ent: &str) -> bool {
        self.vals.keys().any(|(e, _)| e == ent)
            || self.links.keys().any(|(s, _, d)| s == ent || d == ent)
    }
    fn typed(&self, ent: &str) -> bool {
        self.vals
            .get(&(ent.to_owned(), TYPE_KEY.to_owned()))
            .is_some_and(|v| !v.is_null())
    }
    fn archived(&self, ent: &str) -> bool {
        self.vals
            .get(&(ent.to_owned(), TYPE_KEY.to_owned()))
            .is_some_and(Value::is_null)
    }
    /// (other end, sort) in read order.
    fn links_of(
        &self,
        ent: &str,
        ty: &str,
        dir: Direction,
        typed_only: bool,
    ) -> Vec<(String, f64)> {
        let mut out: Vec<(String, f64)> = self
            .links
            .iter()
            .filter_map(|((s, t, d), sort)| {
                let (me, other) = match dir {
                    Direction::Outbound => (s, d),
                    Direction::Inbound => (d, s),
                };
                (t == ty && me == ent && sort.is_some() && (!typed_only || self.typed(other)))
                    .then(|| (other.clone(), sort.unwrap()))
            })
            .collect();
        out.sort_by(|a, b| a.1.total_cmp(&b.1).then_with(|| a.0.cmp(&b.0)));
        out
    }
}

/// A store, its clock, and the insertion order of entities (first write
/// anywhere in the store).
struct T {
    s: Store,
    order: Vec<String>,
}

impl T {
    fn new() -> (T, BranchId) {
        let mut s = Store::open_in_memory().unwrap();
        let clock = Arc::new(AtomicI64::new(1_700_000_000_000));
        s.set_clock(move || clock.fetch_add(1, Ordering::SeqCst));
        let main = s.create_branch("main", &[]).unwrap();
        (
            T {
                s,
                order: Vec::new(),
            },
            main,
        )
    }
    /// Write `ops` on `b` and apply them to every expectation in `exps`.
    fn w(&mut self, b: BranchId, author: &str, ops: Vec<Op>, exps: &mut [&mut Exp]) {
        for op in &ops {
            let ents = match op {
                Op::SetValue { entity, .. } => vec![entity.clone()],
                Op::Link {
                    source,
                    destination,
                    ..
                } => vec![source.clone(), destination.clone()],
            };
            for e in ents {
                if !self.order.contains(&e) {
                    self.order.push(e);
                }
            }
        }
        for e in exps.iter_mut() {
            e.apply(&ops);
        }
        self.s.write(WriteBatch::new(b, author, ops)).unwrap();
    }
    fn rename(&mut self, b: BranchId, e: &str, name: impl Into<Value>, exps: &mut [&mut Exp]) {
        self.w(b, "a", vec![Op::set(e, "name", name)], exps);
    }
}

fn id(i: u32) -> String {
    // Hashed: ID order differs from insertion order.
    format!("{:06x}", i.wrapping_mul(2_654_435_761) >> 8)
}

fn filters() -> Vec<Filter> {
    let t = |s: &str| Value::Text(s.into());
    vec![
        Filter::Eq(k("name"), t("alpha")),
        Filter::Eq(k("name"), t("beta")),
        Filter::Ne(k("name"), t("alpha")),
        Filter::Lt(k("name"), t("beta")),
        Filter::Le(k("name"), t("beta")),
        Filter::Gt(k("name"), t("beta")),
        Filter::Ge(k("name"), t("beta")),
        Filter::Lt(k("n"), Value::Int(3)),
        Filter::Ge(k("n"), Value::Float(2.5)),
        Filter::Contains(k("name"), k("lph")),
        Filter::Contains(k("name"), k("ALPHA")),
        Filter::ContainsIgnoreCase(k("name"), k("ALPHA")),
        Filter::ContainsIgnoreCase(k("name"), k("mm")),
        Filter::Prefix(k("name"), k("al")),
        Filter::Prefix(k("name"), k("gamma")),
        Filter::In(k("name"), vec![t("alpha"), t("gamma"), Value::Int(1)]),
        Filter::IsNull(k("name")),
        Filter::NotNull(k("name")),
        Filter::eq(TYPE_KEY, "task"),
        Filter::eq(TYPE_KEY, "note"),
        Filter::And(vec![
            Filter::Prefix(k("name"), k("al")),
            Filter::Not(Box::new(Filter::eq(TYPE_KEY, "task"))),
        ]),
        Filter::Or(vec![
            Filter::Eq(k("name"), t("beta")),
            Filter::ContainsIgnoreCase(k("name"), k("ZETA")),
        ]),
        Filter::Not(Box::new(Filter::Contains(k("name"), k("a")))),
        Filter::And(vec![]),
        Filter::Or(vec![]),
        Filter::custom(|_, v| matches!(v.get("name"), Some(Value::Text(s)) if s.len() % 2 == 1)),
    ]
}

fn k(s: &str) -> String {
    s.to_owned()
}

/// Every read of `b` against `exp`. `ents`: the entities to ask for.
fn check(t: &T, b: BranchId, exp: &Exp, ctx: &str) {
    let s = &t.s;
    let mut ents: Vec<String> = t.order.clone();
    ents.push("never-written".into());
    let refs: Vec<&str> = ents.iter().map(String::as_str).collect();

    // Batch and single reads.
    let vals = s.values(b, &refs).unwrap();
    let ex = s.exists(b, &refs).unwrap();
    let ar = s.is_archived(b, &refs).unwrap();
    for e in &refs {
        assert_eq!(vals[*e], exp.values(e), "{ctx}: values of {e}");
        assert_eq!(
            s.value(b, e, "name").unwrap(),
            exp.values(e).get("name").cloned(),
            "{ctx}: value name of {e}"
        );
        assert_eq!(ex[*e], exp.exists(e), "{ctx}: exists {e}");
        assert_eq!(ar[*e], exp.archived(e), "{ctx}: archived {e}");
    }
    let groups = [
        ("child", Direction::Outbound),
        ("child", Direction::Inbound),
    ];
    for typed in [true, false] {
        let got = if typed {
            s.links(b, &refs, &groups).unwrap()
        } else {
            s.links_including_archived(b, &refs, &groups).unwrap()
        };
        for e in &refs {
            for (ty, dir) in groups {
                let g: Vec<(String, f64)> = got[*e][&(ty.to_owned(), dir)]
                    .iter()
                    .map(|l| {
                        let other = if dir == Direction::Outbound {
                            &l.destination
                        } else {
                            &l.source
                        };
                        (other.clone(), l.sort)
                    })
                    .collect();
                assert_eq!(
                    g,
                    exp.links_of(e, ty, dir, typed),
                    "{ctx}: links {e} {dir:?} typed={typed}"
                );
            }
        }
    }

    // Walk from every typed entity that has children, in pages of 1 and all.
    let kinds = ["task", "note"];
    let rules: Vec<FollowRule> = kinds
        .iter()
        .map(|k| FollowRule::new("child", Direction::Outbound, *k))
        .collect();
    let follow: Follow = kinds
        .iter()
        .map(|k| ((*k).to_owned(), rules.clone()))
        .collect();
    for root in refs.iter().filter(|e| {
        !exp.links_of(e, "child", Direction::Outbound, true)
            .is_empty()
    }) {
        let mut want: Vec<(Vec<String>, BTreeMap<String, Value>)> = Vec::new();
        fn go(exp: &Exp, path: Vec<String>, out: &mut Vec<(Vec<String>, BTreeMap<String, Value>)>) {
            let id = path.last().unwrap().clone();
            out.push((path.clone(), exp.values(&id)));
            if path.len() >= 4 || !exp.typed(&id) {
                return;
            }
            for (kid, _) in exp.links_of(&id, "child", Direction::Outbound, true) {
                if !path.contains(&kid) {
                    let mut p = path.clone();
                    p.push(kid);
                    go(exp, p, out);
                }
            }
        }
        go(exp, vec![root.to_string()], &mut want);
        for page in [usize::MAX, 1] {
            let mut path = vec![root.to_string()];
            let mut got = Vec::new();
            loop {
                let prefs: Vec<&str> = path.iter().map(String::as_str).collect();
                let w = s
                    .walk(
                        b,
                        &prefs,
                        &follow,
                        |p, _| p.len() < 4,
                        |_, _| true,
                        WalkOptions {
                            steps: page,
                            ..Default::default()
                        },
                    )
                    .unwrap();
                for p in w.paths {
                    let values = w.cache.nodes[p.last().unwrap()].values.clone();
                    got.push((p, values));
                }
                match w.next {
                    Some(n) => path = n,
                    None => break,
                }
            }
            assert_eq!(got, want, "{ctx}: walk from {root} page {page}");
        }
    }

    // find: every filter, order, page size and archive setting.
    let rank: HashMap<&str, usize> = t
        .order
        .iter()
        .enumerate()
        .map(|(i, e)| (e.as_str(), i))
        .collect();
    let orders = [
        FindOrder::EntityId,
        FindOrder::Insertion,
        FindOrder::ValueAsc(k("name")),
        FindOrder::ValueDesc(k("name")),
        FindOrder::ValueAsc(k("n")),
    ];
    for f in filters() {
        for include_archived in [false, true] {
            let base: Vec<&String> = t
                .order
                .iter()
                .filter(|e| exp.exists(e) && (include_archived || exp.typed(e)))
                .filter(|e| crate::find::matches(&f, e, &exp.values(e)))
                .collect();
            for order in &orders {
                let mut want: Vec<String> = base.iter().map(|e| e.to_string()).collect();
                match order {
                    FindOrder::EntityId => want.sort(),
                    FindOrder::Insertion => want.sort_by_key(|e| rank[e.as_str()]),
                    FindOrder::ValueAsc(key) | FindOrder::ValueDesc(key) => {
                        want.retain(|e| exp.values(e).contains_key(key));
                        let kv = |e: &String| crate::find::sql_value(&exp.values(e)[key]);
                        want.sort_by(|a, b| {
                            crate::find::cmp_sql(&kv(a), &kv(b))
                                .then(rank[a.as_str()].cmp(&rank[b.as_str()]))
                        });
                        if matches!(order, FindOrder::ValueDesc(_)) {
                            want.reverse();
                        }
                    }
                }
                for limit in [1, 2, 3, 1000] {
                    let values = limit != 2;
                    let mut got: Vec<String> = Vec::new();
                    let mut cursor = None;
                    for _ in 0..1000 {
                        let p = s
                            .find(
                                b,
                                &f,
                                FindOptions {
                                    limit,
                                    cursor,
                                    order: order.clone(),
                                    values,
                                    include_archived,
                                    ..Default::default()
                                },
                            )
                            .unwrap();
                        assert!(p.rows.len() <= limit);
                        for r in &p.rows {
                            let v = if values {
                                exp.values(&r.id)
                            } else {
                                BTreeMap::new()
                            };
                            assert_eq!(r.values, v, "{ctx}: find values of {}", r.id);
                            got.push(r.id.clone());
                        }
                        cursor = p.next;
                        if cursor.is_none() {
                            break;
                        }
                    }
                    assert!(cursor.is_none(), "{ctx}: find did not end");
                    let uniq: HashSet<&String> = got.iter().collect();
                    assert_eq!(
                        uniq.len(),
                        got.len(),
                        "{ctx}: repeated rows {f:?} {order:?} limit {limit}"
                    );
                    assert_eq!(
                        got, want,
                        "{ctx}: find {f:?} {order:?} limit {limit} archived {include_archived}"
                    );
                }
            }
        }
    }
}

const NAMES: [&str; 7] = [
    "alpha",
    "beta",
    "gamma",
    "Alpha",
    "ALPHA zeta",
    "delta",
    "mmm",
];

/// `n` entities typed task/note with a name and a number, children of a
/// root. Returns their IDs.
fn seed(t: &mut T, b: BranchId, n: u32, exps: &mut [&mut Exp]) -> Vec<String> {
    let ids: Vec<String> = (0..n).map(id).collect();
    let mut ops = vec![
        Op::set("root", TYPE_KEY, "task"),
        Op::set("root", "name", "root"),
    ];
    for (i, e) in ids.iter().enumerate() {
        ops.push(Op::set(
            e.as_str(),
            TYPE_KEY,
            if i % 3 == 0 { "task" } else { "note" },
        ));
        ops.push(Op::set(e.as_str(), "name", NAMES[i % NAMES.len()]));
        ops.push(Op::set(e.as_str(), "n", (i % 5) as i64));
    }
    t.w(b, "a", ops, exps);
    let ops = ids
        .iter()
        .enumerate()
        .map(|(i, e)| Op::link("root", e.as_str(), "child", SortOrder::Explicit(i as f64)))
        .collect();
    t.w(b, "a", ops, exps);
    ids
}

#[test]
fn many_renames_find_and_reads_see_only_the_final_name() {
    let (mut t, b) = T::new();
    let mut exp = Exp::default();
    let ids = seed(&mut t, b, 12, &mut [&mut exp]);
    check(&t, b, &exp, "seeded");
    // Entity i goes through 5 to 10 names: an old name matched a filter
    // and the final one does not (and the reverse), back to an earlier
    // name, the same name twice in a row, a clear and a set again.
    for round in 0..10usize {
        for (i, e) in ids.iter().enumerate() {
            if round >= 5 + i % 6 {
                continue;
            }
            let name: Value = match (i + round) % 9 {
                7 => Value::Null,
                8 => Value::Int(1),
                j => NAMES[(j + i) % NAMES.len()].into(),
            };
            t.rename(b, e, name.clone(), &mut [&mut exp]);
            if round % 3 == 1 {
                t.rename(b, e, name, &mut [&mut exp]); // the same value again
            }
        }
        if round % 3 == 2 {
            check(&t, b, &exp, &format!("round {round}"));
        }
    }
    // Back to the very first names.
    for (i, e) in ids.iter().enumerate() {
        t.rename(b, e, NAMES[i % NAMES.len()], &mut [&mut exp]);
    }
    check(&t, b, &exp, "back to the first names");
}

#[test]
fn a_frozen_fork_after_rename_k_reads_the_names_as_of_k() {
    let (mut t, main) = T::new();
    let mut exp = Exp::default();
    let ids = seed(&mut t, main, 9, &mut [&mut exp]);
    let mut forks: Vec<(BranchId, Exp, usize)> = Vec::new();
    for k in 0..8usize {
        for (i, e) in ids.iter().enumerate() {
            t.rename(main, e, NAMES[(i + k + 1) % NAMES.len()], &mut [&mut exp]);
        }
        if k % 2 == 1 {
            // Also archive one and restore another in history.
            t.w(
                main,
                "a",
                vec![Op::clear(ids[k].as_str(), TYPE_KEY)],
                &mut [&mut exp],
            );
            t.w(
                main,
                "a",
                vec![Op::set(ids[k - 1].as_str(), TYPE_KEY, "task")],
                &mut [&mut exp],
            );
        }
        if k == 1 || k == 4 || k == 6 {
            let f = t.s.fork(main, &format!("fork{k}")).unwrap();
            forks.push((f, exp.clone(), k));
        }
    }
    check(&t, main, &exp, "main after all renames");
    for (f, fexp, k) in &forks {
        check(&t, *f, fexp, &format!("fork after rename {k}"));
    }
    // A fork of a fork, then renames on the fork; the parent's later
    // renames stay invisible.
    let (f1, mut e1, _) = forks[0].clone();
    let ff = t.s.fork(f1, "ff").unwrap();
    let mut eff = e1.clone();
    t.rename(f1, &ids[0], "on fork", &mut [&mut e1]);
    t.rename(ff, &ids[1], "beta", &mut [&mut eff]);
    t.rename(main, &ids[1], "main later", &mut [&mut exp]);
    check(&t, f1, &e1, "fork1 with own rename");
    check(&t, ff, &eff, "fork of fork");
    check(&t, main, &exp, "main");
}

#[test]
fn archived_and_restored_entities_and_cleared_values() {
    let (mut t, main) = T::new();
    let mut exp = Exp::default();
    let ids = seed(&mut t, main, 8, &mut [&mut exp]);
    // Archive through the API (type null), clear names, remove a link.
    for e in &ids[..4] {
        t.s.archive_entity(main, "a", e).unwrap();
        exp.apply(&[Op::clear(e.as_str(), TYPE_KEY)]);
    }
    t.w(
        main,
        "a",
        vec![
            Op::clear(ids[4].as_str(), "name"),
            Op::clear(ids[5].as_str(), "n"),
        ],
        &mut [&mut exp],
    );
    t.w(
        main,
        "a",
        vec![Op::unlink("root", ids[6].as_str(), "child")],
        &mut [&mut exp],
    );
    let archived_fork = t.s.fork(main, "while archived").unwrap();
    let mut exp_af = exp.clone();
    check(&t, main, &exp, "archived");
    // Restore: type back (a different one for some), names back, link back
    // with a new sort; then archive one again.
    for (i, e) in ids[..4].iter().enumerate() {
        let ty = if i % 2 == 0 { "task" } else { "note" };
        t.w(
            main,
            "a",
            vec![Op::set(e.as_str(), TYPE_KEY, ty)],
            &mut [&mut exp],
        );
    }
    t.w(
        main,
        "a",
        vec![Op::set(ids[4].as_str(), "name", "alpha")],
        &mut [&mut exp],
    );
    t.w(
        main,
        "a",
        vec![Op::link(
            "root",
            ids[6].as_str(),
            "child",
            SortOrder::Explicit(-1.0),
        )],
        &mut [&mut exp],
    );
    t.s.archive_entity(main, "a", &ids[2]).unwrap();
    exp.apply(&[Op::clear(ids[2].as_str(), TYPE_KEY)]);
    check(&t, main, &exp, "restored");
    check(&t, archived_fork, &exp_af, "fork taken while archived");
    // The fork restores on its own.
    t.w(
        archived_fork,
        "b",
        vec![Op::set(ids[0].as_str(), TYPE_KEY, "note")],
        &mut [&mut exp_af],
    );
    check(&t, archived_fork, &exp_af, "fork restores");
}

#[test]
fn three_branch_stacks_with_frozen_and_open_ranges() {
    // base <- mid (fork: frozen) <- top (open range on mid); renames in all
    // three after each was made, and on base after mid's fork.
    let (mut t, base) = T::new();
    let mut eb = Exp::default();
    let ids = seed(&mut t, base, 10, &mut [&mut eb]);
    for (i, e) in ids.iter().enumerate() {
        t.rename(base, e, NAMES[(i + 2) % NAMES.len()], &mut [&mut eb]);
    }
    let mid = t.s.fork(base, "mid").unwrap();
    let mut mid_view = eb.clone(); // frozen copy of base
    let mut mid_own = Exp::default();
    for (i, e) in ids.iter().enumerate().filter(|(i, _)| i % 2 == 0) {
        t.rename(base, e, "base later", &mut [&mut eb]);
        t.rename(mid, e, NAMES[(i + 4) % NAMES.len()], &mut [&mut mid_own]);
    }
    let top =
        t.s.create_branch("top", &[ParentRef::new(mid, SeqRange::new(1, None))])
            .unwrap();
    let mut top_own = Exp::default();
    for (i, e) in ids.iter().enumerate().filter(|(i, _)| i % 3 == 0) {
        t.rename(top, e, NAMES[(i + 1) % NAMES.len()], &mut [&mut top_own]);
    }
    // mid goes on renaming: top follows (open range), base does not.
    t.rename(mid, &ids[1], "gamma", &mut [&mut mid_own]);
    t.rename(mid, &ids[3], Value::Null, &mut [&mut mid_own]);
    t.w(
        mid,
        "a",
        vec![Op::clear(ids[5].as_str(), TYPE_KEY)],
        &mut [&mut mid_own],
    );
    t.rename(base, &ids[1], "base only", &mut [&mut eb]);
    t.rename(base, &ids[7], "base after fork", &mut [&mut eb]); // no other layer renames it
    mid_view = mid_view.under(&mid_own);
    check(&t, base, &eb, "base");
    check(&t, mid, &mid_view, "mid");
    check(&t, top, &mid_view.under(&top_own), "top");
    // A fourth branch pulls a frozen range of base (up to now) and an
    // open range of top on top of it.
    let head = t.s.branch(base).unwrap().head;
    let side =
        t.s.create_branch(
            "side",
            &[
                ParentRef::new(base, SeqRange::up_to(head)),
                ParentRef::new(top, SeqRange::new(1, None)),
            ],
        )
        .unwrap();
    let base_at_head = eb.clone();
    t.rename(base, &ids[2], "after side", &mut [&mut eb]);
    t.rename(top, &ids[2], "top after side", &mut [&mut top_own]);
    let top_view = mid_view.under(&top_own);
    // The open range of top chains through mid (R26); base is reached twice
    // (frozen up to its head here, and under mid at the fork) and sits at its
    // first place with the greater range. So: base at its head, then mid's
    // own events, then top's.
    let side_exp = base_at_head.under(&mid_own).under(&top_own);
    check(&t, top, &top_view, "top later");
    check(&t, side, &side_exp, "side");
}

#[test]
fn merge_after_renames_puts_the_branch_over_main() {
    let (mut t, main) = T::new();
    let mut em = Exp::default();
    let ids = seed(&mut t, main, 8, &mut [&mut em]);
    let agent = t.s.fork(main, "agent").unwrap();
    let mut ea_view = em.clone();
    let mut ea_own = Exp::default();
    for (i, e) in ids.iter().enumerate() {
        if i % 2 == 0 {
            t.rename(
                agent,
                e,
                NAMES[(i + 3) % NAMES.len()],
                &mut [&mut ea_own, &mut ea_view],
            );
        }
        if i % 3 == 0 {
            t.rename(main, e, "main edit", &mut [&mut em]);
        }
    }
    t.w(
        agent,
        "b",
        vec![Op::clear(ids[1].as_str(), TYPE_KEY)],
        &mut [&mut ea_own, &mut ea_view],
    );
    check(&t, agent, &ea_view, "agent before merge");
    t.s.merge(main, agent).unwrap();
    em = em.under(&ea_own);
    check(&t, main, &em, "main after merge");
    // Main's later writes sit on top; later agent work needs a new merge.
    t.rename(main, &ids[0], "after merge", &mut [&mut em]);
    t.rename(
        agent,
        &ids[2],
        "agent later",
        &mut [&mut ea_own, &mut ea_view],
    );
    check(&t, main, &em, "main later");
    check(&t, agent, &ea_view, "agent later");
}

#[test]
fn undo_of_renames_brings_the_old_names_back_everywhere() {
    let (mut t, main) = T::new();
    let mut em = Exp::default();
    let ids = seed(&mut t, main, 6, &mut [&mut em]);
    // A follower with an open range sees main live, including undo.
    let follower =
        t.s.create_branch("follower", &[ParentRef::new(main, SeqRange::new(1, None))])
            .unwrap();
    let before = em.clone();
    t.rename(main, &ids[0], "beta", &mut [&mut em]);
    t.rename(main, &ids[1], "alpha", &mut [&mut em]);
    t.w(
        main,
        "a",
        vec![
            Op::clear(ids[2].as_str(), TYPE_KEY),
            Op::set(ids[3].as_str(), "name", "gamma"),
        ],
        &mut [&mut em],
    );
    check(&t, main, &em, "renamed");
    check(&t, follower, &em, "follower renamed");
    let mut steps = 0;
    while t.s.undo(main, "a").unwrap().is_some() {
        steps += 1;
        if steps == 3 {
            break;
        }
    }
    assert_eq!(steps, 3);
    check(&t, main, &before, "undone");
    check(&t, follower, &before, "follower undone");
    // Rename again after the undo: the new event wins over the restored row.
    let mut em = before.clone();
    t.rename(main, &ids[0], "delta", &mut [&mut em]);
    check(&t, main, &em, "renamed after undo");
}

#[test]
fn pages_with_history_never_skip_or_repeat() {
    // 150 entities with 3 to 7 renames each, read in pages of 1, 2, 3 and
    // 1000 in every order on main, a frozen fork and an open stack.
    let (mut t, main) = T::new();
    let mut em = Exp::default();
    let ids = seed(&mut t, main, 150, &mut [&mut em]);
    let fork_at = 3;
    let mut fork = None;
    for round in 0..7usize {
        let ops: Vec<Op> = ids
            .iter()
            .enumerate()
            .filter(|(i, _)| round < 3 + i % 5)
            .map(|(i, e)| Op::set(e.as_str(), "name", NAMES[(i * 3 + round) % NAMES.len()]))
            .collect();
        t.w(main, "a", ops, &mut [&mut em]);
        if round == fork_at {
            fork = Some((t.s.fork(main, "f").unwrap(), em.clone()));
        }
    }
    let (f, ef) = fork.unwrap();
    let top =
        t.s.create_branch("top", &[ParentRef::new(f, SeqRange::new(1, None))])
            .unwrap();
    let mut top_own = Exp::default();
    let ops: Vec<Op> = ids
        .iter()
        .step_by(4)
        .map(|e| Op::set(e.as_str(), "name", "alpha"))
        .collect();
    t.w(top, "a", ops, &mut [&mut top_own]);
    check(&t, main, &em, "main");
    check(&t, f, &ef, "fork");
    check(&t, top, &ef.under(&top_own), "top");
}
