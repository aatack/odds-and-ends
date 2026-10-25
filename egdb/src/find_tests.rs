//! Unit tests of `find`: filter semantics, views, archiving, paging.

use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

use crate::*;

fn store() -> (Store, BranchId) {
    let mut s = Store::open_in_memory().unwrap();
    let clock = Arc::new(AtomicI64::new(1_700_000_000_000));
    s.set_clock(move || clock.fetch_add(1, Ordering::SeqCst));
    let main = s.create_branch("main", &[]).unwrap();
    (s, main)
}

fn w(s: &mut Store, b: BranchId, ops: Vec<Op>) {
    s.write(WriteBatch::new(b, "t", ops)).unwrap();
}

fn ids(s: &Store, b: BranchId, f: &Filter) -> Vec<String> {
    s.find(b, f, FindOptions::default())
        .unwrap()
        .rows
        .into_iter()
        .map(|r| r.id)
        .collect()
}

fn k(s: &str) -> String {
    s.to_owned()
}

/// a: Int 1, b: Float 1.0, c: Bool true, d: Text "1", e: Float 2.5,
/// f: Int -3, g: Text "Hello World", h: no `n`, i: Json [1].
fn sample() -> (Store, BranchId) {
    let (mut s, main) = store();
    let mut ops = Vec::new();
    for e in ["a", "b", "c", "d", "e", "f", "g", "h", "i"] {
        ops.push(Op::set(e, TYPE_KEY, "note"));
    }
    ops.extend([
        Op::set("a", "n", 1),
        Op::set("b", "n", 1.0),
        Op::set("c", "n", true),
        Op::set("d", "n", "1"),
        Op::set("e", "n", 2.5),
        Op::set("f", "n", -3),
        Op::set("g", "n", "Hello World"),
        Op::set("i", "n", Value::Json(serde_json::json!([1]))),
    ]);
    w(&mut s, main, ops);
    (s, main)
}

#[test]
fn eq_is_typed_and_null_means_absent() {
    let (s, b) = sample();
    assert_eq!(ids(&s, b, &Filter::eq("n", 1)), ["a"]);
    assert_eq!(ids(&s, b, &Filter::eq("n", 1.0)), ["b"]);
    assert_eq!(ids(&s, b, &Filter::eq("n", true)), ["c"]);
    assert_eq!(ids(&s, b, &Filter::eq("n", "1")), ["d"]);
    assert_eq!(
        ids(
            &s,
            b,
            &Filter::Eq(k("n"), Value::Json(serde_json::json!([1])))
        ),
        ["i"]
    );
    assert_eq!(ids(&s, b, &Filter::Eq(k("n"), Value::Null)), ["h"]);
    assert_eq!(ids(&s, b, &Filter::IsNull(k("n"))), ["h"]);
    assert_eq!(
        ids(&s, b, &Filter::Ne(k("n"), 1.into())),
        ["b", "c", "d", "e", "f", "g", "h", "i"]
    );
    assert_eq!(
        ids(
            &s,
            b,
            &Filter::In(k("n"), vec![1.into(), "1".into(), Value::Null])
        ),
        ["a", "d", "h"]
    );
}

#[test]
fn ranges_compare_numbers_with_numbers_and_text_with_text() {
    let (s, b) = sample();
    assert_eq!(ids(&s, b, &Filter::Gt(k("n"), 1.into())), ["e"]);
    assert_eq!(ids(&s, b, &Filter::Ge(k("n"), 1.into())), ["a", "b", "e"]);
    assert_eq!(ids(&s, b, &Filter::Lt(k("n"), 1.0.into())), ["f"]);
    assert_eq!(
        ids(&s, b, &Filter::Le(k("n"), 2.5.into())),
        ["a", "b", "e", "f"]
    );
    assert_eq!(ids(&s, b, &Filter::Gt(k("n"), "1".into())), ["g"]);
    assert_eq!(ids(&s, b, &Filter::Ge(k("n"), "1".into())), ["d", "g"]);
    assert_eq!(ids(&s, b, &Filter::Lt(k("n"), "Z".into())), ["d", "g"]);
    assert!(matches!(
        s.find(b, &Filter::Gt(k("n"), true.into()), FindOptions::default()),
        Err(Error::Invalid(_))
    ));
}

#[test]
fn text_filters_are_case_sensitive_except_the_ascii_variant() {
    let (s, b) = sample();
    assert_eq!(ids(&s, b, &Filter::Contains(k("n"), k("World"))), ["g"]);
    assert!(ids(&s, b, &Filter::Contains(k("n"), k("world"))).is_empty());
    assert_eq!(
        ids(&s, b, &Filter::ContainsIgnoreCase(k("n"), k("wORLD"))),
        ["g"]
    );
    assert_eq!(ids(&s, b, &Filter::Prefix(k("n"), k("Hell"))), ["g"]);
    assert!(ids(&s, b, &Filter::Prefix(k("n"), k("ello"))).is_empty());
    // Contains on a number never matches: it is text only.
    assert_eq!(ids(&s, b, &Filter::Contains(k("n"), k("1"))), ["d"]);
}

#[test]
fn prefix_handles_non_ascii_and_the_last_code_point() {
    let (mut s, b) = store();
    let names = ["é", "éa", "é\u{10FFFF}z", "ê", "e"];
    let mut ops = Vec::new();
    for (i, n) in names.iter().enumerate() {
        let e = format!("x{i}");
        ops.push(Op::set(e.as_str(), TYPE_KEY, "note"));
        ops.push(Op::set(e.as_str(), "name", *n));
    }
    w(&mut s, b, ops);
    assert_eq!(
        ids(&s, b, &Filter::Prefix(k("name"), k("é"))),
        ["x0", "x1", "x2"]
    );
    assert_eq!(
        ids(&s, b, &Filter::Prefix(k("name"), k("é\u{10FFFF}"))),
        ["x2"]
    );
}

#[test]
fn and_or_not_and_custom() {
    let (s, b) = sample();
    let f = Filter::And(vec![
        Filter::Ge(k("n"), 1.into()),
        Filter::Not(Box::new(Filter::eq("n", 1.0))),
    ]);
    assert_eq!(ids(&s, b, &f), ["a", "e"]);
    let f = Filter::Or(vec![
        Filter::eq("n", -3),
        Filter::Contains(k("n"), k("Hello")),
    ]);
    assert_eq!(ids(&s, b, &f), ["f", "g"]);
    assert_eq!(ids(&s, b, &Filter::And(vec![])).len(), 9);
    assert!(ids(&s, b, &Filter::Or(vec![])).is_empty());
    let f = Filter::custom(|id, v| id > "c" && v.contains_key("n"));
    assert_eq!(ids(&s, b, &f), ["d", "e", "f", "g", "i"]);
}

#[test]
fn find_reads_the_final_value_through_the_stack() {
    let (mut s, main) = store();
    w(
        &mut s,
        main,
        vec![
            Op::set("x", TYPE_KEY, "task"),
            Op::set("x", "p", 5),
            Op::set("y", TYPE_KEY, "task"),
            Op::set("y", "p", 5),
        ],
    );
    let child = s
        .create_branch("child", &[ParentRef::new(main, SeqRange::ALL)])
        .unwrap();
    w(
        &mut s,
        child,
        vec![Op::set("x", "p", 1), Op::clear("y", "p")],
    );
    // The parent's old value is in the index, but the child overrode it.
    assert!(ids(&s, child, &Filter::eq("p", 5)).is_empty());
    assert_eq!(ids(&s, child, &Filter::eq("p", 1)), ["x"]);
    assert_eq!(ids(&s, child, &Filter::IsNull(k("p"))), ["y"]);
    assert_eq!(ids(&s, main, &Filter::eq("p", 5)), ["x", "y"]);
}

#[test]
fn a_frozen_fork_finds_values_that_are_now_in_history() {
    let (mut s, main) = store();
    w(
        &mut s,
        main,
        vec![Op::set("x", TYPE_KEY, "task"), Op::set("x", "p", 1)],
    );
    let fork = s.fork(main, "fork").unwrap();
    w(
        &mut s,
        main,
        vec![
            Op::set("x", "p", 2),
            Op::set("z", TYPE_KEY, "task"),
            Op::set("z", "p", 1),
        ],
    );
    assert_eq!(ids(&s, fork, &Filter::eq("p", 1)), ["x"]);
    assert_eq!(ids(&s, fork, &Filter::Gt(k("p"), 0.into())), ["x"]);
    assert!(ids(&s, fork, &Filter::eq("p", 2)).is_empty());
    assert_eq!(ids(&s, main, &Filter::eq("p", 1)), ["z"]);
    assert_eq!(ids(&s, main, &Filter::eq("p", 2)), ["x"]);
}

#[test]
fn archived_and_untyped_entities_are_hidden_unless_asked_for() {
    let (mut s, b) = store();
    w(
        &mut s,
        b,
        vec![
            Op::set("a", TYPE_KEY, "task"),
            Op::set("a", "p", 1),
            Op::set("gone", TYPE_KEY, "task"),
            Op::set("gone", "p", 1),
            Op::set("raw", "p", 1),
            Op::link("a", "linked", "child", SortOrder::Timestamp),
        ],
    );
    s.archive_entity(b, "t", "gone").unwrap();
    assert_eq!(ids(&s, b, &Filter::eq("p", 1)), ["a"]);
    let all = |f: &Filter| -> Vec<String> {
        s.find(
            b,
            f,
            FindOptions {
                include_archived: true,
                ..Default::default()
            },
        )
        .unwrap()
        .rows
        .into_iter()
        .map(|r| r.id)
        .collect()
    };
    assert_eq!(all(&Filter::eq("p", 1)), ["a", "gone", "raw"]);
    // An entity that exists only through a link has no values at all.
    assert_eq!(all(&Filter::IsNull(k("p"))), ["linked"]);
    assert_eq!(all(&Filter::And(vec![])), ["a", "gone", "linked", "raw"]);
    assert_eq!(ids(&s, b, &Filter::eq(TYPE_KEY, "task")), ["a"]);
}

#[test]
fn pages_concatenate_to_one_big_find_in_both_orders() {
    let (mut s, b) = store();
    let mut ops = Vec::new();
    for i in 0..500u32 {
        let e = format!("{:08x}", i.wrapping_mul(2_654_435_761));
        ops.push(Op::set(
            e.as_str(),
            TYPE_KEY,
            if i % 3 == 0 { "task" } else { "note" },
        ));
        ops.push(Op::set(e.as_str(), "n", i as i64));
    }
    w(&mut s, b, ops);
    for f in [
        Filter::eq(TYPE_KEY, "task"),
        Filter::Gt(k("n"), 400.into()),
        Filter::And(vec![]),
        Filter::Not(Box::new(Filter::eq(TYPE_KEY, "task"))),
    ] {
        for order in [
            FindOrder::EntityId,
            FindOrder::Insertion,
            FindOrder::ValueAsc(TYPE_KEY.into()),
            FindOrder::ValueDesc(TYPE_KEY.into()),
        ] {
            let one = s
                .find(
                    b,
                    &f,
                    FindOptions {
                        limit: 10_000,
                        order: order.clone(),
                        ..Default::default()
                    },
                )
                .unwrap();
            assert!(one.next.is_none());
            let mut got = Vec::new();
            let mut cursor = None;
            loop {
                let page = s
                    .find(
                        b,
                        &f,
                        FindOptions {
                            limit: 37,
                            cursor,
                            order: order.clone(),
                            values: false,
                            ..Default::default()
                        },
                    )
                    .unwrap();
                got.extend(page.rows.into_iter().map(|r| r.id));
                cursor = page.next;
                if cursor.is_none() {
                    break;
                }
            }
            let want: Vec<String> = one.rows.iter().map(|r| r.id.clone()).collect();
            assert_eq!(got, want, "{f:?} {order:?}");
            if order == FindOrder::EntityId {
                let mut sorted = want.clone();
                sorted.sort();
                assert_eq!(want, sorted);
            }
        }
    }
    // A scan limit returns early, with a cursor, and loses nothing.
    let f = Filter::eq("n", 499);
    let mut cursor = None;
    let mut found = Vec::new();
    let mut pages = 0;
    loop {
        let page = s
            .find(
                b,
                &Filter::And(vec![f.clone(), Filter::custom(|_, _| true)]),
                FindOptions {
                    scan_limit: 1,
                    cursor,
                    ..Default::default()
                },
            )
            .unwrap();
        pages += 1;
        found.extend(page.rows.into_iter().map(|r| r.id));
        cursor = page.next;
        if cursor.is_none() {
            break;
        }
    }
    assert_eq!(found.len(), 1);
    assert!(pages <= 2);
}

/// Non-ASCII names for the `ContainsIgnoreCase` tests: German, Turkish,
/// Greek, combining marks, a NUL byte, a 4-byte code point.
const FOLD_NAMES: [&str; 22] = [
    "Ä",
    "ä",
    "Straße",
    "STRASSE",
    "straẞe",
    "İstanbul",
    "istanbul",
    "ISTANBUL",
    "ıI",
    "Café",
    "Cafe\u{301}",
    "CAFÉ",
    "ΣΊΣΥΦΟΣ",
    "σίσυφος",
    "ﬁle",
    "FILE",
    "a\0Budget",
    "Ｆｕｌｌ",
    "x😀y",
    "KELVIN \u{212A}",
    "kelvin k",
    "Ǆ ǅ ǆ",
];
const FOLD_NEEDLES: [&str; 24] = [
    "ä", "Ä", "ß", "ss", "SS", "ẞ", "i", "I", "İ", "ı", "é", "É", "e\u{301}", "e", "σ", "Σ", "ς",
    "fi", "ﬁ", "budget", "ｆ", "😀", "k", "ǅ",
];

/// What `ContainsIgnoreCase` means (DESIGN.md, "find"): fold ASCII `A`-`Z`
/// only, then a byte substring test. No Unicode case folding and no
/// normalisation.
fn fold_reference(name: &str, needle: &str) -> bool {
    name.to_ascii_lowercase()
        .contains(&needle.to_ascii_lowercase())
}

#[test]
fn contains_function_agrees_with_instr_lower_on_non_ascii_text() {
    // The SQL that `ContainsIgnoreCase` used before the Rust function, the
    // function itself, and the reference, on every pair.
    let conn = rusqlite::Connection::open_in_memory().unwrap();
    crate::find::register_functions(&conn).unwrap();
    for h in FOLD_NAMES {
        for n in FOLD_NEEDLES.iter().chain(&["", "xyz", "a\0b"]) {
            let low = n.to_ascii_lowercase();
            let (old, new): (bool, bool) = conn
                .query_row(
                    "SELECT instr(lower(?1), ?2) > 0, egdb_contains(?1, ?2, 1)",
                    rusqlite::params![h, low],
                    |r| Ok((r.get(0)?, r.get(1)?)),
                )
                .unwrap();
            let want = fold_reference(h, n);
            assert_eq!(old, want, "instr(lower({h:?}), {low:?})");
            assert_eq!(new, want, "egdb_contains({h:?}, {low:?})");
            let cs: (bool, bool) = conn
                .query_row(
                    "SELECT instr(?1, ?2) > 0, egdb_contains(?1, ?2, 0)",
                    rusqlite::params![h, n],
                    |r| Ok((r.get(0)?, r.get(1)?)),
                )
                .unwrap();
            assert_eq!(
                cs,
                (h.contains(n), h.contains(n)),
                "case-sensitive {h:?} {n:?}"
            );
        }
    }
    // Not text: never a match.
    let v: bool = conn
        .query_row(
            "SELECT egdb_contains(12, '1', 1) OR egdb_contains(NULL, '', 1)",
            [],
            |r| r.get(0),
        )
        .unwrap();
    assert!(!v);
}

#[test]
fn contains_ignore_case_folds_ascii_only_on_every_path() {
    // Current values, a frozen fork that reads them from history, and both
    // SQL forms (`NO_CONTAINS_FN`); probe and sorted plans.
    use crate::find::knobs::{NO_CONTAINS_FN, PROBE};
    let (mut s, b) = store();
    let mut ops = Vec::new();
    for (i, n) in FOLD_NAMES.iter().enumerate() {
        let e = format!("x{i:02}");
        ops.push(Op::set(e.as_str(), TYPE_KEY, "note"));
        ops.push(Op::set(e.as_str(), "name", *n));
    }
    w(&mut s, b, ops);
    let fork = s.fork(b, "f").unwrap();
    let mut ops = Vec::new();
    for i in 0..FOLD_NAMES.len() {
        ops.push(Op::set(format!("x{i:02}").as_str(), "name", "renamed"));
    }
    w(&mut s, b, ops);
    for n in FOLD_NEEDLES {
        let want: Vec<String> = FOLD_NAMES
            .iter()
            .enumerate()
            .filter(|(_, h)| fold_reference(h, n))
            .map(|(i, _)| format!("x{i:02}"))
            .collect();
        let f = Filter::ContainsIgnoreCase(k("name"), k(n));
        for fn_knob in [0, 1] {
            for probe in [1, 2] {
                NO_CONTAINS_FN.store(fn_knob, Ordering::Relaxed);
                PROBE.store(probe, Ordering::Relaxed);
                let got = ids(&s, fork, &f);
                PROBE.store(0, Ordering::Relaxed);
                NO_CONTAINS_FN.store(0, Ordering::Relaxed);
                assert_eq!(got, want, "fork {n:?} fn_knob {fn_knob} probe {probe}");
            }
        }
        assert!(
            ids(&s, b, &f).is_empty() || fold_reference("renamed", n),
            "{n:?}"
        );
    }
    // Spot checks of the documented meaning.
    let one = |n: &str| ids(&s, fork, &Filter::ContainsIgnoreCase(k("name"), k(n)));
    assert_eq!(one("ä"), ["x01"]); // "Ä" (x00) is not folded
    assert_eq!(one("SS"), ["x03"]); // "ß" and "ẞ" are not "ss"
    assert_eq!(one("I"), ["x06", "x07", "x08", "x15", "x19", "x20"]); // not "İ" (x05)
    assert_eq!(one("İ"), ["x05"]);
    assert_eq!(one("é"), ["x09"]); // not "É" or "e" + U+0301
    assert_eq!(one("e\u{301}"), ["x10"]);
    assert_eq!(one("σ"), ["x13"]);
    assert_eq!(one("BUDGET"), ["x16"]); // after a NUL byte
}

/// All pages of `f` on `b`, concatenated.
fn all_pages(
    s: &Store,
    b: BranchId,
    f: &Filter,
    limit: usize,
    scan_limit: usize,
    order: &FindOrder,
) -> Vec<String> {
    let mut got = Vec::new();
    let mut cursor = None;
    loop {
        let page = s
            .find(
                b,
                f,
                FindOptions {
                    limit,
                    cursor,
                    order: order.clone(),
                    scan_limit,
                    ..Default::default()
                },
            )
            .unwrap();
        got.extend(page.rows.into_iter().map(|r| r.id));
        cursor = page.next;
        if cursor.is_none() {
            return got;
        }
    }
}

#[test]
fn sparse_overfetch_pages_are_the_same_as_one_chunk_pages() {
    // A stack where most candidates of the parent layer are overridden by
    // the child (rejected after the roll-up), so pages need several chunks.
    use crate::find::knobs::{NO_OVERFETCH, PROBE};
    let (mut s, base) = store();
    let mut ops = Vec::new();
    for i in 0..3000u32 {
        let e = format!("{:08x}", i.wrapping_mul(2_654_435_761));
        ops.push(Op::set(e.as_str(), TYPE_KEY, "note"));
        ops.push(Op::set(e.as_str(), "name", format!("Hit {i}")));
    }
    w(&mut s, base, ops);
    let top = s
        .create_branch("top", &[ParentRef::new(base, SeqRange::new(1, None))])
        .unwrap();
    let mut ops = Vec::new();
    for i in (0..3000u32).filter(|i| i % 10 != 0) {
        let e = format!("{:08x}", i.wrapping_mul(2_654_435_761));
        ops.push(Op::set(e.as_str(), "name", format!("miss {i}")));
    }
    w(&mut s, top, ops);
    let mut want: Vec<String> = (0..3000u32)
        .filter(|i| i % 10 == 0)
        .map(|i| format!("{:08x}", i.wrapping_mul(2_654_435_761)))
        .collect();
    want.sort();
    for f in [
        Filter::ContainsIgnoreCase(k("name"), k("hit")),
        Filter::Contains(k("name"), k("Hit")),
        Filter::Prefix(k("name"), k("Hit")),
    ] {
        for order in [FindOrder::EntityId, FindOrder::Insertion] {
            for (limit, scan_limit) in [
                (1, usize::MAX),
                (7, usize::MAX),
                (100, usize::MAX),
                (1000, usize::MAX),
                (50, 70),
                (1000, 3),
            ] {
                for knob in [0, 1] {
                    PROBE.store(2, Ordering::Relaxed);
                    NO_OVERFETCH.store(knob, Ordering::Relaxed);
                    let mut got = all_pages(&s, top, &f, limit, scan_limit, &order);
                    NO_OVERFETCH.store(0, Ordering::Relaxed);
                    PROBE.store(0, Ordering::Relaxed);
                    let n = got.len();
                    got.sort();
                    got.dedup();
                    assert_eq!(
                        got.len(),
                        n,
                        "duplicates: {f:?} {order:?} {limit} {scan_limit} {knob}"
                    );
                    assert_eq!(
                        got, want,
                        "{f:?} {order:?} limit {limit} scan {scan_limit} knob {knob}"
                    );
                }
            }
        }
    }
}

#[test]
fn lone_eq_gives_the_same_pages_at_every_dense_cap() {
    // About 8% `doc` on a 3-branch stack with history (frozen fork, open
    // pull) and retypes in every layer, so the count falls between the
    // general cap and the `Eq` cap for small and large pages.
    use crate::find::knobs::{EQ_CAP, PROBE};
    let (mut s, base) = store();
    let id = |i: u32| format!("{:08x}", i.wrapping_mul(2_654_435_761));
    let ty = |i: u32| match i % 25 {
        0 | 1 => "doc",
        2..=12 => "task",
        _ => "note",
    };
    let ops = (0..2000u32)
        .map(|i| Op::set(id(i).as_str(), TYPE_KEY, ty(i)))
        .collect();
    w(&mut s, base, ops);
    let ops = (0..2000u32)
        .filter(|i| i % 7 == 0)
        .map(|i| Op::set(id(i).as_str(), TYPE_KEY, "doc"))
        .collect();
    w(&mut s, base, ops);
    let mid = s.fork(base, "mid").unwrap();
    let ops = (0..2000u32)
        .filter(|i| i % 5 == 0)
        .map(|i| Op::set(id(i).as_str(), TYPE_KEY, "task"))
        .collect();
    w(&mut s, base, ops); // after the fork: mid must not see these
    let ops = (0..2000u32)
        .filter(|i| i % 11 == 0)
        .map(|i| Op::clear(id(i).as_str(), TYPE_KEY))
        .collect();
    w(&mut s, mid, ops);
    let top = s
        .create_branch("top", &[ParentRef::new(mid, SeqRange::new(1, None))])
        .unwrap();
    let ops = (0..2000u32)
        .filter(|i| i % 13 == 0)
        .map(|i| Op::set(id(i).as_str(), TYPE_KEY, "doc"))
        .collect();
    w(&mut s, top, ops);
    let f = Filter::eq(TYPE_KEY, "doc");
    for b in [base, mid, top] {
        let want: Vec<String> = {
            let ids: Vec<String> = (0..2000u32).map(id).collect();
            let refs: Vec<&str> = ids.iter().map(String::as_str).collect();
            let vals = s.values(b, &refs).unwrap();
            let mut v: Vec<String> = ids
                .iter()
                .filter(|e| vals[e.as_str()].get(TYPE_KEY) == Some(&Value::Text("doc".into())))
                .cloned()
                .collect();
            v.sort();
            v
        };
        assert!(want.len() > 100, "{}", want.len());
        for order in [FindOrder::EntityId, FindOrder::Insertion] {
            for limit in [1, 5, 50, 1000] {
                for (cap, probe) in [(0, 0), (1, 0), (50, 0), (0, 1), (0, 2)] {
                    EQ_CAP.store(cap, Ordering::Relaxed);
                    PROBE.store(probe, Ordering::Relaxed);
                    let mut got = all_pages(&s, b, &f, limit, usize::MAX, &order);
                    EQ_CAP.store(0, Ordering::Relaxed);
                    PROBE.store(0, Ordering::Relaxed);
                    let n = got.len();
                    got.sort();
                    got.dedup();
                    assert_eq!(got.len(), n, "duplicates {order:?} {limit} {cap} {probe}");
                    assert_eq!(got, want, "{order:?} limit {limit} cap {cap} probe {probe}");
                }
            }
        }
    }
}
