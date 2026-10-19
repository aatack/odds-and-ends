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
        for order in [FindOrder::EntityId, FindOrder::Insertion] {
            let one = s
                .find(
                    b,
                    &f,
                    FindOptions {
                        limit: 10_000,
                        order,
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
                            order,
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
