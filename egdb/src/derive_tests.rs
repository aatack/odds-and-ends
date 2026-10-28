//! Derived entities: an entity whose `derives_from` value names a source takes
//! the source's values first, then its own on top.

use std::collections::BTreeMap;

use crate::*;

fn store() -> (Store, BranchId) {
    let mut store = Store::open_in_memory().unwrap();
    let main = store.create_branch("main", &[]).unwrap();
    (store, main)
}

fn write(store: &mut Store, branch: BranchId, ops: Vec<Op>) {
    store.write(WriteBatch::new(branch, "alex", ops)).unwrap();
}

fn values(store: &Store, branch: BranchId, id: &str) -> BTreeMap<String, Value> {
    store.values(branch, &[id]).unwrap().remove(id).unwrap()
}

fn text(s: &str) -> Value {
    Value::Text(s.into())
}

#[test]
fn a_derived_entity_takes_its_sources_values_and_its_own_win() {
    let (mut store, main) = store();
    write(
        &mut store,
        main,
        vec![
            Op::set("snooze", "type", "script"),
            Op::set("snooze", "source", "return {}"),
            Op::set("snooze", "description", "Snooze"),
            Op::set("snooze", "wake_at", "+1h"),
            Op::link("snooze", "other", "child", SortOrder::Timestamp),
            Op::set("other", "type", "note"),
            Op::set("mine", "type", "script-copy"),
            Op::set("mine", DERIVES_FROM_KEY, "snooze"),
            Op::set("mine", "wake_at", "+2h"),
            Op::set("mine", "description", Value::Null),
        ],
    );
    let v = values(&store, main, "mine");
    assert_eq!(v.get("source"), Some(&text("return {}")));
    assert_eq!(v.get("wake_at"), Some(&text("+2h")), "own values win");
    assert_eq!(
        v.get("description"),
        None,
        "an own null clears an inherited value"
    );
    assert_eq!(
        v.get("type"),
        Some(&text("script-copy")),
        "the type is never inherited"
    );
    assert_eq!(v.get(DERIVES_FROM_KEY), Some(&text("snooze")));
    assert_eq!(
        store.value(main, "mine", "source").unwrap(),
        Some(text("return {}"))
    );
    assert_eq!(
        store.value(main, "mine", "wake_at").unwrap(),
        Some(text("+2h"))
    );
    assert_eq!(store.value(main, "mine", "description").unwrap(), None);
    // Links are not inherited.
    assert!(store.children(main, "mine", "child").unwrap().is_empty());
    // The event of an inherited value is the source's event.
    let ev = store.value_events(main, &["mine", "snooze"]).unwrap();
    assert_eq!(ev["mine"]["source"], ev["snooze"]["source"]);
    // The source reads as before.
    assert_eq!(
        values(&store, main, "snooze").get("wake_at"),
        Some(&text("+1h"))
    );
}

#[test]
fn a_fix_to_the_source_shows_in_its_copies() {
    let (mut store, main) = store();
    write(
        &mut store,
        main,
        vec![
            Op::set("snooze", "type", "script"),
            Op::set("snooze", "source", "v1"),
            Op::set("a", "type", "script"),
            Op::set("a", DERIVES_FROM_KEY, "snooze"),
        ],
    );
    write(&mut store, main, vec![Op::set("snooze", "source", "v2")]);
    assert_eq!(store.value(main, "a", "source").unwrap(), Some(text("v2")));
}

#[test]
fn chains_take_values_from_every_source_nearest_first() {
    let (mut store, main) = store();
    write(
        &mut store,
        main,
        vec![
            Op::set("c", "type", "script"),
            Op::set("c", "x", 1i64),
            Op::set("c", "y", 1i64),
            Op::set("c", "z", 1i64),
            Op::set("b", "type", "script"),
            Op::set("b", DERIVES_FROM_KEY, "c"),
            Op::set("b", "y", 2i64),
            Op::set("b", "z", 2i64),
            Op::set("a", "type", "script"),
            Op::set("a", DERIVES_FROM_KEY, "b"),
            Op::set("a", "z", 3i64),
        ],
    );
    let v = values(&store, main, "a");
    assert_eq!(v.get("x"), Some(&Value::Int(1)));
    assert_eq!(v.get("y"), Some(&Value::Int(2)));
    assert_eq!(v.get("z"), Some(&Value::Int(3)));
    assert_eq!(v.get(DERIVES_FROM_KEY), Some(&text("b")));
    // One batch read gives each its own chain.
    let all = store.values(main, &["a", "b", "c"]).unwrap();
    assert_eq!(all["b"].get("z"), Some(&Value::Int(2)));
    assert_eq!(all["b"].get("x"), Some(&Value::Int(1)));
    assert_eq!(all["c"].get(DERIVES_FROM_KEY), None);
}

#[test]
fn a_two_entity_cycle_stops_where_it_repeats() {
    let (mut store, main) = store();
    write(
        &mut store,
        main,
        vec![
            Op::set("a", "type", "script"),
            Op::set("a", DERIVES_FROM_KEY, "b"),
            Op::set("a", "x", "from a"),
            Op::set("a", "only_a", true),
            Op::set("b", "type", "script"),
            Op::set("b", DERIVES_FROM_KEY, "a"),
            Op::set("b", "x", "from b"),
            Op::set("b", "only_b", true),
        ],
    );
    let all = store.values(main, &["a", "b"]).unwrap();
    // a takes b, and b's source (a) is cut: a's own values still win.
    assert_eq!(all["a"].get("x"), Some(&text("from a")));
    assert_eq!(all["a"].get("only_b"), Some(&Value::Bool(true)));
    assert_eq!(all["a"].get(DERIVES_FROM_KEY), Some(&text("b")));
    assert_eq!(all["b"].get("x"), Some(&text("from b")));
    assert_eq!(all["b"].get("only_a"), Some(&Value::Bool(true)));
    assert_eq!(
        store.value(main, "a", "only_b").unwrap(),
        Some(Value::Bool(true))
    );
}

#[test]
fn a_self_reference_inherits_nothing() {
    let (mut store, main) = store();
    write(
        &mut store,
        main,
        vec![
            Op::set("a", "type", "script"),
            Op::set("a", DERIVES_FROM_KEY, "a"),
            Op::set("a", "x", 1i64),
        ],
    );
    let v = values(&store, main, "a");
    assert_eq!(v.len(), 3);
    assert_eq!(v.get("x"), Some(&Value::Int(1)));
    assert_eq!(store.value(main, "a", "x").unwrap(), Some(Value::Int(1)));
}

#[test]
fn a_cycle_behind_a_chain_ends_too() {
    let (mut store, main) = store();
    write(
        &mut store,
        main,
        vec![
            Op::set("a", DERIVES_FROM_KEY, "b"),
            Op::set("b", DERIVES_FROM_KEY, "c"),
            Op::set("c", DERIVES_FROM_KEY, "b"),
            Op::set("c", "x", 1i64),
        ],
    );
    assert_eq!(store.value(main, "a", "x").unwrap(), Some(Value::Int(1)));
}

#[test]
fn unknown_and_non_text_sources_inherit_nothing() {
    let (mut store, main) = store();
    write(
        &mut store,
        main,
        vec![
            Op::set("a", "type", "script"),
            Op::set("a", DERIVES_FROM_KEY, "nowhere"),
            Op::set("b", "type", "script"),
            Op::set("b", DERIVES_FROM_KEY, 7i64),
        ],
    );
    assert_eq!(values(&store, main, "a").len(), 2);
    assert_eq!(values(&store, main, "b").len(), 2);
}

#[test]
fn derivation_reads_each_branchs_own_view() {
    let (mut store, main) = store();
    write(
        &mut store,
        main,
        vec![
            Op::set("s", "type", "script"),
            Op::set("s", "source", "v1"),
            Op::set("a", "type", "script"),
            Op::set("a", DERIVES_FROM_KEY, "s"),
        ],
    );
    let fork = store.fork(main, "agent").unwrap();
    write(&mut store, fork, vec![Op::set("s", "source", "v2")]);
    assert_eq!(store.value(main, "a", "source").unwrap(), Some(text("v1")));
    assert_eq!(store.value(fork, "a", "source").unwrap(), Some(text("v2")));
    // Clearing the flag ends the derivation.
    write(
        &mut store,
        main,
        vec![Op::set("a", DERIVES_FROM_KEY, Value::Null)],
    );
    assert_eq!(store.value(main, "a", "source").unwrap(), None);
}
