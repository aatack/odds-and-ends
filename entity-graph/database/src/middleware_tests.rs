//! Tests for write middleware (see `src/middleware.rs`). They cover the
//! transform / add / drop / drop-all / reject paths, ordering, the context
//! fields, and that middleware runs on forks, merges, history reads, undo and
//! archive/restore. The last test runs a few seeds of the differential model
//! test with an identity middleware: it must change nothing.

use std::sync::{Arc, Mutex};

use crate::*;

/// A fresh in-memory store with a fixed clock and one empty branch.
fn new_store() -> (Store, BranchId) {
    let mut store = Store::open_in_memory().unwrap();
    store.set_clock(|| 1000);
    let branch = store.create_branch("main", &[]).unwrap();
    (store, branch)
}

fn val(store: &Store, branch: BranchId, entity: &str, key: &str) -> Option<Value> {
    store.value(branch, entity, key).unwrap()
}

/// Recorded [`WriteContext`] fields: branch, author, timestamp, now, head.
type Seen = Arc<Mutex<Option<(BranchId, String, Timestamp, Timestamp, Seq)>>>;

/// A middleware as a struct (not a closure): lowercase every text value.
struct Lowercase;

impl WriteMiddleware for Lowercase {
    fn on_write(&self, _ctx: &WriteContext, ops: &mut Vec<Op>) -> Result<()> {
        for op in ops.iter_mut() {
            if let Op::SetValue {
                value: Value::Text(t),
                ..
            } = op
            {
                *t = t.to_lowercase();
            }
        }
        Ok(())
    }
}

#[test]
fn transform_rewrites_values() {
    let (mut store, b) = new_store();
    store.add_middleware(Lowercase); // registered as a struct impl
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "name", "HELLO")]))
        .unwrap();
    assert_eq!(
        val(&store, b, "e", "name"),
        Some(Value::Text("hello".into()))
    );
}

#[test]
fn adds_ops() {
    let (mut store, b) = new_store();
    // A closure middleware, written with the plain argument annotations.
    store.add_middleware(|ctx: &WriteContext, ops: &mut Vec<Op>| {
        let mut touched: Vec<String> = Vec::new();
        for op in ops.iter() {
            if let Op::SetValue { entity, .. } = op {
                if !touched.contains(entity) {
                    touched.push(entity.clone());
                }
            }
        }
        for e in touched {
            ops.push(Op::set(e, "updated_by", ctx.author));
        }
        Ok(())
    });
    store
        .write(WriteBatch::new(b, "alice", vec![Op::set("e", "x", 1)]))
        .unwrap();
    assert_eq!(val(&store, b, "e", "x"), Some(Value::Int(1)));
    assert_eq!(
        val(&store, b, "e", "updated_by"),
        Some(Value::Text("alice".into()))
    );
}

#[test]
fn drops_ops() {
    let (mut store, b) = new_store();
    store.add_middleware(|_: &WriteContext, ops: &mut Vec<Op>| {
        ops.retain(|op| !matches!(op, Op::SetValue { key, .. } if key == "secret"));
        Ok(())
    });
    store
        .write(WriteBatch::new(
            b,
            "a",
            vec![Op::set("e", "secret", 1), Op::set("e", "public", 2)],
        ))
        .unwrap();
    assert_eq!(val(&store, b, "e", "secret"), None);
    assert_eq!(val(&store, b, "e", "public"), Some(Value::Int(2)));
}

#[test]
fn drops_all_ops_writes_nothing() {
    let (mut store, b) = new_store();
    // One real batch first (seq 1).
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "v", 1)]))
        .unwrap();
    assert_eq!(store.branch(b).unwrap().head, 1);

    store.add_middleware(|_: &WriteContext, ops: &mut Vec<Op>| {
        ops.clear();
        Ok(())
    });
    let r = store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "v", 2)]))
        .unwrap();
    // Empty range: first_seq = head + 1, last_seq = head. Nothing written.
    assert_eq!((r.first_seq, r.last_seq), (2, 1));
    assert_eq!(store.branch(b).unwrap().head, 1);
    assert_eq!(val(&store, b, "e", "v"), Some(Value::Int(1)));

    // No `batches` row was inserted, so undo removes the previous real batch.
    let undone = store.undo(b, "a").unwrap().expect("something to undo");
    assert_eq!(undone.removed, (1, 1));
    assert_eq!(val(&store, b, "e", "v"), None);

    // A following normal write gets the next seq (seqs are never reused).
    store.clear_middleware();
    let r = store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "v", 3)]))
        .unwrap();
    assert_eq!((r.first_seq, r.last_seq), (2, 2));
}

#[test]
fn rejects_the_batch_atomically() {
    let (mut store, b) = new_store();
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "v", 1)]))
        .unwrap();

    store.add_middleware(|_: &WriteContext, _: &mut Vec<Op>| Err(Error::Rejected("nope".into())));
    let err = store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "v", 2)]))
        .unwrap_err();
    assert!(matches!(err, Error::Rejected(_)));
    // Store and head unchanged.
    assert_eq!(store.branch(b).unwrap().head, 1);
    assert_eq!(val(&store, b, "e", "v"), Some(Value::Int(1)));

    // A following normal write gets head + 1.
    store.clear_middleware();
    let r = store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "v", 3)]))
        .unwrap();
    assert_eq!((r.first_seq, r.last_seq), (2, 2));
}

/// Append `ch` to the value of key `s`.
fn append_to_s(ch: char) -> impl Fn(&WriteContext, &mut Vec<Op>) -> Result<()> + Send + Sync {
    move |_ctx, ops| {
        for op in ops.iter_mut() {
            if let Op::SetValue {
                key,
                value: Value::Text(t),
                ..
            } = op
            {
                if key == "s" {
                    t.push(ch);
                }
            }
        }
        Ok(())
    }
}

#[test]
fn order_is_observable() {
    let run = |first: char, second: char| {
        let (mut store, b) = new_store();
        store.add_middleware(append_to_s(first));
        store.add_middleware(append_to_s(second));
        assert_eq!(store.middleware_count(), 2);
        store
            .write(WriteBatch::new(b, "a", vec![Op::set("e", "s", "x")]))
            .unwrap();
        match val(&store, b, "e", "s") {
            Some(Value::Text(t)) => t,
            other => panic!("unexpected {other:?}"),
        }
    };
    // Each middleware sees the previous one's output, so order shows.
    assert_eq!(run('a', 'b'), "xab");
    assert_eq!(run('b', 'a'), "xba");
}

#[test]
fn context_fields_are_correct() {
    let (mut store, b) = new_store();
    // First write fixes last_ts = 1000 and head = 1.
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "x", 1)]))
        .unwrap();

    let seen: Seen = Arc::new(Mutex::new(None));
    let sink = seen.clone();
    store.add_middleware(move |ctx: &WriteContext, _: &mut Vec<Op>| {
        *sink.lock().unwrap() = Some((
            ctx.branch,
            ctx.author.to_owned(),
            ctx.timestamp,
            ctx.now,
            ctx.head,
        ));
        Ok(())
    });

    // A timestamp below last_ts is clamped up to it.
    store
        .write(WriteBatch {
            branch: b,
            author: "bob".into(),
            timestamp: Some(500),
            ops: vec![Op::set("e", "y", 2)],
        })
        .unwrap();

    let (branch, author, ts, now, head) = seen.lock().unwrap().take().unwrap();
    assert_eq!(branch, b);
    assert_eq!(author, "bob");
    assert_eq!(ts, 1000); // clamped (not 500)
    assert_eq!(now, 1000);
    assert_eq!(head, 1);
}

#[test]
fn applies_on_a_forked_branch() {
    let (mut store, b) = new_store();
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "type", "T")]))
        .unwrap();
    let f = store.fork(b, "f").unwrap();
    store.add_middleware(Lowercase);
    store
        .write(WriteBatch::new(f, "a", vec![Op::set("e", "name", "HELLO")]))
        .unwrap();
    assert_eq!(
        val(&store, f, "e", "name"),
        Some(Value::Text("hello".into()))
    );
    // The parent's value is seen through the fork, unchanged.
    assert_eq!(val(&store, f, "e", "type"), Some(Value::Text("T".into())));
    // The fork is isolated from main.
    assert_eq!(val(&store, b, "e", "name"), None);
}

#[test]
fn transformed_events_are_seen_through_a_merge() {
    let (mut store, _main) = new_store();
    let a = store.create_branch("a", &[]).unwrap();
    let src = store.create_branch("b", &[]).unwrap();
    store.add_middleware(Lowercase);
    store
        .write(WriteBatch::new(src, "a", vec![Op::set("e", "k", "XYZ")]))
        .unwrap();
    store.merge(a, src).unwrap();
    assert_eq!(val(&store, a, "e", "k"), Some(Value::Text("xyz".into())));
}

#[test]
fn a_frozen_range_sees_the_transformed_events() {
    let (mut store, b) = new_store();
    store.add_middleware(Lowercase);
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "type", "T")]))
        .unwrap();
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "name", "FIRST")]))
        .unwrap();
    let f = store.fork(b, "f").unwrap(); // frozen here
    store
        .write(WriteBatch::new(
            b,
            "a",
            vec![Op::set("e", "name", "SECOND")],
        ))
        .unwrap();
    // The fork reads the transformed value as of its frozen bound.
    assert_eq!(
        val(&store, f, "e", "name"),
        Some(Value::Text("first".into()))
    );
    assert_eq!(
        val(&store, b, "e", "name"),
        Some(Value::Text("second".into()))
    );
}

#[test]
fn undo_removes_a_transformed_batch_as_one_unit() {
    let (mut store, b) = new_store();
    // Stamp an extra op; the batch is then two events (seq 1 and 2).
    store.add_middleware(|_: &WriteContext, ops: &mut Vec<Op>| {
        ops.push(Op::set("e", "updated_by", "a"));
        Ok(())
    });
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "x", 1)]))
        .unwrap();
    assert_eq!(store.branch(b).unwrap().head, 2);

    let undone = store.undo(b, "a").unwrap().expect("something to undo");
    assert_eq!(undone.removed, (1, 2));
    assert_eq!(val(&store, b, "e", "x"), None);
    assert_eq!(val(&store, b, "e", "updated_by"), None);
}

#[test]
fn archive_and_restore_go_through_middleware() {
    // Reject path: a middleware that refuses to archive (clear the type).
    let (mut store, b) = new_store();
    store
        .write(WriteBatch::new(b, "a", vec![Op::set("e", "type", "task")]))
        .unwrap();
    store.add_middleware(|_: &WriteContext, ops: &mut Vec<Op>| {
        if ops
            .iter()
            .any(|op| matches!(op, Op::SetValue { key, value, .. } if key == TYPE_KEY && value.is_null()))
        {
            return Err(Error::Rejected("archiving is not allowed".into()));
        }
        Ok(())
    });
    let err = store.archive_entity(b, "a", "e").unwrap_err();
    assert!(matches!(err, Error::Rejected(_)));
    assert!(!store.is_archived(b, &["e"]).unwrap()["e"]);

    // Transform path: restore goes through a value-rewriting middleware.
    let (mut store, b) = new_store();
    store.add_middleware(Lowercase); // also affects the written type
    store.restore_entity(b, "a", "e", "TASK").unwrap();
    assert_eq!(
        val(&store, b, "e", "type"),
        Some(Value::Text("task".into()))
    );
}

#[test]
fn identity_middleware_leaves_the_model_results_unchanged() {
    crate::model_tests::run_with_identity_middleware();
}
