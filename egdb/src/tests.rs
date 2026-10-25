use std::collections::HashSet;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

use crate::*;

const T0: Timestamp = 1_700_000_000_000;

/// A store with a manual clock, and a `main` branch.
struct Fixture {
    store: Store,
    clock: Arc<AtomicI64>,
    main: BranchId,
}

impl Fixture {
    fn new() -> Self {
        let mut store = Store::open_in_memory().unwrap();
        let clock = Arc::new(AtomicI64::new(T0));
        let c = clock.clone();
        store.set_clock(move || c.load(Ordering::SeqCst));
        let main = store.create_branch("main", &[]).unwrap();
        Self { store, clock, main }
    }

    fn tick(&self, ms: i64) {
        self.clock.fetch_add(ms, Ordering::SeqCst);
    }

    fn write(&mut self, branch: BranchId, author: &str, ops: Vec<Op>) -> WriteResult {
        self.tick(1);
        self.store
            .write(WriteBatch::new(branch, author, ops))
            .unwrap()
    }

    fn count(&self, table: &str) -> i64 {
        self.store
            .conn
            .query_row(&format!("SELECT COUNT(*) FROM {table}"), [], |r| r.get(0))
            .unwrap()
    }

    fn text(&self, branch: BranchId, entity: &str, key: &str) -> Option<String> {
        match self.store.value(branch, entity, key).unwrap() {
            Some(Value::Text(s)) => Some(s),
            None => None,
            other => panic!("not text: {other:?}"),
        }
    }

    /// Children without the type filter, so tests need not write types.
    fn all_children(&self, branch: BranchId, entity: &str, link_type: &str) -> Vec<Link> {
        self.all_links(branch, entity, Direction::Outbound, link_type)
    }

    fn all_links(&self, b: BranchId, entity: &str, d: Direction, ty: &str) -> Vec<Link> {
        self.store
            .links_including_archived(b, &[entity], &[(ty, d)])
            .unwrap()
            .remove(entity)
            .unwrap()
            .remove(&(ty.to_owned(), d))
            .unwrap()
    }

    fn exists(&self, b: BranchId, entity: &str) -> bool {
        self.store.exists(b, &[entity]).unwrap()[entity]
    }

    fn is_archived(&self, b: BranchId, entity: &str) -> bool {
        self.store.is_archived(b, &[entity]).unwrap()[entity]
    }

    fn values(&self, b: BranchId, entity: &str) -> std::collections::BTreeMap<String, Value> {
        self.store
            .values(b, &[entity])
            .unwrap()
            .remove(entity)
            .unwrap()
    }

    fn child_ids(&self, branch: BranchId, entity: &str, link_type: &str) -> Vec<String> {
        self.all_children(branch, entity, link_type)
            .into_iter()
            .map(|l| l.destination)
            .collect()
    }
}

// --- Schema ---------------------------------------------------------------

#[test]
fn migrates_a_version_1_file_to_the_covering_history_index() {
    let dir = std::env::temp_dir().join(format!("egdb-{}", uuid::Uuid::now_v7()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("store.db");
    {
        let conn = rusqlite::Connection::open(&path).unwrap();
        conn.execute_batch(crate::schema::V1).unwrap();
        conn.pragma_update(None, "user_version", 1).unwrap();
    }
    let store = Store::open(&path).unwrap();
    assert_eq!(store.schema_version().unwrap(), SCHEMA_VERSION);
    let cols: Vec<String> = store
        .conn
        .prepare("SELECT name FROM pragma_index_info('value_history_value')")
        .unwrap()
        .query_map([], |r| r.get(0))
        .unwrap()
        .collect::<rusqlite::Result<_>>()
        .unwrap();
    assert_eq!(cols, ["bid", "key", "value", "eid", "seq", "superseded_by"]);
    let cols: Vec<String> = store
        .conn
        .prepare("SELECT name FROM pragma_index_info('value_current_value')")
        .unwrap()
        .query_map([], |r| r.get(0))
        .unwrap()
        .collect::<rusqlite::Result<_>>()
        .unwrap();
    assert_eq!(cols, ["bid", "key", "value", "eid", "seq"]);
}

#[test]
fn migrates_a_new_file_and_refuses_a_newer_one() {
    let dir = std::env::temp_dir().join(format!("egdb-{}", uuid::Uuid::now_v7()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join("store.db");
    {
        let store = Store::open(&path).unwrap();
        assert_eq!(store.schema_version().unwrap(), SCHEMA_VERSION);
    }
    // Opening again applies nothing and keeps the version.
    let store = Store::open(&path).unwrap();
    assert_eq!(store.schema_version().unwrap(), SCHEMA_VERSION);
    store.conn.pragma_update(None, "user_version", 99).unwrap();
    drop(store);
    assert!(matches!(
        Store::open(&path),
        Err(Error::SchemaTooNew { found: 99, .. })
    ));
    std::fs::remove_dir_all(dir).unwrap();
}

// --- Writes ---------------------------------------------------------------

#[test]
fn sequence_ids_are_per_branch_and_count_events() {
    let mut f = Fixture::new();
    let other = f.store.create_branch("other", &[]).unwrap();
    let a = f.write(
        f.main,
        "alex",
        vec![Op::set("n", "text", "a"), Op::set("n", "x", 1)],
    );
    let b = f.write(other, "alex", vec![Op::set("n", "text", "b")]);
    let c = f.write(f.main, "alex", vec![Op::set("n", "text", "c")]);
    assert_eq!((a.first_seq, a.last_seq), (1, 2));
    assert_eq!((b.first_seq, b.last_seq), (1, 1));
    assert_eq!((c.first_seq, c.last_seq), (3, 3));
    assert_eq!(f.store.branch(f.main).unwrap().head, 3);
}

#[test]
fn superseded_events_move_to_history() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("n", "text", "one")]);
    f.write(
        f.main,
        "alex",
        vec![Op::set("n", "text", "two"), Op::set("n", "text", "three")],
    );
    assert_eq!(f.count("value_current"), 1);
    assert_eq!(f.count("value_history"), 2);
    let superseded: Vec<(i64, i64)> = f
        .store
        .conn
        .prepare("SELECT seq, superseded_by FROM value_history ORDER BY seq")
        .unwrap()
        .query_map([], |r| Ok((r.get(0)?, r.get(1)?)))
        .unwrap()
        .collect::<rusqlite::Result<_>>()
        .unwrap();
    assert_eq!(superseded, vec![(1, 2), (2, 3)]);
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("three"));

    f.write(
        f.main,
        "alex",
        vec![Op::link("n", "c", "child", SortOrder::Timestamp)],
    );
    f.write(f.main, "alex", vec![Op::unlink("n", "c", "child")]);
    assert_eq!(f.count("link_current"), 1);
    assert_eq!(f.count("link_history"), 1);
}

#[test]
fn null_clears_a_value() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![Op::set("n", "text", "hi"), Op::set("n", "x", 2)],
    );
    f.write(f.main, "alex", vec![Op::clear("n", "text")]);
    let values = f.values(f.main, "n");
    assert_eq!(values.len(), 1);
    assert_eq!(values["x"], Value::Int(2));
    assert_eq!(f.store.value(f.main, "n", "text").unwrap(), None);
}

#[test]
fn typed_values_round_trip() {
    let mut f = Fixture::new();
    let json = serde_json::json!({"a": [1, 2]});
    f.write(
        f.main,
        "alex",
        vec![
            Op::set("n", "b", true),
            Op::set("n", "i", -7),
            Op::set("n", "f", 1.5),
            Op::set("n", "t", "text"),
            Op::set("n", "j", Value::Json(json.clone())),
        ],
    );
    let v = f.values(f.main, "n");
    assert_eq!(v["b"], Value::Bool(true));
    assert_eq!(v["i"], Value::Int(-7));
    assert_eq!(v["f"], Value::Float(1.5));
    assert_eq!(v["t"], Value::Text("text".into()));
    assert_eq!(v["j"], Value::Json(json));
}

#[test]
fn rejects_bad_batches() {
    let mut f = Fixture::new();
    let empty = f.store.write(WriteBatch::new(f.main, "alex", vec![]));
    assert!(matches!(empty, Err(Error::Invalid(_))));
    let nan = f.store.write(WriteBatch::new(
        f.main,
        "alex",
        vec![Op::link("a", "b", "child", SortOrder::Explicit(f64::NAN))],
    ));
    assert!(matches!(nan, Err(Error::Invalid(_))));
    let no_type = f.store.write(WriteBatch::new(
        f.main,
        "alex",
        vec![Op::link("a", "b", "", SortOrder::Timestamp)],
    ));
    assert!(matches!(no_type, Err(Error::Invalid(_))));
    // Nothing was written.
    assert_eq!(f.store.branch(f.main).unwrap().head, 0);
}

#[test]
fn timestamps_never_go_down_within_a_branch() {
    let mut f = Fixture::new();
    let a = f.write(f.main, "alex", vec![Op::set("n", "x", 1)]);
    let mut batch = WriteBatch::new(f.main, "alex", vec![Op::set("n", "x", 2)]);
    batch.timestamp = Some(a.timestamp - 10_000);
    let b = f.store.write(batch).unwrap();
    assert_eq!(b.timestamp, a.timestamp);
    assert_eq!(
        f.store.value(f.main, "n", "x").unwrap(),
        Some(Value::Int(2))
    );
}

// --- Links ----------------------------------------------------------------

#[test]
fn children_sort_by_timestamp_by_default() {
    let mut f = Fixture::new();
    for child in ["c", "a", "b"] {
        f.write(
            f.main,
            "alex",
            vec![Op::link("p", child, "child", SortOrder::Timestamp)],
        );
    }
    assert_eq!(f.child_ids(f.main, "p", "child"), ["c", "a", "b"]);
    let links = f.all_children(f.main, "p", "child");
    // The default is stored as the event's own timestamp.
    assert_eq!(links[0].sort, links[0].event.timestamp as f64);
}

#[test]
fn explicit_sort_orders_and_removed_links() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![
            Op::link("p", "a", "child", SortOrder::Explicit(2.0)),
            Op::link("p", "b", "child", SortOrder::Explicit(1.0)),
            Op::link("p", "c", "child", SortOrder::Explicit(3.0)),
        ],
    );
    assert_eq!(f.child_ids(f.main, "p", "child"), ["b", "a", "c"]);
    // The last event per (source, destination, type) wins.
    f.write(
        f.main,
        "alex",
        vec![Op::link("p", "b", "child", SortOrder::Explicit(9.0))],
    );
    f.write(f.main, "alex", vec![Op::unlink("p", "a", "child")]);
    assert_eq!(f.child_ids(f.main, "p", "child"), ["c", "b"]);
    // Re-adding a removed link brings it back.
    f.write(
        f.main,
        "alex",
        vec![Op::link("p", "a", "child", SortOrder::Explicit(0.5))],
    );
    assert_eq!(f.child_ids(f.main, "p", "child"), ["a", "c", "b"]);
}

#[test]
fn links_store_one_nullable_sort_and_a_default_readd_moves_to_the_end() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![Op::link("p", "a", "child", SortOrder::Timestamp)],
    );
    f.write(
        f.main,
        "alex",
        vec![Op::link("p", "b", "child", SortOrder::Timestamp)],
    );
    f.write(f.main, "alex", vec![Op::unlink("p", "a", "child")]);
    let stored: Option<f64> = f
        .store
        .conn
        .query_row(
            "SELECT l.sort FROM link_current l JOIN entities e ON e.eid = l.destination
             WHERE e.ext_id = 'a'",
            [],
            |r| r.get(0),
        )
        .unwrap();
    assert_eq!(stored, None);
    // Re-adding with the default takes the time of the new event.
    let readd = f.write(
        f.main,
        "alex",
        vec![Op::link("p", "a", "child", SortOrder::Timestamp)],
    );
    assert_eq!(f.child_ids(f.main, "p", "child"), ["b", "a"]);
    assert_eq!(
        f.all_children(f.main, "p", "child")[1].sort,
        readd.timestamp as f64
    );
}

#[test]
fn links_group_by_type_and_read_both_ways() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![
            Op::link("p", "a", "child", SortOrder::Timestamp),
            Op::link("p", "b", "ref", SortOrder::Timestamp),
            Op::link("q", "a", "child", SortOrder::Timestamp),
        ],
    );
    let out = f
        .store
        .links_including_archived(
            f.main,
            &["p", "q"],
            &[("child", Direction::Outbound), ("ref", Direction::Outbound)],
        )
        .unwrap();
    let sizes = |e: &str| out[e].values().map(Vec::len).collect::<Vec<_>>();
    assert_eq!(sizes("p"), [1, 1]);
    assert_eq!(sizes("q"), [1, 0]);
    assert_eq!(f.child_ids(f.main, "p", "ref"), ["b"]);
    let parents: Vec<String> = f
        .all_links(f.main, "a", Direction::Inbound, "child")
        .into_iter()
        .map(|l| l.source)
        .collect();
    assert_eq!(parents, ["p", "q"]);
    assert!(f.all_children(f.main, "a", "child").is_empty());
}

// --- Entities -------------------------------------------------------------

#[test]
fn an_entity_exists_when_it_has_events_and_archiving_clears_its_type() {
    let mut f = Fixture::new();
    assert!(!f.exists(f.main, "n"));
    f.write(
        f.main,
        "alex",
        vec![Op::link("p", "n", "child", SortOrder::Timestamp)],
    );
    assert!(f.exists(f.main, "n"));
    assert!(f.exists(f.main, "p"));

    // No type yet: not archived.
    assert!(!f.is_archived(f.main, "n"));
    f.write(f.main, "alex", vec![Op::set("n", TYPE_KEY, "task")]);
    f.tick(1);
    f.store.archive_entity(f.main, "alex", "n").unwrap();
    assert!(f.is_archived(f.main, "n"));
    assert_eq!(f.store.value(f.main, "n", TYPE_KEY).unwrap(), None);
    assert!(f.exists(f.main, "n"));
    // The caller supplies the type to restore.
    f.tick(1);
    f.store.restore_entity(f.main, "alex", "n", "task").unwrap();
    assert!(!f.is_archived(f.main, "n"));
    assert_eq!(f.text(f.main, "n", TYPE_KEY).as_deref(), Some("task"));
}

#[test]
fn link_reads_show_only_entities_with_a_type() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![
            Op::set("live", TYPE_KEY, "task"),
            Op::set("gone", TYPE_KEY, "task"),
            Op::link("p", "live", "child", SortOrder::Timestamp),
            Op::link("p", "gone", "child", SortOrder::Timestamp),
            Op::link("p", "untyped", "child", SortOrder::Timestamp),
        ],
    );
    f.store.archive_entity(f.main, "alex", "gone").unwrap();
    let ids = |links: Vec<Link>| links.into_iter().map(|l| l.destination).collect::<Vec<_>>();
    assert_eq!(
        ids(f.store.children(f.main, "p", "child").unwrap()),
        ["live"]
    );
    assert_eq!(
        f.child_ids(f.main, "p", "child"),
        ["gone", "live", "untyped"]
    );
    // Parents: the source needs a type too.
    assert!(f.store.parents(f.main, "live", "child").unwrap().is_empty());
    f.write(f.main, "alex", vec![Op::set("p", TYPE_KEY, "list")]);
    assert_eq!(f.store.parents(f.main, "live", "child").unwrap().len(), 1);
    // A type from a parent branch counts; the winning event decides.
    let agent = f.store.fork(f.main, "agent").unwrap();
    assert_eq!(
        ids(f.store.children(agent, "p", "child").unwrap()),
        ["live"]
    );
    f.store
        .restore_entity(agent, "bot", "gone", "task")
        .unwrap();
    assert_eq!(
        ids(f.store.children(agent, "p", "child").unwrap()),
        ["gone", "live"]
    );
    assert_eq!(
        ids(f.store.children(f.main, "p", "child").unwrap()),
        ["live"]
    );
}

#[test]
fn the_type_index_serves_a_search_by_type() {
    let f = Fixture::new();
    let plan: String = f
        .store
        .conn
        .query_row(
            "EXPLAIN QUERY PLAN SELECT eid FROM value_current
             WHERE bid = 1 AND key = 'type' AND value = 'task'",
            [],
            |r| r.get(3),
        )
        .unwrap();
    assert!(plan.contains("value_current_value"), "{plan}");
}

// --- Branches -------------------------------------------------------------

#[test]
fn a_fork_does_not_see_later_parent_events() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("n", "text", "before")]);
    let agent = f.store.fork(f.main, "agent").unwrap();
    assert_eq!(
        f.store.branch(agent).unwrap().parents,
        vec![ParentRef::new(f.main, SeqRange::up_to(1))]
    );
    f.write(
        f.main,
        "alex",
        vec![Op::set("n", "text", "after"), Op::set("m", "text", "new")],
    );
    // The child reads the frozen parent through history.
    assert_eq!(f.text(agent, "n", "text").as_deref(), Some("before"));
    assert!(!f.exists(agent, "m"));
    // The child's own writes do not reach the parent.
    f.write(agent, "bot", vec![Op::set("k", "text", "mine")]);
    assert_eq!(f.store.branch(agent).unwrap().head, 1);
    assert!(!f.exists(f.main, "k"));
}

#[test]
fn a_fork_of_a_fork_copies_the_whole_view_frozen() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("a", "text", "main")]);
    let one = f.store.fork(f.main, "one").unwrap();
    f.write(one, "bot", vec![Op::set("b", "text", "one")]);
    let two = f.store.fork(one, "two").unwrap();
    f.write(f.main, "alex", vec![Op::set("a", "text", "main later")]);
    f.write(one, "bot", vec![Op::set("b", "text", "one later")]);
    assert_eq!(f.text(two, "a", "text").as_deref(), Some("main"));
    assert_eq!(f.text(two, "b", "text").as_deref(), Some("one"));
}

#[test]
fn merge_puts_the_branch_on_top_of_what_main_had() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![Op::set("n", "text", "v1"), Op::set("n", "x", 1)],
    );
    let agent = f.store.fork(f.main, "agent").unwrap();
    f.write(
        agent,
        "bot",
        vec![
            Op::set("n", "text", "agent"),
            Op::link("n", "c", "child", SortOrder::Timestamp),
        ],
    );
    f.write(f.main, "alex", vec![Op::set("n", "x", 2)]);

    f.store.merge(f.main, agent).unwrap();
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("agent"));
    assert_eq!(
        f.store.value(f.main, "n", "x").unwrap(),
        Some(Value::Int(2))
    );
    assert_eq!(f.child_ids(f.main, "n", "child"), ["c"]);
    // Main's events so far are frozen under the agent's; the agent's stale
    // copy of main is left out, so main's concurrent `x = 2` survives.
    assert_eq!(
        f.store.branch(f.main).unwrap().parents,
        vec![
            ParentRef::new(f.main, SeqRange::up_to(3)),
            ParentRef::new(agent, SeqRange::up_to(2)),
        ]
    );
    // Main's writes after the merge sit on top again.
    f.write(f.main, "alex", vec![Op::set("n", "text", "main again")]);
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("main again"));
    f.write(f.main, "alex", vec![Op::set("n", "text", "agent")]);

    // The merge is frozen: later agent work needs another merge.
    f.write(agent, "bot", vec![Op::set("n", "text", "agent 2")]);
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("agent"));
    f.store.merge(f.main, agent).unwrap();
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("agent 2"));
    // The older agent entry is covered by the new one and is dropped.
    assert_eq!(
        f.store.branch(f.main).unwrap().parents,
        vec![
            ParentRef::new(f.main, SeqRange::up_to(5)),
            ParentRef::new(agent, SeqRange::up_to(3)),
        ]
    );
}

#[test]
fn pull_reads_a_range_and_open_ranges_follow_growth() {
    let mut f = Fixture::new();
    let feed = f.store.create_branch("feed", &[]).unwrap();
    f.write(feed, "slack", vec![Op::set("m1", "text", "one")]);
    f.write(feed, "slack", vec![Op::set("m2", "text", "two")]);
    f.write(feed, "slack", vec![Op::set("m3", "text", "three")]);

    f.store
        .pull(f.main, feed, SeqRange::new(2, Some(2)))
        .unwrap();
    assert!(!f.exists(f.main, "m1"));
    assert!(f.exists(f.main, "m2"));
    assert!(!f.exists(f.main, "m3"));

    f.store.pull(f.main, feed, SeqRange::from(3)).unwrap();
    f.write(feed, "slack", vec![Op::set("m4", "text", "four")]);
    assert!(f.exists(f.main, "m4"));
    assert_eq!(
        f.store.branch(f.main).unwrap().parents,
        vec![ParentRef::new(feed, SeqRange::from(2))]
    );
}

#[test]
fn parent_list_order_decides_and_own_events_sit_on_top() {
    let mut f = Fixture::new();
    let a = f.store.create_branch("a", &[]).unwrap();
    let b = f.store.create_branch("b", &[]).unwrap();
    // B writes first in time; order across branches ignores time.
    f.write(b, "bot", vec![Op::set("n", "text", "b")]);
    f.write(a, "bot", vec![Op::set("n", "text", "a")]);

    f.store
        .set_parents(
            f.main,
            &[
                ParentRef::new(a, SeqRange::ALL),
                ParentRef::new(b, SeqRange::ALL),
            ],
        )
        .unwrap();
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("b"));
    f.store
        .set_parents(
            f.main,
            &[
                ParentRef::new(b, SeqRange::ALL),
                ParentRef::new(a, SeqRange::ALL),
            ],
        )
        .unwrap();
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("a"));

    // The branch's own events win over every parent.
    f.write(f.main, "alex", vec![Op::set("n", "text", "own")]);
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("own"));
    // A self entry places own events inside the stack; later own events stay on top.
    f.store
        .set_parents(
            f.main,
            &[
                ParentRef::new(f.main, SeqRange::up_to(1)),
                ParentRef::new(a, SeqRange::ALL),
            ],
        )
        .unwrap();
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("a"));
    f.write(f.main, "alex", vec![Op::set("n", "text", "own 2")]);
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("own 2"));
}

#[test]
fn rebase_retargets_the_parent_list() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("n", "text", "v1")]);
    let agent = f.store.fork(f.main, "agent").unwrap();
    f.write(f.main, "alex", vec![Op::set("n", "text", "v2")]);
    assert_eq!(f.text(agent, "n", "text").as_deref(), Some("v1"));
    f.store
        .set_parents(agent, &[ParentRef::new(f.main, SeqRange::up_to(2))])
        .unwrap();
    assert_eq!(f.text(agent, "n", "text").as_deref(), Some("v2"));
    let bad = f
        .store
        .set_parents(agent, &[ParentRef::new(f.main, SeqRange::new(3, Some(2)))]);
    assert!(matches!(bad, Err(Error::Invalid(_))));
}

#[test]
fn frozen_and_merged_branches_accept_writes_but_archived_ones_do_not() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("n", "x", 1)]);
    let agent = f.store.fork(f.main, "agent").unwrap();
    f.store.merge(f.main, agent).unwrap();
    f.write(agent, "bot", vec![Op::set("n", "x", 2)]);

    f.store.archive_branch(agent, true).unwrap();
    assert!(f.store.branch(agent).unwrap().archived);
    let refused = f
        .store
        .write(WriteBatch::new(agent, "bot", vec![Op::set("n", "x", 3)]));
    assert!(matches!(refused, Err(Error::BranchArchived(_))));
    // Archived branches can still be read.
    assert_eq!(f.store.value(agent, "n", "x").unwrap(), Some(Value::Int(2)));
    assert_eq!(f.store.branches().unwrap().len(), 2);
}

// --- Undo -----------------------------------------------------------------

#[test]
fn undo_removes_events_and_moves_the_prior_rows_back() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![
            Op::set("n", "text", "one"),
            Op::link("n", "c", "child", SortOrder::Explicit(1.0)),
        ],
    );
    f.write(
        f.main,
        "alex",
        vec![
            Op::set("n", "text", "two"),
            Op::set("n", "text", "three"),
            Op::set("n", "x", 1),
            Op::unlink("n", "c", "child"),
            Op::link("n", "d", "child", SortOrder::Timestamp),
        ],
    );
    assert_eq!(f.count("value_history"), 2);

    let out = f.store.undo(f.main, "alex").unwrap().unwrap();
    assert_eq!(out.removed, (3, 7));
    assert_eq!(out.events, 5);
    assert_eq!(out.restored, 2);
    // Back to the first batch, with nothing left in history.
    assert_eq!(f.count("value_current"), 1);
    assert_eq!(f.count("value_history"), 0);
    assert_eq!(f.count("link_current"), 1);
    assert_eq!(f.count("link_history"), 0);
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("one"));
    assert_eq!(f.store.value(f.main, "n", "x").unwrap(), None);
    assert_eq!(f.child_ids(f.main, "n", "child"), ["c"]);

    // Sequence IDs are not reused: the next event leaves a gap.
    assert_eq!(f.store.branch(f.main).unwrap().head, 7);
    let next = f.write(f.main, "alex", vec![Op::set("n", "x", 2)]);
    assert_eq!(next.first_seq, 8);
}

#[test]
fn undo_steps_back_one_batch_at_a_time() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("n", "text", "one")]);
    f.write(f.main, "alex", vec![Op::set("n", "text", "two")]);
    f.store.undo(f.main, "alex").unwrap().unwrap();
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("one"));
    f.store.undo(f.main, "alex").unwrap().unwrap();
    assert_eq!(f.store.value(f.main, "n", "text").unwrap(), None);
    assert!(!f.exists(f.main, "n"));
    assert_eq!(f.store.undo(f.main, "alex").unwrap(), None);
    assert_eq!(f.count("batches"), 0);
}

#[test]
fn undo_is_one_author() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("a", "text", "alex")]);
    f.write(f.main, "sam", vec![Op::set("b", "text", "sam")]);
    // Alex's undo reaches past Sam's newer batch to Alex's own.
    f.store.undo(f.main, "alex").unwrap().unwrap();
    assert!(!f.exists(f.main, "a"));
    assert_eq!(f.text(f.main, "b", "text").as_deref(), Some("sam"));
}

#[test]
fn undo_is_refused_when_a_later_event_replaced_one_of_its_events() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![Op::set("n", "text", "alex"), Op::set("n", "x", 1)],
    );
    f.write(f.main, "sam", vec![Op::set("n", "text", "sam")]);
    let refused = f.store.undo(f.main, "alex");
    assert!(matches!(refused, Err(Error::UndoRefused(_))));
    // Nothing changed.
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("sam"));
    assert_eq!(
        f.store.value(f.main, "n", "x").unwrap(),
        Some(Value::Int(1))
    );
    // The same rule holds for the author's own later batch: undo that first.
    f.store.undo(f.main, "sam").unwrap().unwrap();
    f.store.undo(f.main, "alex").unwrap().unwrap();
    assert!(!f.exists(f.main, "n"));
}

#[test]
fn undo_reaches_back_five_minutes_only() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("n", "x", 1)]);
    f.tick(UNDO_WINDOW_MS + 1);
    assert_eq!(f.store.undo(f.main, "alex").unwrap(), None);
    assert_eq!(
        f.store.value(f.main, "n", "x").unwrap(),
        Some(Value::Int(1))
    );
}

#[test]
fn undo_is_refused_inside_another_branchs_frozen_range() {
    let mut f = Fixture::new();
    f.write(f.main, "alex", vec![Op::set("n", "x", 1)]);
    let agent = f.store.fork(f.main, "agent").unwrap();
    // The fork froze main at seq 1, so seq 1 can no longer be removed.
    assert!(matches!(
        f.store.undo(f.main, "alex"),
        Err(Error::UndoRefused(_))
    ));
    // A later batch is outside the frozen range.
    f.write(f.main, "alex", vec![Op::set("n", "x", 2)]);
    f.store.undo(f.main, "alex").unwrap().unwrap();
    assert_eq!(
        f.store.value(f.main, "n", "x").unwrap(),
        Some(Value::Int(1))
    );
    assert_eq!(f.store.value(agent, "n", "x").unwrap(), Some(Value::Int(1)));

    // Merged agent work is frozen into main, so the agent cannot remove it.
    f.write(agent, "bot", vec![Op::set("n", "y", 1)]);
    f.store.merge(f.main, agent).unwrap();
    assert!(matches!(
        f.store.undo(agent, "bot"),
        Err(Error::UndoRefused(_))
    ));
}

#[test]
fn undo_is_allowed_under_an_open_pull_and_the_follower_sees_it() {
    let mut f = Fixture::new();
    let feed = f.store.create_branch("feed", &[]).unwrap();
    f.store.pull(f.main, feed, SeqRange::ALL).unwrap();
    f.write(feed, "slack", vec![Op::set("m", "text", "oops")]);
    assert!(f.exists(f.main, "m"));
    f.store.undo(feed, "slack").unwrap().unwrap();
    assert!(!f.exists(f.main, "m"));
}

#[test]
fn undo_on_a_fork_brings_back_what_it_saw_from_its_parent() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![
            Op::set("n", "text", "main"),
            Op::link("n", "c", "child", SortOrder::Explicit(1.0)),
        ],
    );
    let agent = f.store.fork(f.main, "agent").unwrap();
    f.write(
        agent,
        "bot",
        vec![Op::set("n", "text", "agent"), Op::unlink("n", "c", "child")],
    );
    assert!(f.child_ids(agent, "n", "child").is_empty());
    let out = f.store.undo(agent, "bot").unwrap().unwrap();
    assert_eq!(out.restored, 0);
    assert_eq!(f.text(agent, "n", "text").as_deref(), Some("main"));
    assert_eq!(f.child_ids(agent, "n", "child"), ["c"]);
}

// --- Reads never need history for the current state ------------------------

#[test]
fn reads_of_own_and_open_ranges_do_not_use_history() {
    let mut f = Fixture::new();
    let feed = f.store.create_branch("feed", &[]).unwrap();
    f.store.pull(f.main, feed, SeqRange::ALL).unwrap();
    for i in 0..3 {
        f.write(
            f.main,
            "alex",
            vec![
                Op::set("n", "x", i),
                Op::link("n", "c", "child", SortOrder::Explicit(i as f64)),
            ],
        );
        f.write(
            feed,
            "slack",
            vec![
                Op::set("m", "x", i),
                Op::link("m", "n", "ref", SortOrder::Timestamp),
            ],
        );
    }
    let before = (
        f.values(f.main, "n"),
        f.values(f.main, "m"),
        f.store
            .links_including_archived(
                f.main,
                &["n", "m"],
                &[("ref", Direction::Outbound), ("ref", Direction::Inbound)],
            )
            .unwrap(),
    );
    // Remove all history. If a read used it, its answer would change.
    f.store
        .conn
        .execute_batch("DELETE FROM value_history; DELETE FROM link_history;")
        .unwrap();
    let after = (
        f.values(f.main, "n"),
        f.values(f.main, "m"),
        f.store
            .links_including_archived(
                f.main,
                &["n", "m"],
                &[("ref", Direction::Outbound), ("ref", Direction::Inbound)],
            )
            .unwrap(),
    );
    assert_eq!(before, after);
    assert!(f.exists(f.main, "m"));
}

// --- Flattened views (MRO) and the shared roll-up --------------------------

fn plan(f: &Fixture, sql: &str) -> String {
    let mut stmt = f
        .store
        .conn
        .prepare(&format!("EXPLAIN QUERY PLAN {sql}"))
        .unwrap();
    let rows = stmt
        .query_map(rusqlite::params![1, 1, "k", 1, 2], |r| {
            r.get::<_, String>(3)
        })
        .unwrap();
    rows.map(|r| r.unwrap()).collect::<Vec<_>>().join(" | ")
}

fn snapshot(f: &Fixture, b: BranchId) -> impl PartialEq + std::fmt::Debug {
    (
        f.values(b, "n"),
        f.store
            .children(b, "n", "child")
            .unwrap()
            .into_iter()
            .map(|l| (l.destination, l.sort, l.event.seq))
            .collect::<Vec<_>>(),
        f.exists(b, "c1"),
    )
}

#[test]
fn frozen_and_head_reads_agree_through_the_shared_roll_up() {
    let mut f = Fixture::new();
    for i in 0..4 {
        f.write(
            f.main,
            "alex",
            vec![
                Op::set("n", "text", format!("v{i}")),
                Op::set("n", TYPE_KEY, "task"),
                Op::set(format!("c{i}"), TYPE_KEY, "task"),
                Op::link(
                    "n",
                    format!("c{i}"),
                    "child",
                    SortOrder::Explicit(f64::from(4 - i)),
                ),
            ],
        );
    }
    f.write(
        f.main,
        "alex",
        vec![Op::unlink("n", "c1", "child"), Op::clear("n", "text")],
    );
    // The fork reads main through a bounded range: current UNION ALL history.
    let fork = f.store.fork(f.main, "fork").unwrap();
    let head = snapshot(&f, f.main);
    assert_eq!(snapshot(&f, fork), head);
    // Main moves on; the fork still matches main's old state.
    f.write(f.main, "alex", vec![Op::set("n", "text", "later")]);
    assert_eq!(snapshot(&f, fork), head);
}

#[test]
fn open_ranges_chain_through_the_expansion() {
    let mut f = Fixture::new();
    let m = f.store.create_branch("m", &[]).unwrap();
    let a = f
        .store
        .create_branch("a", &[ParentRef::new(m, SeqRange::ALL)])
        .unwrap();
    let b = f
        .store
        .create_branch("b", &[ParentRef::new(m, SeqRange::ALL)])
        .unwrap();
    f.write(
        m,
        "x",
        vec![Op::set("n", "text", "m"), Op::set("n", "y", 1)],
    );
    f.write(a, "x", vec![Op::set("n", "text", "a")]);
    // A diamond: x -> [a, b] -> m. m first appears below a, so a's edit wins
    // even though b (which never touched the key) is higher in the list.
    let x = f
        .store
        .create_branch(
            "x",
            &[
                ParentRef::new(a, SeqRange::ALL),
                ParentRef::new(b, SeqRange::ALL),
            ],
        )
        .unwrap();
    assert_eq!(f.text(x, "n", "text").as_deref(), Some("a"));
    // Live through the chain.
    f.write(m, "x", vec![Op::set("n", "y", 2)]);
    assert_eq!(f.store.value(x, "n", "y").unwrap(), Some(Value::Int(2)));
    // B1: a fork of x is frozen along the whole chain.
    let fork = f.store.fork(x, "fork").unwrap();
    f.write(m, "x", vec![Op::set("n", "y", 3)]);
    f.write(a, "x", vec![Op::set("n", "text", "a2")]);
    assert_eq!(f.store.value(fork, "n", "y").unwrap(), Some(Value::Int(2)));
    assert_eq!(f.text(fork, "n", "text").as_deref(), Some("a"));
    assert_eq!(f.store.value(x, "n", "y").unwrap(), Some(Value::Int(3)));
}

#[test]
fn a_frozen_range_gives_only_that_branchs_own_events() {
    let mut f = Fixture::new();
    let m = f.store.create_branch("m", &[]).unwrap();
    let a = f
        .store
        .create_branch("a", &[ParentRef::new(m, SeqRange::ALL)])
        .unwrap();
    f.write(m, "x", vec![Op::set("n", "text", "m")]);
    f.write(a, "x", vec![Op::set("k", "text", "a")]);
    f.store.pull(f.main, a, SeqRange::up_to(1)).unwrap();
    assert!(f.exists(f.main, "k"));
    assert!(!f.exists(f.main, "n"));
}

#[test]
fn an_open_range_wins_over_a_frozen_range_of_the_same_branch() {
    let mut f = Fixture::new();
    let m = f.store.create_branch("m", &[]).unwrap();
    f.write(m, "x", vec![Op::set("n", "text", "m")]);
    let a = f
        .store
        .create_branch("a", &[ParentRef::new(m, SeqRange::ALL)])
        .unwrap();
    f.write(a, "x", vec![Op::set("n", "text", "a")]);
    // m comes in twice: open through a (below a), and frozen on top. After
    // dedupe the open layer keeps m's events, so a's edit still wins.
    f.store
        .set_parents(
            f.main,
            &[
                ParentRef::new(a, SeqRange::ALL),
                ParentRef::new(m, SeqRange::up_to(1)),
            ],
        )
        .unwrap();
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("a"));
}

#[test]
fn a_cached_view_sees_parent_changes_from_another_connection() {
    let path = std::env::temp_dir().join(format!("egdb-cache-{}.db", uuid::Uuid::now_v7()));
    let mut one = Store::open(&path).unwrap();
    let main = one.create_branch("main", &[]).unwrap();
    let feed = one.create_branch("feed", &[]).unwrap();
    one.write(WriteBatch::new(
        feed,
        "x",
        vec![Op::set("n", "text", "feed")],
    ))
    .unwrap();
    assert_eq!(one.value(main, "n", "text").unwrap(), None); // view now cached
    let mut two = Store::open(&path).unwrap();
    two.pull(main, feed, SeqRange::ALL).unwrap();
    assert_eq!(
        one.value(main, "n", "text").unwrap(),
        Some(Value::from("feed"))
    );
    drop((one, two));
    for ext in ["", "-wal", "-shm"] {
        let _ = std::fs::remove_file(format!("{}{ext}", path.display()));
    }
}

#[test]
fn roll_up_queries_use_indexes() {
    use crate::read::{links_sql, with_source, VALUES_SQL};
    use crate::view::EventSource;
    let f = Fixture::new();
    let head = plan(
        &f,
        &with_source(VALUES_SQL, EventSource::Current, "value_history_target"),
    );
    assert!(!head.contains("history"), "{head}");
    assert!(head.contains("PRIMARY KEY (bid=? AND eid=?"), "{head}");
    let frozen = plan(
        &f,
        &with_source(
            VALUES_SQL,
            EventSource::CurrentAndHistory,
            "value_history_target",
        ),
    );
    assert!(
        frozen.contains("value_history_target (bid=? AND eid=?"),
        "{frozen}"
    );
    for (direction, current, history) in [
        (
            Direction::Outbound,
            "PRIMARY KEY (bid=? AND source=? AND link_type=?)",
            "link_history_target (bid=? AND source=? AND link_type=?)",
        ),
        (
            Direction::Inbound,
            "link_current_in (bid=? AND destination=? AND link_type=?)",
            "link_history_in (bid=? AND destination=? AND link_type=?",
        ),
    ] {
        let (sql, index) = links_sql(direction);
        let frozen = plan(
            &f,
            &with_source(&sql, EventSource::CurrentAndHistory, index),
        );
        assert!(frozen.contains(current), "{frozen}");
        assert!(frozen.contains(history), "{frozen}");
    }
}

#[test]
fn a_parent_list_keeps_each_branch_at_its_first_place_with_the_greatest_range() {
    let mut f = Fixture::new();
    let a = f.store.create_branch("a", &[]).unwrap();
    let b = f.store.create_branch("b", &[]).unwrap();
    f.store
        .set_parents(
            f.main,
            &[
                ParentRef::new(a, SeqRange::new(4, Some(5))),
                ParentRef::new(b, SeqRange::ALL),
                ParentRef::new(a, SeqRange::up_to(3)),
            ],
        )
        .unwrap();
    // `a` stays first; its range spans both entries. No other order check.
    assert_eq!(
        f.store.branch(f.main).unwrap().parents,
        vec![
            ParentRef::new(a, SeqRange::up_to(5)),
            ParentRef::new(b, SeqRange::ALL),
        ]
    );
    // Open beats frozen in the greatest range.
    f.store
        .set_parents(
            f.main,
            &[
                ParentRef::new(b, SeqRange::up_to(2)),
                ParentRef::new(b, SeqRange::from(3)),
            ],
        )
        .unwrap();
    assert_eq!(
        f.store.branch(f.main).unwrap().parents,
        vec![ParentRef::new(b, SeqRange::ALL)]
    );
}

#[test]
fn a_branch_reached_twice_sits_where_it_first_appears() {
    let mut f = Fixture::new();
    let m = f.store.create_branch("m", &[]).unwrap();
    let a = f.store.create_branch("a", &[]).unwrap();
    let c = f
        .store
        .create_branch("c", &[ParentRef::new(m, SeqRange::ALL)])
        .unwrap();
    f.write(m, "x", vec![Op::set("n", "text", "m")]);
    f.write(a, "x", vec![Op::set("n", "text", "a")]);
    // Expansion of [m frozen, a, c open]: m, a, m (through c), c. The second
    // m is deduped into the first place, so a still wins over m.
    f.store
        .set_parents(
            f.main,
            &[
                ParentRef::new(m, SeqRange::up_to(1)),
                ParentRef::new(a, SeqRange::ALL),
                ParentRef::new(c, SeqRange::ALL),
            ],
        )
        .unwrap();
    assert_eq!(f.text(f.main, "n", "text").as_deref(), Some("a"));
    // The greatest range: m's later events are visible through the open path.
    f.write(m, "x", vec![Op::set("k", "text", "m2")]);
    assert_eq!(f.text(f.main, "k", "text").as_deref(), Some("m2"));
}

#[test]
fn open_cycles_are_cut_by_the_dedupe() {
    let mut f = Fixture::new();
    let a = f.store.create_branch("a", &[]).unwrap();
    let b = f
        .store
        .create_branch("b", &[ParentRef::new(a, SeqRange::ALL)])
        .unwrap();
    f.store.pull(a, b, SeqRange::ALL).unwrap();
    f.write(a, "x", vec![Op::set("from_a", "text", "a")]);
    f.write(b, "x", vec![Op::set("from_b", "text", "b")]);
    assert_eq!(f.text(a, "from_b", "text").as_deref(), Some("b"));
    assert_eq!(f.text(b, "from_a", "text").as_deref(), Some("a"));
    // Own events still win on both ends.
    f.write(a, "x", vec![Op::set("n", "text", "a")]);
    f.write(b, "x", vec![Op::set("n", "text", "b")]);
    assert_eq!(f.text(a, "n", "text").as_deref(), Some("a"));
    assert_eq!(f.text(b, "n", "text").as_deref(), Some("b"));
}

#[test]
fn batch_reads_key_results_by_id_and_match_single_reads() {
    let mut f = Fixture::new();
    f.write(
        f.main,
        "alex",
        vec![
            Op::set("a", TYPE_KEY, "task"),
            Op::set("b", TYPE_KEY, "task"),
            Op::set("a", "text", "A"),
            Op::link("a", "b", "child", SortOrder::Timestamp),
            Op::link("b", "a", "ref", SortOrder::Timestamp),
        ],
    );
    f.store.archive_entity(f.main, "alex", "b").unwrap();
    // A frozen view, so the batch also reads through history.
    let fork = f.store.fork(f.main, "fork").unwrap();
    for b in [f.main, fork] {
        let ids = ["a", "b", "never"];
        let values = f.store.values(b, &ids).unwrap();
        assert_eq!(values.len(), 3);
        assert_eq!(values["a"], f.values(b, "a"));
        assert!(values["never"].is_empty());
        let exists = f.store.exists(b, &ids).unwrap();
        assert_eq!(
            (exists["a"], exists["b"], exists["never"]),
            (true, true, false)
        );
        let archived = f.store.is_archived(b, &ids).unwrap();
        assert_eq!((archived["a"], archived["b"]), (false, true));
        let pairs = [("child", Direction::Outbound), ("ref", Direction::Inbound)];
        let links = f.store.links(b, &ids, &pairs).unwrap();
        // b is archived, so a's child and a's inbound ref are both hidden.
        assert!(links["a"].values().all(Vec::is_empty));
        let all = f.store.links_including_archived(b, &ids, &pairs).unwrap();
        assert_eq!(
            all["a"][&("child".to_owned(), Direction::Outbound)][0].destination,
            "b"
        );
        assert_eq!(
            all["a"][&("ref".to_owned(), Direction::Inbound)][0].source,
            "b"
        );
        assert!(all["b"].values().all(Vec::is_empty));
        assert!(all["never"].values().all(Vec::is_empty));
    }
}

// --- Walk -------------------------------------------------------------------

/// root(list) -> a(list) -> a1, a2 (task); root -> b(task) -> x (task, not
/// followed: tasks follow nothing); root -> c(list) -> c1(list) -> c2(task).
fn walk_tree(f: &mut Fixture) {
    let mut ops = Vec::new();
    for (id, ty) in [
        ("root", "list"),
        ("a", "list"),
        ("a1", "task"),
        ("a2", "task"),
        ("b", "task"),
        ("x", "task"),
        ("c", "list"),
        ("c1", "list"),
        ("c2", "task"),
    ] {
        ops.push(Op::set(id, TYPE_KEY, ty));
    }
    for (i, (s, d)) in [
        ("root", "a"),
        ("root", "b"),
        ("root", "c"),
        ("a", "a1"),
        ("a", "a2"),
        ("b", "x"),
        ("c", "c1"),
        ("c1", "c2"),
    ]
    .into_iter()
    .enumerate()
    {
        ops.push(Op::link(s, d, "child", SortOrder::Explicit(i as f64)));
    }
    f.write(f.main, "alex", ops);
}

fn follow_children() -> Follow {
    Follow::from([(
        "list".to_owned(),
        vec![("child".to_owned(), Direction::Outbound)],
    )])
}

fn opts(steps: usize, read_ahead: ReadAhead) -> WalkOptions {
    WalkOptions {
        steps,
        read_ahead,
        ..WalkOptions::default()
    }
}

fn walk_ids(walk: &Walk) -> Vec<String> {
    walk.rows.iter().map(|r| r.path.join("/")).collect()
}

fn walk_all(f: &Fixture, b: BranchId, start: &[&str], steps: usize) -> Walk {
    f.store
        .walk(
            b,
            start,
            &follow_children(),
            |_, _| true,
            |_, _| true,
            opts(steps, ReadAhead::default()),
        )
        .unwrap()
}

#[test]
fn walk_is_depth_first_in_sort_order_and_follows_by_type() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    let w = walk_all(&f, f.main, &["root"], 100);
    // x is not reached: b is a task, and tasks follow nothing.
    assert_eq!(
        walk_ids(&w),
        [
            "root",
            "root/a",
            "root/a/a1",
            "root/a/a2",
            "root/b",
            "root/c",
            "root/c/c1",
            "root/c/c1/c2"
        ]
    );
    assert_eq!((w.steps, w.next.clone()), (8, None));
    assert_eq!(w.rows[1].node.values[TYPE_KEY], Value::from("list"));
}

#[test]
fn walk_predicates_give_max_depth_collapse_and_filters() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    let follow = follow_children();
    let ra = ReadAhead::default();
    let depth = f
        .store
        .walk(
            f.main,
            &["root"],
            &follow,
            |p, _| p.len() < 2,
            |_, _| true,
            opts(100, ra),
        )
        .unwrap();
    assert_eq!(walk_ids(&depth), ["root", "root/a", "root/b", "root/c"]);
    let collapsed = f
        .store
        .walk(
            f.main,
            &["root"],
            &follow,
            |_, n| n.id != "a",
            |_, _| true,
            opts(100, ra),
        )
        .unwrap();
    assert_eq!(
        walk_ids(&collapsed),
        [
            "root",
            "root/a",
            "root/b",
            "root/c",
            "root/c/c1",
            "root/c/c1/c2"
        ]
    );
    let is_task = |_: &[String], n: &WalkNode| n.values.get(TYPE_KEY) == Some(&Value::from("task"));
    let tasks = f
        .store
        .walk(
            f.main,
            &["root"],
            &follow,
            |_, _| true,
            is_task,
            opts(100, ra),
        )
        .unwrap();
    assert_eq!(
        walk_ids(&tasks),
        ["root/a/a1", "root/a/a2", "root/b", "root/c/c1/c2"]
    );
    assert_eq!(tasks.steps, 8);
    // Filtered nodes still cost steps: 3 steps reach only a1 among tasks.
    let short = f
        .store
        .walk(
            f.main,
            &["root"],
            &follow,
            |_, _| true,
            is_task,
            opts(3, ra),
        )
        .unwrap();
    assert_eq!(walk_ids(&short), ["root/a/a1"]);
    assert_eq!(
        short.next,
        Some(vec!["root".into(), "a".into(), "a2".into()])
    );
}

#[test]
fn walk_continuation_round_trip_equals_one_long_walk() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    let full = walk_all(&f, f.main, &["root"], 100);
    for page in 1..=4 {
        let mut rows = Vec::new();
        let mut start = vec!["root".to_owned()];
        loop {
            let refs: Vec<&str> = start.iter().map(String::as_str).collect();
            let w = walk_all(&f, f.main, &refs, page);
            assert!(w.steps <= page);
            rows.extend(w.rows);
            match w.next {
                Some(next) => start = next,
                None => break,
            }
        }
        assert_eq!(rows, full.rows, "page size {page}");
    }
    let none = walk_all(&f, f.main, &["root"], 0);
    assert_eq!(
        (none.rows.len(), none.next),
        (0, Some(vec!["root".to_owned()]))
    );
}

fn walk_back(
    f: &Fixture,
    start: &[&str],
    steps: usize,
    descend: impl FnMut(&[String], &WalkNode) -> bool,
) -> Walk {
    f.store
        .walk(
            f.main,
            start,
            &follow_children(),
            descend,
            |_, _| true,
            WalkOptions {
                steps,
                direction: WalkDirection::Backward,
                ..WalkOptions::default()
            },
        )
        .unwrap()
}

#[test]
fn walk_backward_is_the_forward_order_reversed_from_any_row() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    // c1 also links to a, so a subtree shows twice and a cycle back to root
    // is skipped both ways.
    f.write(
        f.main,
        "alex",
        vec![
            Op::link("c1", "a", "child", SortOrder::Explicit(100.0)),
            Op::link("c1", "root", "child", SortOrder::Explicit(101.0)),
        ],
    );
    let forward = walk_ids(&walk_all(&f, f.main, &["root"], 100));
    for (at, path) in forward.iter().enumerate() {
        let start: Vec<&str> = path.split('/').collect();
        let back = walk_back(&f, &start, 100, |_, _| true);
        let mut expected: Vec<String> = forward[..=at].to_vec();
        expected.reverse();
        assert_eq!(walk_ids(&back), expected, "from {path}");
        assert_eq!(back.next, None);
    }
}

#[test]
fn walk_backward_pages_and_respects_descend() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    let last = ["root", "c", "c1", "c2"];
    let full = walk_back(&f, &last, 100, |_, _| true);
    for page in 1..=3 {
        let mut rows = Vec::new();
        let mut start: Vec<String> = last.iter().map(|s| (*s).to_owned()).collect();
        loop {
            let refs: Vec<&str> = start.iter().map(String::as_str).collect();
            let w = walk_back(&f, &refs, page, |_, _| true);
            assert!(w.steps <= page);
            rows.extend(w.rows);
            match w.next {
                Some(next) => start = next,
                None => break,
            }
        }
        assert_eq!(rows, full.rows, "page size {page}");
    }
    // Collapsing a: the walk back from b goes to a, not into a's children.
    let collapsed = walk_back(&f, &["root", "b"], 100, |_, n| n.id != "a");
    assert_eq!(walk_ids(&collapsed), ["root/b", "root/a", "root"]);
    // A path that is no longer in the tree goes back to its parent.
    let stale = walk_back(&f, &["root", "gone"], 100, |_, _| true);
    assert_eq!(walk_ids(&stale), ["root/gone", "root"]);
}

#[test]
fn walk_backward_refuses_a_visited_set() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    let err = f
        .store
        .walk(
            f.main,
            &["root"],
            &follow_children(),
            |_, _| true,
            |_, _| true,
            WalkOptions {
                visited: Some(HashSet::new()),
                direction: WalkDirection::Backward,
                ..WalkOptions::default()
            },
        )
        .unwrap_err();
    assert!(matches!(err, Error::Invalid(_)));
}

#[test]
fn walk_does_not_revisit_the_path_but_repeats_other_paths() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    // c1 links back to root (a cycle) and to a (a second path to a).
    f.write(
        f.main,
        "alex",
        vec![
            Op::link("c1", "root", "child", SortOrder::Explicit(100.0)),
            Op::link("c1", "a", "child", SortOrder::Explicit(101.0)),
        ],
    );
    let w = walk_all(&f, f.main, &["root"], 100);
    let ids = walk_ids(&w);
    assert!(!ids.iter().any(|p| p.starts_with("root/c/c1/root")));
    assert!(ids.contains(&"root/c/c1/a/a1".to_owned()));
    assert_eq!(ids.iter().filter(|p| p.ends_with("/a1")).count(), 2);
}

#[test]
fn walk_applies_the_archived_filter_and_reads_frozen_views() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    let fork = f.store.fork(f.main, "fork").unwrap();
    let before = walk_all(&f, f.main, &["root"], 100);
    f.store.archive_entity(f.main, "alex", "a").unwrap();
    f.write(
        f.main,
        "alex",
        vec![
            Op::set("d", TYPE_KEY, "task"),
            Op::link("root", "d", "child", SortOrder::Timestamp),
        ],
    );
    let after = walk_all(&f, f.main, &["root"], 100);
    assert_eq!(
        walk_ids(&after),
        [
            "root",
            "root/b",
            "root/c",
            "root/c/c1",
            "root/c/c1/c2",
            "root/d"
        ]
    );
    // The fork reads main through a frozen range (current + history).
    assert_eq!(walk_all(&f, fork, &["root"], 100), before);
}

#[test]
fn walk_read_ahead_cuts_load_rounds() {
    let mut f = Fixture::new();
    // A tree of depth 4 and fan-out 3: 1 + 3 + 9 + 27 + 81 nodes.
    let mut ops = vec![Op::set("n", TYPE_KEY, "list")];
    let mut layer = vec!["n".to_owned()];
    for _ in 0..4 {
        let mut next = Vec::new();
        for p in &layer {
            for i in 0..3 {
                let c = format!("{p}.{i}");
                ops.push(Op::set(c.as_str(), TYPE_KEY, "list"));
                ops.push(Op::link(
                    p.as_str(),
                    c.as_str(),
                    "child",
                    SortOrder::Explicit(i as f64),
                ));
                next.push(c);
            }
        }
        layer = next;
    }
    f.write(f.main, "alex", ops);
    let follow = follow_children();
    let run = |ra: ReadAhead| {
        f.store
            .walk(
                f.main,
                &["n"],
                &follow,
                |_, _| true,
                |_, _| true,
                opts(1000, ra),
            )
            .unwrap()
    };
    let none = run(ReadAhead {
        depth: 0,
        width: 32,
    });
    let ahead = run(ReadAhead::default());
    assert_eq!(none.rows, ahead.rows);
    assert_eq!(none.steps, 121);
    assert_eq!(none.loads, 121, "one round per node without read-ahead");
    assert!(
        ahead.loads * 3 < none.loads,
        "{} vs {}",
        ahead.loads,
        none.loads
    );
    // Bounded by the page: a 5-step page does not load the whole tree.
    let small = f
        .store
        .walk(
            f.main,
            &["n"],
            &follow,
            |_, _| true,
            |_, _| true,
            opts(5, ReadAhead::default()),
        )
        .unwrap();
    assert!(
        small.loads < ahead.loads,
        "{} vs {}",
        small.loads,
        ahead.loads
    );
}

#[test]
fn walk_skips_nodes_on_the_current_path_entirely() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    // c2 becomes a list that links back to c and root: both are on its path.
    f.write(
        f.main,
        "alex",
        vec![
            Op::set("c2", TYPE_KEY, "list"),
            Op::link("c2", "c", "child", SortOrder::Explicit(0.0)),
            Op::link("c2", "root", "child", SortOrder::Explicit(1.0)),
        ],
    );
    let w = walk_all(&f, f.main, &["root"], 100);
    // Same rows and same step count as without the back links: the skipped
    // nodes are not visited, emitted, expanded or counted.
    assert_eq!(w.steps, 8);
    assert_eq!(
        walk_ids(&w).last().map(String::as_str),
        Some("root/c/c1/c2")
    );
    assert_eq!(w.next, None);
}

#[test]
fn walk_returns_its_cache_and_it_matches_separate_reads() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    let w = f
        .store
        .walk(
            f.main,
            &["root"],
            &follow_children(),
            |_, _| true,
            |_, _| true,
            WalkOptions {
                return_cache: true,
                ..WalkOptions::default()
            },
        )
        .unwrap();
    let cache = w.cache.expect("asked for");
    for row in &w.rows {
        let id = row.node.id.as_str();
        assert_eq!(cache.nodes[id].values, f.values(f.main, id));
        if row.node.values[TYPE_KEY] == Value::from("list") {
            let links = f
                .store
                .links(f.main, &[id], &[("child", Direction::Outbound)])
                .unwrap()
                .remove(id)
                .unwrap();
            assert_eq!(cache.links[id], links);
        } else {
            assert!(!cache.links.contains_key(id));
        }
    }
    let plain = walk_all(&f, f.main, &["root"], 100);
    assert_eq!(plain.cache, None);
}

#[test]
fn walk_visited_set_dedupes_across_pages_like_one_long_walk() {
    let mut f = Fixture::new();
    walk_tree(&mut f);
    // A second path to a (and its subtree) through c1.
    f.write(
        f.main,
        "alex",
        vec![Op::link("c1", "a", "child", SortOrder::Explicit(100.0))],
    );
    let run = |start: &[&str], steps: usize, visited: HashSet<String>| {
        f.store
            .walk(
                f.main,
                start,
                &follow_children(),
                |_, _| true,
                |_, _| true,
                WalkOptions {
                    steps,
                    visited: Some(visited),
                    ..WalkOptions::default()
                },
            )
            .unwrap()
    };
    let long = run(&["root"], 100, HashSet::new());
    // a is visited once only; without the set it would appear twice.
    assert_eq!(
        walk_ids(&long).iter().filter(|p| p.ends_with("/a")).count(),
        1
    );
    assert_eq!(long.steps, 8);
    for page in 1..=3 {
        let mut rows = Vec::new();
        let mut start = vec!["root".to_owned()];
        let mut visited = HashSet::new();
        loop {
            let refs: Vec<&str> = start.iter().map(String::as_str).collect();
            let w = run(&refs, page, visited);
            rows.extend(w.rows);
            visited = w.visited.expect("asked for");
            match w.next {
                Some(next) => start = next,
                None => break,
            }
        }
        assert_eq!(rows, long.rows, "page size {page}");
        assert_eq!(Some(visited), long.visited.clone());
    }
}

// --- The type filter inside the link query (equivalence) -----------------------

/// Per asked entity: its links as (source, destination, seq).
type TypedLinks = Vec<(String, Vec<(String, String, i64)>)>;

/// The reference for the implicit type filter: all links, then keep those whose
/// other entity has a non-null winning `type` value in a separate read.
fn typed_links_by_reading_values(
    f: &Fixture,
    branch: BranchId,
    ents: &[&str],
    dir: Direction,
) -> TypedLinks {
    let all = f
        .store
        .links_including_archived(branch, ents, &[("child", dir)])
        .unwrap();
    ents.iter()
        .map(|e| {
            let links = all[*e][&("child".to_owned(), dir)]
                .iter()
                .filter(|l| {
                    let other = if dir == Direction::Outbound {
                        &l.destination
                    } else {
                        &l.source
                    };
                    matches!(f.values(branch, other).get(TYPE_KEY), Some(v) if !v.is_null())
                })
                .map(|l| (l.source.clone(), l.destination.clone(), l.event.seq))
                .collect();
            ((*e).to_owned(), links)
        })
        .collect()
}

fn typed_links_in_query(
    f: &Fixture,
    branch: BranchId,
    ents: &[&str],
    dir: Direction,
) -> TypedLinks {
    let got = f.store.links(branch, ents, &[("child", dir)]).unwrap();
    ents.iter()
        .map(|e| {
            let links = got[*e][&("child".to_owned(), dir)]
                .iter()
                .map(|l| (l.source.clone(), l.destination.clone(), l.event.seq))
                .collect();
            ((*e).to_owned(), links)
        })
        .collect()
}

#[test]
fn the_sql_type_filter_equals_reading_the_type_for_stacks_forks_and_archives() {
    // A tiny xorshift, so that the scenario is the same on every run.
    let mut x = 0x2545_F491_4F6C_DD1Du64;
    let mut rnd = move |n: u64| {
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        x % n
    };
    for _case in 0..60 {
        let mut f = Fixture::new();
        let names: Vec<String> = (0..8).map(|i| format!("e{i}")).collect();
        let ents: Vec<&str> = names.iter().map(String::as_str).collect();
        let mut branches = vec![f.main];
        let mut frozen_forks = vec![];
        for round in 0..24 {
            let b = branches[rnd(branches.len() as u64) as usize];
            let e = &names[rnd(8) as usize];
            let op = match rnd(6) {
                0 => Op::set(e.as_str(), TYPE_KEY, "task"),
                1 => Op::set(e.as_str(), TYPE_KEY, "list"),
                2 => Op::clear(e.as_str(), TYPE_KEY),
                3 | 4 => Op::link(
                    names[rnd(8) as usize].as_str(),
                    e.as_str(),
                    "child",
                    if rnd(4) == 0 {
                        SortOrder::Removed
                    } else {
                        SortOrder::Explicit(rnd(5) as f64)
                    },
                ),
                _ => Op::set(e.as_str(), "title", format!("t{round}")),
            };
            f.write(b, "alex", vec![op]);
            if round % 5 == 4 && branches.len() < 5 {
                let from = branches[rnd(branches.len() as u64) as usize];
                let name = format!("b{round}");
                let nb = match rnd(3) {
                    0 => {
                        let nb = f.store.fork(from, &name).unwrap();
                        frozen_forks.push(nb);
                        nb
                    }
                    1 => f
                        .store
                        .create_branch(&name, &[ParentRef::new(from, SeqRange::ALL)])
                        .unwrap(),
                    _ => f
                        .store
                        .create_branch(&name, &[ParentRef::new(from, SeqRange::up_to(2))])
                        .unwrap(),
                };
                branches.push(nb);
            }
        }
        for &b in &branches {
            for dir in [Direction::Outbound, Direction::Inbound] {
                assert_eq!(
                    typed_links_in_query(&f, b, &ents, dir),
                    typed_links_by_reading_values(&f, b, &ents, dir)
                );
            }
        }
        let _ = frozen_forks;
    }
}
