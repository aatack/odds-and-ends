//! Durability and robustness: a killed writer, concurrent readers, a race
//! on first open, and very large IDs.

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use crate::*;

fn temp_dir(tag: &str) -> PathBuf {
    let dir = std::env::temp_dir().join(format!("egdb-{tag}-{}", uuid::Uuid::now_v7()));
    std::fs::create_dir_all(&dir).unwrap();
    dir
}

const BATCH: usize = 50;

/// Child process of the crash test: write batches of `BATCH` events for ever.
/// It does nothing unless the parent set `EGDB_CRASH_FILE`.
#[test]
fn crash_child_writer() {
    let Ok(path) = std::env::var("EGDB_CRASH_FILE") else {
        return;
    };
    let mut store = Store::open(&path).unwrap();
    let branch = store.branches().unwrap()[0].id;
    let mut n = 0u64;
    loop {
        n += 1;
        let mut ops = Vec::new();
        for k in 0..BATCH as u64 - 1 {
            ops.push(Op::set(format!("e{}", (n * 7 + k) % 500), "v", n as i64));
        }
        ops.push(Op::link(
            "root",
            format!("e{}", n % 500),
            "child",
            SortOrder::Timestamp,
        ));
        store.write(WriteBatch::new(branch, "crash", ops)).unwrap();
    }
}

fn check_file(path: &Path) -> i64 {
    let store = Store::open(path).unwrap();
    let c = &store.conn;
    let one = |sql: &str| -> String { c.query_row(sql, [], |r| r.get::<_, String>(0)).unwrap() };
    assert_eq!(one("PRAGMA integrity_check"), "ok");
    let n = |sql: &str| -> i64 { c.query_row(sql, [], |r| r.get(0)).unwrap() };
    let head = n("SELECT head_seq FROM branches");
    // Every batch is whole: the head counts whole batches, and every
    // sequence ID up to the head is either an event or a replaced event.
    assert_eq!(
        head % BATCH as i64,
        0,
        "head {head} is not a whole number of batches"
    );
    let events = n("SELECT (SELECT count(*) FROM value_current) + (SELECT count(*) FROM value_history)
                         + (SELECT count(*) FROM link_current) + (SELECT count(*) FROM link_history)");
    assert_eq!(events, head, "events {events} vs head {head}");
    assert_eq!(n("SELECT count(*) FROM batches"), head / BATCH as i64);
    assert_eq!(n("SELECT coalesce(max(last_seq), 0) FROM batches"), head);
    assert_eq!(n("SELECT count(*) FROM pragma_foreign_key_check"), 0);
    head
}

#[test]
fn a_writer_killed_at_random_moments_leaves_whole_batches() {
    let dir = temp_dir("crash");
    let path = dir.join("store.db");
    {
        let mut s = Store::open(&path).unwrap();
        s.create_branch("main", &[]).unwrap();
    }
    let exe = std::env::current_exe().unwrap();
    let rounds: u64 = std::env::var("EGDB_CRASH_ROUNDS")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(10);
    let mut last = 0;
    for round in 0..rounds {
        let mut child = Command::new(&exe)
            .args(["robust_tests::crash_child_writer", "--exact", "--nocapture"])
            .env("EGDB_CRASH_FILE", &path)
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        std::thread::sleep(Duration::from_millis(150 + (round * 37) % 200));
        child.kill().unwrap(); // SIGKILL: no clean-up code runs
        child.wait().unwrap();
        let head = check_file(&path);
        assert!(head >= last, "committed batches were lost: {head} < {last}");
        last = head;
    }
    assert!(last > 0, "the child never committed a batch");
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn readers_on_other_connections_see_whole_batches_and_new_parent_lists() {
    let dir = temp_dir("readers");
    let path = dir.join("store.db");
    let (main, other);
    {
        let mut w = Store::open(&path).unwrap();
        main = w.create_branch("main", &[]).unwrap();
        other = w.create_branch("other", &[]).unwrap();
        w.write(WriteBatch::new(
            main,
            "a",
            vec![Op::set("x", "n", 0), Op::set("y", "n", 0)],
        ))
        .unwrap();
    }
    let stop = Arc::new(AtomicBool::new(false));
    let readers: Vec<_> = (0..4)
        .map(|_| {
            let (path, stop) = (path.clone(), stop.clone());
            std::thread::spawn(move || {
                let r = Store::open(&path).unwrap();
                let mut reads = 0u64;
                while !stop.load(Ordering::SeqCst) {
                    let v = r.values(main, &["x", "y"]).unwrap();
                    assert_eq!(v["x"]["n"], v["y"]["n"], "a batch was seen in part");
                    reads += 1;
                }
                reads
            })
        })
        .collect();
    let mut w = Store::open(&path).unwrap();
    for n in 1..=300i64 {
        w.write(WriteBatch::new(
            main,
            "a",
            vec![Op::set("x", "n", n), Op::set("y", "n", n)],
        ))
        .unwrap();
        if n % 100 == 0 {
            w.checkpoint().unwrap();
        }
    }
    // A reader that cached a view sees a parent list changed elsewhere.
    let r = Store::open(&path).unwrap();
    assert_eq!(r.value(other, "x", "n").unwrap(), None);
    w.pull(other, main, SeqRange::ALL).unwrap();
    assert_eq!(r.value(other, "x", "n").unwrap(), Some(Value::Int(300)));
    stop.store(true, Ordering::SeqCst);
    for h in readers {
        assert!(h.join().unwrap() > 0);
    }
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn many_connections_opening_a_new_file_at_once_all_succeed() {
    for _ in 0..20 {
        let dir = temp_dir("open");
        let path = dir.join("store.db");
        let handles: Vec<_> = (0..8)
            .map(|_| {
                let path = path.clone();
                std::thread::spawn(move || Store::open(&path).map(|s| s.schema_version().unwrap()))
            })
            .collect();
        for h in handles {
            assert_eq!(h.join().unwrap().unwrap(), SCHEMA_VERSION);
        }
        std::fs::remove_dir_all(dir).unwrap();
    }
}

#[test]
fn two_writers_on_one_file_keep_sequence_ids_unique() {
    let dir = temp_dir("writers");
    let path = dir.join("store.db");
    let main = Store::open(&path)
        .unwrap()
        .create_branch("main", &[])
        .unwrap();
    let handles: Vec<_> = (0..4)
        .map(|t| {
            let path = path.clone();
            std::thread::spawn(move || {
                let mut s = Store::open(&path).unwrap();
                for i in 0..50 {
                    s.write(WriteBatch::new(
                        main,
                        "w",
                        vec![Op::set(format!("t{t}"), "i", i as i64)],
                    ))
                    .unwrap();
                }
            })
        })
        .collect();
    for h in handles {
        h.join().unwrap();
    }
    let s = Store::open(&path).unwrap();
    assert_eq!(s.branch(main).unwrap().head, 200);
    std::fs::remove_dir_all(dir).unwrap();
}

#[test]
fn large_ids_round_trip() {
    let mut store = Store::open_in_memory().unwrap();
    let main = store.create_branch("main", &[]).unwrap();
    // Entity IDs: very long, and with unusual characters.
    let long = "é".repeat(500_000);
    let odd = "a\u{0}b 'quoted' \"x\" \n 日本語 🦀";
    store
        .write(WriteBatch::new(
            main,
            "a",
            vec![
                Op::set(long.clone(), "type", "note"),
                Op::set(odd, "type", "note"),
                Op::link(odd, long.clone(), "child", SortOrder::Explicit(1.0)),
            ],
        ))
        .unwrap();
    assert_eq!(
        store.value(main, &long, "type").unwrap(),
        Some(Value::Text("note".into()))
    );
    assert_eq!(
        store.children(main, odd, "child").unwrap()[0].destination,
        long
    );

    // Internal keys beyond 2^32 and 2^53 (no float conversion on the way).
    for base in [1i64 << 33, (1i64 << 53) + 1, 1i64 << 60] {
        store
            .conn
            .execute(
                "INSERT INTO entities (eid, ext_id) VALUES (?1, ?2)",
                rusqlite::params![base, format!("big{base}")],
            )
            .unwrap();
    }
    for base in [1i64 << 33, (1i64 << 53) + 1, 1i64 << 60] {
        let id = format!("big{base}");
        store
            .write(WriteBatch::new(
                main,
                "a",
                vec![
                    Op::set(id.clone(), "type", "note"),
                    Op::set(id.clone(), "n", base),
                ],
            ))
            .unwrap();
        assert_eq!(store.value(main, &id, "n").unwrap(), Some(Value::Int(base)));
        assert_eq!(
            store
                .find(main, "n", &Value::Int(base))
                .map(|v| v.len())
                .unwrap_or(1),
            1
        );
    }
    // Sequence IDs beyond 2^53, and extreme integers as values.
    store
        .conn
        .execute("UPDATE branches SET head_seq = ?1", [(1i64 << 60) - 3])
        .unwrap();
    let r = store
        .write(WriteBatch::new(
            main,
            "a",
            vec![
                Op::set("s", "type", "note"),
                Op::set("s", "min", i64::MIN),
                Op::set("s", "max", i64::MAX),
                Op::set("s", "f", f64::MAX),
            ],
        ))
        .unwrap();
    assert_eq!((r.first_seq, r.last_seq), ((1 << 60) - 2, (1 << 60) + 1));
    let v = store.values(main, &["s"]).unwrap();
    assert_eq!(v["s"]["min"], Value::Int(i64::MIN));
    assert_eq!(v["s"]["max"], Value::Int(i64::MAX));
    assert_eq!(v["s"]["f"], Value::Float(f64::MAX));
    let fork = store.fork(main, "fork").unwrap();
    assert_eq!(
        store.value(fork, "s", "max").unwrap(),
        Some(Value::Int(i64::MAX))
    );
    store
        .write(WriteBatch::new(main, "a", vec![Op::set("s", "max", 1)]))
        .unwrap();
    assert_eq!(
        store.value(fork, "s", "max").unwrap(),
        Some(Value::Int(i64::MAX))
    );
}
