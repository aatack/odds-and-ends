//! Walk benchmark. Run in release:
//!
//! ```text
//! cargo run --release --example bench_walk -- [sizes] [data dir]
//! cargo run --release --example bench_walk -- 1000,10000,100000 /workspace/bench-data
//! ```
//!
//! For each size it builds a file-backed store (WAL): mostly a tree (each
//! node has 2..=15 children, filled breadth-first), a few typed values per
//! node, and cross links of another link type on a share of the nodes. Then
//! it times walks in several scenarios. The generator streams batches and
//! keeps only O(N) small integers, so 1M and 10M nodes are possible.

use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::Instant;

use egdb::{
    BranchId, Direction, Follow, Op, ReadAhead, SortOrder, Store, Walk, WalkOptions, WriteBatch,
    TYPE_KEY,
};

const TYPES: [&str; 4] = ["list", "task", "note", "section"];
const BATCH_OPS: usize = 2_000;
const CROSS_SHARE: f64 = 0.05;
const RUNS: usize = 3;
const SEED: u64 = 0x5eed;

/// SplitMix64: small, fast, seeded, no dependency.
struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
    fn range(&mut self, lo: u64, hi: u64) -> u64 {
        lo + self.next() % (hi - lo + 1)
    }
    fn chance(&mut self, p: f64) -> bool {
        (self.next() >> 11) as f64 / (1u64 << 53) as f64 <= p
    }
}

fn id(n: u64) -> String {
    format!("n{n}")
}

fn peak_rss_mb() -> f64 {
    std::fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|s| {
            s.lines()
                .find(|l| l.starts_with("VmHWM:"))
                .and_then(|l| l.split_whitespace().nth(1)?.parse::<f64>().ok())
        })
        .map_or(0.0, |kb| kb / 1024.0)
}

fn file_mb(path: &Path) -> f64 {
    let mut total = 0;
    for ext in ["", "-wal"] {
        if let Ok(m) = std::fs::metadata(format!("{}{ext}", path.display())) {
            total += m.len();
        }
    }
    total as f64 / 1_048_576.0
}

struct Built {
    store: Store,
    main: BranchId,
    fork: BranchId,
    path: PathBuf,
    events: u64,
    write_secs: f64,
    supersede_events: u64,
}

fn flush(store: &mut Store, branch: BranchId, ops: &mut Vec<Op>, events: &mut u64) {
    if ops.is_empty() {
        return;
    }
    *events += ops.len() as u64;
    store
        .write(WriteBatch::new(branch, "bench", std::mem::take(ops)))
        .expect("write");
}

fn build(n: u64, dir: &Path) -> Built {
    let path = dir.join(format!("walk-{n}.db"));
    for ext in ["", "-wal", "-shm"] {
        let _ = std::fs::remove_file(format!("{}{ext}", path.display()));
    }
    let mut store = Store::open(&path).expect("open");
    let main = store.create_branch("main", &[]).expect("branch");
    let mut rng = Rng(SEED ^ n);
    let mut ops = Vec::with_capacity(BATCH_OPS + 8);
    let mut events = 0;
    let started = Instant::now();

    let node_ops = |rng: &mut Rng, i: u64, ops: &mut Vec<Op>| {
        let e = id(i);
        let ty = TYPES[rng.range(0, 3) as usize];
        ops.push(Op::set(e.as_str(), TYPE_KEY, ty));
        ops.push(Op::set(
            e.as_str(),
            "title",
            format!("Item {i}: some title text"),
        ));
        ops.push(Op::set(e.as_str(), "done", rng.chance(0.3)));
        ops.push(Op::set(e.as_str(), "priority", rng.range(0, 5) as i64));
    };
    node_ops(&mut rng, 0, &mut ops);
    // Breadth-first: parents in creation order; children get the next IDs.
    let mut next_id = 1;
    let mut parent = 0;
    while next_id < n {
        let k = rng.range(2, 15);
        for _ in 0..k {
            if next_id >= n {
                break;
            }
            node_ops(&mut rng, next_id, &mut ops);
            ops.push(Op::link(
                id(parent),
                id(next_id),
                "child",
                SortOrder::Timestamp,
            ));
            next_id += 1;
            if ops.len() >= BATCH_OPS {
                flush(&mut store, main, &mut ops, &mut events);
            }
        }
        parent += 1;
    }
    for i in 0..n {
        if rng.chance(CROSS_SHARE) {
            let to = rng.range(0, n - 1);
            ops.push(Op::link(id(i), id(to), "ref", SortOrder::Timestamp));
            if ops.len() >= BATCH_OPS {
                flush(&mut store, main, &mut ops, &mut events);
            }
        }
    }
    flush(&mut store, main, &mut ops, &mut events);
    let write_secs = started.elapsed().as_secs_f64();

    // Fork, then supersede 10% of titles on main so history is not empty.
    let fork = store.fork(main, "frozen").expect("fork");
    let mut supersede_events = 0;
    for i in 0..n {
        if rng.chance(0.10) {
            ops.push(Op::set(
                id(i).as_str(),
                "title",
                format!("Item {i}: edited"),
            ));
            if ops.len() >= BATCH_OPS {
                flush(&mut store, main, &mut ops, &mut supersede_events);
            }
        }
    }
    flush(&mut store, main, &mut ops, &mut supersede_events);
    Built {
        store,
        main,
        fork,
        path,
        events,
        write_secs,
        supersede_events,
    }
}

fn follow(cross: bool) -> Follow {
    let mut pairs = vec![("child".to_owned(), Direction::Outbound)];
    if cross {
        pairs.push(("ref".to_owned(), Direction::Outbound));
    }
    TYPES
        .iter()
        .map(|t| ((*t).to_owned(), pairs.clone()))
        .collect::<HashMap<_, _>>()
}

struct Scenario {
    name: &'static str,
    page: Option<usize>,
    cross: bool,
    visited: bool,
    cache: bool,
    read_ahead: ReadAhead,
    fork: bool,
}

/// Walk to the end, page by page. Returns (entities visited, loads).
fn run(b: &Built, s: &Scenario) -> (usize, usize) {
    let follow = follow(s.cross);
    let branch = if s.fork { b.fork } else { b.main };
    let mut start = vec![id(0)];
    let mut visited_set = s.visited.then(HashSet::new);
    let (mut steps, mut loads) = (0, 0);
    loop {
        let refs: Vec<&str> = start.iter().map(String::as_str).collect();
        let w: Walk = b
            .store
            .walk(
                branch,
                &refs,
                &follow,
                |_, _| true,
                |_, _| true,
                WalkOptions {
                    steps: s.page.unwrap_or(usize::MAX),
                    read_ahead: s.read_ahead,
                    return_cache: s.cache,
                    visited: visited_set.take(),
                },
            )
            .expect("walk");
        steps += w.steps;
        loads += w.loads;
        visited_set = w.visited;
        std::hint::black_box(&w.rows);
        match w.next {
            Some(n) => start = n,
            None => return (steps, loads),
        }
    }
}

fn main() {
    let mut args = std::env::args().skip(1);
    let sizes: Vec<u64> = args
        .next()
        .unwrap_or_else(|| "1000,10000,100000".into())
        .split(',')
        .map(|s| s.parse().expect("size"))
        .collect();
    let dir = PathBuf::from(
        args.next()
            .unwrap_or_else(|| "/workspace/bench-data".into()),
    );
    std::fs::create_dir_all(&dir).expect("data dir");

    let off = ReadAhead {
        depth: 0,
        width: 32,
    };
    let def = ReadAhead::default();
    let scenarios = [
        Scenario {
            name: "tree, page 100",
            page: Some(100),
            cross: false,
            visited: false,
            cache: false,
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree, page 1000",
            page: Some(1000),
            cross: false,
            visited: false,
            cache: false,
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree, one page",
            page: None,
            cross: false,
            visited: false,
            cache: false,
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree, page 1000, read-ahead off",
            page: Some(1000),
            cross: false,
            visited: false,
            cache: false,
            read_ahead: off,
            fork: false,
        },
        Scenario {
            name: "tree, page 1000, read-ahead width 256",
            page: Some(1000),
            cross: false,
            visited: false,
            cache: false,
            read_ahead: ReadAhead {
                depth: 2,
                width: 256,
            },
            fork: false,
        },
        Scenario {
            name: "tree, page 1000, return cache",
            page: Some(1000),
            cross: false,
            visited: false,
            cache: true,
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree+cross, page 1000, visited set",
            page: Some(1000),
            cross: true,
            visited: true,
            cache: false,
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree, page 1000, frozen fork",
            page: Some(1000),
            cross: false,
            visited: false,
            cache: false,
            read_ahead: def,
            fork: true,
        },
    ];

    println!("| nodes | scenario | visited | median s | entities/s | loads per 1k |");
    println!("| ---: | --- | ---: | ---: | ---: | ---: |");
    let mut builds = Vec::new();
    for &n in &sizes {
        let b = build(n, &dir);
        for s in &scenarios {
            let mut times = Vec::new();
            let mut last = (0, 0);
            for _ in 0..RUNS {
                let t = Instant::now();
                last = run(&b, s);
                times.push(t.elapsed().as_secs_f64());
            }
            times.sort_by(f64::total_cmp);
            let median = times[RUNS / 2];
            println!(
                "| {n} | {} | {} | {median:.3} | {:.0} | {:.1} |",
                s.name,
                last.0,
                last.0 as f64 / median,
                last.1 as f64 * 1000.0 / last.0 as f64
            );
        }
        builds.push((
            n,
            b.events,
            b.write_secs,
            b.supersede_events,
            file_mb(&b.path),
        ));
    }
    println!();
    println!("| nodes | events | write s | events/s | supersede events | file MB |");
    println!("| ---: | ---: | ---: | ---: | ---: | ---: |");
    for (n, ev, secs, sup, mb) in builds {
        println!(
            "| {n} | {ev} | {secs:.2} | {:.0} | {sup} | {mb:.1} |",
            ev as f64 / secs
        );
    }
    println!();
    println!("peak RSS: {:.0} MB", peak_rss_mb());
}
