//! Walk benchmark. Run in release:
//!
//! ```text
//! cargo run --release --example bench_walk -- [sizes] [data dir]
//! cargo run --release --example bench_walk -- 1000,10000,100000 /workspace/bench-data
//! cargo run --release --example bench_walk -- 100000 /workspace/bench-data \
//!     --reuse --cold --cache-size 256 --scenarios 1,2,3,7
//! ```
//!
//! Flags:
//! - `--reuse`: open the existing `walk-N.db` (no build, no write table).
//! - `--cold`: before each run, checkpoint the WAL and evict the db and WAL
//!   files from the OS page cache (`posix_fadvise(POSIX_FADV_DONTNEED)`).
//!   Each run also opens a new connection, so the SQLite cache is empty.
//! - `--cache-size KIB`: SQLite page cache size (default 2000 KiB).
//!   `mmap_size` is always 0.
//! - `--random-ids`: hashed entity IDs (file `walk-N-rand.db`), so key
//!   order does not follow tree order. Closer to real UUID-keyed data.
//! - `--scenarios I,J`: run only these scenario indexes (0-based).
//!
//! Per run it also records bytes read from storage (`/proc/self/io`
//! `read_bytes`) and major page faults (`/proc/self/stat`).
//!
//! For each size it builds a file-backed store (WAL): mostly a tree (each
//! node has 2..=15 children, filled breadth-first), a few typed values per
//! node, and cross links of another link type on a share of the nodes. Then
//! it times walks in several scenarios. The generator streams batches and
//! keeps only O(N) small integers, so 1M and 10M nodes are possible.

use std::collections::{HashMap, HashSet};
use std::os::fd::AsRawFd;
use std::path::{Path, PathBuf};
use std::time::Instant;

extern "C" {
    fn posix_fadvise(fd: i32, offset: i64, len: i64, advice: i32) -> i32;
}
const POSIX_FADV_DONTNEED: i32 = 4;

/// Drop the file's clean pages from the OS page cache.
fn evict(path: &Path) {
    for ext in ["", "-wal"] {
        if let Ok(f) = std::fs::File::open(format!("{}{ext}", path.display())) {
            // SAFETY: valid fd for the duration of the call.
            let rc = unsafe { posix_fadvise(f.as_raw_fd(), 0, 0, POSIX_FADV_DONTNEED) };
            assert_eq!(rc, 0, "posix_fadvise");
        }
    }
}

/// (bytes read from storage, major faults) for this process so far.
fn io_counters() -> (u64, u64) {
    let io = std::fs::read_to_string("/proc/self/io").unwrap_or_default();
    let rb = io
        .lines()
        .find_map(|l| l.strip_prefix("read_bytes: "))
        .and_then(|v| v.trim().parse().ok())
        .unwrap_or(0);
    let stat = std::fs::read_to_string("/proc/self/stat").unwrap_or_default();
    // Fields after the ")" of comm: state is field 3; majflt is field 12.
    let after = stat.rsplit_once(')').map(|x| x.1).unwrap_or("");
    let majflt = after
        .split_whitespace()
        .nth(9)
        .and_then(|v| v.parse().ok())
        .unwrap_or(0);
    (rb, majflt)
}

use egdb::{
    BranchId, Direction, Follow, FollowRule, Op, ReadAhead, SortOrder, Store, Walk, WalkOptions,
    WriteBatch, TYPE_KEY,
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

static RANDOM_IDS: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

/// Entity ID for node `n`. With `--random-ids` the IDs are a hash of `n`,
/// so B-tree order has no relation to tree order (like UUIDs).
fn id(n: u64) -> String {
    if RANDOM_IDS.load(std::sync::atomic::Ordering::Relaxed) {
        let mut z = n.wrapping_add(0x9e37_79b9_7f4a_7c15);
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        format!("{:016x}", z ^ (z >> 31))
    } else {
        format!("n{n}")
    }
}

fn db_path(dir: &Path, n: u64) -> PathBuf {
    let rand = RANDOM_IDS.load(std::sync::atomic::Ordering::Relaxed);
    dir.join(format!("walk-{n}{}.db", if rand { "-rand" } else { "" }))
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
    let path = db_path(dir, n);
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

fn open_existing(n: u64, dir: &Path, cache_kib: Option<i64>) -> Built {
    let path = db_path(dir, n);
    assert!(
        path.exists(),
        "{} not found; run without --reuse",
        path.display()
    );
    let store = Store::open(&path).expect("open");
    tune(&store, cache_kib);
    let find = |name: &str| {
        store
            .branches()
            .expect("branches")
            .into_iter()
            .find(|b| b.name == name)
            .map(|b| b.id)
            .expect("branch")
    };
    let (main, fork) = (find("main"), find("frozen"));
    Built {
        store,
        main,
        fork,
        path,
        events: 0,
        write_secs: 0.0,
        supersede_events: 0,
    }
}

fn tune(store: &Store, cache_kib: Option<i64>) {
    store.set_mmap_size(0).expect("mmap_size");
    if let Some(k) = cache_kib {
        store.set_cache_size_kib(k).expect("cache_size");
    }
}

fn follow(cross: bool) -> Follow {
    let mut links = vec!["child"];
    if cross {
        links.push("ref");
    }
    let pairs: Vec<FollowRule> = links
        .iter()
        .flat_map(|l| {
            TYPES
                .iter()
                .map(|t| FollowRule::new(*l, Direction::Outbound, *t))
        })
        .collect();
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
                    visited: visited_set.take(),
                    ..WalkOptions::default()
                },
            )
            .expect("walk");
        steps += w.steps;
        loads += w.loads;
        visited_set = w.visited;
        std::hint::black_box(&w.paths);
        match w.next {
            Some(n) => start = n,
            None => return (steps, loads),
        }
    }
}

fn main() {
    let all: Vec<String> = std::env::args().skip(1).collect();
    let flag = |f: &str| all.iter().any(|a| a == f);
    let value = |f: &str| {
        all.iter()
            .position(|a| a == f)
            .and_then(|i| all.get(i + 1).cloned())
    };
    let (reuse, cold) = (flag("--reuse"), flag("--cold"));
    RANDOM_IDS.store(flag("--random-ids"), std::sync::atomic::Ordering::Relaxed);
    let cache_kib: Option<i64> = value("--cache-size").map(|v| v.parse().expect("cache size"));
    let only: Option<Vec<usize>> =
        value("--scenarios").map(|v| v.split(',').map(|x| x.parse().expect("scenario")).collect());
    let mut skip = false;
    let positional: Vec<String> = all
        .iter()
        .filter(|a| {
            if skip {
                skip = false;
                return false;
            }
            if *a == "--cache-size" || *a == "--scenarios" {
                skip = true;
                return false;
            }
            !a.starts_with("--")
        })
        .cloned()
        .collect();
    let mut args = positional.into_iter();
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
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree, page 1000",
            page: Some(1000),
            cross: false,
            visited: false,
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree, one page",
            page: None,
            cross: false,
            visited: false,
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree, page 1000, read-ahead off",
            page: Some(1000),
            cross: false,
            visited: false,
            read_ahead: off,
            fork: false,
        },
        Scenario {
            name: "tree, page 1000, read-ahead width 256",
            page: Some(1000),
            cross: false,
            visited: false,
            read_ahead: ReadAhead {
                depth: 2,
                width: 256,
            },
            fork: false,
        },
        Scenario {
            name: "tree+cross, page 1000, visited set",
            page: Some(1000),
            cross: true,
            visited: true,
            read_ahead: def,
            fork: false,
        },
        Scenario {
            name: "tree, page 1000, frozen fork",
            page: Some(1000),
            cross: false,
            visited: false,
            read_ahead: def,
            fork: true,
        },
    ];

    println!(
        "| nodes | scenario | visited | median s | entities/s | loads per 1k | MB read per run | major faults per run |"
    );
    println!("| ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: |");
    let mut builds = Vec::new();
    for &n in &sizes {
        let mut b = if reuse {
            open_existing(n, &dir, cache_kib)
        } else {
            let b = build(n, &dir);
            tune(&b.store, cache_kib);
            b
        };
        b.store.checkpoint().expect("checkpoint");
        for (i, s) in scenarios.iter().enumerate() {
            if only.as_ref().is_some_and(|o| !o.contains(&i)) {
                continue;
            }
            let mut times = Vec::new();
            let mut last = (0, 0);
            let (mut read, mut faults) = (0u64, 0u64);
            for _ in 0..RUNS {
                if cold {
                    // New connection: empty SQLite cache. Then evict OS pages.
                    let (main, fork) = (b.main, b.fork);
                    drop(std::mem::replace(
                        &mut b.store,
                        Store::open_in_memory().expect("placeholder"),
                    ));
                    evict(&b.path);
                    b.store = Store::open(&b.path).expect("reopen");
                    tune(&b.store, cache_kib);
                    (b.main, b.fork) = (main, fork);
                    evict(&b.path);
                }
                let before = io_counters();
                let t = Instant::now();
                last = run(&b, s);
                times.push(t.elapsed().as_secs_f64());
                let after = io_counters();
                read += after.0 - before.0;
                faults += after.1 - before.1;
            }
            times.sort_by(f64::total_cmp);
            let median = times[RUNS / 2];
            println!(
                "| {n} | {} | {} | {median:.3} | {:.0} | {:.1} | {:.1} | {} |",
                s.name,
                last.0,
                last.0 as f64 / median,
                last.1 as f64 * 1000.0 / last.0 as f64,
                read as f64 / RUNS as f64 / 1e6,
                faults / RUNS as u64
            );
        }
        if !reuse {
            builds.push((
                n,
                b.events,
                b.write_secs,
                b.supersede_events,
                file_mb(&b.path),
            ));
        }
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
