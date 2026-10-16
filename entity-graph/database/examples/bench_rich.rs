//! Rich-history walk benchmark: value and link history, and a stack of
//! branches. Run in release:
//!
//! ```text
//! cargo run --release --example bench_rich -- 100000 /workspace/bench-data
//! cargo run --release --example bench_rich -- 100000 /workspace/bench-data --reuse --cold
//! ```
//!
//! Flags: `--reuse` (open the existing `rich-N.db`), `--cold` (new connection
//! and evicted OS cache before each run, as in `bench_walk`),
//! `--random-ids` (hashed entity IDs, file `rich-N-rand.db`).
//!
//! Graph: the same tree as `bench_walk` (2..=15 children, same seed and
//! generator calls, about 5% `ref` cross links). On top of it, on `base`:
//! - Every entity gets 1..=3 more revisions (2..=4 in all) of 3 or 4 of its
//!   fields `title`, `done`, `priority`, `note`. `priority` and `note` are set
//!   to null in 15% of the revisions.
//! - 30% of the `child` links get an explicit sort key; 15% are removed and
//!   added again with another explicit sort key.
//!
//! Branch stack (the same edits go to the frozen and the open variant):
//! - `mid_frozen` = fork of `base`; `mid_open` = open pull of `base`. Each
//!   gets its own edits: 15% of titles and priorities overridden, 2% of
//!   entities archived (null type), 1% of child links removed.
//! - `top_frozen` = fork of `mid_frozen`; `top_open` = open pull of
//!   `mid_open`. Each gets 5% titles overridden, 1% archived, and 2% of
//!   child links with a new explicit sort key.
//! - Then `base` gets 10% more title edits, so the open stack sees them and
//!   the frozen stack does not.
//!
//! Walks use page 1000, depth 2, width 32, `child` links only.

use std::collections::HashMap;
use std::os::fd::AsRawFd;
use std::path::{Path, PathBuf};
use std::time::Instant;

use egdb::{
    BranchId, Direction, Follow, Op, ParentRef, ReadAhead, SeqRange, SortOrder, Store, Walk,
    WalkOptions, WriteBatch, TYPE_KEY,
};

extern "C" {
    fn posix_fadvise(fd: i32, offset: i64, len: i64, advice: i32) -> i32;
}

fn evict(path: &Path) {
    for ext in ["", "-wal"] {
        if let Ok(f) = std::fs::File::open(format!("{}{ext}", path.display())) {
            // SAFETY: valid fd for the duration of the call.
            let rc = unsafe { posix_fadvise(f.as_raw_fd(), 0, 0, 4) };
            assert_eq!(rc, 0, "posix_fadvise");
        }
    }
}

fn read_bytes() -> u64 {
    std::fs::read_to_string("/proc/self/io")
        .unwrap_or_default()
        .lines()
        .find_map(|l| l.strip_prefix("read_bytes: "))
        .and_then(|v| v.trim().parse().ok())
        .unwrap_or(0)
}

const TYPES: [&str; 4] = ["list", "task", "note", "section"];
const BATCH_OPS: usize = 2_000;
const CROSS_SHARE: f64 = 0.05;
const RUNS: usize = 3;
const SEED: u64 = 0x5eed;

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
    dir.join(format!("rich-{n}{}.db", if rand { "-rand" } else { "" }))
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

struct Writer<'a> {
    store: &'a mut Store,
    ops: Vec<Op>,
    events: u64,
}

impl Writer<'_> {
    fn push(&mut self, branch: BranchId, op: Op) {
        self.ops.push(op);
        if self.ops.len() >= BATCH_OPS {
            self.flush(branch);
        }
    }
    fn flush(&mut self, branch: BranchId) {
        if self.ops.is_empty() {
            return;
        }
        self.events += self.ops.len() as u64;
        self.store
            .write(WriteBatch::new(branch, "bench", std::mem::take(&mut self.ops)))
            .expect("write");
    }
}

struct Built {
    store: Store,
    path: PathBuf,
    branches: HashMap<&'static str, BranchId>,
}

const NAMES: [&str; 5] = ["base", "top_frozen", "top_open", "mid_frozen", "mid_open"];

fn build(n: u64, dir: &Path) -> Built {
    let path = db_path(dir, n);
    for ext in ["", "-wal", "-shm"] {
        let _ = std::fs::remove_file(format!("{}{ext}", path.display()));
    }
    let mut store = Store::open(&path).expect("open");
    let base = store.create_branch("base", &[]).expect("branch");
    let started = Instant::now();
    // The tree: identical generator calls to bench_walk's `build`.
    let mut rng = Rng(SEED ^ n);
    let mut parent_of: Vec<u32> = vec![0; n as usize];
    let mut w = Writer {
        store: &mut store,
        ops: Vec::new(),
        events: 0,
    };
    let node_ops = |rng: &mut Rng, i: u64, w: &mut Writer| {
        let e = id(i);
        let ty = TYPES[rng.range(0, 3) as usize];
        w.ops.push(Op::set(e.as_str(), TYPE_KEY, ty));
        w.ops.push(Op::set(e.as_str(), "title", format!("Item {i}: some title text")));
        w.ops.push(Op::set(e.as_str(), "done", rng.chance(0.3)));
        w.ops.push(Op::set(e.as_str(), "priority", rng.range(0, 5) as i64));
    };
    node_ops(&mut rng, 0, &mut w);
    let mut next_id = 1;
    let mut parent = 0;
    while next_id < n {
        let k = rng.range(2, 15);
        for _ in 0..k {
            if next_id >= n {
                break;
            }
            node_ops(&mut rng, next_id, &mut w);
            w.ops.push(Op::link(id(parent), id(next_id), "child", SortOrder::Timestamp));
            parent_of[next_id as usize] = parent as u32;
            next_id += 1;
            if w.ops.len() >= BATCH_OPS {
                w.flush(base);
            }
        }
        parent += 1;
    }
    for i in 0..n {
        if rng.chance(CROSS_SHARE) {
            let to = rng.range(0, n - 1);
            w.push(base, Op::link(id(i), id(to), "ref", SortOrder::Timestamp));
        }
    }
    w.flush(base);
    let tree_events = w.events;
    let tree_secs = started.elapsed().as_secs_f64();

    // History on base. A separate RNG, so the tree above equals bench_walk's.
    let mut h = Rng(SEED ^ n ^ 0xA11CE);
    for i in 0..n {
        let e = id(i);
        for rev in 0..h.range(1, 3) {
            let fields: &[&str] = if h.chance(0.5) {
                &["title", "done", "priority", "note"]
            } else {
                &["title", "done", "priority"]
            };
            for f in fields {
                let op = match *f {
                    "title" => Op::set(e.as_str(), "title", format!("Item {i}: rev {rev}")),
                    "done" => Op::set(e.as_str(), "done", h.chance(0.5)),
                    "priority" if h.chance(0.15) => Op::clear(e.as_str(), "priority"),
                    "priority" => Op::set(e.as_str(), "priority", h.range(0, 9) as i64),
                    _ if h.chance(0.15) => Op::clear(e.as_str(), "note"),
                    _ => Op::set(e.as_str(), "note", format!("note {i} r{rev}")),
                };
                w.push(base, op);
            }
        }
    }
    for i in 1..n {
        if h.chance(0.30) {
            let (p, c) = (id(parent_of[i as usize] as u64), id(i));
            w.push(base, Op::link(p.as_str(), c.as_str(), "child", SortOrder::Explicit(h.range(0, 1000) as f64)));
            if h.chance(0.5) {
                w.push(base, Op::unlink(p.as_str(), c.as_str(), "child"));
                w.push(base, Op::link(p.as_str(), c.as_str(), "child", SortOrder::Explicit(h.range(0, 1000) as f64)));
            }
        }
    }
    w.flush(base);
    let base_secs = started.elapsed().as_secs_f64();

    // The stack.
    let mid_frozen = w.store.fork(base, "mid_frozen").expect("fork");
    let mid_open = w
        .store
        .create_branch("mid_open", &[ParentRef::new(base, SeqRange::ALL)])
        .expect("branch");
    for b in [mid_frozen, mid_open] {
        let mut m = Rng(SEED ^ n ^ 0xB0B);
        for i in 0..n {
            let e = id(i);
            let over = m.chance(0.15);
            let archive = i > 0 && m.chance(0.02);
            let cut = i > 0 && m.chance(0.01);
            if over {
                w.push(b, Op::set(e.as_str(), "title", format!("Item {i}: mid")));
                w.push(b, Op::set(e.as_str(), "priority", (i % 7) as i64));
            }
            if archive {
                w.push(b, Op::clear(e.as_str(), TYPE_KEY));
            }
            if cut {
                w.push(b, Op::unlink(id(parent_of[i as usize] as u64), e.as_str(), "child"));
            }
        }
        w.flush(b);
    }
    let top_frozen = w.store.fork(mid_frozen, "top_frozen").expect("fork");
    let top_open = w
        .store
        .create_branch("top_open", &[ParentRef::new(mid_open, SeqRange::ALL)])
        .expect("branch");
    for b in [top_frozen, top_open] {
        let mut t = Rng(SEED ^ n ^ 0x70B);
        for i in 0..n {
            let e = id(i);
            let over = t.chance(0.05);
            let archive = i > 0 && t.chance(0.01);
            let resort = i > 0 && t.chance(0.02);
            let key = t.range(0, 1000) as f64;
            if over {
                w.push(b, Op::set(e.as_str(), "title", format!("Item {i}: top")));
            }
            if archive {
                w.push(b, Op::clear(e.as_str(), TYPE_KEY));
            }
            if resort {
                w.push(b, Op::link(id(parent_of[i as usize] as u64), e.as_str(), "child", SortOrder::Explicit(key)));
            }
        }
        w.flush(b);
    }
    for i in 0..n {
        if h.chance(0.10) {
            w.push(base, Op::set(id(i).as_str(), "title", format!("Item {i}: late")));
        }
    }
    w.flush(base);
    let (events, total_secs) = (w.events, started.elapsed().as_secs_f64());
    store.checkpoint().expect("checkpoint");
    println!("| nodes | tree events | tree write s | base write s (tree + history) | all events | total build s | file MB |");
    println!("| ---: | ---: | ---: | ---: | ---: | ---: | ---: |");
    let _ = (events, tree_events);
    println!(
        "| {n} | {tree_events} | {tree_secs:.2} | {base_secs:.2} | {events} | {total_secs:.2} | {:.1} |",
        file_mb(&path)
    );
    let branches = HashMap::from([
        ("base", base),
        ("mid_frozen", mid_frozen),
        ("mid_open", mid_open),
        ("top_frozen", top_frozen),
        ("top_open", top_open),
    ]);
    Built { store, path, branches }
}

fn open_existing(n: u64, dir: &Path) -> Built {
    let path = db_path(dir, n);
    assert!(path.exists(), "{} not found; run without --reuse", path.display());
    let store = Store::open(&path).expect("open");
    let branches = branch_map(&store);
    Built { store, path, branches }
}

fn branch_map(store: &Store) -> HashMap<&'static str, BranchId> {
    let all = store.branches().expect("branches");
    NAMES
        .iter()
        .map(|n| (*n, all.iter().find(|b| b.name == *n).expect("branch").id))
        .collect()
}

fn follow() -> Follow {
    TYPES
        .iter()
        .map(|t| ((*t).to_owned(), vec![("child".to_owned(), Direction::Outbound)]))
        .collect()
}

/// Walk to the end, page by page. Returns (visited, emitted, loads).
fn run(store: &Store, branch: BranchId, read_ahead: ReadAhead) -> (usize, usize, usize) {
    let follow = follow();
    let mut start = vec![id(0)];
    let (mut steps, mut loads, mut emitted) = (0, 0, 0);
    // QCOUNT-START
    loop {
        let refs: Vec<&str> = start.iter().map(String::as_str).collect();
        let w: Walk = store
            .walk(
                branch,
                &refs,
                &follow,
                |_, _| true,
                |_, _| true,
                WalkOptions {
                    steps: 1000,
                    read_ahead,
                    return_cache: false,
                    visited: None,
                },
            )
            .expect("walk");
        steps += w.steps;
        loads += w.loads;
        emitted += w.rows.len();
        std::hint::black_box(&w.rows);
        match w.next {
            Some(n) => start = n,
            None => {
                // QCOUNT-END
                return (steps, emitted, loads);
            }
        }
    }
}

fn main() {
    let all: Vec<String> = std::env::args().skip(1).collect();
    let flag = |f: &str| all.iter().any(|a| a == f);
    let (reuse, cold) = (flag("--reuse"), flag("--cold"));
    RANDOM_IDS.store(flag("--random-ids"), std::sync::atomic::Ordering::Relaxed);
    let pos: Vec<&String> = all.iter().filter(|a| !a.starts_with("--")).collect();
    let n: u64 = pos.first().map_or(100_000, |s| s.parse().expect("size"));
    let dir = PathBuf::from(pos.get(1).map_or("/workspace/bench-data", |s| s.as_str()));
    std::fs::create_dir_all(&dir).expect("dir");
    let mut b = if reuse { open_existing(n, &dir) } else { build(n, &dir) };
    b.store.set_mmap_size(0).expect("mmap");

    // Table row counts.
    let conn = rusqlite::Connection::open_with_flags(&b.path, rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY).expect("ro");
    println!();
    println!("| table | rows |");
    println!("| --- | ---: |");
    for t in ["value_current", "value_history", "link_current", "link_history"] {
        let c: i64 = conn.query_row(&format!("SELECT count(*) FROM {t}"), [], |r| r.get(0)).unwrap();
        println!("| {t} | {c} |");
    }
    drop(conn);
    println!();
    println!("| scenario | visited | emitted | median s | entities/s | loads per 1k | MB read per run |");
    println!("| --- | ---: | ---: | ---: | ---: | ---: | ---: |");
    let def = ReadAhead::default();
    let off = ReadAhead { depth: 0, width: 32 };
    let scenarios = [
        ("base, page 1000", "base", def),
        ("mid_frozen, page 1000", "mid_frozen", def),
        ("mid_open, page 1000", "mid_open", def),
        ("top_frozen, page 1000", "top_frozen", def),
        ("top_open, page 1000", "top_open", def),
        ("top_open, page 1000, read-ahead off", "top_open", off),
    ];
    for (name, br, ra) in scenarios {
        let mut times = Vec::new();
        let (mut last, mut read) = ((0, 0, 0), 0u64);
        for _ in 0..RUNS {
            if cold {
                let placeholder = Store::open_in_memory().expect("placeholder");
                drop(std::mem::replace(&mut b.store, placeholder));
                evict(&b.path);
                b.store = Store::open(&b.path).expect("reopen");
                b.store.set_mmap_size(0).expect("mmap");
                b.branches = branch_map(&b.store);
                evict(&b.path);
            }
            let before = read_bytes();
            let t = Instant::now();
            last = run(&b.store, b.branches[br], ra);
            times.push(t.elapsed().as_secs_f64());
            read += read_bytes() - before;
        }
        times.sort_by(f64::total_cmp);
        let median = times[RUNS / 2];
        println!(
            "| {name} | {} | {} | {median:.3} | {:.0} | {:.1} | {:.1} |",
            last.0,
            last.1,
            last.0 as f64 / median,
            last.2 as f64 * 1000.0 / last.0 as f64,
            read as f64 / RUNS as f64 / 1e6
        );
    }
}
