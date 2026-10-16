//! Scale benchmark: grow one store to many events and measure at milestones.
//!
//! ```text
//! cargo run --release --example bench_scale -- --dir /workspace/egdb-scale \
//!     --milestones 100000,1000000,10000000 [--batch 2000] [--page-size 4096] \
//!     [--cache-kib 2000] [--mmap 0] [--sequential-ids] [--no-cold] [--no-dbstat]
//! ```
//!
//! The generator is the one of `bench_walk`: a tree where each node has 2..=15
//! children, four typed values per node (4 events), one `child` link (1 event)
//! and a `ref` link on 5% of nodes. Entity IDs are hashed (like UUIDs) unless
//! `--sequential-ids` is given. About 5.05 events per node.
//!
//! At each milestone (entity count) it checkpoints and reports: events, file
//! size, bytes per event, write rate since the last milestone, warm and cold
//! point reads (`values` of one random entity, `children` of one random
//! internal node), warm and cold walks (page 1000, from random internal
//! nodes), and the size of each table and index (`dbstat`).
//! "Cold" means the OS page cache for the file is dropped and a new
//! connection is opened (SQLite cache empty); one read of the root runs first,
//! so the schema is loaded and the upper levels of the root path are cached.

use std::collections::HashMap;
use std::os::fd::AsRawFd;
use std::path::{Path, PathBuf};
use std::time::Instant;

use egdb::{
    BranchId, Direction, Follow, Op, ReadAhead, SortOrder, Store, WalkOptions, WriteBatch, TYPE_KEY,
};

extern "C" {
    fn posix_fadvise(fd: i32, offset: i64, len: i64, advice: i32) -> i32;
}

fn evict(path: &Path) {
    for ext in ["", "-wal"] {
        if let Ok(f) = std::fs::File::open(format!("{}{ext}", path.display())) {
            // SAFETY: valid fd for the duration of the call.
            let rc = unsafe { posix_fadvise(f.as_raw_fd(), 0, 0, 4) };
            assert_eq!(rc, 0);
        }
    }
}

const TYPES: [&str; 4] = ["list", "task", "note", "section"];

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

static SEQUENTIAL: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

fn id(n: u64) -> String {
    if SEQUENTIAL.load(std::sync::atomic::Ordering::Relaxed) {
        format!("n{n}")
    } else {
        let mut z = n.wrapping_add(0x9e37_79b9_7f4a_7c15);
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        format!("{:016x}", z ^ (z >> 31))
    }
}

fn rss_mb() -> f64 {
    std::fs::read_to_string("/proc/self/status")
        .ok()
        .and_then(|s| {
            s.lines()
                .find(|l| l.starts_with("VmRSS:"))
                .and_then(|l| l.split_whitespace().nth(1)?.parse::<f64>().ok())
        })
        .map_or(0.0, |kb| kb / 1024.0)
}

fn meminfo() -> String {
    let m = std::fs::read_to_string("/proc/meminfo").unwrap_or_default();
    let get = |k: &str| -> f64 {
        m.lines()
            .find(|l| l.starts_with(k))
            .and_then(|l| l.split_whitespace().nth(1)?.parse::<f64>().ok())
            .unwrap_or(0.0)
            / 1_048_576.0
    };
    format!(
        "RAM total {:.1} GB, available {:.1} GB, page cache {:.1} GB",
        get("MemTotal:"),
        get("MemAvailable:"),
        get("Cached:")
    )
}

/// (bytes written to storage, bytes passed to write calls) for this process so far.
fn write_counters() -> (u64, u64) {
    let io = std::fs::read_to_string("/proc/self/io").unwrap_or_default();
    let get = |k: &str| -> u64 {
        io.lines()
            .find_map(|l| l.strip_prefix(k))
            .and_then(|v| v.trim().parse().ok())
            .unwrap_or(0)
    };
    (get("write_bytes: "), get("wchar: "))
}

fn file_bytes(path: &Path) -> u64 {
    std::fs::metadata(path).map_or(0, |m| m.len())
}

fn follow() -> Follow {
    let pairs = vec![("child".to_owned(), Direction::Outbound)];
    TYPES
        .iter()
        .map(|t| ((*t).to_owned(), pairs.clone()))
        .collect::<HashMap<_, _>>()
}

fn pct(v: &mut [f64], p: f64) -> f64 {
    v.sort_by(f64::total_cmp);
    v[((v.len() - 1) as f64 * p).round() as usize]
}

struct World {
    store: Store,
    main: BranchId,
    path: PathBuf,
    cache_kib: i64,
    mmap: i64,
}

impl World {
    fn tune(&self) {
        self.store.set_mmap_size(self.mmap).unwrap();
        self.store.set_cache_size_kib(self.cache_kib).unwrap();
    }
    fn reopen_cold(&mut self) {
        let placeholder = Store::open_in_memory().unwrap();
        drop(std::mem::replace(&mut self.store, placeholder));
        evict(&self.path);
        self.store = Store::open(&self.path).unwrap();
        self.tune();
        // Load the schema and the root path; then drop the OS cache again.
        self.store.values(self.main, &[&id(0)]).unwrap();
        evict(&self.path);
    }
}

fn point_reads(w: &World, nodes: u64, internal: u64, n: usize, rng: &mut Rng) -> (Vec<f64>, Vec<f64>) {
    let (mut v, mut c) = (Vec::new(), Vec::new());
    for _ in 0..n {
        let e = id(rng.range(0, nodes - 1));
        let t = Instant::now();
        std::hint::black_box(w.store.values(w.main, &[&e]).unwrap());
        v.push(t.elapsed().as_secs_f64() * 1e6);
        let p = id(rng.range(0, internal.max(1) - 1));
        let t = Instant::now();
        std::hint::black_box(w.store.children(w.main, &p, "child").unwrap());
        c.push(t.elapsed().as_secs_f64() * 1e6);
    }
    (v, c)
}

/// Walk `pages` pages of 1000 steps from the root, each page resuming from the
/// last path (stateless paging). With `cold`, the OS and SQLite caches are
/// dropped before each page. Returns (entities, seconds).
fn walk_pages(w: &mut World, pages: usize, cold: bool) -> (usize, f64) {
    let follow = follow();
    let (mut seen, mut secs) = (0, 0.0);
    let mut start = vec![id(0)];
    for _ in 0..pages {
        if cold {
            w.reopen_cold();
        }
        let refs: Vec<&str> = start.iter().map(String::as_str).collect();
        let t = Instant::now();
        let walk = w
            .store
            .walk(
                w.main,
                &refs,
                &follow,
                |_, _| true,
                |_, _| true,
                WalkOptions { steps: 1000, read_ahead: ReadAhead::default(), return_cache: false, visited: None },
            )
            .unwrap();
        secs += t.elapsed().as_secs_f64();
        seen += walk.steps;
        match walk.next {
            Some(n) => start = n,
            None => break,
        }
    }
    (seen, secs)
}

fn dbstat(path: &Path) {
    let conn = rusqlite::Connection::open_with_flags(path, rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY).unwrap();
    let ps: i64 = conn.query_row("PRAGMA page_size", [], |r| r.get(0)).unwrap();
    let pages: i64 = conn.query_row("PRAGMA page_count", [], |r| r.get(0)).unwrap();
    println!("page size {ps}, pages {pages}");
    let mut stmt = match conn.prepare(
        "SELECT name, count(*) AS pages, sum(pgsize) AS bytes, sum(unused) AS unused,
                max(CASE WHEN pagetype='internal' THEN 1 ELSE 0 END) AS has_internal,
                sum(CASE WHEN pagetype='internal' THEN 1 ELSE 0 END) AS internal_pages
         FROM dbstat WHERE aggregate = 0 GROUP BY name ORDER BY bytes DESC",
    ) {
        Ok(s) => s,
        Err(e) => {
            println!("dbstat not available: {e}");
            return;
        }
    };
    println!();
    println!("| table / index | MB | % of file | internal pages | unused % |");
    println!("| --- | ---: | ---: | ---: | ---: |");
    let rows: Vec<(String, i64, i64, i64, i64)> = stmt
        .query_map([], |r| Ok((r.get(0)?, r.get(1)?, r.get(2)?, r.get(3)?, r.get(5)?)))
        .unwrap()
        .map(Result::unwrap)
        .collect();
    let total: i64 = rows.iter().map(|r| r.2).sum();
    for (name, _p, bytes, unused, internal) in rows {
        println!(
            "| {name} | {:.0} | {:.1} | {internal} | {:.1} |",
            bytes as f64 / 1e6,
            100.0 * bytes as f64 / total as f64,
            100.0 * unused as f64 / bytes as f64
        );
    }
}

fn main() {
    let all: Vec<String> = std::env::args().skip(1).collect();
    let flag = |f: &str| all.iter().any(|a| a == f);
    let value = |f: &str| all.iter().position(|a| a == f).map(|i| all[i + 1].clone());
    SEQUENTIAL.store(flag("--sequential-ids"), std::sync::atomic::Ordering::Relaxed);
    let dir = PathBuf::from(value("--dir").unwrap_or("/workspace/egdb-scale".into()));
    std::fs::create_dir_all(&dir).unwrap();
    let milestones: Vec<u64> = value("--milestones")
        .unwrap_or("100000,1000000".into())
        .split(',')
        .map(|s| s.parse().unwrap())
        .collect();
    let batch: usize = value("--batch").map_or(2000, |v| v.parse().unwrap());
    let page_size: Option<i64> = value("--page-size").map(|v| v.parse().unwrap());
    let cache_kib: i64 = value("--cache-kib").map_or(2000, |v| v.parse().unwrap());
    let mmap: i64 = value("--mmap").map_or(0, |v| v.parse().unwrap());
    let name = value("--name").unwrap_or("scale".into());
    let path = dir.join(format!("{name}.db"));
    for ext in ["", "-wal", "-shm"] {
        let _ = std::fs::remove_file(format!("{}{ext}", path.display()));
    }
    if let Some(ps) = page_size {
        let c = rusqlite::Connection::open(&path).unwrap();
        c.pragma_update(None, "page_size", ps).unwrap();
        c.pragma_update_and_check(None, "journal_mode", "WAL", |_| Ok(())).unwrap();
    }
    let mut store = Store::open(&path).unwrap();
    let main = store.create_branch("main", &[]).unwrap();
    let mut w = World { store, main, path: path.clone(), cache_kib, mmap };
    w.tune();
    println!("# bench_scale {name}: batch {batch} ops, page size {page_size:?}, cache {cache_kib} KiB, mmap {mmap}, {} ids", if flag("--sequential-ids") { "sequential" } else { "hashed" });
    println!("{}", meminfo());
    println!();
    println!("| entities | events | interval events/s | cumulative events/s | file MB (after checkpoint) |");
    println!("| ---: | ---: | ---: | ---: | ---: |");

    let mut rng = Rng(0x5eed);
    let mut mrng = Rng(0xbeef);
    let mut ops: Vec<Op> = Vec::with_capacity(batch + 16);
    let mut events: u64 = 0;
    let (mut next_id, mut parent) = (1u64, 0u64);
    let mut flush_secs = 0.0;
    let (mut last_events, mut last_secs) = (0u64, 0.0);
    let started = Instant::now();
    let mut results = Vec::new();

    let node_ops = |rng: &mut Rng, i: u64, ops: &mut Vec<Op>| {
        let e = id(i);
        let ty = TYPES[rng.range(0, 3) as usize];
        ops.push(Op::set(e.as_str(), TYPE_KEY, ty));
        ops.push(Op::set(e.as_str(), "title", format!("Item {i}: some title text")));
        ops.push(Op::set(e.as_str(), "done", rng.chance(0.3)));
        ops.push(Op::set(e.as_str(), "priority", rng.range(0, 5) as i64));
    };
    let flush = |w: &mut World, ops: &mut Vec<Op>, events: &mut u64, secs: &mut f64| {
        if ops.is_empty() {
            return;
        }
        *events += ops.len() as u64;
        let t = Instant::now();
        w.store.write(WriteBatch::new(w.main, "bench", std::mem::take(ops))).unwrap();
        *secs += t.elapsed().as_secs_f64();
    };
    node_ops(&mut rng, 0, &mut ops);

    let target = *milestones.last().unwrap();
    let mut mi = 0;
    let mut next_log = 1_000_000u64;
    while next_id < target {
        let k = rng.range(2, 15);
        for _ in 0..k {
            if next_id >= target {
                break;
            }
            node_ops(&mut rng, next_id, &mut ops);
            ops.push(Op::link(id(parent), id(next_id), "child", SortOrder::Timestamp));
            if rng.chance(0.05) {
                let to = rng.range(0, next_id);
                ops.push(Op::link(id(next_id), id(to), "ref", SortOrder::Timestamp));
            }
            next_id += 1;
            if ops.len() >= batch {
                flush(&mut w, &mut ops, &mut events, &mut flush_secs);
            }
            if events >= next_log {
                next_log += 1_000_000;
                if events % 5_000_000 < 1_000_000 {
                    eprintln!("[{:>7.0}s] {} events, {:.0} events/s since last", started.elapsed().as_secs_f64(), events, (events - last_events) as f64 / (flush_secs - last_secs));
                    last_events = events;
                    last_secs = flush_secs;
                }
            }
            if mi < milestones.len() && next_id >= milestones[mi] {
                flush(&mut w, &mut ops, &mut events, &mut flush_secs);
                let t = Instant::now();
                w.store.checkpoint().unwrap();
                let ck = t.elapsed().as_secs_f64();
                let n = milestones[mi];
                println!(
                    "| {n} | {events} | {:.0} | {:.0} | {:.0} |",
                    (events - last_events) as f64 / (flush_secs - last_secs),
                    events as f64 / flush_secs,
                    file_bytes(&path) as f64 / 1e6
                );
                results.push((n, events, ck));
                measure(&mut w, n, parent, events, ck, &mut mrng, flag("--no-cold"), flag("--no-dbstat"));
                last_events = events;
                last_secs = flush_secs;
                mi += 1;
            }
        }
        parent += 1;
    }
    println!();
    println!("total write time {:.0}s, {:.0} events/s overall (write calls only)", flush_secs, events as f64 / flush_secs);
    let (wb, wc) = write_counters();
    println!(
        "process wrote {:.0} MB to storage (write_bytes) and {:.0} MB through write calls (wchar), for {:.0} MB of final file: {:.1} bytes written per event",
        wb as f64 / 1e6,
        wc as f64 / 1e6,
        file_bytes(&path) as f64 / 1e6,
        wb as f64 / events as f64
    );
}

#[allow(clippy::too_many_arguments)]
fn measure(w: &mut World, nodes: u64, internal: u64, events: u64, ck: f64, rng: &mut Rng, no_cold: bool, no_dbstat: bool) {
    eprintln!("-- measuring at {nodes} entities");
    let mut out = String::new();
    out += &format!("\n### {nodes} entities, {events} events\n\n");
    out += &format!("{}; process RSS {:.0} MB; checkpoint {:.2}s\n\n", meminfo(), rss_mb(), ck);
    let bytes = file_bytes(&w.path);
    out += &format!("file {:.0} MB = {:.1} bytes per event, {:.0} bytes per entity\n\n", bytes as f64 / 1e6, bytes as f64 / events as f64, bytes as f64 / nodes as f64);
    out += "| measurement | value |\n| --- | ---: |\n";
    // Warm: prime with a few thousand random reads first.
    let _ = point_reads(w, nodes, internal, 2000, rng);
    let (mut v, mut c) = point_reads(w, nodes, internal, 3000, rng);
    out += &format!("| warm `values` of one entity, p50 / p99 (us) | {:.0} / {:.0} |\n", pct(&mut v, 0.5), pct(&mut v, 0.99));
    out += &format!("| warm `children` of one node, p50 / p99 (us) | {:.0} / {:.0} |\n", pct(&mut c, 0.5), pct(&mut c, 0.99));
    let (seen, secs) = walk_pages(w, 20, false);
    out += &format!("| warm walk from the root, 20 pages of 1000: entities/s | {:.0} |\n", seen as f64 / secs);
    if !no_cold {
        let (mut v, mut c) = (Vec::new(), Vec::new());
        for _ in 0..60 {
            w.reopen_cold();
            let e = id(rng.range(0, nodes - 1));
            let t = Instant::now();
            std::hint::black_box(w.store.values(w.main, &[&e]).unwrap());
            v.push(t.elapsed().as_secs_f64() * 1e6);
            let p = id(rng.range(0, internal.max(1) - 1));
            let t = Instant::now();
            std::hint::black_box(w.store.children(w.main, &p, "child").unwrap());
            c.push(t.elapsed().as_secs_f64() * 1e6);
        }
        out += &format!("| cold `values`, p50 / p99 (us), 60 samples | {:.0} / {:.0} |\n", pct(&mut v, 0.5), pct(&mut v, 0.99));
        out += &format!("| cold `children` after one values read, p50 / p99 (us) | {:.0} / {:.0} |\n", pct(&mut c, 0.5), pct(&mut c, 0.99));
        let (seen, secs) = walk_pages(w, 8, true);
        out += &format!("| cold walk, 8 pages of 1000, caches dropped before each page: entities/s | {:.0} |\n", seen as f64 / secs);
        w.store.set_cache_size_kib(w.cache_kib).unwrap();
    }
    println!("{out}");
    if !no_dbstat {
        dbstat(&w.path);
    }
    println!("{}", meminfo());
    println!();
    println!("| entities | events | interval events/s | cumulative events/s | file MB (after checkpoint) |");
    println!("| ---: | ---: | ---: | ---: | ---: |");
}

