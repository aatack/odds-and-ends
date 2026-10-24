//! Write-throughput benchmark: batches/s and events/s with 0, 1 or 3 identity
//! middleware, on a temp file db. CPU time via `/proc/thread-self/schedstat`,
//! like `examples/bench_find.rs`.
//!
//! ```text
//! cargo run --release --example bench_write -- [N] [M] [--mw 0|1|3] [--runs K]
//! ```
//!
//! `N` batches (default 1000) of `M` ops each (default 10), a mix of value and
//! link ops. Without `--mw` it runs all three middleware counts (0, 1, 3);
//! with `--mw` it runs only that one. Each config is run `--runs` times
//! (default 5) on a fresh db; the reported figures are the medians. It stays
//! well under 30 s at the defaults.

use std::path::PathBuf;
use std::time::Instant;

use egdb::{Op, SortOrder, Store, Value, WriteBatch, WriteContext};

/// CPU time of this thread in ns (`/proc/thread-self/schedstat`): stable when
/// other jobs share the machine, unlike wall time.
fn cpu_ns() -> u64 {
    std::fs::read_to_string("/proc/thread-self/schedstat")
        .ok()
        .and_then(|s| s.split_whitespace().next()?.parse().ok())
        .unwrap_or(0)
}

/// Entities are reused across batches, so links connect real entities.
const ENTITY_SPACE: u64 = 4096;

fn op(k: u64) -> Op {
    let e = format!("e{}", k % ENTITY_SPACE);
    // One op in three is a link; the rest are value writes.
    if k % 3 == 2 {
        let f = format!("e{}", (k + 1) % ENTITY_SPACE);
        Op::link(e, f, "rel", SortOrder::Timestamp)
    } else {
        Op::set(e, "k", Value::Int(k as i64))
    }
}

struct TempDb(PathBuf);

impl TempDb {
    fn new(tag: usize) -> Self {
        let path =
            std::env::temp_dir().join(format!("egdb_bench_write_{}_{tag}.db", std::process::id()));
        let db = Self(path);
        db.remove();
        db
    }

    fn remove(&self) {
        for suffix in ["", "-wal", "-shm"] {
            let _ = std::fs::remove_file(format!("{}{suffix}", self.0.display()));
        }
    }
}

impl Drop for TempDb {
    fn drop(&mut self) {
        self.remove();
    }
}

struct Sample {
    batches_per_s: f64,
    events_per_s: f64,
    cpu_s: f64,
    wall_s: f64,
}

fn one_run(tag: usize, mw: usize, n: u64, m: u64) -> Sample {
    let db = TempDb::new(tag);
    let mut store = Store::open(&db.0).unwrap();
    store.set_mmap_size(0).unwrap();
    let branch = store.create_branch("main", &[]).unwrap();
    for _ in 0..mw {
        store.add_middleware(|_: &WriteContext, _: &mut Vec<Op>| Ok(()));
    }

    let wall = Instant::now();
    let cpu = cpu_ns();
    let mut k = 0u64;
    for _ in 0..n {
        let ops: Vec<Op> = (0..m)
            .map(|_| {
                let o = op(k);
                k += 1;
                o
            })
            .collect();
        store.write(WriteBatch::new(branch, "bench", ops)).unwrap();
    }
    let cpu_s = (cpu_ns() - cpu) as f64 / 1e9;
    let wall_s = wall.elapsed().as_secs_f64();
    Sample {
        batches_per_s: n as f64 / wall_s,
        events_per_s: (n * m) as f64 / wall_s,
        cpu_s,
        wall_s,
    }
}

fn median(mut xs: Vec<f64>) -> f64 {
    xs.sort_by(f64::total_cmp);
    xs[xs.len() / 2]
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    let value = |f: &str| {
        args.iter()
            .position(|a| a == f)
            .and_then(|i| args.get(i + 1))
            .cloned()
    };
    let pos: Vec<&String> = args
        .iter()
        .enumerate()
        .filter(|(i, a)| !a.starts_with("--") && (*i == 0 || !args[i - 1].starts_with("--")))
        .map(|(_, a)| a)
        .collect();
    let n: u64 = pos.first().map(|s| s.parse().unwrap()).unwrap_or(1000);
    let m: u64 = pos.get(1).map(|s| s.parse().unwrap()).unwrap_or(10);
    let runs: usize = value("--runs").map(|s| s.parse().unwrap()).unwrap_or(5);
    let configs: Vec<usize> = match value("--mw") {
        Some(s) => vec![s.parse().unwrap()],
        None => vec![0, 1, 3],
    };

    println!("N={n} batches, M={m} ops/batch, {runs} runs each (median reported)");
    println!();
    println!("| middleware | batches/s | events/s | CPU s | wall s |");
    println!("| ---: | ---: | ---: | ---: | ---: |");
    let mut tag = 0;
    for mw in configs {
        let mut samples = Vec::with_capacity(runs);
        for _ in 0..runs {
            samples.push(one_run(tag, mw, n, m));
            tag += 1;
        }
        println!(
            "| {mw} | {:.0} | {:.0} | {:.3} | {:.3} |",
            median(samples.iter().map(|s| s.batches_per_s).collect()),
            median(samples.iter().map(|s| s.events_per_s).collect()),
            median(samples.iter().map(|s| s.cpu_s).collect()),
            median(samples.iter().map(|s| s.wall_s).collect()),
        );
    }
}
