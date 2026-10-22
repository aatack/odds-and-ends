//! Text and type search benchmark for `find`: `type` equality, `name`
//! contains / contains-ignore-case / prefix, and fuzzy (typo-tolerant)
//! matching done with the current API (a `Custom` predicate, with and
//! without a `ContainsIgnoreCase` pre-filter).
//!
//! ```text
//! cargo run --release --example bench_search -- build N DIR
//! cargo run --release --example bench_search -- run FILE [--branches base,top_open,top_frozen] \
//!     [--queries t1,c2,...] [--repeats 7] [--full-repeats 3] [--limit 1000] [--cold] [--knob probe=1]...
//! ```
//!
//! Data (`build`, file `DIR/search-N.db`, hashed 16-hex entity IDs, batches
//! of 2000 ops):
//! - Every entity on `base`: `type` (task 50%, note 25%, project 10%, person
//!   9.9%, doc 5%, invoice 0.1%), `name` (2 to 5 words from a fixed list of
//!   about 300 words, Zipf-like word frequencies, mixed case: 60% Title Case,
//!   25% sentence case, 10% lower case, 5% upper case; 10% get a number such
//!   as "2026" or "#417"), `done`, `priority`, and one `child` link.
//! - History on `base`: 30% of the entities are renamed 1 or 2 times, 3% get
//!   another type, 20% get `done` toggled.
//! - The stack of `bench_rich`: `mid_frozen` = fork of base, `mid_open` =
//!   open pull of base; each renames 10%, archives 1% (null type), retypes
//!   0.5%. `top_frozen` = fork of `mid_frozen`, `top_open` = open pull of
//!   `mid_open`; each renames 3% and archives 0.5%. Then `base` renames 5%
//!   more (only the open stack sees them).
//!
//! `run` reports for each query and branch: matches (rows of the full read),
//! the first page (limit 1000, ids only) and the full read (all pages of
//! 1000), each as wall ms and thread CPU ms (median of the repeats), and the
//! candidates checked (`Find::scanned`). `--cold` replaces the warm repeats
//! with one first page and one full read, each after a new connection and
//! `posix_fadvise(DONTNEED)` on the file.

use std::os::fd::AsRawFd;
use std::path::{Path, PathBuf};
use std::time::Instant;

use egdb::{
    BranchId, Filter, FindOptions, Op, ParentRef, SeqRange, SortOrder, Store, Value, WriteBatch,
    TYPE_KEY,
};

const BATCH_OPS: usize = 2_000;
const SEED: u64 = 0x5ea4c4;

/// Words in rough frequency order (Zipf-like weights by rank).
const WORDS: &[&str] = &[
    "review", "plan", "update", "meeting", "notes", "project", "report", "weekly", "draft",
    "team", "design", "sync", "new", "client", "follow", "up", "release", "product", "data",
    "budget", "q3", "roadmap", "launch", "sprint", "customer", "feedback", "research", "sales",
    "marketing", "strategy", "planning", "monthly", "website", "check", "call", "list", "ideas",
    "onboarding", "hiring", "management", "quarterly", "analysis", "summary", "proposal",
    "contract", "invoice", "travel", "event", "board", "goals", "metrics", "user", "interview",
    "retro", "notes", "api", "migration", "mobile", "app", "backend", "frontend", "bug", "fix",
    "support", "ticket", "legal", "finance", "office", "kitchen", "garden", "home", "renovation",
    "trip", "paris", "london", "berlin", "tokyo", "family", "birthday", "party", "gift", "book",
    "reading", "course", "learning", "rust", "sqlite", "database", "performance", "security",
    "audit", "compliance", "policy", "vendor", "partner", "pricing", "growth", "retention",
    "churn", "dashboard", "experiment", "test", "prototype", "spec", "architecture", "infra",
    "deploy", "pipeline", "build", "ci", "outage", "incident", "postmortem", "dinner", "lunch",
    "workshop", "conference", "talk", "slides", "deck", "investor", "pitch", "fundraising",
    "recruiting", "candidate", "offer", "salary", "benefits", "insurance", "tax", "return",
    "receipt", "expense", "reimbursement", "laptop", "hardware", "printer", "network", "wifi",
    "server", "cluster", "kubernetes", "docker", "backup", "restore", "archive", "cleanup",
    "refactor", "cache", "index", "search", "query", "schema", "model", "training", "evaluation",
    "benchmark", "latency", "throughput", "memory", "storage", "cost", "estimate", "timeline",
    "milestone", "deadline", "priority", "backlog", "triage", "standup", "agenda", "minutes",
    "decision", "approval", "signoff", "handover", "documentation", "wiki", "guide", "tutorial",
    "template", "checklist", "inventory", "warehouse", "shipping", "logistics", "supplier",
    "order", "delivery", "returns", "warranty", "repair", "maintenance", "cleaning", "groceries",
    "recipe", "fitness", "running", "marathon", "yoga", "doctor", "dentist", "appointment",
    "school", "homework", "exam", "thesis", "paper", "journal", "blog", "newsletter", "podcast",
    "video", "photo", "album", "music", "concert", "festival", "holiday", "vacation", "flight",
    "hotel", "visa", "passport", "apartment", "lease", "mortgage", "bank", "account", "savings",
    "portfolio", "stocks", "crypto", "garage", "car", "bicycle", "boat", "camping", "hiking",
    "mountain", "beach", "island", "lighthouse", "observatory", "telescope", "astronomy",
    "chemistry", "biology", "physics", "mathematics", "philosophy", "history", "poetry",
    "novel", "screenplay", "orchestra", "violin", "saxophone", "harpsichord", "accordion",
    "origami", "pottery", "calligraphy", "taxidermy", "falconry", "beekeeping", "kombucha",
    "sourdough", "fermentation", "aquarium", "terrarium", "bonsai", "orchid", "succulent",
    "zeppelin", "quokka", "narwhal", "axolotl", "platypus", "pangolin", "okapi", "xylophone",
];

const TYPES: &[(&str, f64)] = &[
    ("task", 0.50),
    ("note", 0.25),
    ("project", 0.10),
    ("person", 0.099),
    ("doc", 0.05),
    ("invoice", 0.001),
];

struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E37_79B9_7F4A_7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58_476D_1CE4_E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D0_49BB_1331_11EB);
        z ^ (z >> 31)
    }
    fn unit(&mut self) -> f64 {
        (self.next() >> 11) as f64 / (1u64 << 53) as f64
    }
    fn range(&mut self, lo: u64, hi: u64) -> u64 {
        lo + self.next() % (hi - lo + 1)
    }
    fn chance(&mut self, p: f64) -> bool {
        self.unit() < p
    }
}

fn id(n: u64) -> String {
    let mut z = n.wrapping_add(0x9e37_79b9_7f4a_7c15);
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    format!("{:016x}", z ^ (z >> 31))
}

/// Cumulative Zipf-like weights (exponent 0.9) over `WORDS`.
fn word_cdf() -> Vec<f64> {
    let w: Vec<f64> = (0..WORDS.len())
        .map(|r| 1.0 / ((r + 1) as f64).powf(0.9))
        .collect();
    let total: f64 = w.iter().sum();
    let mut acc = 0.0;
    w.iter()
        .map(|x| {
            acc += x / total;
            acc
        })
        .collect()
}

fn pick_word(rng: &mut Rng, cdf: &[f64]) -> &'static str {
    let u = rng.unit();
    WORDS[cdf.partition_point(|&c| c < u).min(WORDS.len() - 1)]
}

fn cap(w: &str) -> String {
    let mut c = w.chars();
    match c.next() {
        Some(f) => f.to_ascii_uppercase().to_string() + c.as_str(),
        None => String::new(),
    }
}

fn gen_name(rng: &mut Rng, cdf: &[f64]) -> String {
    let n = rng.range(2, 5);
    let words: Vec<&str> = (0..n).map(|_| pick_word(rng, cdf)).collect();
    let style = rng.unit();
    let mut out: Vec<String> = words
        .iter()
        .enumerate()
        .map(|(i, w)| {
            if style < 0.60 {
                cap(w)
            } else if style < 0.85 {
                if i == 0 {
                    cap(w)
                } else {
                    (*w).to_owned()
                }
            } else if style < 0.95 {
                (*w).to_owned()
            } else {
                w.to_ascii_uppercase()
            }
        })
        .collect();
    if rng.chance(0.10) {
        let num = if rng.chance(0.5) {
            format!("{}", 2015 + rng.range(0, 12))
        } else {
            format!("#{}", rng.range(1, 999))
        };
        out.push(num);
    }
    out.join(" ")
}

fn pick_type(rng: &mut Rng) -> &'static str {
    let mut u = rng.unit();
    for (t, p) in TYPES {
        if u < *p {
            return t;
        }
        u -= p;
    }
    TYPES[0].0
}

struct Writer<'a> {
    store: &'a mut Store,
    ops: Vec<Op>,
    events: u64,
    secs: f64,
}

impl Writer<'_> {
    fn push(&mut self, b: BranchId, op: Op) {
        self.ops.push(op);
        if self.ops.len() >= BATCH_OPS {
            self.flush(b);
        }
    }
    fn flush(&mut self, b: BranchId) {
        if self.ops.is_empty() {
            return;
        }
        self.events += self.ops.len() as u64;
        let t = Instant::now();
        self.store
            .write(WriteBatch::new(b, "bench", std::mem::take(&mut self.ops)))
            .expect("write");
        self.secs += t.elapsed().as_secs_f64();
    }
}

fn file_mb(path: &Path) -> f64 {
    ["", "-wal"]
        .iter()
        .filter_map(|e| std::fs::metadata(format!("{}{e}", path.display())).ok())
        .map(|m| m.len())
        .sum::<u64>() as f64
        / 1e6
}

fn build(n: u64, dir: &Path) {
    std::fs::create_dir_all(dir).expect("dir");
    let path = dir.join(format!("search-{n}.db"));
    for ext in ["", "-wal", "-shm"] {
        let _ = std::fs::remove_file(format!("{}{ext}", path.display()));
    }
    let cdf = word_cdf();
    let mut store = Store::open(&path).expect("open");
    let base = store.create_branch("base", &[]).expect("branch");
    let started = Instant::now();
    let mut w = Writer {
        store: &mut store,
        ops: Vec::new(),
        events: 0,
        secs: 0.0,
    };
    let mut rng = Rng(SEED ^ n);
    for i in 0..n {
        let e = id(i);
        w.push(base, Op::set(e.as_str(), TYPE_KEY, pick_type(&mut rng)));
        w.push(base, Op::set(e.as_str(), "name", gen_name(&mut rng, &cdf)));
        w.push(base, Op::set(e.as_str(), "done", rng.chance(0.3)));
        w.push(base, Op::set(e.as_str(), "priority", rng.range(0, 4) as i64));
        if i > 0 {
            let p = id(rng.range(0, i - 1).min(i / 8));
            w.push(base, Op::link(p, e.as_str(), "child", SortOrder::Timestamp));
        }
    }
    let mut h = Rng(SEED ^ n ^ 0xA11CE);
    for i in 0..n {
        let e = id(i);
        if h.chance(0.30) {
            for _ in 0..h.range(1, 2) {
                w.push(base, Op::set(e.as_str(), "name", gen_name(&mut h, &cdf)));
            }
        }
        if h.chance(0.03) {
            w.push(base, Op::set(e.as_str(), TYPE_KEY, pick_type(&mut h)));
        }
        if h.chance(0.20) {
            w.push(base, Op::set(e.as_str(), "done", h.chance(0.5)));
        }
    }
    w.flush(base);
    let mid_frozen = w.store.fork(base, "mid_frozen").expect("fork");
    let mid_open = w
        .store
        .create_branch("mid_open", &[ParentRef::new(base, SeqRange::ALL)])
        .expect("branch");
    for b in [mid_frozen, mid_open] {
        let mut m = Rng(SEED ^ n ^ 0xB0B);
        for i in 0..n {
            let e = id(i);
            if m.chance(0.10) {
                w.push(b, Op::set(e.as_str(), "name", gen_name(&mut m, &cdf)));
            }
            if m.chance(0.01) {
                w.push(b, Op::clear(e.as_str(), TYPE_KEY));
            } else if m.chance(0.005) {
                w.push(b, Op::set(e.as_str(), TYPE_KEY, pick_type(&mut m)));
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
            if t.chance(0.03) {
                w.push(b, Op::set(e.as_str(), "name", gen_name(&mut t, &cdf)));
            }
            if t.chance(0.005) {
                w.push(b, Op::clear(e.as_str(), TYPE_KEY));
            }
        }
        w.flush(b);
    }
    let mut l = Rng(SEED ^ n ^ 0x1A7E);
    for i in 0..n {
        if l.chance(0.05) {
            w.push(base, Op::set(id(i).as_str(), "name", gen_name(&mut l, &cdf)));
        }
    }
    w.flush(base);
    let (events, write_secs) = (w.events, w.secs);
    store.checkpoint().expect("checkpoint");
    println!(
        "built {}: {n} entities, {events} events, write {write_secs:.1} s ({:.1} µs per event), \
         total {:.1} s, file {:.1} MB",
        path.display(),
        write_secs * 1e6 / events as f64,
        started.elapsed().as_secs_f64(),
        file_mb(&path)
    );
}

// --- Fuzzy matching (Rust side, after the roll-up) ----------------------------

/// Levenshtein distance of `a` and `b` if it is at most `k` (banded DP).
fn lev_within(a: &[u8], b: &[u8], k: usize) -> bool {
    if a.len().abs_diff(b.len()) > k {
        return false;
    }
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut cur = vec![0; b.len() + 1];
    for (i, ca) in a.iter().enumerate() {
        cur[0] = i + 1;
        let mut best = cur[0];
        for (j, cb) in b.iter().enumerate() {
            let sub = prev[j] + usize::from(ca != cb);
            cur[j + 1] = sub.min(prev[j + 1] + 1).min(cur[j] + 1);
            best = best.min(cur[j + 1]);
        }
        if best > k {
            return false;
        }
        std::mem::swap(&mut prev, &mut cur);
    }
    prev[b.len()] <= k
}

/// True when some word of `text` (ASCII-folded) is within `k` edits of `q`.
fn fuzzy_word(text: &str, q: &[u8], k: usize) -> bool {
    text.split(|c: char| !c.is_ascii_alphanumeric())
        .filter(|w| !w.is_empty())
        .any(|w| lev_within(&w.to_ascii_lowercase().into_bytes(), q, k))
}

/// Split `q` into `k + 1` disjoint pieces: a word within `k` edits of `q`
/// contains at least one of them unchanged (pigeonhole).
fn pieces(q: &str, k: usize) -> Vec<String> {
    let b = q.as_bytes();
    let parts = k + 1;
    (0..parts)
        .map(|i| String::from_utf8(b[i * b.len() / parts..(i + 1) * b.len() / parts].to_vec()).unwrap())
        .collect()
}

fn trigrams(q: &str) -> Vec<String> {
    let b = q.as_bytes();
    let mut v: Vec<String> = b.windows(3).map(|w| String::from_utf8(w.to_vec()).unwrap()).collect();
    v.sort();
    v.dedup();
    v
}

fn fuzzy_filter(key: &str, q: &str, k: usize) -> Filter {
    let key = key.to_owned();
    let qb = q.to_ascii_lowercase().into_bytes();
    Filter::custom(move |_, v| match v.get(&key) {
        Some(Value::Text(t)) => fuzzy_word(t, &qb, k),
        _ => false,
    })
}

/// (label, filter). Labels start with a short code for `--queries`.
pub fn queries() -> Vec<(String, Filter)> {
    let mut q: Vec<(String, Filter)> = Vec::new();
    for (c, t) in [("t1", "task"), ("t2", "project"), ("t5", "doc"), ("t6", "person"), ("t3", "invoice"), ("t4", "spaceship")] {
        q.push((format!("{c} type = {t:?}"), Filter::eq(TYPE_KEY, t)));
    }
    for (c, s) in [("c1", "Zeppelin"), ("c2", "Budget"), ("c3", "in"), ("c4", "e")] {
        q.push((format!("{c} name contains {s:?}"), Filter::Contains("name".into(), s.into())));
    }
    for (c, s) in [("i1", "zeppelin"), ("i2", "budget"), ("i3", "re"), ("i4", "e")] {
        q.push((
            format!("{c} name contains-ignore-case {s:?}"),
            Filter::ContainsIgnoreCase("name".into(), s.into()),
        ));
    }
    for (c, s) in [("p1", "Zep"), ("p2", "Weekly"), ("p3", "R")] {
        q.push((format!("{c} name prefix {s:?}"), Filter::Prefix("name".into(), s.into())));
    }
    // Fuzzy: "managment" is 1 edit from "management"; "mangment" is 2.
    for (c, s, k) in [("f1", "managment", 1), ("f2", "mangment", 2), ("f3", "zepelin", 1)] {
        q.push((format!("{c} name fuzzy {s:?} <= {k} edits, Custom scan"), fuzzy_filter("name", s, k)));
        let pre = Filter::Or(
            pieces(s, k)
                .into_iter()
                .map(|p| Filter::ContainsIgnoreCase("name".into(), p))
                .collect(),
        );
        q.push((
            format!("{c}p name fuzzy {s:?} <= {k}, pre-filter pieces {:?}", pieces(s, k)),
            Filter::And(vec![pre, fuzzy_filter("name", s, k)]),
        ));
        if k == 1 {
            let tri = Filter::Or(
                trigrams(s)
                    .into_iter()
                    .map(|p| Filter::ContainsIgnoreCase("name".into(), p))
                    .collect(),
            );
            q.push((
                format!("{c}t name fuzzy {s:?} <= {k}, pre-filter any of {} trigrams", trigrams(s).len()),
                Filter::And(vec![tri, fuzzy_filter("name", s, k)]),
            ));
        }
    }
    q
}

// --- Measurement -------------------------------------------------------------

extern "C" {
    fn posix_fadvise(fd: i32, offset: i64, len: i64, advice: i32) -> i32;
}

fn evict(path: &str) {
    for ext in ["", "-wal"] {
        if let Ok(f) = std::fs::File::open(format!("{path}{ext}")) {
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

#[repr(C)]
struct Timespec {
    sec: i64,
    nsec: i64,
}

extern "C" {
    fn clock_gettime(clock: i32, ts: *mut Timespec) -> i32;
}

/// CPU time of this thread in ns (`CLOCK_THREAD_CPUTIME_ID`).
fn cpu_ns() -> u64 {
    let mut ts = Timespec { sec: 0, nsec: 0 };
    // SAFETY: a valid pointer to a timespec; clock 3 is CLOCK_THREAD_CPUTIME_ID.
    let rc = unsafe { clock_gettime(3, &mut ts) };
    assert_eq!(rc, 0, "clock_gettime");
    ts.sec as u64 * 1_000_000_000 + ts.nsec as u64
}

/// Page size: `--limit N` (default 1000).
static LIMIT: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(1000);
fn page_limit() -> usize {
    LIMIT.load(std::sync::atomic::Ordering::Relaxed)
}

fn median(mut v: Vec<f64>) -> f64 {
    v.sort_by(f64::total_cmp);
    if v.is_empty() {
        return f64::NAN;
    }
    v[v.len() / 2]
}

/// (wall ms, cpu ms, scanned, rows, pages)
fn first_page(store: &Store, b: BranchId, f: &Filter) -> (f64, f64, usize) {
    let t = Instant::now();
    let c = cpu_ns();
    let p = store
        .find(b, f, FindOptions { values: false, limit: page_limit(), ..FindOptions::default() })
        .expect("find");
    let cpu = (cpu_ns() - c) as f64 / 1e6;
    (t.elapsed().as_secs_f64() * 1e3, cpu, p.scanned)
}

fn full_read(store: &Store, b: BranchId, f: &Filter) -> (f64, f64, usize, usize, usize) {
    let t = Instant::now();
    let c = cpu_ns();
    let (mut rows, mut scanned, mut pages, mut cursor) = (0, 0, 0, None);
    loop {
        let p = store
            .find(b, f, FindOptions { values: false, cursor, limit: page_limit(), ..FindOptions::default() })
            .expect("find");
        rows += p.rows.len();
        scanned += p.scanned;
        pages += 1;
        cursor = p.next;
        if cursor.is_none() {
            break;
        }
    }
    let cpu = (cpu_ns() - c) as f64 / 1e6;
    (t.elapsed().as_secs_f64() * 1e3, cpu, scanned, rows, pages)
}

fn open(path: &str) -> Store {
    let s = Store::open(path).expect("open");
    s.set_mmap_size(0).expect("mmap");
    s
}

fn branch_named(store: &Store, name: &str) -> BranchId {
    store
        .branches()
        .unwrap()
        .into_iter()
        .find(|b| b.name == name)
        .map(|b| b.id)
        .unwrap_or_else(|| panic!("no branch {name}"))
}

fn run(path: &str, args: &[String]) {
    let value = |f: &str| args.iter().position(|a| a == f).map(|i| args[i + 1].clone());
    let cold = args.iter().any(|a| a == "--cold");
    let reps: usize = value("--repeats").map_or(7, |v| v.parse().unwrap());
    let full_reps: usize = value("--full-repeats").map_or(3, |v| v.parse().unwrap());
    if let Some(l) = value("--limit") {
        LIMIT.store(l.parse().expect("--limit N"), std::sync::atomic::Ordering::Relaxed);
    }
    let branches: Vec<String> = value("--branches")
        .unwrap_or("base,top_open,top_frozen".into())
        .split(',')
        .map(str::to_owned)
        .collect();
    let only: Option<Vec<String>> = value("--queries").map(|q| q.split(',').map(str::to_owned).collect());
    // `--knob name=value`, repeatable: planner switches (`egdb::find_knobs`).
    for (i, a) in args.iter().enumerate() {
        if a == "--knob" {
            let (k, v) = args[i + 1].split_once('=').expect("name=value");
            let knob = match k {
                "probe" => &egdb::find_knobs::PROBE,
                "no_intersect" => &egdb::find_knobs::NO_INTERSECT,
                "dense" => &egdb::find_knobs::DENSE,
                "exact_count" => &egdb::find_knobs::EXACT_COUNT,
                "no_contains_fn" => &egdb::find_knobs::NO_CONTAINS_FN,
                "no_overfetch" => &egdb::find_knobs::NO_OVERFETCH,
                "eq_cap" => &egdb::find_knobs::EQ_CAP,
                _ => panic!("unknown knob {k}"),
            };
            knob.store(v.parse().unwrap(), std::sync::atomic::Ordering::Relaxed);
        }
    }
    let mut store = open(path);
    println!("file {path} ({:.1} MB), {}", file_mb(Path::new(path)), if cold { "cold" } else { "warm" });
    println!();
    println!("| branch | query | matches | first page wall ms | first page CPU ms | checked first page | all pages wall ms | all pages CPU ms | pages | checked all pages | MB read |");
    println!("| --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |");
    for name in &branches {
        for (label, f) in queries() {
            let code = label.split(' ').next().unwrap().to_owned();
            if only.as_ref().is_some_and(|o| !o.contains(&code)) {
                continue;
            }
            let (fw, fc, fs, aw, ac, asn, rows, pages, mb);
            if cold {
                let r0 = read_bytes();
                drop(std::mem::replace(&mut store, Store::open_in_memory().unwrap()));
                evict(path);
                store = open(path);
                let b = branch_named(&store, name);
                evict(path);
                (fw, fc, fs) = first_page(&store, b, &f);
                drop(std::mem::replace(&mut store, Store::open_in_memory().unwrap()));
                evict(path);
                store = open(path);
                let b = branch_named(&store, name);
                evict(path);
                (aw, ac, asn, rows, pages) = full_read(&store, b, &f);
                mb = (read_bytes() - r0) as f64 / 1e6;
            } else {
                let b = branch_named(&store, name);
                let _ = first_page(&store, b, &f); // warm-up
                let (mut w, mut c) = (Vec::new(), Vec::new());
                let mut s = 0;
                for _ in 0..reps {
                    let r = first_page(&store, b, &f);
                    w.push(r.0);
                    c.push(r.1);
                    s = r.2;
                }
                (fw, fc, fs) = (median(w), median(c), s);
                let (mut w, mut c) = (Vec::new(), Vec::new());
                let mut last = (0, 0, 0);
                for _ in 0..full_reps {
                    let r = full_read(&store, b, &f);
                    w.push(r.0);
                    c.push(r.1);
                    last = (r.2, r.3, r.4);
                }
                (aw, ac) = (median(w), median(c));
                (asn, rows, pages) = last;
                mb = 0.0;
            }
            println!(
                "| {name} | {label} | {rows} | {fw:.2} | {fc:.2} | {fs} | {aw:.1} | {ac:.1} | {pages} | {asn} | {mb:.1} |"
            );
        }
    }
}

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();
    match args.first().map(String::as_str) {
        Some("build") => build(args[1].parse().expect("N"), &PathBuf::from(&args[2])),
        Some("run") => run(&args[1], &args[2..]),
        _ => eprintln!("usage: bench_search build N DIR | run FILE [flags]"),
    }
}

