//! `prove` benchmark on a realistic permission graph. Run in release:
//!
//! ```text
//! cargo run --release --example bench_prove -- --size 100000 [--data DIR] \
//!     [--reps N] [--cold] [--only SCENARIO] [--strategy backward|bidirectional|walk]
//! ```
//!
//! Flags:
//! - `--size N`: entity count target (10000 / 100000 / 1000000). The store is
//!   built once per size under `--data` (default `/workspace/egdb-perm/data`)
//!   and reused if a `__meta__/built = N` marker is present.
//! - `--reps N`: repeats per (scenario, strategy). Default 11 warm, 5 cold.
//! - `--cold`: before each rep, open a fresh `Store` (empty SQLite cache) and
//!   `posix_fadvise(DONTNEED)` the db and `-wal` files (OS page cache dropped).
//! - `--only SCENARIO`: run only the named scenario.
//! - `--strategy S`: run only one strategy (default: all three).
//!
//! ## The graph (deterministic, seeded SplitMix64)
//!
//! Types: `org`, `team`, `folder`, `doc`, `group`, `user`. The downward
//! relationship map used for every proof (keyed by the UPPER entity's type):
//!
//! ```text
//! org    --child(Outbound)-->      team
//! team   --child(Outbound)-->      folder
//! folder --child(Outbound)-->      folder | doc          (nesting + leaves)
//! group  --child(Outbound)-->      group                 (group nesting)
//! group  --grant(Outbound)-->      folder | doc          (access grant)
//! group  --shared_with(Inbound)--> doc                   (doc->group link)
//! user   --member(Outbound)-->     group                 (user->group link)
//! ```
//!
//! The `shared_with` pair is read Inbound: a doc stores a link `doc -> group`,
//! so from a group (the upper node) the step is Inbound. This exercises an
//! inbound-direction entry in the map. "Can user U see doc D" is modelled as a
//! proof with `root = U`: `U --member--> group --shared_with(Inbound)--> D`, or
//! with the groups as roots directly; we document the group-root form and use
//! it for `fanin100`.
//!
//! Shapes: one deep `child` chain of 11 nested folders (proof depth up to 13);
//! one "huge" org with thousands of teams directly under it (fan-out near the
//! root); a `fanin` doc shared with 100 groups; a `popular` doc shared with
//! over 1000 groups (for the budget/large-ancestor-set cases); a normal forest of
//! orgs (5-20 teams, folders fanning 3-10, depth 2-4) that provides volume and
//! decoy roots; a few archived folders. A 3-branch stack (open and frozen
//! ranges) on top renames, re-parents a doc and removes/re-adds links.

use std::collections::HashMap;
use std::os::fd::AsRawFd;
use std::path::{Path, PathBuf};
use std::time::Instant;

use egdb::{
    BranchId, Direction, Follow, Op, ParentRef, ProveOptions, ProveOutcome, ProveStrategy,
    SeqRange, SortOrder, Store, Value, WriteBatch, TYPE_KEY,
};

#[repr(C)]
struct Timespec {
    tv_sec: i64,
    tv_nsec: i64,
}

extern "C" {
    fn posix_fadvise(fd: i32, offset: i64, len: i64, advice: i32) -> i32;
    fn clock_gettime(clk: i32, tp: *mut Timespec) -> i32;
}
const POSIX_FADV_DONTNEED: i32 = 4;
const CLOCK_THREAD_CPUTIME_ID: i32 = 3;

/// Drop the db and `-wal` files from the OS page cache.
fn evict(path: &Path) {
    for ext in ["", "-wal"] {
        if let Ok(f) = std::fs::File::open(format!("{}{ext}", path.display())) {
            // SAFETY: valid fd for the duration of the call.
            let rc = unsafe { posix_fadvise(f.as_raw_fd(), 0, 0, POSIX_FADV_DONTNEED) };
            assert_eq!(rc, 0, "posix_fadvise");
        }
    }
}

/// This thread's CPU time in ns (`clock_gettime(CLOCK_THREAD_CPUTIME_ID)`):
/// stable when the machine is shared, unlike wall time, and nanosecond
/// resolution (unlike `/proc/thread-self/schedstat`, which is ~1 ms here).
fn cpu_ns() -> u64 {
    let mut ts = Timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    // SAFETY: valid pointer; the clock id is a Linux constant.
    unsafe { clock_gettime(CLOCK_THREAD_CPUTIME_ID, &mut ts) };
    (ts.tv_sec as u64) * 1_000_000_000 + ts.tv_nsec as u64
}

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
}

/// Hashed entity ID for entity number `n` (like UUID keys: B-tree order has no
/// relation to creation order).
fn id(n: u64) -> String {
    let mut z = n.wrapping_add(0x9e37_79b9_7f4a_7c15);
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    format!("{:016x}", z ^ (z >> 31))
}

const CHILD: &str = "child";
const GRANT: &str = "grant";
const SHARED: &str = "shared_with";
const MEMBER: &str = "member";

/// The downward relationship map used for every proof in this benchmark.
fn relationships() -> Follow {
    use Direction::{Inbound, Outbound};
    let mut f: Follow = HashMap::new();
    f.insert("org".into(), vec![(CHILD.into(), Outbound)]);
    f.insert("team".into(), vec![(CHILD.into(), Outbound)]);
    f.insert("folder".into(), vec![(CHILD.into(), Outbound)]);
    f.insert(
        "group".into(),
        vec![
            (CHILD.into(), Outbound),
            (GRANT.into(), Outbound),
            (SHARED.into(), Inbound),
        ],
    );
    f.insert("user".into(), vec![(MEMBER.into(), Outbound)]);
    f.insert("doc".into(), vec![]);
    f
}

const BATCH: usize = 5000;

/// Entity numbers of the landmark nodes a scenario needs. Computed the same way
/// whether or not the store is written, so a reused file resolves identically.
#[derive(Clone, Debug)]
struct Land {
    /// `[org, team, f0..f10, doc]` (14 nodes); proof depths 1/3/6/13 by root.
    chain: Vec<u64>,
    huge_org: u64,
    huge_doc: u64,
    fanin_doc: u64,
    fanin_group0: u64,
    popular_doc: u64,
    rp_team: u64,
    rp_folder_f: u64,
    rp_doc_d: u64,
    rp_folder_g: u64,
    far_org: u64,
    decoys: Vec<u64>,
    total: u64,
}

/// Accumulates ops into batches when writing; a no-op (just counts) otherwise.
struct Gen<'a> {
    rng: Rng,
    n: u64,
    store: Option<(&'a mut Store, BranchId)>,
    ops: Vec<Op>,
}

impl<'a> Gen<'a> {
    fn new(store: Option<(&'a mut Store, BranchId)>) -> Self {
        Self {
            rng: Rng(0x5eed_9209),
            n: 0,
            store,
            ops: Vec::with_capacity(BATCH + 16),
        }
    }
    fn flush(&mut self) {
        if let Some((s, b)) = self.store.as_mut() {
            if !self.ops.is_empty() {
                s.write(WriteBatch::new(*b, "bench", std::mem::take(&mut self.ops)))
                    .expect("write");
            }
        }
    }
    fn maybe_flush(&mut self) {
        if self.ops.len() >= BATCH {
            self.flush();
        }
    }
    fn entity(&mut self, ty: &str) -> u64 {
        let num = self.n;
        self.n += 1;
        if self.store.is_some() {
            let e = id(num);
            self.ops.push(Op::set(e.as_str(), TYPE_KEY, ty));
            self.ops
                .push(Op::set(e.as_str(), "title", format!("e{num}")));
            self.maybe_flush();
        }
        num
    }
    fn link(&mut self, src: u64, dst: u64, lt: &str) {
        if self.store.is_some() {
            self.ops
                .push(Op::link(id(src), id(dst), lt, SortOrder::Timestamp));
            self.maybe_flush();
        }
    }
    fn archive(&mut self, num: u64) {
        if self.store.is_some() {
            self.ops.push(Op::clear(id(num), TYPE_KEY));
            self.maybe_flush();
        }
    }
}

fn huge_teams(size: u64) -> u64 {
    (size / 25).clamp(200, 3000)
}
fn popular_groups(size: u64) -> u64 {
    (size / 10).clamp(1500, 3000)
}

/// One normal org: 5-20 teams, each a bounded folder subtree (depth 2-4,
/// fan-out 3-10). Stops early when the global target is reached.
fn normal_org(g: &mut Gen, decoys: &mut Vec<u64>, size: u64) -> u64 {
    let org = g.entity("org");
    if decoys.len() < 256 {
        decoys.push(org);
    }
    let teams = g.rng.range(5, 20);
    for _ in 0..teams {
        if g.n >= size {
            break;
        }
        let team = g.entity("team");
        g.link(org, team, CHILD);
        if decoys.len() < 256 {
            decoys.push(team);
        }
        let maxd = g.rng.range(2, 4);
        let mut stack = vec![(team, 0u64)];
        while let Some((parent, d)) = stack.pop() {
            if g.n >= size {
                break;
            }
            let k = g.rng.range(3, 10);
            for _ in 0..k {
                if g.n >= size {
                    break;
                }
                if d + 1 < maxd && g.rng.next() % 5 < 2 {
                    let f = g.entity("folder");
                    g.link(parent, f, CHILD);
                    stack.push((f, d + 1));
                } else {
                    let doc = g.entity("doc");
                    g.link(parent, doc, CHILD);
                }
            }
        }
    }
    org
}

/// Build (or just enumerate) the whole graph and return its landmarks.
fn generate(size: u64, store: Option<(&mut Store, BranchId)>) -> Land {
    let mut g = Gen::new(store);

    // 1. Deep chain: org -> team -> f0 -> .. -> f10 -> doc (13 links).
    let mut chain = Vec::new();
    let org = g.entity("org");
    let team = g.entity("team");
    g.link(org, team, CHILD);
    chain.push(org);
    chain.push(team);
    let mut prev = team;
    for _ in 0..11 {
        let f = g.entity("folder");
        g.link(prev, f, CHILD);
        chain.push(f);
        prev = f;
    }
    let cdoc = g.entity("doc");
    g.link(prev, cdoc, CHILD);
    chain.push(cdoc);

    // 2. Huge org: thousands of teams -> folder -> doc directly under one org.
    let huge_org = g.entity("org");
    let mut huge_doc = 0;
    for t in 0..huge_teams(size) {
        let tm = g.entity("team");
        g.link(huge_org, tm, CHILD);
        let f = g.entity("folder");
        g.link(tm, f, CHILD);
        let d = g.entity("doc");
        g.link(f, d, CHILD);
        if t == 0 {
            huge_doc = d;
        }
    }

    // 3. Fan-in doc shared with 100 groups; group 0 has a member user.
    let fanin_doc = g.entity("doc");
    let mut fanin_group0 = 0;
    for gi in 0..100 {
        let grp = g.entity("group");
        g.link(fanin_doc, grp, SHARED); // doc -> group: Inbound from the group
        if gi == 0 {
            fanin_group0 = grp;
            let u = g.entity("user");
            g.link(u, grp, MEMBER);
        }
    }

    // 4. Popular doc shared with >1000 groups (budget / large ancestor set).
    let popular_doc = g.entity("doc");
    for _ in 0..popular_groups(size) {
        let grp = g.entity("group");
        g.link(popular_doc, grp, SHARED);
    }

    // 5. Reparent structure (edited on the branch stack).
    let rp_org = g.entity("org");
    let rp_team = g.entity("team");
    g.link(rp_org, rp_team, CHILD);
    let rp_folder_f = g.entity("folder");
    g.link(rp_team, rp_folder_f, CHILD);
    let rp_doc_d = g.entity("doc");
    g.link(rp_folder_f, rp_doc_d, CHILD);
    let rp_folder_g = g.entity("folder");
    g.link(rp_team, rp_folder_g, CHILD);

    // 6. Normal forest until the target is reached.
    let mut decoys = Vec::new();
    while g.n < size {
        normal_org(&mut g, &mut decoys, size);
    }
    let far_org = decoys.first().copied().unwrap_or(rp_org);

    // 7. A few archived leaf folders.
    for _ in 0..5 {
        let f = g.entity("folder");
        g.archive(f);
    }

    g.flush();
    let total = g.n;
    Land {
        chain,
        huge_org,
        huge_doc,
        fanin_doc,
        fanin_group0,
        popular_doc,
        rp_team,
        rp_folder_f,
        rp_doc_d,
        rp_folder_g,
        far_org,
        decoys,
        total,
    }
}

const CHILD_B: &str = "stack_child";
const FROZEN_B: &str = "stack_frozen";
const GRAND_B: &str = "stack_grand";

#[derive(Clone, Copy)]
struct Branches {
    main: BranchId,
    grand: BranchId,
    frozen: BranchId,
}

fn find_branch(store: &Store, name: &str) -> BranchId {
    store
        .branches()
        .unwrap()
        .into_iter()
        .find(|b| b.name == name)
        .map(|b| b.id)
        .unwrap_or_else(|| panic!("no branch {name}"))
}

/// A 3-branch stack on top of `main`: `child` (open -> main) renames a folder,
/// then `frozen` is forked BEFORE a re-parenting, then `child` re-parents doc D
/// from F to G and removes+re-adds the team->F link, then `grand` (open ->
/// child) sits on top. Built once; looked up by name on reuse.
fn build_stack(store: &mut Store, main: BranchId, land: &Land) {
    let (f, d, g, team) = (
        id(land.rp_folder_f),
        id(land.rp_doc_d),
        id(land.rp_folder_g),
        id(land.rp_team),
    );
    let child = store
        .create_branch(CHILD_B, &[ParentRef::new(main, SeqRange::ALL)])
        .unwrap();
    store
        .write(WriteBatch::new(
            child,
            "stack",
            vec![Op::set(f.as_str(), "title", "renamed on child")],
        ))
        .unwrap();
    store.fork(child, FROZEN_B).unwrap();
    store
        .write(WriteBatch::new(
            child,
            "stack",
            vec![
                Op::unlink(f.as_str(), d.as_str(), CHILD),
                Op::link(g.as_str(), d.as_str(), CHILD, SortOrder::Timestamp),
                Op::unlink(team.as_str(), f.as_str(), CHILD),
                Op::link(team.as_str(), f.as_str(), CHILD, SortOrder::Timestamp),
            ],
        ))
        .unwrap();
    store
        .create_branch(GRAND_B, &[ParentRef::new(child, SeqRange::ALL)])
        .unwrap();
}

#[derive(Clone, Copy)]
enum Br {
    Main,
    Grand,
    Frozen,
}

#[derive(Clone, Copy, PartialEq)]
enum Exp {
    Proved(usize),
    NoPath,
    Unknown,
}

struct Scenario {
    name: &'static str,
    br: Br,
    item: u64,
    roots: Vec<u64>,
    max_depth: Option<usize>,
    max_nodes: Option<usize>,
    exp: Exp,
}

fn scenarios(land: &Land) -> Vec<Scenario> {
    let c = &land.chain;
    let doc = c[13];
    let mut roots100 = vec![c[0]];
    roots100.extend(land.decoys.iter().copied().take(99));
    let sc = |name, br, item, roots, max_depth, max_nodes, exp| Scenario {
        name,
        br,
        item,
        roots,
        max_depth,
        max_nodes,
        exp,
    };
    vec![
        sc(
            "depth1",
            Br::Main,
            doc,
            vec![c[12]],
            None,
            None,
            Exp::Proved(1),
        ),
        sc(
            "depth3",
            Br::Main,
            doc,
            vec![c[10]],
            None,
            None,
            Exp::Proved(3),
        ),
        sc(
            "depth6",
            Br::Main,
            doc,
            vec![c[7]],
            None,
            None,
            Exp::Proved(6),
        ),
        sc(
            "depth10plus",
            Br::Main,
            doc,
            vec![c[0]],
            None,
            None,
            Exp::Proved(13),
        ),
        sc(
            "fanin100",
            Br::Main,
            land.fanin_doc,
            vec![land.fanin_group0],
            None,
            None,
            Exp::Proved(1),
        ),
        sc(
            "fanout_root",
            Br::Main,
            land.huge_doc,
            vec![land.huge_org],
            None,
            None,
            Exp::Proved(3),
        ),
        sc(
            "nopath",
            Br::Main,
            land.popular_doc,
            vec![land.far_org],
            None,
            None,
            Exp::NoPath,
        ),
        sc(
            "nopath_depth",
            Br::Main,
            land.popular_doc,
            vec![land.far_org],
            Some(4),
            None,
            Exp::NoPath,
        ),
        sc(
            "nopath_budget",
            Br::Main,
            land.popular_doc,
            vec![land.far_org],
            None,
            Some(1000),
            Exp::Unknown,
        ),
        sc(
            "roots100",
            Br::Main,
            doc,
            roots100,
            None,
            None,
            Exp::Proved(13),
        ),
        sc(
            "roots1",
            Br::Main,
            doc,
            vec![c[0]],
            None,
            None,
            Exp::Proved(13),
        ),
        sc(
            "stack_head",
            Br::Grand,
            land.rp_doc_d,
            vec![land.rp_folder_g],
            None,
            None,
            Exp::Proved(1),
        ),
        sc(
            "stack_head_nof",
            Br::Grand,
            land.rp_doc_d,
            vec![land.rp_folder_f],
            None,
            None,
            Exp::NoPath,
        ),
        sc(
            "stack_frozen",
            Br::Frozen,
            land.rp_doc_d,
            vec![land.rp_folder_f],
            None,
            None,
            Exp::Proved(1),
        ),
        sc(
            "stack_frozen_nog",
            Br::Frozen,
            land.rp_doc_d,
            vec![land.rp_folder_g],
            None,
            None,
            Exp::NoPath,
        ),
    ]
}

const STRATEGIES: [(&str, ProveStrategy); 3] = [
    ("backward", ProveStrategy::Backward),
    ("bidirectional", ProveStrategy::Bidirectional),
    ("walk", ProveStrategy::Walk),
];

struct Row {
    scenario: &'static str,
    strategy: &'static str,
    outcome: &'static str,
    pathlen: Option<usize>,
    wall_us: f64,
    cpu_us: f64,
    nodes: usize,
    link_reads: usize,
    value_reads: usize,
    sql: usize,
}

fn median(v: &mut [f64]) -> f64 {
    v.sort_by(f64::total_cmp);
    v[v.len() / 2]
}

fn tune(store: &Store) {
    store.set_mmap_size(0).unwrap();
    store.set_cache_size_kib(2000).unwrap();
}

fn resolve(br: Br, b: &Branches) -> BranchId {
    match br {
        Br::Main => b.main,
        Br::Grand => b.grand,
        Br::Frozen => b.frozen,
    }
}

fn outcome_str(o: &ProveOutcome) -> &'static str {
    match o {
        ProveOutcome::Proved(_) => "Proved",
        ProveOutcome::NoPath => "NoPath",
        ProveOutcome::Unknown => "Unknown",
    }
}

/// Assert the outcome matches the scenario's expectation; for the shortest
/// strategies also assert the path length. A wrong answer fails the bench.
fn check(sc: &Scenario, strategy: &str, o: &ProveOutcome) {
    let want = match sc.exp {
        Exp::Proved(_) => "Proved",
        Exp::NoPath => "NoPath",
        Exp::Unknown => "Unknown",
    };
    assert_eq!(
        outcome_str(o),
        want,
        "{}/{strategy}: wrong outcome",
        sc.name
    );
    if let (Exp::Proved(len), ProveOutcome::Proved(p)) = (sc.exp, o) {
        if strategy != "walk" {
            assert_eq!(
                p.links.len(),
                len,
                "{}/{strategy}: wrong path length",
                sc.name
            );
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn measure(
    path: &Path,
    warm_store: &Store,
    b: &Branches,
    sc: &Scenario,
    strategy: (&'static str, ProveStrategy),
    reps: usize,
    cold: bool,
) -> Row {
    let branch = resolve(sc.br, b);
    let rel = relationships();
    let item = id(sc.item);
    let roots: Vec<String> = sc.roots.iter().map(|r| id(*r)).collect();
    let opts = || ProveOptions {
        max_depth: sc.max_depth,
        max_nodes: sc.max_nodes,
        include_archived: false,
        strategy: strategy.1,
    };
    let call = |s: &Store| {
        let rr: Vec<&str> = roots.iter().map(String::as_str).collect();
        s.prove(branch, &item, &rr, &rel, opts()).unwrap()
    };
    if !cold {
        std::hint::black_box(call(warm_store)); // prime
    }
    // The thread CPU clock is jiffy-backed (~1 ms) on this VM, so a single
    // sub-ms proof reads 0. Warm reps therefore run an adaptive inner loop
    // (call until >= 20 ms of CPU) and divide, which recovers µs resolution
    // for both wall and CPU. Cold reps must stay single (a second call would
    // be warm), so cold CPU keeps the 1 ms floor; cold is read off wall time.
    const TARGET_NS: u64 = 20_000_000;
    let mut walls = Vec::new();
    let mut cpus = Vec::new();
    let mut last = None;
    for _ in 0..reps {
        if cold {
            let st = Store::open(path).unwrap();
            tune(&st);
            evict(path);
            let c = cpu_ns();
            let t = Instant::now();
            let p = call(&st);
            walls.push(t.elapsed().as_secs_f64() * 1e6);
            cpus.push((cpu_ns() - c) as f64 / 1e3);
            last = Some(p);
        } else {
            let c = cpu_ns();
            let t = Instant::now();
            let mut p = call(warm_store);
            let mut iters = 1u64;
            while cpu_ns() - c < TARGET_NS && iters < 200_000 {
                p = call(warm_store);
                iters += 1;
            }
            let n = iters as f64;
            walls.push(t.elapsed().as_secs_f64() * 1e6 / n);
            cpus.push((cpu_ns() - c) as f64 / 1e3 / n);
            last = Some(p);
        }
    }
    let p = last.unwrap();
    check(sc, strategy.0, &p.outcome);
    Row {
        scenario: sc.name,
        strategy: strategy.0,
        outcome: outcome_str(&p.outcome),
        pathlen: match &p.outcome {
            ProveOutcome::Proved(pf) => Some(pf.links.len()),
            _ => None,
        },
        wall_us: median(&mut walls),
        cpu_us: median(&mut cpus),
        nodes: p.stats.nodes_visited,
        link_reads: p.stats.link_reads,
        value_reads: p.stats.value_reads,
        sql: p.stats.sql_queries,
    }
}

fn marker(store: &Store, main: BranchId) -> Option<i64> {
    match store.value(main, "__meta__", "built").unwrap() {
        Some(Value::Int(n)) => Some(n),
        _ => None,
    }
}

/// Build the whole graph + the branch stack + the completion marker.
fn build(path: &Path, size: u64) -> (BranchId, Land, f64) {
    for ext in ["", "-wal", "-shm"] {
        let _ = std::fs::remove_file(format!("{}{ext}", path.display()));
    }
    let mut store = Store::open(path).unwrap();
    tune(&store);
    let main = store.create_branch("main", &[]).unwrap();
    let t = Instant::now();
    let land = generate(size, Some((&mut store, main)));
    build_stack(&mut store, main, &land);
    store
        .write(WriteBatch::new(
            main,
            "bench",
            vec![Op::set("__meta__", "built", size as i64)],
        ))
        .unwrap();
    store.checkpoint().unwrap();
    (main, land, t.elapsed().as_secs_f64())
}

fn table(rows: &[Row]) -> String {
    let mut s = String::new();
    s += "| scenario | strategy | outcome | path len (links) | wall µs (median) | CPU µs (median) | nodes visited | link-read rounds | type-read rounds | SQL queries |\n";
    s += "| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |\n";
    for r in rows {
        let pl = r.pathlen.map_or("-".to_string(), |n| n.to_string());
        s += &format!(
            "| {} | {} | {} | {pl} | {:.1} | {:.1} | {} | {} | {} | {} |\n",
            r.scenario,
            r.strategy,
            r.outcome,
            r.wall_us,
            r.cpu_us,
            r.nodes,
            r.link_reads,
            r.value_reads,
            r.sql,
        );
    }
    s
}

fn main() {
    let all: Vec<String> = std::env::args().skip(1).collect();
    let flag = |f: &str| all.iter().any(|a| a == f);
    let value = |f: &str| all.iter().position(|a| a == f).map(|i| all[i + 1].clone());
    let size: u64 = value("--size").map_or(100000, |v| v.parse().unwrap());
    let dir = PathBuf::from(value("--data").unwrap_or("/workspace/egdb-perm/data".into()));
    std::fs::create_dir_all(&dir).unwrap();
    let path = dir.join(format!("perm-{size}.db"));
    let cold = flag("--cold");
    let reps = value("--reps").map_or(if cold { 5 } else { 11 }, |v| v.parse().unwrap());
    let only = value("--only");
    let strat_filter = value("--strategy");

    // Build or reuse.
    let mut store = Store::open(&path).unwrap();
    tune(&store);
    let main0 = store
        .branches()
        .ok()
        .and_then(|bs| bs.into_iter().find(|b| b.name == "main").map(|b| b.id));
    let (land, build_s) = match main0 {
        Some(m) if marker(&store, m) == Some(size as i64) => {
            eprintln!("reusing {}", path.display());
            (generate(size, None), 0.0)
        }
        _ => {
            drop(store);
            eprintln!("building {} entities at {}", size, path.display());
            let (_m, land, s) = build(&path, size);
            eprintln!("build {s:.1}s");
            store = Store::open(&path).unwrap();
            tune(&store);
            (land, s)
        }
    };
    let b = Branches {
        main: find_branch(&store, "main"),
        grand: find_branch(&store, GRAND_B),
        frozen: find_branch(&store, FROZEN_B),
    };
    store.checkpoint().unwrap();

    let scs = scenarios(&land);
    let mut rows = Vec::new();
    for sc in &scs {
        if only.as_ref().is_some_and(|o| o != sc.name) {
            continue;
        }
        for st in STRATEGIES {
            if strat_filter.as_ref().is_some_and(|s| s != st.0) {
                continue;
            }
            eprintln!("{} / {}", sc.name, st.0);
            rows.push(measure(&path, &store, &b, sc, st, reps, cold));
        }
    }

    let header = format!(
        "# Prove bench: {} entities (total {}), {} reps, {}\n\nbuild {:.1}s; huge teams {}, popular groups {}\n\n",
        size,
        land.total,
        reps,
        if cold { "cold" } else { "warm" },
        build_s,
        huge_teams(size),
        popular_groups(size),
    );
    let body = table(&rows);
    print!("{header}{body}");
    let out = PathBuf::from("/workspace/egdb-perm").join(format!(
        "results-{size}{}.md",
        if cold { "-cold" } else { "" }
    ));
    std::fs::write(&out, format!("{header}{body}")).unwrap();
    eprintln!("wrote {}", out.display());
}
