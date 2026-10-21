//! `find` benchmark on an existing file from `bench_rich` or `bench_scale`.
//!
//! ```text
//! cargo run --release --example bench_find -- FILE N [--branches base,top_open] \
//!     [--queries 0,3,...] [--baseline | --naive] [--no-full] [--add-score BRANCH,...] \
//!     [--values] [--insertion | --order-by KEY | --order-desc KEY] [--cache-kib N] [--knob probe|no_intersect|dense|intersect_sql|exact_count=V]...
//! ```
//!
//! `N` is the entity count of the file (IDs are the hashed IDs of the
//! generators). `--add-score` first writes `score = hash(i) % 10000` on the
//! named branches (one batch per 5000 entities), once.
//!
//! For each query and branch it reports the first page (limit 1000: median of
//! 5 runs, candidates checked) and a full read (all pages of 1000: rows, time,
//! rows/s, candidates checked). With `--baseline` it also runs the same
//! predicate as a `Filter::Custom`, which no index can serve: the scan of every
//! typed entity of the view (`baseline`), and with `include_archived` the scan
//! of the whole entity dictionary (`naive`).

use std::collections::BTreeMap;
use std::time::Instant;

use egdb::{BranchId, Filter, FindOptions, FindOrder, Op, Store, Value, WriteBatch, TYPE_KEY};

fn id(n: u64) -> String {
    let mut z = n.wrapping_add(0x9e37_79b9_7f4a_7c15);
    z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    format!("{:016x}", z ^ (z >> 31))
}

fn score(i: u64) -> i64 {
    (id(i ^ 0x5555)
        .as_bytes()
        .iter()
        .fold(0u64, |a, b| a.wrapping_mul(31).wrapping_add(*b as u64))
        % 10_000) as i64
}

type Check = Box<dyn Fn(&BTreeMap<String, Value>) -> bool + Send + Sync>;

fn text<'a>(v: &'a BTreeMap<String, Value>, k: &str) -> Option<&'a str> {
    match v.get(k) {
        Some(Value::Text(t)) => Some(t),
        _ => None,
    }
}
fn int(v: &BTreeMap<String, Value>, k: &str) -> Option<i64> {
    match v.get(k) {
        Some(Value::Int(i)) => Some(*i),
        _ => None,
    }
}

/// (name, filter, the same predicate in Rust).
fn queries(store: &Store, b: BranchId, n: u64) -> Vec<(&'static str, Filter, Check)> {
    // A title that exists on this branch, for the exact-match query.
    let probe = id(n / 3 + 7);
    let title = match store.value(b, &probe, "title").unwrap() {
        Some(Value::Text(t)) => t,
        _ => "none".into(),
    };
    let t2 = title.clone();
    vec![
        (
            "type = task",
            Filter::eq(TYPE_KEY, "task"),
            Box::new(|v| text(v, TYPE_KEY) == Some("task")),
        ),
        (
            "title = (one entity)",
            Filter::eq("title", title),
            Box::new(move |v| text(v, "title") == Some(t2.as_str())),
        ),
        (
            "score > 9899 (1%)",
            Filter::Gt("score".into(), 9899.into()),
            Box::new(|v| int(v, "score").is_some_and(|s| s > 9899)),
        ),
        (
            "score > 4999 (50%)",
            Filter::Gt("score".into(), 4999.into()),
            Box::new(|v| int(v, "score").is_some_and(|s| s > 4999)),
        ),
        (
            "title contains \"12345\" (rare)",
            Filter::Contains("title".into(), "12345".into()),
            Box::new(|v| text(v, "title").is_some_and(|t| t.contains("12345"))),
        ),
        (
            "title contains \"9\" (common)",
            Filter::Contains("title".into(), "9".into()),
            Box::new(|v| text(v, "title").is_some_and(|t| t.contains('9'))),
        ),
        (
            "type = task AND title contains \"99\"",
            Filter::And(vec![
                Filter::eq(TYPE_KEY, "task"),
                Filter::Contains("title".into(), "99".into()),
            ]),
            Box::new(|v| {
                text(v, TYPE_KEY) == Some("task")
                    && text(v, "title").is_some_and(|t| t.contains("99"))
            }),
        ),
        (
            "priority = 1 OR priority = 2",
            Filter::Or(vec![Filter::eq("priority", 1), Filter::eq("priority", 2)]),
            Box::new(|v| matches!(int(v, "priority"), Some(1) | Some(2))),
        ),
        (
            "score > 9899 AND type = task",
            Filter::And(vec![
                Filter::Gt("score".into(), 9899.into()),
                Filter::eq(TYPE_KEY, "task"),
            ]),
            Box::new(|v| {
                int(v, "score").is_some_and(|s| s > 9899) && text(v, TYPE_KEY) == Some("task")
            }),
        ),
    ]
}

/// CPU time of this thread in ns (`/proc/thread-self/schedstat`): stable
/// when other jobs share the machine, unlike wall time.
fn cpu_ns() -> u64 {
    std::fs::read_to_string("/proc/thread-self/schedstat")
        .ok()
        .and_then(|s| s.split_whitespace().next()?.parse().ok())
        .unwrap_or(0)
}

struct Run {
    first_cpu_ms: f64,
    full_cpu_s: f64,
    first_ms: f64,
    first_scanned: usize,
    rows: usize,
    full_s: f64,
    scanned: usize,
}

fn run(store: &Store, b: BranchId, f: &Filter, base: &FindOptions, full: bool) -> Run {
    let mut times = Vec::new();
    let mut cpus = Vec::new();
    let mut first_scanned = 0;
    for _ in 0..5 {
        let t = Instant::now();
        let c = cpu_ns();
        let page = store.find(b, f, base.clone()).unwrap();
        cpus.push((cpu_ns() - c) as f64 / 1e6);
        times.push(t.elapsed().as_secs_f64() * 1e3);
        first_scanned = page.scanned;
    }
    times.sort_by(f64::total_cmp);
    cpus.sort_by(f64::total_cmp);
    let mut full_cpu_s = 0.0;
    let (mut rows, mut scanned, mut full_s) = (0, 0, 0.0);
    if full {
        let t = Instant::now();
        let c = cpu_ns();
        let mut cursor = None;
        loop {
            let page = store
                .find(
                    b,
                    f,
                    FindOptions {
                        cursor,
                        ..base.clone()
                    },
                )
                .unwrap();
            rows += page.rows.len();
            scanned += page.scanned;
            cursor = page.next;
            if cursor.is_none() {
                break;
            }
        }
        full_s = t.elapsed().as_secs_f64();
        full_cpu_s = (cpu_ns() - c) as f64 / 1e9;
    }
    Run {
        first_cpu_ms: cpus[2],
        full_cpu_s,
        first_ms: times[2],
        first_scanned,
        rows,
        full_s,
        scanned,
    }
}

fn main() {
    let all: Vec<String> = std::env::args().skip(1).collect();
    let flag = |f: &str| all.iter().any(|a| a == f);
    let value = |f: &str| all.iter().position(|a| a == f).map(|i| all[i + 1].clone());
    let pos: Vec<&String> = all
        .iter()
        .enumerate()
        .filter(|(i, a)| {
            !a.starts_with("--")
                && (*i == 0
                    || !all[i - 1].starts_with("--")
                    || ["--baseline", "--no-full", "--values", "--insertion", "--naive"]
                        .contains(&all[i - 1].as_str()))
        })
        .map(|(_, a)| a)
        .collect();
    let path = pos[0].clone();
    let n: u64 = pos[1].parse().unwrap();
    let mut store = Store::open(&path).unwrap();
    store.set_mmap_size(0).unwrap();
    if let Some(kib) = value("--cache-kib") {
        store.set_cache_size_kib(kib.parse().unwrap()).unwrap();
    }
    // `--knob name=value`, repeatable: planner switches (`egdb::find_knobs`).
    for (i, a) in all.iter().enumerate() {
        if a == "--knob" {
            let (k, v) = all[i + 1].split_once('=').expect("name=value");
            let v: u8 = v.parse().unwrap();
            let knob = match k {
                "probe" => &egdb::find_knobs::PROBE,
                "no_intersect" => &egdb::find_knobs::NO_INTERSECT,
                "dense" => &egdb::find_knobs::DENSE,
                "intersect_sql" => &egdb::find_knobs::INTERSECT_SQL,
                "exact_count" => &egdb::find_knobs::EXACT_COUNT,
                _ => panic!("unknown knob {k}"),
            };
            knob.store(v, std::sync::atomic::Ordering::Relaxed);
        }
    }
    let named = |store: &Store, name: &str| {
        store
            .branches()
            .unwrap()
            .into_iter()
            .find(|b| b.name == name)
            .map(|b| b.id)
            .unwrap_or_else(|| panic!("no branch {name}"))
    };
    if let Some(list) = value("--add-score") {
        for name in list.split(',') {
            let b = named(&store, name);
            if store.value(b, &id(0), "score").unwrap().is_some() {
                continue;
            }
            let t = Instant::now();
            let mut ops = Vec::new();
            for i in 0..n {
                ops.push(Op::set(id(i).as_str(), "score", score(i)));
                if ops.len() == 5000 {
                    store
                        .write(WriteBatch::new(b, "bench", std::mem::take(&mut ops)))
                        .unwrap();
                }
            }
            if !ops.is_empty() {
                store.write(WriteBatch::new(b, "bench", ops)).unwrap();
            }
            eprintln!("score on {name}: {:.1}s", t.elapsed().as_secs_f64());
        }
        store.checkpoint().unwrap();
    }
    let branches: Vec<String> = value("--branches")
        .unwrap_or("base".into())
        .split(',')
        .map(str::to_owned)
        .collect();
    let only: Option<Vec<String>> =
        value("--queries").map(|q| q.split(',').map(str::to_owned).collect());
    let full = !flag("--no-full");
    let opts = FindOptions {
        values: flag("--values"),
        order: if let Some(k) = value("--order-by") {
            FindOrder::ValueAsc(k)
        } else if let Some(k) = value("--order-desc") {
            FindOrder::ValueDesc(k)
        } else if flag("--insertion") {
            FindOrder::Insertion
        } else {
            FindOrder::EntityId
        },
        ..FindOptions::default()
    };
    println!(
        "file {path}, values {}, order {:?}",
        opts.values, opts.order
    );
    println!();
    println!("| branch | query | path | first page ms | checked (first page) | rows | full read s | rows/s | checked (full) | first page CPU ms | full read CPU s |");
    println!("| --- | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |");
    for name in &branches {
        let b = named(&store, name);
        for (i, (q, f, check)) in queries(&store, b, n).into_iter().enumerate() {
            if only.as_ref().is_some_and(|o| !o.contains(&i.to_string())) {
                continue;
            }
            let mut variants: Vec<(&str, Filter, FindOptions)> =
                vec![("index", f.clone(), opts.clone())];
            if flag("--naive") {
                let check = std::sync::Arc::new(check);
                variants = vec![(
                    "naive",
                    Filter::custom(move |_, v| v.contains_key(TYPE_KEY) && check(v)),
                    FindOptions {
                        include_archived: true,
                        ..opts.clone()
                    },
                )];
            } else if flag("--baseline") {
                let check = std::sync::Arc::new(check);
                let c1 = check.clone();
                variants.push(("baseline", Filter::custom(move |_, v| c1(v)), opts.clone()));
                let c2 = check.clone();
                variants.push((
                    "naive",
                    Filter::custom(move |_, v| v.contains_key(TYPE_KEY) && c2(v)),
                    FindOptions {
                        include_archived: true,
                        ..opts.clone()
                    },
                ));
            }
            for (label, filter, o) in variants {
                let r = run(&store, b, &filter, &o, full);
                println!(
                    "| {name} | {q} | {label} | {:.2} | {} | {} | {:.3} | {:.0} | {} | {:.2} | {:.3} |",
                    r.first_ms,
                    r.first_scanned,
                    r.rows,
                    r.full_s,
                    if r.full_s > 0.0 {
                        r.rows as f64 / r.full_s
                    } else {
                        0.0
                    },
                    r.scanned,
                    r.first_cpu_ms,
                    r.full_cpu_s
                );
            }
        }
    }
}
