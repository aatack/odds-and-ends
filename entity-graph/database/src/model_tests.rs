//! A randomised differential test. A naive in-memory model replays the events
//! by the rules of DESIGN.md. The real store must give the same answers.
//!
//! The model keeps every event of every branch forever (undo removes the
//! batch). It has no current and history tables, no SQL and no cache. A read
//! scans all events. It differs from the store in that way on purpose.
//!
//! Number of random cases: the environment variable `EGDB_DIFF_CASES`
//! (default 300). The seed of a failing case is in the panic message.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;

use crate::*;

const OPEN: i64 = i64::MAX;
const T0: Timestamp = 1_700_000_000_000;

#[derive(Clone, Debug)]
enum Kind {
    Val {
        ent: String,
        key: String,
        val: Value,
    },
    Lnk {
        src: String,
        dst: String,
        ty: String,
        sort: Option<f64>,
    },
}

#[derive(Clone, Debug)]
struct Ev {
    seq: i64,
    ts: i64,
    author: String,
    kind: Kind,
}

#[derive(Clone, Debug)]
struct MBranch {
    id: BranchId,
    events: Vec<Ev>,
    head: i64,
    last_ts: i64,
    /// (parent index, lo, hi); hi = OPEN for an open range. Deduped.
    parents: Vec<(usize, i64, i64)>,
    archived: bool,
}

#[derive(Clone, Debug)]
struct Batch {
    branch: usize,
    author: String,
    first: i64,
    last: i64,
    created: i64,
}

struct Model {
    bs: Vec<MBranch>,
    batches: Vec<Batch>,
}

type Entry = (usize, i64, i64);
type Won = (usize, i64, usize, Ev);

fn dedupe(entries: Vec<Entry>) -> Vec<Entry> {
    let mut out: Vec<Entry> = Vec::new();
    for (b, lo, hi) in entries {
        if let Some(e) = out.iter_mut().find(|e| e.0 == b) {
            e.1 = e.1.min(lo);
            e.2 = e.2.max(hi);
        } else {
            out.push((b, lo, hi));
        }
    }
    out
}

/// What a read of a branch sees: layers from the bottom, each a branch and
/// the sequence ranges that the layer reads from it.
struct Layers(Vec<(usize, Vec<(i64, i64)>)>);

impl Model {
    fn expand(&self, b: usize, lo: i64, path: &mut Vec<usize>, out: &mut Vec<Entry>) {
        path.push(b);
        for &(p, plo, phi) in &self.bs[b].parents {
            if phi != OPEN || p == b {
                out.push((p, plo, phi));
            } else if !path.contains(&p) {
                self.expand(p, plo, path, out);
            }
        }
        path.pop();
        out.push((b, lo, OPEN));
    }

    /// The flat list of a branch, without its uncovered own events.
    fn flat(&self, b: usize) -> Vec<Entry> {
        let mut all = Vec::new();
        self.expand(b, 1, &mut Vec::new(), &mut all);
        all.pop();
        dedupe(all)
    }

    /// The own events of `b` that no entry of its flat list covers, as ranges.
    fn own_tail(&self, b: usize, flat: &[Entry]) -> Vec<(i64, i64)> {
        let mut cuts: Vec<(i64, i64)> = flat
            .iter()
            .filter(|e| e.0 == b)
            .map(|e| (e.1, e.2))
            .collect();
        cuts.sort();
        let mut out = Vec::new();
        let mut at = 1i64;
        for (lo, hi) in cuts {
            if lo > at {
                out.push((at, lo - 1));
            }
            if hi == OPEN {
                return out;
            }
            at = at.max(hi + 1);
        }
        out.push((at, OPEN));
        out
    }

    fn layers(&self, b: usize) -> Layers {
        let flat = self.flat(b);
        let tail = self.own_tail(b, &flat);
        let mut l: Vec<(usize, Vec<(i64, i64)>)> =
            flat.iter().map(|e| (e.0, vec![(e.1, e.2)])).collect();
        if !tail.is_empty() {
            l.push((b, tail));
        }
        Layers(l)
    }

    /// The frozen version of the view: every range capped at the head.
    fn frozen_view(&self, b: usize) -> Vec<Entry> {
        let mut out = Vec::new();
        for (br, ivs) in self.layers(b).0 {
            for (lo, hi) in ivs {
                let hi = hi.min(self.bs[br].head);
                if lo <= hi {
                    out.push((br, lo, hi));
                }
            }
        }
        out
    }

    fn set_parents(&mut self, b: usize, entries: Vec<Entry>) {
        self.bs[b].parents = dedupe(entries);
    }

    /// The winning events of a branch's view: by target, the one with the
    /// largest `(layer, seq)`.
    fn view(&self, b: usize) -> Winners {
        let mut vals: HashMap<(String, String), Won> = HashMap::new();
        let mut lnks: HashMap<(String, String, String), Won> = HashMap::new();
        for (layer, (br, ivs)) in self.layers(b).0.iter().enumerate() {
            for ev in &self.bs[*br].events {
                if !ivs.iter().any(|&(lo, hi)| ev.seq >= lo && ev.seq <= hi) {
                    continue;
                }
                let better = |old: &Won| (layer, ev.seq) > (old.0, old.1);
                match &ev.kind {
                    Kind::Val { ent, key, .. } => {
                        let k = (ent.clone(), key.clone());
                        if vals.get(&k).is_none_or(better) {
                            vals.insert(k, (layer, ev.seq, *br, ev.clone()));
                        }
                    }
                    Kind::Lnk { src, dst, ty, .. } => {
                        let k = (src.clone(), ty.clone(), dst.clone());
                        if lnks.get(&k).is_none_or(better) {
                            lnks.insert(k, (layer, ev.seq, *br, ev.clone()));
                        }
                    }
                }
            }
        }
        Winners { vals, lnks }
    }
}

struct Winners {
    vals: HashMap<(String, String), Won>,
    lnks: HashMap<(String, String, String), Won>,
}

impl Winners {
    fn values(&self, ent: &str) -> BTreeMap<String, Value> {
        self.vals
            .iter()
            .filter(|((e, _), (_, _, _, ev))| {
                e == ent && matches!(&ev.kind, Kind::Val { val, .. } if !val.is_null())
            })
            .map(|((_, k), (_, _, _, ev))| match &ev.kind {
                Kind::Val { val, .. } => (k.clone(), val.clone()),
                _ => unreachable!(),
            })
            .collect()
    }

    fn typed(&self, ent: &str) -> bool {
        self.vals
            .get(&(ent.to_owned(), TYPE_KEY.to_owned()))
            .is_some_and(
                |(_, _, _, ev)| matches!(&ev.kind, Kind::Val { val, .. } if !val.is_null()),
            )
    }

    fn exists(&self, ent: &str) -> bool {
        self.vals.keys().any(|(e, _)| e == ent)
            || self.lnks.keys().any(|(s, _, d)| s == ent || d == ent)
    }

    fn archived(&self, ent: &str) -> bool {
        self.vals
            .get(&(ent.to_owned(), TYPE_KEY.to_owned()))
            .is_some_and(|(_, _, _, ev)| matches!(&ev.kind, Kind::Val { val, .. } if val.is_null()))
    }

    fn links(&self, m: &Model, ent: &str, ty: &str, dir: Direction, typed_only: bool) -> Vec<Link> {
        let mut out: Vec<Link> = Vec::new();
        for ((s, t, d), (_, seq, br, ev)) in &self.lnks {
            if t != ty {
                continue;
            }
            let (me, other) = match dir {
                Direction::Outbound => (s, d),
                Direction::Inbound => (d, s),
            };
            let Kind::Lnk {
                sort: Some(sort), ..
            } = &ev.kind
            else {
                continue;
            };
            if me != ent || (typed_only && !self.typed(other)) {
                continue;
            }
            out.push(Link {
                source: s.clone(),
                destination: d.clone(),
                link_type: t.clone(),
                sort: *sort,
                event: EventRef {
                    branch: m.bs[*br].id,
                    seq: *seq,
                    timestamp: ev.ts,
                    author: ev.author.clone(),
                },
            });
        }
        out.sort_by(|a, b| {
            a.sort.total_cmp(&b.sort).then_with(|| match dir {
                Direction::Outbound => a.destination.cmp(&b.destination),
                Direction::Inbound => a.source.cmp(&b.source),
            })
        });
        out
    }
}

// --- the model's writes and undo ------------------------------------------

impl Model {
    fn write(&mut self, b: usize, author: &str, ops: &[Op], now: i64) -> Option<(i64, i64)> {
        if self.bs[b].archived {
            return None;
        }
        let br = &mut self.bs[b];
        let ts = now.max(br.last_ts);
        let first = br.head + 1;
        for op in ops {
            br.head += 1;
            let kind = match op {
                Op::SetValue { entity, key, value } => Kind::Val {
                    ent: entity.clone(),
                    key: key.clone(),
                    val: value.clone(),
                },
                Op::Link {
                    source,
                    destination,
                    link_type,
                    sort,
                } => Kind::Lnk {
                    src: source.clone(),
                    dst: destination.clone(),
                    ty: link_type.clone(),
                    sort: match sort {
                        SortOrder::Timestamp => Some(ts as f64),
                        SortOrder::Explicit(f) => Some(*f),
                        SortOrder::Removed => None,
                    },
                },
            };
            br.events.push(Ev {
                seq: br.head,
                ts,
                author: author.to_owned(),
                kind,
            });
        }
        br.last_ts = ts;
        let last = br.head;
        self.batches.push(Batch {
            branch: b,
            author: author.to_owned(),
            first,
            last,
            created: now,
        });
        Some((first, last))
    }

    fn target(kind: &Kind) -> (u8, String, String, String) {
        match kind {
            Kind::Val { ent, key, .. } => (0, ent.clone(), key.clone(), String::new()),
            Kind::Lnk { src, dst, ty, .. } => (1, src.clone(), ty.clone(), dst.clone()),
        }
    }

    /// `Err(true)` for a refusal, `Err(false)` for an archived branch.
    fn undo(&mut self, b: usize, author: &str, now: i64) -> Result<Option<UndoOutcome>, bool> {
        if self.bs[b].archived {
            return Err(false);
        }
        let Some(pos) = self
            .batches
            .iter()
            .enumerate()
            .filter(|(_, x)| x.branch == b && x.author == author && x.created >= now - 300_000)
            .max_by_key(|(_, x)| x.first)
            .map(|(i, _)| i)
        else {
            return Ok(None);
        };
        let Batch { first, last, .. } = self.batches[pos].clone();
        // A frozen range of another branch over the batch.
        for (i, o) in self.bs.iter().enumerate() {
            if i != b
                && o.parents
                    .iter()
                    .any(|&(p, lo, hi)| p == b && hi != OPEN && lo <= last && hi >= first)
            {
                return Err(true);
            }
        }
        let evs = &self.bs[b].events;
        let in_batch: Vec<&Ev> = evs
            .iter()
            .filter(|e| e.seq >= first && e.seq <= last)
            .collect();
        // A later event on the same target replaced one of the batch's events.
        for e in &in_batch {
            let t = Self::target(&e.kind);
            if evs
                .iter()
                .any(|x| x.seq > last && Self::target(&x.kind) == t)
            {
                return Err(true);
            }
        }
        // restored: targets of the batch that had an older event.
        let mut targets: Vec<_> = in_batch.iter().map(|e| Self::target(&e.kind)).collect();
        targets.sort();
        targets.dedup();
        let restored = targets
            .iter()
            .filter(|t| {
                evs.iter()
                    .any(|x| x.seq < first && &Self::target(&x.kind) == *t)
            })
            .count();
        let removed = in_batch.len();
        self.bs[b].events.retain(|e| e.seq < first || e.seq > last);
        self.batches.remove(pos);
        Ok(Some(UndoOutcome {
            removed: (first, last),
            events: removed,
            restored,
        }))
    }
}

// --- the random driver ---------------------------------------------------------

struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }
    fn below(&mut self, n: usize) -> usize {
        (self.next() % n as u64) as usize
    }
    fn chance(&mut self, num: u64, den: u64) -> bool {
        self.next() % den < num
    }
}

const ENTS: usize = 6;

struct Rig {
    store: Store,
    clock: Arc<AtomicI64>,
    m: Model,
    ids: Vec<BranchId>,
}

impl Rig {
    fn new() -> Self {
        let mut store = Store::open_in_memory().unwrap();
        let clock = Arc::new(AtomicI64::new(T0));
        let c = clock.clone();
        store.set_clock(move || c.load(Ordering::SeqCst));
        let main = store.create_branch("main", &[]).unwrap();
        let m = Model {
            bs: vec![MBranch {
                id: main,
                events: vec![],
                head: 0,
                last_ts: 0,
                parents: vec![],
                archived: false,
            }],
            batches: vec![],
        };
        Self {
            store,
            clock,
            m,
            ids: vec![main],
        }
    }

    fn now(&self) -> i64 {
        self.clock.load(Ordering::SeqCst)
    }

    fn add_branch(&mut self, id: BranchId, entries: Vec<Entry>) {
        self.ids.push(id);
        self.m.bs.push(MBranch {
            id,
            events: vec![],
            head: 0,
            last_ts: 0,
            parents: dedupe(entries),
            archived: false,
        });
    }

    fn pref(&self, e: Entry) -> ParentRef {
        ParentRef::new(
            self.ids[e.0],
            SeqRange::new(e.1, (e.2 != OPEN).then_some(e.2)),
        )
    }
}

fn ent(i: usize) -> String {
    format!("e{i}")
}

fn rand_op(r: &mut Rng) -> Op {
    let e = ent(r.below(ENTS));
    match r.below(8) {
        0 => Op::set(e, TYPE_KEY, if r.chance(1, 2) { "task" } else { "list" }),
        1 => Op::set(e, TYPE_KEY, "note"),
        2 => Op::clear(e, TYPE_KEY),
        3 => Op::set(e, "title", format!("t{}", r.below(5))),
        4 => {
            let v: Value = match r.below(5) {
                0 => Value::Int(r.below(7) as i64),
                1 => Value::Bool(r.chance(1, 2)),
                2 => Value::Null,
                3 => Value::Float(r.below(7) as f64 / 2.0),
                _ => Value::Text("x".into()),
            };
            Op::set(e, "priority", v)
        }
        _ => {
            let other = ent(r.below(ENTS));
            let ty = if r.chance(2, 3) { "child" } else { "ref" };
            let sort = match r.below(4) {
                0 => SortOrder::Timestamp,
                1 => SortOrder::Explicit(r.below(4) as f64),
                2 => SortOrder::Explicit(r.below(4) as f64 + 0.5),
                _ => SortOrder::Removed,
            };
            Op::link(e, other, ty, sort)
        }
    }
}

fn rand_range(r: &mut Rng, head: i64) -> (i64, i64) {
    let lo = if r.chance(1, 2) {
        1
    } else {
        1 + r.below(head.max(1) as usize) as i64
    };
    match r.below(3) {
        0 => (lo, OPEN),
        _ => (lo, lo + r.below(head.max(1) as usize + 1) as i64),
    }
}

fn step(rig: &mut Rig, r: &mut Rng, seed: u64, i: usize) {
    let n = rig.ids.len();
    let b = r.below(n);
    let ctx = format!("seed {seed} op {i}");
    rig.clock
        .fetch_add(1 + r.below(50) as i64, Ordering::SeqCst);
    let tr = std::env::var("EGDB_DIFF_TRACE").is_ok();
    match r.below(20) {
        0..=8 => {
            let author = if r.chance(1, 2) { "a" } else { "b" };
            let ops: Vec<Op> = (0..1 + r.below(4)).map(|_| rand_op(r)).collect();
            if tr {
                eprintln!("{ctx}: write b{b} {author} @{} {ops:?}", rig.now());
            }
            let want = rig.m.write(b, author, &ops, rig.now());
            let got = rig.store.write(WriteBatch::new(rig.ids[b], author, ops));
            match (want, got) {
                (Some((f, l)), Ok(w)) => assert_eq!((f, l), (w.first_seq, w.last_seq), "{ctx}"),
                (None, Err(Error::BranchArchived(_))) => {}
                (w, g) => panic!("{ctx}: write {w:?} vs {g:?}"),
            }
        }
        9 if n < 6 => {
            let entries = rig.m.frozen_view(b);
            if tr {
                eprintln!("{ctx}: fork b{b} -> b{n} entries {entries:?}");
            }
            let id = rig.store.fork(rig.ids[b], "f").unwrap();
            rig.add_branch(id, entries);
        }
        10 if n < 6 => {
            let mut entries = Vec::new();
            for _ in 0..1 + r.below(3) {
                let p = r.below(n);
                let (lo, hi) = rand_range(r, rig.m.bs[p].head);
                entries.push((p, lo, hi));
            }
            let prefs: Vec<ParentRef> = entries.iter().map(|e| rig.pref(*e)).collect();
            if tr {
                eprintln!("{ctx}: create_branch b{n} parents {entries:?}");
            }
            let id = rig.store.create_branch("c", &prefs).unwrap();
            rig.add_branch(id, entries);
        }
        11 => {
            let from = r.below(n);
            let (lo, hi) = rand_range(r, rig.m.bs[from].head);
            let mut parents = rig.m.bs[b].parents.clone();
            if tr {
                eprintln!("{ctx}: pull into b{b} from b{from} {lo}..{hi}");
            }
            parents.push((from, lo, hi));
            rig.store
                .pull(
                    rig.ids[b],
                    rig.ids[from],
                    SeqRange::new(lo, (hi != OPEN).then_some(hi)),
                )
                .unwrap();
            rig.m.set_parents(b, parents);
        }
        12 => {
            let from = r.below(n);
            // `merge` freezes what the branch's own parent list leaves uncovered
            // (not what the expanded list covers); see the audit notes.
            let flat = rig.m.bs[b].parents.clone();
            let tail: Vec<Entry> = rig
                .m
                .own_tail(b, &flat)
                .into_iter()
                .filter_map(|(lo, hi)| {
                    let hi = hi.min(rig.m.bs[b].head);
                    (lo <= hi).then_some((b, lo, hi))
                })
                .collect();
            let mut parents = rig.m.bs[b].parents.clone();
            parents.extend(tail);
            parents.extend(rig.m.frozen_view(from).into_iter().filter(|e| e.0 != b));
            if tr {
                eprintln!("{ctx}: merge into b{b} from b{from} -> parents {parents:?}");
            }
            rig.store.merge(rig.ids[b], rig.ids[from]).unwrap();
            rig.m.set_parents(b, parents);
        }
        13 => {
            let mut entries = Vec::new();
            for _ in 0..r.below(4) {
                let p = r.below(n);
                let (lo, hi) = rand_range(r, rig.m.bs[p].head);
                entries.push((p, lo, hi));
            }
            let prefs: Vec<ParentRef> = entries.iter().map(|e| rig.pref(*e)).collect();
            if tr {
                eprintln!("{ctx}: set_parents b{b} {entries:?}");
            }
            rig.store.set_parents(rig.ids[b], &prefs).unwrap();
            rig.m.set_parents(b, entries);
        }
        14 => {
            let e = ent(r.below(ENTS));
            if tr {
                eprintln!("{ctx}: archive_entity b{b} {e}");
            }
            let want = rig
                .m
                .write(b, "a", &[Op::clear(e.as_str(), TYPE_KEY)], rig.now());
            let got = rig.store.archive_entity(rig.ids[b], "a", &e);
            assert_eq!(want.is_some(), got.is_ok(), "{ctx}: archive");
        }
        15..=16 => {
            let author = if r.chance(1, 2) { "a" } else { "b" };
            let want = rig.m.undo(b, author, rig.now());
            if tr {
                eprintln!("{ctx}: undo b{b} {author} @{} -> {want:?}", rig.now());
            }
            let got = rig.store.undo(rig.ids[b], author);
            match (want, got) {
                (Ok(w), Ok(g)) => assert_eq!(w, g, "{ctx}: undo"),
                (Err(true), Err(Error::UndoRefused(_))) => {}
                (Err(false), Err(Error::BranchArchived(_))) => {}
                (w, g) => panic!("{ctx}: undo {w:?} vs {g:?}"),
            }
        }
        17 => {
            // Move the clock past the undo window now and then.
            rig.clock.fetch_add(
                if r.chance(1, 2) { 301_000 } else { 100_000 },
                Ordering::SeqCst,
            );
        }
        18 if r.chance(1, 4) => {
            let flag = !rig.m.bs[b].archived;
            rig.store.archive_branch(rig.ids[b], flag).unwrap();
            rig.m.bs[b].archived = flag;
        }
        _ => {}
    }
}

// --- comparing reads ----------------------------------------------------------------

fn follow_map() -> Follow {
    HashMap::from([
        (
            "task".to_owned(),
            vec![("child".to_owned(), Direction::Outbound)],
        ),
        (
            "list".to_owned(),
            vec![
                ("child".to_owned(), Direction::Outbound),
                ("ref".to_owned(), Direction::Outbound),
            ],
        ),
        (
            "note".to_owned(),
            vec![("child".to_owned(), Direction::Inbound)],
        ),
    ])
}

fn descend(path: &[String], _: &WalkNode) -> bool {
    path.len() < 6
}

fn include(_: &[String], n: &WalkNode) -> bool {
    !matches!(n.values.get("priority"), Some(Value::Int(x)) if x % 3 == 0)
}

/// The model's depth-first walk from `start`: visited nodes in order, with
/// whether each is emitted.
fn model_walk(
    m: &Model,
    _b: usize,
    w: &Winners,
    start: &str,
    use_visited: bool,
) -> Vec<(Vec<String>, bool, WalkNode)> {
    let follow = follow_map();
    let mut out = Vec::new();
    let mut seen: HashSet<String> = HashSet::new();
    fn go(
        m: &Model,
        w: &Winners,
        follow: &Follow,
        path: Vec<String>,
        use_visited: bool,
        seen: &mut HashSet<String>,
        out: &mut Vec<(Vec<String>, bool, WalkNode)>,
    ) {
        let id = path.last().unwrap().clone();
        let node = WalkNode {
            id: id.clone(),
            values: w.values(&id),
        };
        out.push((path.clone(), include(&path, &node), node.clone()));
        if use_visited {
            seen.insert(id.clone());
        }
        if !descend(&path, &node) {
            return;
        }
        let pairs = match node.values.get(TYPE_KEY) {
            Some(Value::Text(t)) => follow.get(t).cloned().unwrap_or_default(),
            _ => vec![],
        };
        let mut kids: Vec<String> = Vec::new();
        for (ty, dir) in &pairs {
            for l in w.links(m, &id, ty, *dir, true) {
                let other = if *dir == Direction::Outbound {
                    l.destination
                } else {
                    l.source
                };
                if !kids.contains(&other) {
                    kids.push(other);
                }
            }
        }
        for kid in kids {
            if path.contains(&kid) || (use_visited && seen.contains(&kid)) {
                continue;
            }
            let mut p = path.clone();
            p.push(kid);
            go(m, w, follow, p, use_visited, seen, out);
        }
    }
    go(
        m,
        w,
        &follow,
        vec![start.to_owned()],
        use_visited,
        &mut seen,
        &mut out,
    );
    out
}

fn compare_reads(rig: &Rig, ctx: &str) {
    let names: Vec<String> = (0..ENTS + 1).map(ent).collect(); // one never written
    let refs: Vec<&str> = names.iter().map(String::as_str).collect();
    for (b, id) in rig.ids.iter().enumerate() {
        let w = rig.m.view(b);
        let got = rig.store.values(*id, &refs).unwrap();
        if std::env::var("EGDB_DIFF_DUMP").is_ok() && got["e0"] != w.values("e0") && b == 0 {
            eprintln!("MODEL layers {:?}", rig.m.layers(b).0);
            eprintln!("MODEL parents {:?}", rig.m.bs[b].parents);
            for ev in &rig.m.bs[b].events {
                eprintln!("  b0 ev {ev:?}");
            }
            eprintln!("STORE {:?}", rig.store.branch(*id).unwrap());
        }
        for e in &refs {
            assert_eq!(got[*e], w.values(e), "{ctx}: values of {e} on branch {b}");
            assert_eq!(
                rig.store.value(*id, e, "title").unwrap(),
                w.values(e).get("title").cloned(),
                "{ctx}: value title"
            );
        }
        let ex = rig.store.exists(*id, &refs).unwrap();
        let ar = rig.store.is_archived(*id, &refs).unwrap();
        for e in &refs {
            assert_eq!(ex[*e], w.exists(e), "{ctx}: exists {e} on {b}");
            assert_eq!(ar[*e], w.archived(e), "{ctx}: archived {e} on {b}");
        }
        for typed in [true, false] {
            let groups = [
                ("child", Direction::Outbound),
                ("child", Direction::Inbound),
                ("ref", Direction::Outbound),
                ("ref", Direction::Inbound),
            ];
            let got = if typed {
                rig.store.links(*id, &refs, &groups).unwrap()
            } else {
                rig.store
                    .links_including_archived(*id, &refs, &groups)
                    .unwrap()
            };
            for e in &refs {
                for (ty, dir) in groups {
                    assert_eq!(
                        got[*e][&(ty.to_owned(), dir)],
                        w.links(&rig.m, e, ty, dir, typed),
                        "{ctx}: links {e} {ty} {dir:?} typed={typed} on {b}"
                    );
                }
            }
        }
    }
}

fn compare_walks(rig: &Rig, r: &mut Rng, ctx: &str) {
    let follow = follow_map();
    for (b, id) in rig.ids.iter().enumerate() {
        let w = rig.m.view(b);
        let start = ent(r.below(ENTS));
        for use_visited in [false, true] {
            let want = model_walk(&rig.m, b, &w, &start, use_visited);
            let ra = ReadAhead {
                depth: r.below(3),
                width: 1 + r.below(40),
            };
            for page in [usize::MAX, 1, 2, 3, 7] {
                let mut path: Vec<String> = vec![start.clone()];
                let mut visited = use_visited.then(HashSet::new);
                let mut at = 0usize;
                loop {
                    let refs: Vec<&str> = path.iter().map(String::as_str).collect();
                    let got = rig
                        .store
                        .walk(
                            *id,
                            &refs,
                            &follow,
                            descend,
                            include,
                            WalkOptions {
                                steps: page,
                                read_ahead: ra,
                                return_cache: false,
                                visited: visited.take(),
                            },
                        )
                        .unwrap();
                    let take = page.min(want.len() - at);
                    assert_eq!(
                        got.steps, take,
                        "{ctx}: steps, page {page} start {start} branch {b} visited={use_visited}"
                    );
                    let page_rows: Vec<(Vec<String>, WalkNode)> = want[at..at + take]
                        .iter()
                        .filter(|x| x.1)
                        .map(|x| (x.0.clone(), x.2.clone()))
                        .collect();
                    at += take;
                    let got_rows: Vec<(Vec<String>, WalkNode)> = got
                        .rows
                        .iter()
                        .map(|x| (x.path.clone(), x.node.clone()))
                        .collect();
                    assert_eq!(got_rows, page_rows, "{ctx}: rows, page {page} start {start} branch {b} visited={use_visited} ra={ra:?}");
                    if at == want.len() {
                        assert!(got.next.is_none(), "{ctx}: next should be None");
                        break;
                    }
                    assert_eq!(got.next.as_ref(), Some(&want[at].0), "{ctx}: next path, page {page} start {start} branch {b} visited={use_visited}");
                    path = got.next.unwrap();
                    visited = got.visited;
                }
            }
        }
    }
}

fn run_case(seed: u64) {
    let mut r = Rng(seed.wrapping_mul(0x9E37_79B9_7F4A_7C15) | 1);
    let mut rig = Rig::new();
    let ops = 10 + r.below(30);
    for i in 0..ops {
        step(&mut rig, &mut r, seed, i);
        if i % 12 == 11 {
            compare_reads(&rig, &format!("seed {seed} after op {i}"));
            compare_finds(&rig, &mut r, &format!("seed {seed} after op {i}"));
        }
    }
    let ctx = format!("seed {seed} final");
    compare_reads(&rig, &ctx);
    compare_walks(&rig, &mut r, &ctx);
    compare_finds(&rig, &mut r, &ctx);
}

#[test]
fn the_store_agrees_with_a_naive_model_on_random_histories() {
    let cases: u64 = std::env::var("EGDB_DIFF_CASES")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(300);
    let first: u64 = std::env::var("EGDB_DIFF_FIRST")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(1);
    for seed in first..first + cases {
        run_case(seed);
    }
}

fn rand_value(r: &mut Rng) -> Value {
    match r.below(6) {
        0 => Value::Int(r.below(7) as i64),
        1 => Value::Float(r.below(7) as f64 / 2.0),
        2 => Value::Bool(r.chance(1, 2)),
        3 => Value::Null,
        4 => Value::Text(format!("t{}", r.below(5))),
        _ => Value::Text("x".into()),
    }
}

fn rand_bound(r: &mut Rng) -> Value {
    match r.below(3) {
        0 => Value::Int(r.below(7) as i64),
        1 => Value::Float(r.below(7) as f64 / 2.0 - 0.25),
        _ => Value::Text(format!("t{}", r.below(5))),
    }
}

fn rand_filter(r: &mut Rng, depth: usize) -> Filter {
    let key = |r: &mut Rng| ["type", "title", "priority"][r.below(3)].to_owned();
    if depth > 0 && r.chance(1, 3) {
        let n = r.below(4);
        let parts: Vec<Filter> = (0..n).map(|_| rand_filter(r, depth - 1)).collect();
        return match r.below(3) {
            0 => Filter::And(parts),
            1 => Filter::Or(parts),
            _ => Filter::Not(Box::new(rand_filter(r, depth - 1))),
        };
    }
    match r.below(14) {
        0 => Filter::Eq(TYPE_KEY.into(), ["note", "task", "list"][r.below(3)].into()),
        1 | 2 => Filter::Eq(key(r), rand_value(r)),
        3 => Filter::Ne(key(r), rand_value(r)),
        4 => Filter::Gt(key(r), rand_bound(r)),
        5 => Filter::Ge(key(r), rand_bound(r)),
        6 => Filter::Lt(key(r), rand_bound(r)),
        7 => Filter::Le(key(r), rand_bound(r)),
        8 => Filter::Contains(key(r), ["1", "t", "", "x", "o"][r.below(5)].into()),
        9 => Filter::ContainsIgnoreCase(key(r), ["T1", "X", "NOTE", "t"][r.below(4)].into()),
        10 => Filter::Prefix(key(r), ["t", "t1", "", "n", "ta"][r.below(5)].into()),
        11 => Filter::In(key(r), (0..r.below(3)).map(|_| rand_value(r)).collect()),
        12 => {
            if r.chance(1, 2) {
                Filter::IsNull(key(r))
            } else {
                Filter::NotNull(key(r))
            }
        }
        _ => {
            let odd = r.chance(1, 2);
            Filter::custom(move |id, _| id.ends_with(['1', '3', '5']) == odd)
        }
    }
}

fn compare_finds(rig: &Rig, r: &mut Rng, ctx: &str) {
    let names: Vec<String> = (0..ENTS + 1).map(ent).collect();
    for (b, id) in rig.ids.iter().enumerate() {
        let w = rig.m.view(b);
        for _ in 0..2 {
            let filter = rand_filter(r, 2);
            // Global switches: the model test runs on one thread.
            crate::find::knobs::PROBE.store(r.below(3) as u8, Ordering::Relaxed);
            crate::find::knobs::NO_INTERSECT.store(r.below(2) as u8, Ordering::Relaxed);
            crate::find::knobs::DENSE.store(r.below(3) as u8, Ordering::Relaxed);
            crate::find::knobs::EXACT_COUNT.store(r.below(3) as u8, Ordering::Relaxed);
            let include_archived = r.chance(1, 3);
            let mut want: Vec<String> = names
                .iter()
                .filter(|e| w.exists(e) && (include_archived || w.typed(e)))
                .filter(|e| crate::find::matches(&filter, e, &w.values(e)))
                .cloned()
                .collect();
            want.sort();
            let vkey = ["type", "title", "priority"][r.below(3)].to_owned();
            let order = match r.below(6) {
                0 | 1 => FindOrder::Insertion,
                2 => FindOrder::ValueAsc(vkey.clone()),
                3 => FindOrder::ValueDesc(vkey.clone()),
                _ => FindOrder::EntityId,
            };
            let by_value = matches!(order, FindOrder::ValueAsc(_) | FindOrder::ValueDesc(_));
            if by_value {
                want.retain(|e| w.values(e).contains_key(&vkey));
            }
            let values = r.chance(1, 2);
            let limit = [1, 2, 3, 1000][r.below(4)];
            let scan_limit = if r.chance(1, 4) {
                1 + r.below(4)
            } else {
                usize::MAX
            };
            let mut got: Vec<String> = Vec::new();
            let mut cursor = None;
            for _ in 0..100 {
                let page = rig
                    .store
                    .find(
                        *id,
                        &filter,
                        FindOptions {
                            limit,
                            cursor,
                            order: order.clone(),
                            values,
                            include_archived,
                            scan_limit,
                        },
                    )
                    .unwrap();
                assert!(page.rows.len() <= limit);
                for row in &page.rows {
                    let expect = if values {
                        w.values(&row.id)
                    } else {
                        BTreeMap::new()
                    };
                    assert_eq!(
                        row.values, expect,
                        "{ctx}: find values of {} on {b}",
                        row.id
                    );
                    got.push(row.id.clone());
                }
                cursor = page.next;
                if cursor.is_none() {
                    break;
                }
            }
            assert!(cursor.is_none(), "{ctx}: find did not end");
            if by_value {
                // Non-decreasing (or non-increasing) final values.
                let key_of = |e: &String| crate::find::sql_value(&w.values(e)[&vkey]);
                for pair in got.windows(2) {
                    let o = crate::find::cmp_sql(&key_of(&pair[0]), &key_of(&pair[1]));
                    let bad = if matches!(order, FindOrder::ValueAsc(_)) {
                        o.is_gt()
                    } else {
                        o.is_lt()
                    };
                    assert!(!bad, "{ctx}: {order:?} out of order at {pair:?}");
                }
            }
            if order != FindOrder::EntityId {
                let n = got.len();
                got.sort();
                got.dedup();
                assert_eq!(got.len(), n, "{ctx}: duplicate rows");
            }
            assert_eq!(
                got, want,
                "{ctx}: find {filter:?} on {b} (archived {include_archived}, {order:?}, limit {limit}, scan {scan_limit})"
            );
        }
    }
}
