//! `find`: a paginated, flat list of the entities of a branch whose values
//! match a filter.
//!
//! The result is defined as: roll up every entity of the view, apply the
//! filter to its final values, keep the entities with a non-null `type`
//! (unless `include_archived`), sort, and cut a page. The code gets there
//! faster in two steps:
//!
//! 1. **Candidates.** One SQL query per chunk returns the next entities, in
//!    page order, that *may* match. It reads the value indexes of every layer
//!    of the view (current, plus history for bounded ranges). A candidate set
//!    is always a superset: the winning event of a matching entity is an
//!    event of the view that matches the indexed term.
//! 2. **Check.** The candidates' values are rolled up with the shared
//!    roll-up and the whole filter is applied to the final values. An old
//!    value that a higher layer overrode, or that null cleared, drops out here.
//!
//! The page order is total and the cursor is the last entity ID of the page,
//! so paging is stateless and stable.

use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fmt;
use std::sync::Arc;

use rusqlite::types::Value as SqlValue;

use crate::codec::encode_value;
use crate::error::{Error, Result};
use crate::read::{any_event, resolve_values, resolve_values_of};
use crate::store::Store;
use crate::types::{BranchId, Value, TYPE_KEY};
use crate::view::{View, OPEN};

/// A Rust predicate on an entity's final values: `(entity ID, values)`.
/// Runs after the SQL step, like the predicates of `walk`.
pub type Predicate = Arc<dyn Fn(&str, &BTreeMap<String, Value>) -> bool + Send + Sync>;

/// A filter on an entity's final (rolled-up) values.
///
/// Semantics (two-valued: every filter is true or false for each entity):
/// - A key that was never set, or that was cleared with null, is *absent*.
/// - `Eq(k, v)`: the value of `k` is `v`, with the same type. `Int(1)` does
///   not equal `Float(1.0)` or `Bool(true)`. `Eq(k, Null)` is `IsNull(k)`.
/// - `Ne(k, v)` is `Not(Eq(k, v))`; it is true for an absent key.
/// - `Lt`/`Le`/`Gt`/`Ge(k, bound)`: a number bound (`Int` or `Float`)
///   compares numerically, exactly, with `Int` and `Float` values only; a
///   `Text` bound compares with `Text` values only, by byte (code point)
///   order. Other values and absent keys do not match. Other bound types are
///   an error.
/// - `Contains(k, s)`, `Prefix(k, s)`: `Text` values only, case-sensitive.
///   `ContainsIgnoreCase(k, s)` folds ASCII letters only (`A`-`Z`).
/// - `In(k, vs)`: `Eq(k, v)` for some `v`.
/// - `IsNull(k)`: the key is absent. `NotNull(k)`: it has a value.
/// - `And([])` is true, `Or([])` is false.
/// - `Custom(f)`: `f(entity ID, final values)`.
#[derive(Clone)]
pub enum Filter {
    Eq(String, Value),
    Ne(String, Value),
    Lt(String, Value),
    Le(String, Value),
    Gt(String, Value),
    Ge(String, Value),
    Contains(String, String),
    ContainsIgnoreCase(String, String),
    Prefix(String, String),
    In(String, Vec<Value>),
    IsNull(String),
    NotNull(String),
    And(Vec<Filter>),
    Or(Vec<Filter>),
    Not(Box<Filter>),
    Custom(Predicate),
}

impl fmt::Debug for Filter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Filter::Eq(k, v) => write!(f, "Eq({k:?}, {v:?})"),
            Filter::Ne(k, v) => write!(f, "Ne({k:?}, {v:?})"),
            Filter::Lt(k, v) => write!(f, "Lt({k:?}, {v:?})"),
            Filter::Le(k, v) => write!(f, "Le({k:?}, {v:?})"),
            Filter::Gt(k, v) => write!(f, "Gt({k:?}, {v:?})"),
            Filter::Ge(k, v) => write!(f, "Ge({k:?}, {v:?})"),
            Filter::Contains(k, s) => write!(f, "Contains({k:?}, {s:?})"),
            Filter::ContainsIgnoreCase(k, s) => write!(f, "ContainsIgnoreCase({k:?}, {s:?})"),
            Filter::Prefix(k, s) => write!(f, "Prefix({k:?}, {s:?})"),
            Filter::In(k, vs) => write!(f, "In({k:?}, {vs:?})"),
            Filter::IsNull(k) => write!(f, "IsNull({k:?})"),
            Filter::NotNull(k) => write!(f, "NotNull({k:?})"),
            Filter::And(v) => f.debug_tuple("And").field(v).finish(),
            Filter::Or(v) => f.debug_tuple("Or").field(v).finish(),
            Filter::Not(x) => f.debug_tuple("Not").field(x).finish(),
            Filter::Custom(_) => write!(f, "Custom(..)"),
        }
    }
}

impl Filter {
    pub fn eq(key: impl Into<String>, value: impl Into<Value>) -> Self {
        Filter::Eq(key.into(), value.into())
    }
    pub fn custom(
        f: impl Fn(&str, &BTreeMap<String, Value>) -> bool + Send + Sync + 'static,
    ) -> Self {
        Filter::Custom(Arc::new(f))
    }
}

/// The order of a find.
#[derive(Clone, Debug, PartialEq, Eq, Default)]
pub enum FindOrder {
    /// By entity ID, ascending, by byte order (the default).
    #[default]
    EntityId,
    /// By the order in which the store first saw each entity ID. Stable, but
    /// it has no meaning for the caller. Faster for large `Eq` results.
    Insertion,
    /// By the final value of a key, ascending; ties in insertion order.
    /// Numbers (`Int`, `Float`, and `Bool` as 0 and 1) compare by value and
    /// come before text (`Text`, and `Json` as its text), which compares by
    /// bytes. Only entities with a value for the key are returned: the order
    /// adds `NotNull(key)` to the filter. The cursor is opaque.
    ValueAsc(String),
    /// [`FindOrder::ValueAsc`], descending (ties in reverse insertion order).
    ValueDesc(String),
}

#[derive(Clone, Debug)]
pub struct FindOptions {
    /// The most rows on one page (default 1000).
    pub limit: usize,
    /// Go on after this entity ID: the `next` of the previous page.
    pub cursor: Option<String>,
    pub order: FindOrder,
    /// Return the values of each row (default true). Off: IDs only.
    pub values: bool,
    /// Also return entities with no type (archived, or never typed).
    pub include_archived: bool,
    /// Stop after checking this many candidates, even with fewer than
    /// `limit` rows, and return a cursor (like the step limit of `walk`).
    pub scan_limit: usize,
}

impl Default for FindOptions {
    fn default() -> Self {
        Self {
            limit: 1000,
            cursor: None,
            order: FindOrder::EntityId,
            values: true,
            include_archived: false,
            scan_limit: usize::MAX,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FindRow {
    pub id: String,
    /// The final values, nulls left out (empty when `values` is off).
    pub values: BTreeMap<String, Value>,
}

/// One page of a find.
#[derive(Clone, Debug, PartialEq)]
pub struct Find {
    pub rows: Vec<FindRow>,
    /// Pass as `cursor` to get the next page. `None`: this was the last page.
    pub next: Option<String>,
    /// Candidates that were rolled up and checked (a cost measure).
    pub scanned: usize,
}

// --- Semantics ------------------------------------------------------------

/// Exact comparison of an `i64` with a finite `f64`.
fn cmp_int_float(a: i64, b: f64) -> Ordering {
    const TWO_63: f64 = 9_223_372_036_854_775_808.0;
    if b >= TWO_63 {
        return Ordering::Less;
    }
    if b < -TWO_63 {
        return Ordering::Greater;
    }
    let t = b.trunc();
    match a.cmp(&(t as i64)) {
        Ordering::Equal => 0.0f64.partial_cmp(&(b - t)).unwrap_or(Ordering::Equal),
        o => o,
    }
}

/// How a stored value compares with a range bound, if they are comparable.
fn compare(v: &Value, bound: &Value) -> Option<Ordering> {
    match (v, bound) {
        (Value::Int(a), Value::Int(b)) => Some(a.cmp(b)),
        (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
        (Value::Int(a), Value::Float(b)) => Some(cmp_int_float(*a, *b)),
        (Value::Float(a), Value::Int(b)) => Some(cmp_int_float(*b, *a).reverse()),
        (Value::Text(a), Value::Text(b)) => Some(a.as_bytes().cmp(b.as_bytes())),
        _ => None,
    }
}

fn check_bound(bound: &Value) -> Result<()> {
    match bound {
        Value::Int(_) | Value::Text(_) => Ok(()),
        Value::Float(f) if f.is_finite() => Ok(()),
        other => Err(Error::Invalid(format!(
            "a range bound must be a number or text, not {other:?}"
        ))),
    }
}

fn validate(f: &Filter) -> Result<()> {
    match f {
        Filter::Lt(_, b) | Filter::Le(_, b) | Filter::Gt(_, b) | Filter::Ge(_, b) => check_bound(b),
        Filter::And(v) | Filter::Or(v) => v.iter().try_for_each(validate),
        Filter::Not(x) => validate(x),
        _ => Ok(()),
    }
}

/// Whether the final values of entity `id` match.
pub(crate) fn matches(f: &Filter, id: &str, vals: &BTreeMap<String, Value>) -> bool {
    let text = |k: &str| match vals.get(k) {
        Some(Value::Text(t)) => Some(t.as_str()),
        _ => None,
    };
    let range = |k: &str, b: &Value, ok: fn(Ordering) -> bool| {
        vals.get(k).and_then(|v| compare(v, b)).is_some_and(ok)
    };
    match f {
        Filter::Eq(k, v) if v.is_null() => !vals.contains_key(k),
        Filter::Eq(k, v) => vals.get(k) == Some(v),
        Filter::Ne(k, v) => !matches(&Filter::Eq(k.clone(), v.clone()), id, vals),
        Filter::Lt(k, b) => range(k, b, |o| o == Ordering::Less),
        Filter::Le(k, b) => range(k, b, |o| o != Ordering::Greater),
        Filter::Gt(k, b) => range(k, b, |o| o == Ordering::Greater),
        Filter::Ge(k, b) => range(k, b, |o| o != Ordering::Less),
        Filter::Contains(k, s) => text(k).is_some_and(|t| t.contains(s.as_str())),
        Filter::ContainsIgnoreCase(k, s) => {
            text(k).is_some_and(|t| t.to_ascii_lowercase().contains(&s.to_ascii_lowercase()))
        }
        Filter::Prefix(k, s) => text(k).is_some_and(|t| t.starts_with(s.as_str())),
        Filter::In(k, vs) => vs
            .iter()
            .any(|v| matches(&Filter::Eq(k.clone(), v.clone()), id, vals)),
        Filter::IsNull(k) => !vals.contains_key(k),
        Filter::NotNull(k) => vals.contains_key(k),
        Filter::And(v) => v.iter().all(|x| matches(x, id, vals)),
        Filter::Or(v) => v.iter().any(|x| matches(x, id, vals)),
        Filter::Not(x) => !matches(x, id, vals),
        Filter::Custom(p) => p(id, vals),
    }
}

/// The keys that the filter reads; `None` when it can read any key.
fn keys_of(f: &Filter, out: &mut BTreeSet<String>) -> bool {
    match f {
        Filter::Eq(k, _)
        | Filter::Ne(k, _)
        | Filter::Lt(k, _)
        | Filter::Le(k, _)
        | Filter::Gt(k, _)
        | Filter::Ge(k, _)
        | Filter::Contains(k, _)
        | Filter::ContainsIgnoreCase(k, _)
        | Filter::Prefix(k, _)
        | Filter::In(k, _)
        | Filter::IsNull(k)
        | Filter::NotNull(k) => {
            out.insert(k.clone());
            true
        }
        Filter::And(v) | Filter::Or(v) => v.iter().all(|x| keys_of(x, out)),
        Filter::Not(x) => keys_of(x, out),
        Filter::Custom(_) => false,
    }
}

// --- Candidate plans --------------------------------------------------------

/// A condition on the `value` column that every matching event satisfies.
#[derive(Clone, Debug)]
enum Cond {
    Eq(SqlValue),
    In(Vec<SqlValue>),
    /// Lower and upper bound, each `(value, inclusive)`.
    Range(Option<(SqlValue, bool)>, Option<(SqlValue, bool)>),
    /// Text that contains the string (`lower`: ASCII-folded).
    Instr(String, bool),
    NotNull,
}

/// How to produce the candidates.
#[derive(Clone, Debug)]
enum Plan {
    /// Events of `key` whose value satisfies the condition.
    Term(String, Cond),
    /// Entities that are candidates of every part.
    All(Vec<Plan>),
    /// Entities that are candidates of some part.
    Any(Vec<Plan>),
    /// Every entity in the store dictionary.
    Everything,
}

impl Plan {
    /// A rough cost rank: lower is more selective.
    fn rank(&self) -> u32 {
        match self {
            Plan::Term(_, Cond::Eq(_)) => 1,
            Plan::Term(_, Cond::In(v)) => 1 + v.len() as u32,
            Plan::Term(_, Cond::Range(Some(_), Some(_))) => 8,
            Plan::Term(_, Cond::Range(..)) => 12,
            Plan::Term(_, Cond::Instr(..)) => 20,
            Plan::Term(_, Cond::NotNull) => 30,
            Plan::All(v) => v.iter().map(Plan::rank).min().unwrap_or(40),
            Plan::Any(v) => v.iter().map(Plan::rank).sum(),
            Plan::Everything => 40,
        }
    }
}

pub(crate) fn sql_value(v: &Value) -> SqlValue {
    encode_value(v).1
}

/// The smallest string above every string that starts with `s`, if any.
fn prefix_upper(s: &str) -> Option<String> {
    let mut chars: Vec<char> = s.chars().collect();
    while let Some(c) = chars.pop() {
        let mut n = c as u32 + 1;
        if (0xD800..=0xDFFF).contains(&n) {
            n = 0xE000;
        }
        if let Some(next) = char::from_u32(n) {
            chars.push(next);
            return Some(chars.into_iter().collect());
        }
    }
    None
}

fn text_floor() -> Option<(SqlValue, bool)> {
    Some((SqlValue::Text(String::new()), true))
}

/// The index plan for one filter, or `None` when no index term can bound it.
fn plan_of(f: &Filter) -> Option<Plan> {
    let term = |k: &str, c: Cond| Some(Plan::Term(k.to_owned(), c));
    let range = |k: &str, b: &Value, lower: bool, incl: bool| {
        let bound = Some((sql_value(b), incl));
        // Numbers sort below text in SQLite; text below blobs (none stored).
        let c = match (b, lower) {
            (Value::Text(_), true) => Cond::Range(bound, None),
            (Value::Text(_), false) => Cond::Range(text_floor(), bound),
            (_, true) => Cond::Range(bound, Some((SqlValue::Text(String::new()), false))),
            (_, false) => Cond::Range(None, bound),
        };
        term(k, c)
    };
    match f {
        Filter::Eq(_, v) if v.is_null() => None,
        Filter::Eq(k, v) => term(k, Cond::Eq(sql_value(v))),
        Filter::In(k, vs) if vs.iter().any(Value::is_null) => None,
        Filter::In(_, vs) if vs.is_empty() => Some(Plan::Any(vec![])),
        Filter::In(k, vs) => term(k, Cond::In(vs.iter().map(sql_value).collect())),
        Filter::Gt(k, b) => range(k, b, true, false),
        Filter::Ge(k, b) => range(k, b, true, true),
        Filter::Lt(k, b) => range(k, b, false, false),
        Filter::Le(k, b) => range(k, b, false, true),
        Filter::Prefix(k, s) => term(
            k,
            Cond::Range(
                Some((SqlValue::Text(s.clone()), true)),
                prefix_upper(s).map(|u| (SqlValue::Text(u), false)),
            ),
        ),
        Filter::Contains(k, s) => term(k, Cond::Instr(s.clone(), false)),
        Filter::ContainsIgnoreCase(k, s) => term(k, Cond::Instr(s.to_ascii_lowercase(), true)),
        Filter::NotNull(k) => term(k, Cond::NotNull),
        Filter::And(v) => {
            let mut parts: Vec<Plan> = v.iter().filter_map(plan_of).collect();
            parts.sort_by_key(Plan::rank);
            match parts.len() {
                0 => None,
                1 => parts.pop(),
                _ => Some(Plan::All(parts)),
            }
        }
        Filter::Or(v) => v
            .iter()
            .map(plan_of)
            .collect::<Option<Vec<_>>>()
            .map(Plan::Any),
        _ => None,
    }
}

/// Planner switches. Tests flip them to cover every path.
pub mod knobs {
    use std::sync::atomic::{AtomicU8, Ordering};

    /// 0: choose by density; 1: always probe; 2: never probe.
    pub static PROBE: AtomicU8 = AtomicU8::new(0);
    /// Intersect the two most selective terms of an `And` in SQL (when the
    /// second one is a bounded term), instead of using the best one alone.
    /// 0: yes, 1: no.
    pub static NO_INTERSECT: AtomicU8 = AtomicU8::new(0);
    /// For a dense plan: 0: choose; 1: probe; 2: dictionary scan.
    pub static DENSE: AtomicU8 = AtomicU8::new(0);

    /// 1: `And` intersects its parts with SQL `INTERSECT` (the older plan).
    pub static INTERSECT_SQL: AtomicU8 = AtomicU8::new(0);
    /// Estimates: 0: sample in large stores; 1: count exactly (up to the
    /// cap); 2: always sample.
    pub static EXACT_COUNT: AtomicU8 = AtomicU8::new(0);

    pub fn intersect_sql() -> bool {
        INTERSECT_SQL.load(Ordering::Relaxed) == 1
    }
    pub fn exact_count() -> bool {
        EXACT_COUNT.load(Ordering::Relaxed) == 1
    }
    pub fn always_sample() -> bool {
        EXACT_COUNT.load(Ordering::Relaxed) == 2
    }

    pub fn dense() -> u8 {
        DENSE.load(Ordering::Relaxed)
    }

    pub fn probe() -> Option<bool> {
        match PROBE.load(Ordering::Relaxed) {
            1 => Some(true),
            2 => Some(false),
            _ => None,
        }
    }
    pub fn intersect() -> bool {
        NO_INTERSECT.load(Ordering::Relaxed) == 0
    }
}

/// SQL text with numbered parameters.
#[derive(Default)]
struct Sql {
    params: Vec<SqlValue>,
}

impl Sql {
    fn bind(&mut self, v: SqlValue) -> String {
        self.params.push(v);
        format!("?{}", self.params.len())
    }

    fn cond(&mut self, c: &Cond) -> String {
        match c {
            Cond::Eq(v) => format!("value = {}", self.bind(v.clone())),
            Cond::In(vs) => {
                let list: Vec<String> = vs.iter().map(|v| self.bind(v.clone())).collect();
                format!("value IN ({})", list.join(", "))
            }
            Cond::Range(lo, hi) => {
                let mut parts = Vec::new();
                if let Some((v, incl)) = lo {
                    parts.push(format!(
                        "value {} {}",
                        if *incl { ">=" } else { ">" },
                        self.bind(v.clone())
                    ));
                }
                if let Some((v, incl)) = hi {
                    parts.push(format!(
                        "value {} {}",
                        if *incl { "<=" } else { "<" },
                        self.bind(v.clone())
                    ));
                }
                if parts.is_empty() {
                    parts.push("value IS NOT NULL".into());
                }
                parts.join(" AND ")
            }
            Cond::Instr(s, lower) => {
                let p = self.bind(SqlValue::Text(s.clone()));
                if *lower {
                    format!("value >= '' AND instr(lower(value), {p}) > 0")
                } else {
                    format!("value >= '' AND instr(value, {p}) > 0")
                }
            }
            Cond::NotNull => "value IS NOT NULL".into(),
        }
    }

    /// `SELECT eid ...` of the candidates of a plan over every layer of the view.
    fn gen(&mut self, view: &View, plan: &Plan) -> String {
        match plan {
            Plan::Term(key, cond) => {
                let k = self.bind(SqlValue::Text(key.clone()));
                let c = self.cond(cond);
                let mut arms = Vec::new();
                for src in &view.sources {
                    for &(lo, hi) in &src.intervals {
                        let bid = src.bid;
                        let seq = if lo <= 1 && hi == OPEN {
                            String::new()
                        } else {
                            format!(" AND +seq BETWEEN {lo} AND {hi}")
                        };
                        arms.push(format!(
                            "SELECT eid FROM value_current INDEXED BY value_current_value \
                             WHERE bid = {bid} AND key = {k} AND {c}{seq}"
                        ));
                        if hi != OPEN {
                            arms.push(format!(
                                "SELECT eid FROM value_history INDEXED BY value_history_value \
                                 WHERE bid = {bid} AND key = {k} AND {c} \
                                 AND seq BETWEEN {lo} AND {hi} AND superseded_by > {hi}"
                            ));
                        }
                    }
                }
                if arms.is_empty() {
                    arms.push("SELECT eid FROM entities WHERE 0".into());
                }
                arms.join(" UNION ")
            }
            // The first (smallest) part gives the rows; the others are
            // probed for each of them. `knobs::intersect_sql` keeps the
            // older `INTERSECT` of every part, for comparison.
            Plan::All(parts) if knobs::intersect_sql() => parts
                .iter()
                .map(|p| format!("SELECT eid FROM ({})", self.gen(view, p)))
                .collect::<Vec<_>>()
                .join(" INTERSECT "),
            Plan::All(parts) => {
                let first = self.gen(view, &parts[0]);
                let rest = parts[1..]
                    .iter()
                    .map(|p| self.probe(view, p, "a.eid"))
                    .collect::<Vec<_>>()
                    .join(" AND ");
                format!("SELECT a.eid FROM ({first}) AS a WHERE {rest}")
            }
            Plan::Any(parts) if parts.is_empty() => "SELECT eid FROM entities WHERE 0".into(),
            Plan::Any(parts) => parts
                .iter()
                .map(|p| format!("SELECT eid FROM ({})", self.gen(view, p)))
                .collect::<Vec<_>>()
                .join(" UNION "),
            Plan::Everything => "SELECT eid FROM entities".into(),
        }
    }

    /// A condition on the entity column `col`: it is a candidate of the plan
    /// (per-entity probes).
    fn probe(&mut self, view: &View, plan: &Plan, col: &str) -> String {
        match plan {
            Plan::Term(key, cond) => {
                let k = self.bind(SqlValue::Text(key.clone()));
                let c = self.cond(cond);
                let mut arms = Vec::new();
                for src in &view.sources {
                    for &(lo, hi) in &src.intervals {
                        let bid = src.bid;
                        let seq = if lo <= 1 && hi == OPEN {
                            String::new()
                        } else {
                            format!(" AND seq BETWEEN {lo} AND {hi}")
                        };
                        arms.push(format!(
                            "EXISTS (SELECT 1 FROM value_current WHERE bid = {bid} \
                             AND eid = {col} AND key = {k} AND {c}{seq})"
                        ));
                        if hi != OPEN {
                            arms.push(format!(
                                "EXISTS (SELECT 1 FROM value_history INDEXED BY value_history_target \
                                 WHERE bid = {bid} AND eid = {col} AND key = {k} \
                                 AND seq BETWEEN {lo} AND {hi} AND superseded_by > {hi} AND {c})"
                            ));
                        }
                    }
                }
                if arms.is_empty() {
                    "0".into()
                } else {
                    format!("({})", arms.join(" OR "))
                }
            }
            Plan::All(parts) => format!(
                "({})",
                parts
                    .iter()
                    .map(|p| self.probe(view, p, col))
                    .collect::<Vec<_>>()
                    .join(" AND ")
            ),
            Plan::Any(parts) if parts.is_empty() => "0".into(),
            Plan::Any(parts) => format!(
                "({})",
                parts
                    .iter()
                    .map(|p| self.probe(view, p, col))
                    .collect::<Vec<_>>()
                    .join(" OR ")
            ),
            Plan::Everything => "1".into(),
        }
    }
}

/// Where the next chunk starts.
enum After {
    Start,
    Id(String),
    Eid(i64),
}

/// Whether the view reads no history (no frozen range in any layer).
fn no_history(view: &View) -> bool {
    view.sources
        .iter()
        .all(|src| src.intervals.iter().all(|&(_, hi)| hi == OPEN))
}

/// Estimates look at this many entities (spread by a hash).
const SAMPLE: i64 = 2048;

/// [`SAMPLE`] distinct entity numbers in `1..=total`, the same on every call.
fn sample_eids(total: i64) -> Vec<i64> {
    let mut out: Vec<i64> = (0..SAMPLE as u64)
        .map(|j| {
            let mut z = j.wrapping_add(0x9e37_79b9_7f4a_7c15);
            z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
            z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
            1 + ((z ^ (z >> 31)) % total.max(1) as u64) as i64
        })
        .collect();
    out.sort_unstable();
    out.dedup();
    out
}

/// `And` terms with fewer candidates than this are used alone.
const AND_COUNT_CAP: i64 = 2048;

impl Store {
    /// A page of the entities of `branch` whose final values match `filter`.
    /// See [`Filter`] for the semantics and [`FindOptions`] for paging.
    /// Hides entities whose type is null or absent, unless `include_archived`.
    pub fn find(&self, branch: BranchId, filter: &Filter, options: FindOptions) -> Result<Find> {
        validate(filter)?;
        if options.limit == 0 {
            return Err(Error::Invalid("limit must be at least 1".into()));
        }
        let view = self.branch_view(branch)?;
        let cap = self.dense_cap(options.limit)?;
        match &options.order {
            FindOrder::ValueAsc(k) => {
                return self.find_by_value(&view, filter, &options, k, false, cap)
            }
            FindOrder::ValueDesc(k) => {
                return self.find_by_value(&view, filter, &options, k, true, cap)
            }
            _ => {}
        }
        let mut plan = plan_of(filter);
        if plan.is_none() && !options.include_archived {
            plan = Some(Plan::Term(TYPE_KEY.into(), Cond::NotNull));
        }
        let plan = match plan {
            // The implicit type term matches almost every entity: reading
            // the dictionary in page order and checking the type after the
            // roll-up is cheaper than probing the type of every layer.
            Some(Plan::Term(k, Cond::NotNull)) if k == TYPE_KEY && plan_of(filter).is_none() => {
                Plan::Everything
            }
            Some(p) => self.refine(&view, p)?,
            None => Plan::Everything,
        };

        // Which keys the check needs.
        let mut keys = BTreeSet::new();
        let all_keys = options.values || !keys_of(filter, &mut keys);
        if !options.include_archived {
            keys.insert(TYPE_KEY.to_owned());
        }
        let key = (!all_keys && keys.len() == 1)
            .then(|| keys.iter().next().cloned())
            .flatten();
        let one_key = key.is_some();

        // Sparse candidates are sorted in SQL. Dense ones are found by reading
        // the dictionary in page order: with a probe of each entity when the
        // view reads no history (the probes are primary key lookups), else
        // by checking every entity after the roll-up (measured faster than
        // probes that must also search history, BENCHMARKS.md).
        let (plan, probe) = match (&plan, knobs::probe()) {
            (Plan::Everything, _) => (plan, false),
            (_, Some(forced)) => (plan, forced && matches!(options.order, FindOrder::EntityId)),
            (_, None) if self.count(&view, &plan, cap)? < cap => (plan, false),
            (_, None)
                if matches!(options.order, FindOrder::EntityId)
                    && (knobs::dense() == 1 || (knobs::dense() == 0 && no_history(&view))) =>
            {
                (plan, true)
            }
            (_, None) => (Plan::Everything, false),
        };

        let mut after = match (&options.cursor, &options.order) {
            (None, _) => After::Start,
            (Some(c), FindOrder::EntityId) => After::Id(c.clone()),
            (Some(c), _) => match crate::store::lookup_entity(&self.conn, c)? {
                Some(eid) => After::Eid(eid),
                None => return Err(Error::Invalid(format!("unknown cursor entity {c:?}"))),
            },
        };
        let mut rows = Vec::new();
        let mut scanned = 0usize;
        loop {
            let want = (options.limit - rows.len()).clamp(64, 2048);
            let chunk = want.min(options.scan_limit.saturating_sub(scanned).max(1));
            let cands = self.candidates(&view, &plan, &options.order, probe, &after, chunk)?;
            if cands.is_empty() {
                return Ok(Find {
                    rows,
                    next: None,
                    scanned,
                });
            }
            scanned += cands.len();
            let eids: Vec<i64> = cands.iter().map(|c| c.0).collect();
            let mut vals: HashMap<i64, BTreeMap<String, Value>> = HashMap::new();
            let mut seen: HashSet<i64> = HashSet::new();
            let rolled = if all_keys || one_key {
                resolve_values(&self.conn, &view, &eids, key.as_deref())?
            } else {
                let ks: Vec<&str> = keys.iter().map(String::as_str).collect();
                resolve_values_of(&self.conn, &view, &eids, &ks)?
            };
            for ((eid, k), c) in rolled {
                seen.insert(eid);
                if !c.data.is_null() {
                    vals.entry(eid).or_default().insert(k, c.data);
                }
            }
            // With the whole dictionary as candidates, an entity must exist in
            // the view; an entity with values in the view does.
            let exists: HashSet<i64> =
                if matches!(plan, Plan::Everything) && options.include_archived {
                    let missing: Vec<i64> = eids
                        .iter()
                        .copied()
                        .filter(|e| !all_keys || !seen.contains(e))
                        .collect();
                    let mut found = any_event(&self.conn, &view, &missing)?;
                    found.extend(seen.iter().copied());
                    found
                } else {
                    eids.iter().copied().collect()
                };
            let empty = BTreeMap::new();
            for (eid, id) in &cands {
                if !exists.contains(eid) {
                    continue;
                }
                let v = vals.get(eid).unwrap_or(&empty);
                if !options.include_archived && !v.contains_key(TYPE_KEY) {
                    continue;
                }
                if !matches(filter, id, v) {
                    continue;
                }
                rows.push(FindRow {
                    id: id.clone(),
                    values: if options.values {
                        v.clone()
                    } else {
                        BTreeMap::new()
                    },
                });
                if rows.len() == options.limit {
                    return Ok(Find {
                        rows,
                        next: Some(id.clone()),
                        scanned,
                    });
                }
            }
            let (last_eid, last_id) = cands.last().cloned().expect("not empty");
            if cands.len() < chunk {
                return Ok(Find {
                    rows,
                    next: None,
                    scanned,
                });
            }
            if scanned >= options.scan_limit {
                return Ok(Find {
                    rows,
                    next: Some(last_id),
                    scanned,
                });
            }
            after = match options.order {
                FindOrder::EntityId => After::Id(last_id),
                _ => After::Eid(last_eid),
            };
        }
    }

    /// Pick the terms of each `And`: estimate the candidates of each term
    /// (up to [`AND_COUNT_CAP`]) in rank order (`Eq` first) and use the first
    /// small one alone; the other terms are checked after the roll-up. When
    /// every term is large, intersect the two first in SQL. (Measured: the
    /// intersection beats the smallest large term alone, BENCHMARKS.md.)
    fn refine(&self, view: &View, plan: Plan) -> Result<Plan> {
        Ok(match plan {
            Plan::All(parts) => {
                if !knobs::intersect() {
                    let first = parts.into_iter().next().expect("two or more");
                    return self.refine(view, first);
                }
                let mut large = Vec::new();
                for p in parts {
                    let n = self.count(view, &p, AND_COUNT_CAP)?;
                    if n < AND_COUNT_CAP {
                        return self.refine(view, p);
                    }
                    large.push((n, p));
                }
                if knobs::intersect_sql() {
                    large.truncate(2);
                } else {
                    // The smallest part gives the rows, the others are probed.
                    large.sort_by_key(|(n, _)| *n);
                }
                Plan::All(large.into_iter().map(|(_, p)| p).collect())
            }
            Plan::Any(parts) => Plan::Any(
                parts
                    .into_iter()
                    .map(|p| self.refine(view, p))
                    .collect::<Result<_>>()?,
            ),
            other => other,
        })
    }

    /// An estimate of the number of candidates of a plan, up to `cap`, for
    /// plan choices only. A term counts the matching current rows of each
    /// layer separately (each count streams from the value index and stops
    /// at `cap`) and takes the largest; `And` takes the smallest part, `Or`
    /// the sum. History rows are not counted.
    fn count(&self, view: &View, plan: &Plan, cap: i64) -> Result<i64> {
        Ok(match plan {
            Plan::Term(key, cond) if !knobs::exact_count() => {
                // Sample: the matching current rows of [`SAMPLE`] entity
                // numbers spread by a hash over the store, scaled to all
                // entities. Each sample is a primary key lookup (`+key` keeps
                // SQLite off the value index), so the cost does not depend on
                // the selectivity of the condition.
                let total = self.max_eid()?;
                if total <= SAMPLE * 4 && !knobs::always_sample() {
                    return self.count_exact(view, key, cond, cap);
                }
                let sample = sample_eids(total);
                let json = serde_json::to_string(&sample).expect("ints serialise");
                let mut most = 0;
                for src in &view.sources {
                    let mut sql = Sql::default();
                    let s = sql.bind(SqlValue::Text(json.clone()));
                    let k = sql.bind(SqlValue::Text(key.clone()));
                    let c = sql.cond(cond);
                    let text = format!(
                        "SELECT count(*) FROM value_current WHERE bid = {} \
                         AND eid IN (SELECT value FROM json_each({s})) AND +key = {k} AND {c}",
                        src.bid
                    );
                    let hits: i64 = self
                        .conn
                        .prepare_cached(&text)?
                        .query_row(rusqlite::params_from_iter(sql.params.iter()), |r| r.get(0))?;
                    most = most.max(hits * total / sample.len().max(1) as i64);
                }
                most.min(cap)
            }
            Plan::Term(key, cond) => self.count_exact(view, key, cond, cap)?,
            Plan::All(parts) => {
                let mut least = cap;
                for p in parts {
                    least = least.min(self.count(view, p, cap)?);
                }
                least
            }
            Plan::Any(parts) => {
                let mut sum = 0;
                for p in parts {
                    sum = (sum + self.count(view, p, cap)?).min(cap);
                }
                sum
            }
            Plan::Everything => cap,
        })
    }

    /// The largest entity number (about the number of entities).
    fn max_eid(&self) -> Result<i64> {
        Ok(self
            .conn
            .prepare_cached("SELECT coalesce(max(eid), 0) FROM entities")?
            .query_row([], |r| r.get(0))?)
    }

    /// The matching current rows of each layer, counted up to `cap`
    /// (streams from the value index), the largest of the layers.
    fn count_exact(&self, view: &View, key: &str, cond: &Cond, cap: i64) -> Result<i64> {
        let mut most = 0;
        for src in &view.sources {
            let mut sql = Sql::default();
            let k = sql.bind(SqlValue::Text(key.to_owned()));
            let c = sql.cond(cond);
            let text = format!(
                "SELECT count(*) FROM (SELECT 1 FROM value_current \
                 INDEXED BY value_current_value \
                 WHERE bid = {} AND key = {k} AND {c} LIMIT {cap})",
                src.bid
            );
            let n: i64 = self
                .conn
                .prepare_cached(&text)?
                .query_row(rusqlite::params_from_iter(sql.params.iter()), |r| r.get(0))?;
            most = most.max(n);
        }
        Ok(most)
    }

    /// The candidate count above which a plan is dense: `sqrt(limit × entities)`.
    /// Sorting the candidates costs about their number on each page; reading
    /// the dictionary in order costs about `limit / density`. The two meet
    /// at this count (10 000 for 1000 rows of 100 000 entities).
    fn dense_cap(&self, limit: usize) -> Result<i64> {
        let total = self.max_eid()?;
        Ok(((limit as f64 * total as f64).sqrt() as i64).max(1))
    }

    /// The next `chunk` candidates after `after`, in page order.
    fn candidates(
        &self,
        view: &View,
        plan: &Plan,
        order: &FindOrder,
        probe: bool,
        after: &After,
        chunk: usize,
    ) -> Result<Vec<(i64, String)>> {
        let mut sql = Sql::default();
        let text = match (plan, order) {
            (Plan::Everything, FindOrder::EntityId) => {
                let c = self.after_cond(&mut sql, after, "ext_id", "eid");
                format!("SELECT eid, ext_id FROM entities WHERE {c} ORDER BY ext_id LIMIT {chunk}")
            }
            (Plan::Everything, _) => {
                let c = self.after_cond(&mut sql, after, "ext_id", "eid");
                format!("SELECT eid, ext_id FROM entities WHERE {c} ORDER BY eid LIMIT {chunk}")
            }
            (_, FindOrder::EntityId) if probe => {
                let p = sql.probe(view, plan, "e.eid");
                let c = self.after_cond(&mut sql, after, "e.ext_id", "e.eid");
                format!(
                    "SELECT e.eid, e.ext_id FROM entities e INDEXED BY sqlite_autoindex_entities_1 \
                     WHERE {c} AND {p} ORDER BY e.ext_id LIMIT {chunk}"
                )
            }
            (_, FindOrder::EntityId) => {
                let g = sql.gen(view, plan);
                let c = self.after_cond(&mut sql, after, "e.ext_id", "e.eid");
                format!(
                    "SELECT e.eid, e.ext_id FROM ({g}) AS g JOIN entities e ON e.eid = g.eid \
                     WHERE {c} ORDER BY e.ext_id LIMIT {chunk}"
                )
            }
            (_, _) => {
                let g = sql.gen(view, plan);
                let c = self.after_cond(&mut sql, after, "e.ext_id", "g.eid");
                format!(
                    "SELECT g.eid, e.ext_id FROM ({g}) AS g JOIN entities e ON e.eid = g.eid \
                     WHERE {c} ORDER BY g.eid LIMIT {chunk}"
                )
            }
        };
        let mut stmt = self.conn.prepare_cached(&text)?;
        let rows = stmt
            .query_map(rusqlite::params_from_iter(sql.params.iter()), |r| {
                Ok((r.get(0)?, r.get(1)?))
            })?
            .collect::<rusqlite::Result<Vec<(i64, String)>>>()?;
        Ok(rows)
    }

    fn after_cond(&self, sql: &mut Sql, after: &After, id_col: &str, eid_col: &str) -> String {
        match after {
            After::Start => "1".into(),
            After::Id(id) => format!("{id_col} > {}", sql.bind(SqlValue::Text(id.clone()))),
            After::Eid(e) => format!("{eid_col} > {e}"),
        }
    }
}

/// SQLite's order of stored values: numbers (by value) before text (by bytes).
pub(crate) fn cmp_sql(a: &SqlValue, b: &SqlValue) -> Ordering {
    use SqlValue::{Integer, Real, Text};
    let class = |v: &SqlValue| match v {
        SqlValue::Null => 0,
        Integer(_) | Real(_) => 1,
        Text(_) => 2,
        SqlValue::Blob(_) => 3,
    };
    match (a, b) {
        (Integer(x), Integer(y)) => x.cmp(y),
        (Real(x), Real(y)) => x.total_cmp(y),
        (Integer(x), Real(y)) => cmp_int_float(*x, *y),
        (Real(x), Integer(y)) => cmp_int_float(*y, *x).reverse(),
        (Text(x), Text(y)) => x.as_bytes().cmp(y.as_bytes()),
        _ => class(a).cmp(&class(b)),
    }
}

/// The opaque cursor of a value order: `[tag, value, id]`.
fn value_cursor(v: &SqlValue, id: &str) -> String {
    let (tag, val) = match v {
        SqlValue::Integer(i) => ("i", serde_json::Value::from(*i)),
        SqlValue::Real(f) => (
            "f",
            serde_json::Value::from(format!("{:016x}", f.to_bits())),
        ),
        SqlValue::Text(t) => ("t", serde_json::Value::from(t.as_str())),
        _ => ("n", serde_json::Value::Null),
    };
    serde_json::json!([tag, val, id]).to_string()
}

fn parse_value_cursor(c: &str) -> Result<(SqlValue, String)> {
    let bad = || Error::Invalid(format!("bad value-order cursor {c:?}"));
    let j: serde_json::Value = serde_json::from_str(c).map_err(|_| bad())?;
    let a = j.as_array().filter(|a| a.len() == 3).ok_or_else(bad)?;
    let id = a[2].as_str().ok_or_else(bad)?.to_owned();
    let v = match a[0].as_str().ok_or_else(bad)? {
        "i" => SqlValue::Integer(a[1].as_i64().ok_or_else(bad)?),
        "f" => SqlValue::Real(f64::from_bits(
            u64::from_str_radix(a[1].as_str().ok_or_else(bad)?, 16).map_err(|_| bad())?,
        )),
        "t" => SqlValue::Text(a[1].as_str().ok_or_else(bad)?.to_owned()),
        _ => return Err(bad()),
    };
    Ok((v, id))
}

impl Store {
    /// `find` in the order of the final value of `key` (see
    /// [`FindOrder::ValueAsc`]). Sparse filters: all candidates are rolled
    /// up and sorted on each page. Dense ones: the rows of `key` are read
    /// in `(value, eid)` order from every layer (the value indexes give that
    /// order; SQLite merges the layers), and an entity is emitted at the row
    /// that holds its final value.
    fn find_by_value(
        &self,
        view: &View,
        filter: &Filter,
        options: &FindOptions,
        key: &str,
        desc: bool,
        cap: i64,
    ) -> Result<Find> {
        let order_term = Filter::NotNull(key.to_owned());
        let full = Filter::And(vec![order_term, filter.clone()]);
        let mut keys = BTreeSet::new();
        let all_keys = options.values || !keys_of(&full, &mut keys);
        if !options.include_archived {
            keys.insert(TYPE_KEY.to_owned());
        }
        let keys: Vec<&str> = keys.iter().map(String::as_str).collect();
        let mut after: Option<(SqlValue, i64)> = match &options.cursor {
            None => None,
            Some(c) => {
                let (v, id) = parse_value_cursor(c)?;
                match crate::store::lookup_entity(&self.conn, &id)? {
                    Some(eid) => Some((v, eid)),
                    None => return Err(Error::Invalid(format!("unknown cursor entity {id:?}"))),
                }
            }
        };
        let before = |a: &(SqlValue, i64), b: &(SqlValue, i64)| {
            let o = cmp_sql(&a.0, &b.0).then(a.1.cmp(&b.1));
            if desc {
                o.reverse()
            } else {
                o
            }
        };
        // Roll up a batch; the final values of the entities that match.
        let check = |eids: &[i64]| -> Result<HashMap<i64, BTreeMap<String, Value>>> {
            let rolled = if all_keys {
                resolve_values(&self.conn, view, eids, None)?
            } else {
                resolve_values_of(&self.conn, view, eids, &keys)?
            };
            let mut vals: HashMap<i64, BTreeMap<String, Value>> = HashMap::new();
            for ((eid, k), c) in rolled {
                if !c.data.is_null() {
                    vals.entry(eid).or_default().insert(k, c.data);
                }
            }
            Ok(vals)
        };
        let ext_ids = |eids: &[i64]| -> Result<HashMap<i64, String>> {
            let mut stmt = self.conn.prepare_cached(
                "SELECT eid, ext_id FROM entities WHERE eid IN (SELECT value FROM json_each(?1))",
            )?;
            let json = serde_json::to_string(eids).expect("ints serialise");
            let rows = stmt
                .query_map([json], |r| Ok((r.get(0)?, r.get(1)?)))?
                .collect::<rusqlite::Result<HashMap<i64, String>>>()?;
            Ok(rows)
        };
        let keep = |id: &str, v: &BTreeMap<String, Value>| {
            (options.include_archived || v.contains_key(TYPE_KEY)) && matches(&full, id, v)
        };
        let row = |id: &str, v: BTreeMap<String, Value>| FindRow {
            id: id.to_owned(),
            values: if options.values { v } else { BTreeMap::new() },
        };

        let plan = match plan_of(filter) {
            Some(p) => Some(self.refine(view, p)?),
            None => None,
        };
        let sparse = match (&plan, knobs::probe()) {
            (None, _) => false,
            (Some(_), Some(forced)) => !forced,
            (Some(p), None) => self.count(view, p, cap)? < cap,
        };
        if sparse {
            let plan = plan.expect("sparse needs a plan");
            let mut sql = Sql::default();
            let g = sql.gen(view, &plan);
            let mut stmt = self
                .conn
                .prepare_cached(&format!("SELECT eid FROM ({g})"))?;
            let eids = stmt
                .query_map(rusqlite::params_from_iter(sql.params.iter()), |r| r.get(0))?
                .collect::<rusqlite::Result<Vec<i64>>>()?;
            let mut found: Vec<((SqlValue, i64), BTreeMap<String, Value>)> = Vec::new();
            for batch in eids.chunks(2048) {
                let ids = ext_ids(batch)?;
                for (eid, v) in check(batch)? {
                    let id = &ids[&eid];
                    if !keep(id, &v) {
                        continue;
                    }
                    let pos = (sql_value(&v[key]), eid);
                    if after.as_ref().is_some_and(|a| before(&pos, a).is_le()) {
                        continue;
                    }
                    found.push((pos, v));
                }
            }
            found.sort_by(|a, b| before(&a.0, &b.0));
            let more = found.len() > options.limit;
            found.truncate(options.limit);
            let ids = ext_ids(&found.iter().map(|f| f.0 .1).collect::<Vec<_>>())?;
            let next = more.then(|| {
                let (v, eid) = &found.last().expect("more than limit").0;
                value_cursor(v, &ids[eid])
            });
            let scanned = eids.len();
            let rows = found
                .into_iter()
                .map(|((_, eid), v)| row(&ids[&eid], v))
                .collect();
            return Ok(Find {
                rows,
                next,
                scanned,
            });
        }

        // Dense: merge the rows of `key` of every layer in value order.
        let (cmp, dir) = if desc { ("<", "DESC") } else { (">", "ASC") };
        let mut rows = Vec::new();
        let mut scanned = 0usize;
        loop {
            let want = (options.limit - rows.len()).clamp(64, 2048);
            let chunk = want.min(options.scan_limit.saturating_sub(scanned).max(1));
            let mut sql = Sql::default();
            let k = sql.bind(SqlValue::Text(key.to_owned()));
            let pos = match &after {
                None => "value IS NOT NULL".to_owned(),
                Some((v, eid)) => {
                    let v = sql.bind(v.clone());
                    format!("(value, eid) {cmp} ({v}, {eid})")
                }
            };
            let mut arms = Vec::new();
            for src in &view.sources {
                for &(lo, hi) in &src.intervals {
                    let bid = src.bid;
                    let seq = if lo <= 1 && hi == OPEN {
                        String::new()
                    } else {
                        format!(" AND +seq BETWEEN {lo} AND {hi}")
                    };
                    arms.push(format!(
                        "SELECT value, eid FROM value_current INDEXED BY value_current_value \
                         WHERE bid = {bid} AND key = {k} AND {pos}{seq}"
                    ));
                    if hi != OPEN {
                        arms.push(format!(
                            "SELECT value, eid FROM value_history INDEXED BY value_history_value \
                             WHERE bid = {bid} AND key = {k} AND {pos} \
                             AND seq BETWEEN {lo} AND {hi} AND superseded_by > {hi}"
                        ));
                    }
                }
            }
            if arms.is_empty() {
                return Ok(Find {
                    rows,
                    next: None,
                    scanned,
                });
            }
            let text = format!(
                "{} ORDER BY 1 {dir}, 2 {dir} LIMIT {chunk}",
                arms.join(" UNION ")
            );
            let mut stmt = self.conn.prepare_cached(&text)?;
            let cands = stmt
                .query_map(rusqlite::params_from_iter(sql.params.iter()), |r| {
                    Ok((r.get::<_, SqlValue>(0)?, r.get::<_, i64>(1)?))
                })?
                .collect::<rusqlite::Result<Vec<(SqlValue, i64)>>>()?;
            if cands.is_empty() {
                return Ok(Find {
                    rows,
                    next: None,
                    scanned,
                });
            }
            scanned += cands.len();
            let mut eids: Vec<i64> = cands.iter().map(|c| c.1).collect();
            eids.sort_unstable();
            eids.dedup();
            let mut vals = check(&eids)?;
            let ids = ext_ids(&eids)?;
            for (v, eid) in &cands {
                let Some(fv) = vals.get(eid) else { continue };
                let Some(final_v) = fv.get(key) else { continue };
                // Emit the entity at the row of its final value only.
                if cmp_sql(&sql_value(final_v), v) != Ordering::Equal {
                    continue;
                }
                let id = &ids[eid];
                if !keep(id, fv) {
                    continue;
                }
                let fv = vals.remove(eid).expect("present");
                rows.push(row(id, fv));
                if rows.len() == options.limit {
                    return Ok(Find {
                        rows,
                        next: Some(value_cursor(v, id)),
                        scanned,
                    });
                }
            }
            let (last_v, last_eid) = cands.last().cloned().expect("not empty");
            if cands.len() < chunk {
                return Ok(Find {
                    rows,
                    next: None,
                    scanned,
                });
            }
            if scanned >= options.scan_limit {
                let id = ids[&last_eid].clone();
                return Ok(Find {
                    rows,
                    next: Some(value_cursor(&last_v, &id)),
                    scanned,
                });
            }
            after = Some((last_v, last_eid));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn int_float_comparison_is_exact() {
        assert_eq!(cmp_int_float(1, 1.0), Ordering::Equal);
        assert_eq!(cmp_int_float(1, 1.5), Ordering::Less);
        assert_eq!(cmp_int_float(-1, -1.5), Ordering::Greater);
        assert_eq!(
            cmp_int_float(i64::MAX, 9.223372036854776e18),
            Ordering::Less
        );
        assert_eq!(
            cmp_int_float((1 << 53) + 1, (1u64 << 53) as f64),
            Ordering::Greater
        );
        assert_eq!(
            cmp_int_float(i64::MIN, -9.223372036854776e18),
            Ordering::Equal
        );
    }

    #[test]
    fn prefix_upper_bounds_every_extension() {
        assert_eq!(prefix_upper("ab").as_deref(), Some("ac"));
        assert_eq!(prefix_upper(""), None);
        assert_eq!(prefix_upper("a\u{10FFFF}").as_deref(), Some("b"));
        assert_eq!(prefix_upper("\u{D7FF}").as_deref(), Some("\u{E000}"));
    }
}
