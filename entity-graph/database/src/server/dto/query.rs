//! Wire types for the query endpoints: `find`, `walk` and `prove`. Option
//! structs expose every public field, with the library default when a field
//! is absent.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use super::{link_groups_to_dto, value_from_json, value_map_to_json, DirectionDto, LinkDto};
use crate::types::Direction;
use crate::walk::Follow;
use crate::{
    Filter, Find, FindOptions, FindOrder, Proof, Prove, ProveOptions, ProveOutcome, ProveStats,
    ProveStrategy, ReadAhead, Walk, WalkCache, WalkNode, WalkOptions,
};

// --- find -----------------------------------------------------------------

/// A filter on an entity's final values. Every [`Filter`] variant except
/// `Custom` (a closure, which JSON cannot carry).
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum FilterDto {
    Eq {
        key: String,
        value: serde_json::Value,
    },
    Ne {
        key: String,
        value: serde_json::Value,
    },
    Lt {
        key: String,
        value: serde_json::Value,
    },
    Le {
        key: String,
        value: serde_json::Value,
    },
    Gt {
        key: String,
        value: serde_json::Value,
    },
    Ge {
        key: String,
        value: serde_json::Value,
    },
    Contains {
        key: String,
        value: String,
    },
    ContainsIgnoreCase {
        key: String,
        value: String,
    },
    Prefix {
        key: String,
        value: String,
    },
    In {
        key: String,
        values: Vec<serde_json::Value>,
    },
    IsNull {
        key: String,
    },
    NotNull {
        key: String,
    },
    And(Vec<FilterDto>),
    Or(Vec<FilterDto>),
    Not(Box<FilterDto>),
}

impl From<FilterDto> for Filter {
    fn from(f: FilterDto) -> Self {
        let v = value_from_json;
        match f {
            FilterDto::Eq { key, value } => Filter::Eq(key, v(value)),
            FilterDto::Ne { key, value } => Filter::Ne(key, v(value)),
            FilterDto::Lt { key, value } => Filter::Lt(key, v(value)),
            FilterDto::Le { key, value } => Filter::Le(key, v(value)),
            FilterDto::Gt { key, value } => Filter::Gt(key, v(value)),
            FilterDto::Ge { key, value } => Filter::Ge(key, v(value)),
            FilterDto::Contains { key, value } => Filter::Contains(key, value),
            FilterDto::ContainsIgnoreCase { key, value } => Filter::ContainsIgnoreCase(key, value),
            FilterDto::Prefix { key, value } => Filter::Prefix(key, value),
            FilterDto::In { key, values } => Filter::In(key, values.into_iter().map(v).collect()),
            FilterDto::IsNull { key } => Filter::IsNull(key),
            FilterDto::NotNull { key } => Filter::NotNull(key),
            FilterDto::And(fs) => Filter::And(fs.into_iter().map(Filter::from).collect()),
            FilterDto::Or(fs) => Filter::Or(fs.into_iter().map(Filter::from).collect()),
            FilterDto::Not(f) => Filter::Not(Box::new(Filter::from(*f))),
        }
    }
}

/// The find order: `"entity_id"` (default), `"insertion"`,
/// `{"value_asc": key}` or `{"value_desc": key}`.
#[derive(Clone, Debug, Default, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum FindOrderDto {
    #[default]
    EntityId,
    Insertion,
    ValueAsc(String),
    ValueDesc(String),
}

impl From<FindOrderDto> for FindOrder {
    fn from(o: FindOrderDto) -> Self {
        match o {
            FindOrderDto::EntityId => FindOrder::EntityId,
            FindOrderDto::Insertion => FindOrder::Insertion,
            FindOrderDto::ValueAsc(k) => FindOrder::ValueAsc(k),
            FindOrderDto::ValueDesc(k) => FindOrder::ValueDesc(k),
        }
    }
}

/// Find options; each field falls back to the library default.
#[derive(Clone, Debug, Default, Deserialize)]
pub(crate) struct FindOptionsDto {
    #[serde(default)]
    pub limit: Option<usize>,
    #[serde(default)]
    pub cursor: Option<String>,
    #[serde(default)]
    pub order: Option<FindOrderDto>,
    #[serde(default)]
    pub values: Option<bool>,
    #[serde(default)]
    pub include_archived: Option<bool>,
    #[serde(default)]
    pub scan_limit: Option<usize>,
}

impl From<FindOptionsDto> for FindOptions {
    fn from(d: FindOptionsDto) -> Self {
        let def = FindOptions::default();
        FindOptions {
            limit: d.limit.unwrap_or(def.limit),
            cursor: d.cursor,
            order: d.order.map(Into::into).unwrap_or(def.order),
            values: d.values.unwrap_or(def.values),
            include_archived: d.include_archived.unwrap_or(def.include_archived),
            scan_limit: d.scan_limit.unwrap_or(def.scan_limit),
        }
    }
}

/// One matching entity: `{id, values}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct FindRowDto {
    pub id: String,
    pub values: serde_json::Value,
}

/// A page of a find: `{rows, next, scanned}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct FindDto {
    pub rows: Vec<FindRowDto>,
    pub next: Option<String>,
    pub scanned: usize,
}

impl From<Find> for FindDto {
    fn from(f: Find) -> Self {
        Self {
            rows: f
                .rows
                .into_iter()
                .map(|r| FindRowDto {
                    id: r.id,
                    values: value_map_to_json(&r.values),
                })
                .collect(),
            next: f.next,
            scanned: f.scanned,
        }
    }
}

// --- walk -----------------------------------------------------------------

/// A follow map: `{ type: [[link_type, direction], ...] }`.
#[derive(Clone, Debug, Default, Deserialize)]
pub(crate) struct FollowDto(pub HashMap<String, Vec<(String, DirectionDto)>>);

impl From<FollowDto> for Follow {
    fn from(f: FollowDto) -> Self {
        f.0.into_iter()
            .map(|(ty, pairs)| {
                let pairs: Vec<(String, Direction)> =
                    pairs.into_iter().map(|(t, d)| (t, d.into())).collect();
                (ty, pairs)
            })
            .collect()
    }
}

/// Read-ahead: `{depth?, width?}`.
#[derive(Clone, Copy, Debug, Default, Deserialize)]
pub(crate) struct ReadAheadDto {
    #[serde(default)]
    pub depth: Option<usize>,
    #[serde(default)]
    pub width: Option<usize>,
}

impl From<ReadAheadDto> for ReadAhead {
    fn from(d: ReadAheadDto) -> Self {
        let def = ReadAhead::default();
        ReadAhead {
            depth: d.depth.unwrap_or(def.depth),
            width: d.width.unwrap_or(def.width),
        }
    }
}

/// Walk options; each field falls back to the library default.
#[derive(Clone, Debug, Default, Deserialize)]
pub(crate) struct WalkOptionsDto {
    #[serde(default)]
    pub steps: Option<usize>,
    #[serde(default)]
    pub read_ahead: Option<ReadAheadDto>,
    #[serde(default)]
    pub return_cache: Option<bool>,
    #[serde(default)]
    pub visited: Option<Vec<String>>,
}

impl From<WalkOptionsDto> for WalkOptions {
    fn from(d: WalkOptionsDto) -> Self {
        let def = WalkOptions::default();
        WalkOptions {
            steps: d.steps.unwrap_or(def.steps),
            read_ahead: d.read_ahead.map(Into::into).unwrap_or(def.read_ahead),
            return_cache: d.return_cache.unwrap_or(def.return_cache),
            visited: d.visited.map(|v| v.into_iter().collect()),
        }
    }
}

/// One entity a walk knows of: `{id, values}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct WalkNodeDto {
    pub id: String,
    pub values: serde_json::Value,
}

impl From<WalkNode> for WalkNodeDto {
    fn from(n: WalkNode) -> Self {
        Self {
            id: n.id,
            values: value_map_to_json(&n.values),
        }
    }
}

/// One emitted row: `{path, node}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct WalkRowDto {
    pub path: Vec<String>,
    pub node: WalkNodeDto,
}

/// The walk cache: `{nodes, links}`. Only present when `return_cache` is set.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct WalkCacheDto {
    pub nodes: HashMap<String, WalkNodeDto>,
    pub links: HashMap<String, Vec<super::LinkGroupDto>>,
}

impl From<WalkCache> for WalkCacheDto {
    fn from(c: WalkCache) -> Self {
        Self {
            nodes: c.nodes.into_iter().map(|(id, n)| (id, n.into())).collect(),
            links: c
                .links
                .into_iter()
                .map(|(id, g)| (id, link_groups_to_dto(g)))
                .collect(),
        }
    }
}

/// A page of a walk: `{rows, next, steps, loads, cache?, visited?}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct WalkDto {
    pub rows: Vec<WalkRowDto>,
    pub next: Option<Vec<String>>,
    pub steps: usize,
    pub loads: usize,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub cache: Option<WalkCacheDto>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub visited: Option<Vec<String>>,
}

impl From<Walk> for WalkDto {
    fn from(w: Walk) -> Self {
        // A set has no order; sort the resumable visited set for a stable body.
        let visited = w.visited.map(|set| {
            let mut v: Vec<String> = set.into_iter().collect();
            v.sort();
            v
        });
        Self {
            rows: w
                .rows
                .into_iter()
                .map(|r| WalkRowDto {
                    path: r.path,
                    node: r.node.into(),
                })
                .collect(),
            next: w.next,
            steps: w.steps,
            loads: w.loads,
            cache: w.cache.map(Into::into),
            visited,
        }
    }
}

/// Evaluate an optional filter as a walk predicate on a node's id and values.
/// Absent is always true.
pub(crate) fn node_predicate(filter: Option<Filter>) -> impl FnMut(&[String], &WalkNode) -> bool {
    move |_path: &[String], node: &WalkNode| match &filter {
        Some(f) => crate::find::matches(f, &node.id, &node.values),
        None => true,
    }
}

// --- prove ----------------------------------------------------------------

/// The search strategy: `"backward"` (default), `"bidirectional"` or `"walk"`.
#[derive(Clone, Copy, Debug, Default, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ProveStrategyDto {
    #[default]
    Backward,
    Bidirectional,
    Walk,
}

impl From<ProveStrategyDto> for ProveStrategy {
    fn from(s: ProveStrategyDto) -> Self {
        match s {
            ProveStrategyDto::Backward => ProveStrategy::Backward,
            ProveStrategyDto::Bidirectional => ProveStrategy::Bidirectional,
            ProveStrategyDto::Walk => ProveStrategy::Walk,
        }
    }
}

/// Prove options; each field falls back to the library default.
#[derive(Clone, Debug, Default, Deserialize)]
pub(crate) struct ProveOptionsDto {
    #[serde(default)]
    pub max_depth: Option<usize>,
    #[serde(default)]
    pub max_nodes: Option<usize>,
    #[serde(default)]
    pub include_archived: bool,
    #[serde(default)]
    pub strategy: ProveStrategyDto,
}

impl From<ProveOptionsDto> for ProveOptions {
    fn from(d: ProveOptionsDto) -> Self {
        ProveOptions {
            max_depth: d.max_depth,
            max_nodes: d.max_nodes,
            include_archived: d.include_archived,
            strategy: d.strategy.into(),
        }
    }
}

/// A proof: `{path, links}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct ProofDto {
    pub path: Vec<String>,
    pub links: Vec<LinkDto>,
}

impl From<Proof> for ProofDto {
    fn from(p: Proof) -> Self {
        Self {
            path: p.path,
            links: p.links.into_iter().map(LinkDto::from).collect(),
        }
    }
}

/// The outcome: `{"proved": proof}`, `"no_path"` or `"unknown"`.
#[derive(Clone, Debug, Serialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum ProveOutcomeDto {
    Proved(ProofDto),
    NoPath,
    Unknown,
}

impl From<ProveOutcome> for ProveOutcomeDto {
    fn from(o: ProveOutcome) -> Self {
        match o {
            ProveOutcome::Proved(p) => ProveOutcomeDto::Proved(p.into()),
            ProveOutcome::NoPath => ProveOutcomeDto::NoPath,
            ProveOutcome::Unknown => ProveOutcomeDto::Unknown,
        }
    }
}

/// Prove stats: counts of the work done.
#[derive(Clone, Copy, Debug, Serialize)]
pub(crate) struct ProveStatsDto {
    pub nodes_visited: usize,
    pub link_reads: usize,
    pub value_reads: usize,
    pub sql_queries: usize,
    pub depth_reached: usize,
}

impl From<ProveStats> for ProveStatsDto {
    fn from(s: ProveStats) -> Self {
        Self {
            nodes_visited: s.nodes_visited,
            link_reads: s.link_reads,
            value_reads: s.value_reads,
            sql_queries: s.sql_queries,
            depth_reached: s.depth_reached,
        }
    }
}

/// The full prove result: `{outcome, stats}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct ProveDto {
    pub outcome: ProveOutcomeDto,
    pub stats: ProveStatsDto,
}

impl From<Prove> for ProveDto {
    fn from(p: Prove) -> Self {
        Self {
            outcome: p.outcome.into(),
            stats: p.stats.into(),
        }
    }
}
