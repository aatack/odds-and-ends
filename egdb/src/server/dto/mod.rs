//! Wire types for the HTTP API. These are separate from the library types:
//! the library keeps no serde derives. Each DTO converts to or from a library
//! type. See `API.md` for the JSON shapes.
//!
//! This module holds the core types (values, ops, branches, links). The query
//! types (`find`, `walk`, `prove`) are in [`query`].

pub(crate) mod query;

use std::collections::BTreeMap;

use serde::{Deserialize, Serialize};

use crate::error::{Error, Result};
use crate::read::LinkGroups;
use crate::types::{
    BranchId, BranchInfo, Direction, EventRef, Link, Op, ParentRef, SeqRange, SortOrder,
    UndoOutcome, Value, WriteBatch, WriteResult,
};

/// Parse a branch ID from its UUID string, or a 400-mapped error.
pub(crate) fn parse_branch(s: &str) -> Result<BranchId> {
    uuid::Uuid::parse_str(s)
        .map(BranchId)
        .map_err(|_| Error::Invalid(format!("bad branch id {s:?}")))
}

/// A library [`Value`] as JSON: null/bool/number/string/array/object.
pub(crate) fn value_to_json(v: &Value) -> serde_json::Value {
    use serde_json::Value as J;
    match v {
        Value::Null => J::Null,
        Value::Bool(b) => J::Bool(*b),
        Value::Int(i) => J::Number((*i).into()),
        Value::Float(f) => serde_json::Number::from_f64(*f)
            .map(J::Number)
            .unwrap_or(J::Null),
        Value::Text(s) => J::String(s.clone()),
        Value::Json(j) => j.clone(),
    }
}

/// JSON to a library [`Value`]. A JSON integer becomes `Int`; any other number
/// (for example `2.0`) becomes `Float`; arrays and objects become `Json`.
pub(crate) fn value_from_json(j: serde_json::Value) -> Value {
    use serde_json::Value as J;
    match j {
        J::Null => Value::Null,
        J::Bool(b) => Value::Bool(b),
        J::Number(n) => match n.as_i64() {
            Some(i) => Value::Int(i),
            None => Value::Float(n.as_f64().unwrap_or(0.0)),
        },
        J::String(s) => Value::Text(s),
        other @ (J::Array(_) | J::Object(_)) => Value::Json(other),
    }
}

/// A map of key to value as a JSON object.
pub(crate) fn value_map_to_json(m: &BTreeMap<String, Value>) -> serde_json::Value {
    serde_json::Value::Object(
        m.iter()
            .map(|(k, v)| (k.clone(), value_to_json(v)))
            .collect(),
    )
}

/// A direction: `"outbound"` or `"inbound"`.
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum DirectionDto {
    Outbound,
    Inbound,
}

impl From<DirectionDto> for Direction {
    fn from(d: DirectionDto) -> Self {
        match d {
            DirectionDto::Outbound => Direction::Outbound,
            DirectionDto::Inbound => Direction::Inbound,
        }
    }
}

impl From<Direction> for DirectionDto {
    fn from(d: Direction) -> Self {
        match d {
            Direction::Outbound => DirectionDto::Outbound,
            Direction::Inbound => DirectionDto::Inbound,
        }
    }
}

/// Where an event came from: `{branch, seq, timestamp, author}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct EventRefDto {
    pub branch: String,
    pub seq: i64,
    pub timestamp: i64,
    pub author: String,
}

impl From<EventRef> for EventRefDto {
    fn from(e: EventRef) -> Self {
        Self {
            branch: e.branch.to_string(),
            seq: e.seq,
            timestamp: e.timestamp,
            author: e.author,
        }
    }
}

/// A live link: `{source, destination, link_type, sort, event}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct LinkDto {
    pub source: String,
    pub destination: String,
    pub link_type: String,
    pub sort: f64,
    pub event: EventRefDto,
}

impl From<Link> for LinkDto {
    fn from(l: Link) -> Self {
        Self {
            source: l.source,
            destination: l.destination,
            link_type: l.link_type,
            sort: l.sort,
            event: l.event.into(),
        }
    }
}

/// One group of links: `{link_type, direction, links}`. A JSON object cannot
/// key on the `(type, direction)` tuple, so groups are returned as an array.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct LinkGroupDto {
    pub link_type: String,
    pub direction: DirectionDto,
    pub links: Vec<LinkDto>,
}

/// Convert one entity's grouped links to the wire shape.
pub(crate) fn link_groups_to_dto(groups: LinkGroups) -> Vec<LinkGroupDto> {
    groups
        .into_iter()
        .map(|((link_type, direction), links)| LinkGroupDto {
            link_type,
            direction: direction.into(),
            links: links.into_iter().map(LinkDto::from).collect(),
        })
        .collect()
}

/// The sort state of a link: `"timestamp"` (default), `{"explicit": f}` or
/// `"removed"`.
#[derive(Clone, Copy, Debug, Default, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum SortDto {
    #[default]
    Timestamp,
    Explicit(f64),
    Removed,
}

impl From<SortDto> for SortOrder {
    fn from(s: SortDto) -> Self {
        match s {
            SortDto::Timestamp => SortOrder::Timestamp,
            SortDto::Explicit(f) => SortOrder::Explicit(f),
            SortDto::Removed => SortOrder::Removed,
        }
    }
}

impl From<SortOrder> for SortDto {
    fn from(s: SortOrder) -> Self {
        match s {
            SortOrder::Timestamp => SortDto::Timestamp,
            SortOrder::Explicit(f) => SortDto::Explicit(f),
            SortOrder::Removed => SortDto::Removed,
        }
    }
}

/// One op: `{"set": {...}}` or `{"link": {...}}`.
#[derive(Clone, Debug, Deserialize)]
#[serde(rename_all = "snake_case")]
pub(crate) enum OpDto {
    Set {
        entity: String,
        key: String,
        value: serde_json::Value,
    },
    Link {
        source: String,
        destination: String,
        link_type: String,
        #[serde(default)]
        sort: SortDto,
    },
}

impl From<OpDto> for Op {
    fn from(o: OpDto) -> Self {
        match o {
            OpDto::Set { entity, key, value } => Op::SetValue {
                entity,
                key,
                value: value_from_json(value),
            },
            OpDto::Link {
                source,
                destination,
                link_type,
                sort,
            } => Op::Link {
                source,
                destination,
                link_type,
                sort: sort.into(),
            },
        }
    }
}

/// A write batch: `{branch, author, timestamp?, ops}`.
#[derive(Clone, Debug, Deserialize)]
pub(crate) struct WriteBatchDto {
    pub branch: String,
    pub author: String,
    #[serde(default)]
    pub timestamp: Option<i64>,
    pub ops: Vec<OpDto>,
}

impl TryFrom<WriteBatchDto> for WriteBatch {
    type Error = Error;
    fn try_from(b: WriteBatchDto) -> Result<Self> {
        Ok(WriteBatch {
            branch: parse_branch(&b.branch)?,
            author: b.author,
            timestamp: b.timestamp,
            ops: b.ops.into_iter().map(Op::from).collect(),
        })
    }
}

/// The result of a write: `{branch, first_seq, last_seq, timestamp}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct WriteResultDto {
    pub branch: String,
    pub first_seq: i64,
    pub last_seq: i64,
    pub timestamp: i64,
}

impl From<WriteResult> for WriteResultDto {
    fn from(r: WriteResult) -> Self {
        Self {
            branch: r.branch.to_string(),
            first_seq: r.first_seq,
            last_seq: r.last_seq,
            timestamp: r.timestamp,
        }
    }
}

/// What an undo did: `{removed: [first, last], events, restored}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct UndoOutcomeDto {
    pub removed: [i64; 2],
    pub events: usize,
    pub restored: usize,
}

impl From<UndoOutcome> for UndoOutcomeDto {
    fn from(u: UndoOutcome) -> Self {
        Self {
            removed: [u.removed.0, u.removed.1],
            events: u.events,
            restored: u.restored,
        }
    }
}

/// A sequence range: `{start, end?}`. `end` null or absent is an open range.
#[derive(Clone, Copy, Debug, Serialize, Deserialize)]
pub(crate) struct SeqRangeDto {
    pub start: i64,
    #[serde(default)]
    pub end: Option<i64>,
}

impl From<SeqRange> for SeqRangeDto {
    fn from(r: SeqRange) -> Self {
        Self {
            start: r.start,
            end: r.end,
        }
    }
}

impl From<SeqRangeDto> for SeqRange {
    fn from(r: SeqRangeDto) -> Self {
        SeqRange::new(r.start, r.end)
    }
}

/// One parent entry: `{branch, range}`.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub(crate) struct ParentRefDto {
    pub branch: String,
    pub range: SeqRangeDto,
}

impl From<ParentRef> for ParentRefDto {
    fn from(p: ParentRef) -> Self {
        Self {
            branch: p.branch.to_string(),
            range: p.range.into(),
        }
    }
}

impl TryFrom<ParentRefDto> for ParentRef {
    type Error = Error;
    fn try_from(p: ParentRefDto) -> Result<Self> {
        Ok(ParentRef::new(parse_branch(&p.branch)?, p.range.into()))
    }
}

/// A branch: `{id, name, archived, head, parents, created_at}`.
#[derive(Clone, Debug, Serialize)]
pub(crate) struct BranchInfoDto {
    pub id: String,
    pub name: String,
    pub archived: bool,
    pub head: i64,
    pub parents: Vec<ParentRefDto>,
    pub created_at: i64,
}

impl From<BranchInfo> for BranchInfoDto {
    fn from(b: BranchInfo) -> Self {
        Self {
            id: b.id.to_string(),
            name: b.name,
            archived: b.archived,
            head: b.head,
            parents: b.parents.into_iter().map(Into::into).collect(),
            created_at: b.created_at,
        }
    }
}
