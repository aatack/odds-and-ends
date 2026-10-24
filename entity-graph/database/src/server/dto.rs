//! Wire types for the HTTP API. These are separate from the library types:
//! the library keeps no serde derives. Each DTO converts to or from a library
//! type. See `API.md` for the JSON shapes.

use serde::{Deserialize, Serialize};

use crate::error::{Error, Result};
use crate::types::{
    BranchId, BranchInfo, Op, ParentRef, SeqRange, SortOrder, Value, WriteBatch, WriteResult,
};

/// Parse a branch ID from its UUID string, or a 400-mapped error.
pub(crate) fn parse_branch(s: &str) -> Result<BranchId> {
    uuid::Uuid::parse_str(s)
        .map(BranchId)
        .map_err(|_| Error::Invalid(format!("bad branch id {s:?}")))
}

/// A library [`Value`] as JSON: null/bool/number/string/array/object.
/// Used by the read endpoints added in later rounds.
#[allow(dead_code)]
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
