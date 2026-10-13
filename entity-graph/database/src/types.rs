//! Public data types.

use std::fmt;

use uuid::Uuid;

/// A per-branch sequence ID. The first event on a branch has sequence 1.
/// `(branch, seq)` is unique across the whole store.
pub type Seq = i64;

/// Unix time in milliseconds.
pub type Timestamp = i64;

/// The value key that names an entity's type. Archiving an entity writes null
/// here (there are no delete events), so a search by type skips archived ones.
pub const TYPE_KEY: &str = "type";

/// How long after a batch its author may still undo it.
pub const UNDO_WINDOW_MS: Timestamp = 5 * 60 * 1000;

/// A branch ID. Fixed when the branch is created (UUIDv7), so that two
/// stores can later refer to the same branch.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct BranchId(pub Uuid);

impl BranchId {
    /// A fresh, time-ordered branch ID.
    pub fn generate() -> Self {
        Self(Uuid::now_v7())
    }
}

impl fmt::Display for BranchId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// An inclusive range of sequence IDs on one branch. `end: None` is open:
/// the range follows the branch as it grows. An upper bound freezes it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SeqRange {
    pub start: Seq,
    pub end: Option<Seq>,
}

impl SeqRange {
    /// Every event, now and later.
    pub const ALL: SeqRange = SeqRange {
        start: 1,
        end: None,
    };

    pub fn new(start: Seq, end: Option<Seq>) -> Self {
        Self { start, end }
    }

    /// Events `1..=end`: a frozen parent.
    pub fn up_to(end: Seq) -> Self {
        Self {
            start: 1,
            end: Some(end),
        }
    }

    /// Events from `start` on, open.
    pub fn from(start: Seq) -> Self {
        Self { start, end: None }
    }

    pub fn contains(&self, seq: Seq) -> bool {
        seq >= self.start && self.end.is_none_or(|end| seq <= end)
    }
}

/// One entry in a branch's parent list: a branch and the part of its own
/// events that this branch reads.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ParentRef {
    pub branch: BranchId,
    pub range: SeqRange,
}

impl ParentRef {
    pub fn new(branch: BranchId, range: SeqRange) -> Self {
        Self { branch, range }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BranchInfo {
    pub id: BranchId,
    pub name: String,
    pub archived: bool,
    /// The sequence ID of the last event on this branch (0 when empty).
    pub head: Seq,
    pub parents: Vec<ParentRef>,
    pub created_at: Timestamp,
}

/// A typed value. `Null` clears a field: there is no separate clear event.
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    Text(String),
    /// Arrays and objects.
    Json(serde_json::Value),
}

impl Value {
    pub fn is_null(&self) -> bool {
        matches!(self, Value::Null)
    }
}

impl From<&str> for Value {
    fn from(v: &str) -> Self {
        Value::Text(v.to_owned())
    }
}
impl From<String> for Value {
    fn from(v: String) -> Self {
        Value::Text(v)
    }
}
impl From<i64> for Value {
    fn from(v: i64) -> Self {
        Value::Int(v)
    }
}
impl From<f64> for Value {
    fn from(v: f64) -> Self {
        Value::Float(v)
    }
}
impl From<bool> for Value {
    fn from(v: bool) -> Self {
        Value::Bool(v)
    }
}

/// The sort state that a link event writes. Three states, the same pattern as
/// "null clears a field". Only a nullable float is stored: `Timestamp` becomes
/// the event's timestamp in ms, `Removed` becomes NULL.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum SortOrder {
    /// No explicit order: the link sorts by the timestamp of its last event.
    Timestamp,
    /// An explicit sort key.
    Explicit(f64),
    /// The link is removed.
    Removed,
}

/// One change. Values and links are the only two event types.
#[derive(Clone, Debug, PartialEq)]
pub enum Op {
    /// Set `key` on `entity`. `Value::Null` clears it.
    SetValue {
        entity: String,
        key: String,
        value: Value,
    },
    /// Write the state of the link `(source, destination, link_type)`.
    Link {
        source: String,
        destination: String,
        link_type: String,
        sort: SortOrder,
    },
}

impl Op {
    pub fn set(entity: impl Into<String>, key: impl Into<String>, value: impl Into<Value>) -> Self {
        Op::SetValue {
            entity: entity.into(),
            key: key.into(),
            value: value.into(),
        }
    }

    pub fn clear(entity: impl Into<String>, key: impl Into<String>) -> Self {
        Op::SetValue {
            entity: entity.into(),
            key: key.into(),
            value: Value::Null,
        }
    }

    pub fn link(
        source: impl Into<String>,
        destination: impl Into<String>,
        link_type: impl Into<String>,
        sort: SortOrder,
    ) -> Self {
        Op::Link {
            source: source.into(),
            destination: destination.into(),
            link_type: link_type.into(),
            sort,
        }
    }

    pub fn unlink(
        source: impl Into<String>,
        destination: impl Into<String>,
        link_type: impl Into<String>,
    ) -> Self {
        Self::link(source, destination, link_type, SortOrder::Removed)
    }
}

/// A group of events written in one transaction. It is also the unit of undo.
#[derive(Clone, Debug, PartialEq)]
pub struct WriteBatch {
    pub branch: BranchId,
    pub author: String,
    /// Event timestamp. Defaults to the store clock. Clamped so that
    /// timestamps never go down within a branch.
    pub timestamp: Option<Timestamp>,
    pub ops: Vec<Op>,
}

impl WriteBatch {
    pub fn new(branch: BranchId, author: impl Into<String>, ops: Vec<Op>) -> Self {
        Self {
            branch,
            author: author.into(),
            timestamp: None,
            ops,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct WriteResult {
    pub branch: BranchId,
    pub first_seq: Seq,
    pub last_seq: Seq,
    pub timestamp: Timestamp,
}

/// Where an event came from.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EventRef {
    pub branch: BranchId,
    pub seq: Seq,
    pub timestamp: Timestamp,
    pub author: String,
}

/// A live link as a read returns it.
#[derive(Clone, Debug, PartialEq)]
pub struct Link {
    pub source: String,
    pub destination: String,
    pub link_type: String,
    /// The stored sort key: an explicit value, or the timestamp (ms) of the
    /// event that wrote the default. Removed links are never returned.
    pub sort: f64,
    pub event: EventRef,
}

/// Which side of a link the asked-for entity is on.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Direction {
    /// Links where the entity is the source: its children.
    Outbound,
    /// Links where the entity is the destination: its parents.
    Inbound,
}

/// What an undo did.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UndoOutcome {
    /// The batch that was removed: `(first_seq, last_seq)` on the branch.
    /// These sequence IDs are never used again.
    pub removed: (Seq, Seq),
    /// Events deleted (current rows plus the batch's own superseded rows).
    pub events: usize,
    /// Older rows moved back from history into the current tables.
    pub restored: usize,
}
