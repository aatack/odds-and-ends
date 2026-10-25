//! Mapping between Rust values and SQLite columns.

use rusqlite::types::{Value as SqlValue, ValueRef};

use crate::error::{Error, Result};
use crate::types::{SortOrder, Timestamp, Value};

const V_NULL: i64 = 0;
const V_BOOL: i64 = 1;
const V_INT: i64 = 2;
const V_FLOAT: i64 = 3;
const V_TEXT: i64 = 4;
const V_JSON: i64 = 5;

pub(crate) fn encode_value(value: &Value) -> (i64, SqlValue) {
    match value {
        Value::Null => (V_NULL, SqlValue::Null),
        Value::Bool(b) => (V_BOOL, SqlValue::Integer(i64::from(*b))),
        Value::Int(i) => (V_INT, SqlValue::Integer(*i)),
        Value::Float(f) => (V_FLOAT, SqlValue::Real(*f)),
        Value::Text(s) => (V_TEXT, SqlValue::Text(s.clone())),
        Value::Json(j) => (V_JSON, SqlValue::Text(j.to_string())),
    }
}

pub(crate) fn decode_value(vtype: i64, raw: ValueRef<'_>) -> Result<Value> {
    let corrupt = || Error::Corrupt(format!("value of type {vtype} holds {:?}", raw.data_type()));
    Ok(match (vtype, raw) {
        (V_NULL, _) => Value::Null,
        (V_BOOL, ValueRef::Integer(i)) => Value::Bool(i != 0),
        (V_INT, ValueRef::Integer(i)) => Value::Int(i),
        (V_FLOAT, ValueRef::Real(f)) => Value::Float(f),
        (V_FLOAT, ValueRef::Integer(i)) => Value::Float(i as f64),
        (V_TEXT, ValueRef::Text(t)) => {
            Value::Text(std::str::from_utf8(t).map_err(|_| corrupt())?.to_owned())
        }
        (V_JSON, ValueRef::Text(t)) => {
            Value::Json(serde_json::from_slice(t).map_err(|e| Error::Corrupt(e.to_string()))?)
        }
        _ => return Err(corrupt()),
    })
}

/// The stored sort key of a link event: the explicit value, the event's own
/// timestamp (ms) for the default, or NULL for a removed link.
pub(crate) fn encode_sort(sort: SortOrder, ts: Timestamp) -> Option<f64> {
    match sort {
        SortOrder::Timestamp => Some(ts as f64),
        SortOrder::Explicit(f) => Some(f),
        SortOrder::Removed => None,
    }
}
