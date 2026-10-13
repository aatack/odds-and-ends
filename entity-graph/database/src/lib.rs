//! An event-sourced store of entities, values and links, with branches.
//!
//! - Two event types: value events and link events. There are no create,
//!   delete or clear events: null clears a value, `SortOrder::Removed` removes a
//!   link, and an entity exists when it has events.
//! - Each event has a per-branch sequence ID, unique on `(branch, seq)`.
//! - Hot tables hold only the current event per target. A superseded event
//!   moves to its type's history table in the same transaction.
//! - A branch reads its own events plus parent ranges from its parent list.
//!
//! See `DESIGN.md` in `graph-store/` for the design.

mod codec;
mod error;
mod read;
mod schema;
mod store;
mod types;
mod undo;
mod view;
mod write;

pub use error::{Error, Result};
pub use read::LinkGroups;
pub use schema::SCHEMA_VERSION;
pub use store::Store;
pub use types::*;

#[cfg(test)]
mod tests;
