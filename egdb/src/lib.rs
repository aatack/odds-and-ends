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
//! See `DESIGN.md` and `ARCHITECTURE.md` in this crate for the design.

mod codec;
mod error;
mod find;
mod middleware;
mod prove;
mod read;
mod schema;
#[cfg(feature = "server")]
pub mod server;
mod store;
mod types;
mod undo;
mod view;
mod walk;
mod write;

pub use error::{Error, Result};
/// Planner switches for benchmarks and tests. Not part of the API.
#[doc(hidden)]
pub use find::knobs as find_knobs;
pub use find::{Filter, Find, FindOptions, FindOrder, FindRow, Predicate};
pub use middleware::{WriteContext, WriteMiddleware};
pub use prove::{Proof, Prove, ProveOptions, ProveOutcome, ProveStats};
pub use read::LinkGroups;
pub use schema::SCHEMA_VERSION;
pub use store::Store;
pub use types::*;
pub use walk::{
    Follow, FollowRule, ReadAhead, Walk, WalkCache, WalkDirection, WalkNode,
    WalkOptions,
};

#[cfg(test)]
mod find_tests;
#[cfg(test)]
mod history_tests;
#[cfg(test)]
mod middleware_tests;
#[cfg(test)]
mod model_tests;
#[cfg(test)]
mod prove_tests;
#[cfg(test)]
mod robust_tests;
#[cfg(test)]
mod tests;
