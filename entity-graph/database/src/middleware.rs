//! Write middleware: a hook that transforms the ops of a batch before they
//! are written.
//!
//! Middleware runs inside the write transaction (see [`crate::Store::write`]),
//! after the branch check and the timestamp resolution, and before the ops are
//! validated and written. Each registered middleware sees the batch's
//! [`WriteContext`] and the ops as a `&mut Vec<Op>`: it may change, drop or add
//! ops, or return an error to reject the whole batch. Several middleware run in
//! registration order; each sees the output of the previous one.
//!
//! An error rejects the batch: the transaction rolls back, nothing is written,
//! the branch head does not move and no `batches` row is inserted. Return
//! [`crate::Error::Rejected`] for a plain refusal; any other error is passed
//! through unchanged.
//!
//! If middleware drops every op, the write does nothing: no sequence ID is
//! used, no `batches` row is inserted, and the returned [`crate::WriteResult`]
//! has an empty range (`first_seq = head + 1`, `last_seq = head`).
//!
//! Only [`crate::Store::write`] (and [`crate::Store::archive_entity`] /
//! [`crate::Store::restore_entity`], which call it) runs middleware. Branch
//! operations (fork, merge, pull, set_parents) write no events and undo removes
//! events, so middleware does not run for them. Middleware has no access to the
//! store in this version: it cannot read.

use std::sync::Arc;

use crate::error::Result;
use crate::types::{BranchId, Op, Seq, Timestamp};

/// What a middleware knows about the batch it is transforming. Read-only.
pub struct WriteContext<'a> {
    /// The branch the batch is written to.
    pub branch: BranchId,
    /// The batch's author.
    pub author: &'a str,
    /// The resolved, clamped event timestamp: the value every op in the batch
    /// is stamped with. It never goes down on a branch.
    pub timestamp: Timestamp,
    /// The store clock reading taken for this write.
    pub now: Timestamp,
    /// The branch head before this batch (0 when the branch is empty). The
    /// batch's first sequence ID is `head + 1`.
    pub head: Seq,
}

/// A transform on the ops of a write batch. See the [module docs](self).
pub trait WriteMiddleware: Send + Sync {
    /// Transform `ops` in place, or return an error to reject the batch.
    fn on_write(&self, ctx: &WriteContext, ops: &mut Vec<Op>) -> Result<()>;
}

/// Any suitable closure is a [`WriteMiddleware`].
impl<F> WriteMiddleware for F
where
    F: Fn(&WriteContext, &mut Vec<Op>) -> Result<()> + Send + Sync,
{
    fn on_write(&self, ctx: &WriteContext, ops: &mut Vec<Op>) -> Result<()> {
        self(ctx, ops)
    }
}

/// A registered middleware, kept behind an `Arc` on the [`crate::Store`].
pub(crate) type Registered = Arc<dyn WriteMiddleware>;
