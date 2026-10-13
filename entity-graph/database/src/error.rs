use crate::types::BranchId;

/// Everything that can go wrong in this crate.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error(transparent)]
    Sqlite(#[from] rusqlite::Error),

    #[error("the file has schema version {found}; this crate supports up to {supported}")]
    SchemaTooNew { found: u32, supported: u32 },

    #[error("no branch {0}")]
    UnknownBranch(BranchId),

    #[error("branch {0} is archived and does not accept writes")]
    BranchArchived(BranchId),

    #[error("cannot undo: {0}")]
    UndoRefused(String),

    #[error("invalid input: {0}")]
    Invalid(String),

    #[error("stored data is corrupt: {0}")]
    Corrupt(String),

    #[error("{0} is not implemented yet")]
    NotImplemented(&'static str),
}

pub type Result<T, E = Error> = std::result::Result<T, E>;
