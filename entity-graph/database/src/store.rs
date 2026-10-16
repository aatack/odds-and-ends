//! Opening a store, and the branch operations.

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use rusqlite::{params, Connection, OptionalExtension, TransactionBehavior};

use crate::error::{Error, Result};
use crate::schema;
use crate::types::{BranchId, BranchInfo, ParentRef, Seq, SeqRange, Timestamp};
use crate::view::{branch_uuid, dedupe, load_view, own_tail, parent_rows, View, OPEN};

type Clock = Box<dyn Fn() -> Timestamp + Send>;

/// One store: one SQLite file holding every branch.
pub struct Store {
    pub(crate) conn: Connection,
    clock: Clock,
    /// Flattened views per branch row. Cleared when a parent list changes on
    /// this handle, or when another connection commits (`PRAGMA data_version`).
    views: RefCell<(i64, HashMap<i64, View>)>,
}

fn system_clock() -> Timestamp {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis() as Timestamp)
        .unwrap_or(0)
}

impl Store {
    /// Open or create a store file, and apply any missing migrations.
    pub fn open(path: impl AsRef<Path>) -> Result<Self> {
        let conn = Connection::open(path)?;
        // Switching a new file to WAL needs a lock that the busy timeout does
        // not wait for, so a second connection that opens at the same moment
        // can get "database is locked". Retry for a few seconds.
        let mut tries = 0;
        loop {
            match conn.pragma_update_and_check(None, "journal_mode", "WAL", |_| Ok(())) {
                Err(rusqlite::Error::SqliteFailure(e, _))
                    if e.code == rusqlite::ErrorCode::DatabaseBusy && tries < 500 =>
                {
                    tries += 1;
                    std::thread::sleep(std::time::Duration::from_millis(10));
                }
                other => break other?,
            }
        }
        Self::init(conn)
    }

    /// A store that lives in memory only. For tests and scratch work.
    pub fn open_in_memory() -> Result<Self> {
        Self::init(Connection::open_in_memory()?)
    }

    fn init(mut conn: Connection) -> Result<Self> {
        conn.pragma_update(None, "synchronous", "NORMAL")?;
        conn.pragma_update(None, "foreign_keys", "ON")?;
        conn.set_prepared_statement_cache_capacity(64);
        schema::migrate(&mut conn)?;
        Ok(Self {
            conn,
            clock: Box::new(system_clock),
            views: RefCell::new((-1, HashMap::new())),
        })
    }

    /// Step 1 of every read: the flattened view of a branch row, cached.
    pub(crate) fn view(&self, bid: i64) -> Result<View> {
        let version: i64 = self
            .conn
            .pragma_query_value(None, "data_version", |r| r.get(0))?;
        let mut cache = self.views.borrow_mut();
        if cache.0 != version {
            cache.1.clear();
            cache.0 = version;
        }
        if let Some(v) = cache.1.get(&bid) {
            return Ok(v.clone());
        }
        let v = load_view(&self.conn, bid)?;
        cache.1.insert(bid, v.clone());
        Ok(v)
    }

    /// Drop all cached views. A parent-list change can change the view of
    /// every branch that follows this one through open ranges.
    fn parents_changed(&self) {
        self.views.borrow_mut().1.clear();
    }

    /// Replace the clock (Unix ms). Tests use this to control time.
    pub fn set_clock(&mut self, clock: impl Fn() -> Timestamp + Send + 'static) {
        self.clock = Box::new(clock);
    }

    pub(crate) fn now(&self) -> Timestamp {
        (self.clock)()
    }

    /// The schema version of the open file.
    pub fn schema_version(&self) -> Result<u32> {
        Ok(self
            .conn
            .pragma_query_value(None, "user_version", |r| r.get(0))?)
    }

    /// Set the SQLite page cache size for this connection, in KiB.
    /// SQLite's default is 2000 KiB. For benchmarks and memory-tight hosts.
    pub fn set_cache_size_kib(&self, kib: i64) -> Result<()> {
        self.conn.pragma_update(None, "cache_size", -kib.max(1))?;
        Ok(())
    }

    /// Set `PRAGMA mmap_size` in bytes (0 turns memory-mapped I/O off).
    pub fn set_mmap_size(&self, bytes: i64) -> Result<()> {
        self.conn
            .pragma_update_and_check(None, "mmap_size", bytes, |_| Ok(()))?;
        Ok(())
    }

    /// Copy all WAL content into the main file and truncate the WAL.
    pub fn checkpoint(&self) -> Result<()> {
        self.conn
            .query_row("PRAGMA wal_checkpoint(TRUNCATE)", [], |_| Ok(()))?;
        Ok(())
    }

    // --- Branches -------------------------------------------------------

    /// Create a branch with an explicit parent list.
    pub fn create_branch(&mut self, name: &str, parents: &[ParentRef]) -> Result<BranchId> {
        let now = self.now();
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let id = insert_branch(&tx, name, now)?;
        let bid = branch_row(&tx, id)?.bid;
        write_parents(&tx, bid, parents)?;
        tx.commit()?;
        self.parents_changed();
        Ok(id)
    }

    /// Create a branch that sees what `from` sees now, and nothing that
    /// `from` or its parents write later.
    pub fn fork(&mut self, from: BranchId, name: &str) -> Result<BranchId> {
        let now = self.now();
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let entries = frozen_view(&tx, branch_row(&tx, from)?.bid)?;
        let id = insert_branch(&tx, name, now)?;
        let bid = branch_row(&tx, id)?.bid;
        write_parents(&tx, bid, &entries)?;
        tx.commit()?;
        self.parents_changed();
        Ok(id)
    }

    /// Add one entry at the top of the parent list (below `into`'s own
    /// uncovered events). An open range follows `from` live, with `from`'s
    /// whole expanded chain. A bounded range gives only `from`'s own events
    /// in the range.
    pub fn pull(&mut self, into: BranchId, from: BranchId, range: SeqRange) -> Result<()> {
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let bid = branch_row(&tx, into)?.bid;
        let mut parents = read_parents(&tx, bid)?;
        parents.push(ParentRef::new(from, range));
        write_parents(&tx, bid, &parents)?;
        tx.commit()?;
        self.parents_changed();
        Ok(())
    }

    /// Merge `from` into `into`: put `from`'s whole stack, frozen at the
    /// current heads, on top of everything `into` has now.
    ///
    /// `into`'s own events so far are frozen into a self entry first, so the
    /// merged work wins over them; `into`'s later writes sit on top again.
    /// Entries of `from` that name `into` are left out: `into` already has its
    /// own events, and a stale copy on top would undo its concurrent edits.
    /// There is no conflict resolution: the stack order decides.
    pub fn merge(&mut self, into: BranchId, from: BranchId) -> Result<()> {
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let bid = branch_row(&tx, into)?.bid;
        let rows = parent_rows(&tx, bid)?;
        let mut parents = read_parents(&tx, bid)?;
        parents.extend(frozen_tail(&tx, bid, &rows)?);
        parents.extend(
            frozen_view(&tx, branch_row(&tx, from)?.bid)?
                .into_iter()
                .filter(|p| p.branch != into),
        );
        write_parents(&tx, bid, &parents)?;
        tx.commit()?;
        self.parents_changed();
        Ok(())
    }

    /// Rebase: replace the parent list.
    pub fn set_parents(&mut self, branch: BranchId, parents: &[ParentRef]) -> Result<()> {
        let tx = self
            .conn
            .transaction_with_behavior(TransactionBehavior::Immediate)?;
        let bid = branch_row(&tx, branch)?.bid;
        write_parents(&tx, bid, parents)?;
        tx.commit()?;
        self.parents_changed();
        Ok(())
    }

    /// Archive or restore a branch. Branches are never deleted.
    /// An archived branch refuses writes.
    pub fn archive_branch(&mut self, branch: BranchId, archived: bool) -> Result<()> {
        let n = self
            .conn
            .prepare_cached("UPDATE branches SET archived = ?2 WHERE id = ?1")?
            .execute(params![branch.0.as_bytes().as_slice(), archived])?;
        if n == 0 {
            return Err(Error::UnknownBranch(branch));
        }
        Ok(())
    }

    pub fn branch(&self, branch: BranchId) -> Result<BranchInfo> {
        let row = branch_row(&self.conn, branch)?;
        let (name, created_at): (String, Timestamp) = self
            .conn
            .prepare_cached("SELECT name, created_at FROM branches WHERE bid = ?1")?
            .query_row([row.bid], |r| Ok((r.get(0)?, r.get(1)?)))?;
        Ok(BranchInfo {
            id: branch,
            name,
            archived: row.archived,
            head: row.head,
            parents: read_parents(&self.conn, row.bid)?,
            created_at,
        })
    }

    /// Every branch, oldest first.
    pub fn branches(&self) -> Result<Vec<BranchInfo>> {
        let ids: Vec<Vec<u8>> = self
            .conn
            .prepare_cached("SELECT id FROM branches ORDER BY bid")?
            .query_map([], |r| r.get(0))?
            .collect::<rusqlite::Result<_>>()?;
        ids.iter().map(|id| self.branch(branch_uuid(id)?)).collect()
    }
}

// --- Helpers shared by the other modules --------------------------------

#[derive(Clone, Copy, Debug)]
pub(crate) struct BranchRow {
    pub bid: i64,
    pub archived: bool,
    pub head: Seq,
    pub last_ts: Timestamp,
}

pub(crate) fn branch_row(conn: &Connection, id: BranchId) -> Result<BranchRow> {
    conn.prepare_cached("SELECT bid, archived, head_seq, last_ts FROM branches WHERE id = ?1")?
        .query_row([id.0.as_bytes().as_slice()], |r| {
            Ok(BranchRow {
                bid: r.get(0)?,
                archived: r.get(1)?,
                head: r.get(2)?,
                last_ts: r.get(3)?,
            })
        })
        .optional()?
        .ok_or(Error::UnknownBranch(id))
}

fn insert_branch(conn: &Connection, name: &str, now: Timestamp) -> Result<BranchId> {
    let id = BranchId::generate();
    conn.prepare_cached("INSERT INTO branches (id, name, created_at) VALUES (?1, ?2, ?3)")?
        .execute(params![id.0.as_bytes().as_slice(), name, now])?;
    Ok(id)
}

fn read_parents(conn: &Connection, bid: i64) -> Result<Vec<ParentRef>> {
    let mut stmt = conn.prepare_cached(
        "SELECT b.id, p.seq_start, p.seq_end
         FROM branch_parents p JOIN branches b ON b.bid = p.parent_bid
         WHERE p.bid = ?1 ORDER BY p.pos",
    )?;
    let mut rows = stmt.query([bid])?;
    let mut out = Vec::new();
    while let Some(r) = rows.next()? {
        out.push(ParentRef::new(
            branch_uuid(&r.get::<_, Vec<u8>>(0)?)?,
            SeqRange::new(r.get(1)?, r.get(2)?),
        ));
    }
    Ok(out)
}

/// Store a parent list, in stack order (later entries win).
///
/// The list is deduped with the same rule as reads: each branch keeps the
/// position of its first entry, with the greatest covered range (lowest
/// start, highest end; open beats frozen). Entries may name the branch
/// itself; they place some of its own events inside the stack, and the own
/// events that no entry covers sit on top.
fn write_parents(conn: &Connection, bid: i64, parents: &[ParentRef]) -> Result<()> {
    let mut entries = Vec::with_capacity(parents.len());
    for p in parents {
        if p.range.start < 1 || p.range.end.is_some_and(|end| end < p.range.start) {
            return Err(Error::Invalid(format!(
                "bad range {:?} for {}",
                p.range, p.branch
            )));
        }
        let parent = branch_row(conn, p.branch)?.bid;
        entries.push((parent, p.branch, p.range.start, p.range.end.unwrap_or(OPEN)));
    }
    conn.prepare_cached("DELETE FROM branch_parents WHERE bid = ?1")?
        .execute([bid])?;
    let mut insert = conn.prepare_cached(
        "INSERT INTO branch_parents (bid, pos, parent_bid, seq_start, seq_end)
         VALUES (?1, ?2, ?3, ?4, ?5)",
    )?;
    for (pos, (parent, _, lo, hi)) in dedupe(entries).into_iter().enumerate() {
        let end = (hi != OPEN).then_some(hi);
        insert.execute(params![bid, pos as i64, parent, lo, end])?;
    }
    Ok(())
}

fn head_of(conn: &Connection, bid: i64) -> Result<Seq> {
    Ok(conn
        .prepare_cached("SELECT head_seq FROM branches WHERE bid = ?1")?
        .query_row([bid], |r| r.get(0))?)
}

/// The flattened view of `bid` as entries frozen at the current heads, in
/// stack order. It records sequence bounds only; no event is copied (R24).
fn frozen_view(conn: &Connection, bid: i64) -> Result<Vec<ParentRef>> {
    let view = load_view(conn, bid)?;
    let mut out = Vec::new();
    for src in &view.sources {
        let head = head_of(conn, src.bid)?;
        for &(lo, hi) in &src.intervals {
            let hi = hi.min(head);
            if lo <= hi {
                out.push(ParentRef::new(src.id, SeqRange::new(lo, Some(hi))));
            }
        }
    }
    Ok(out)
}

/// The branch's own uncovered events up to its head, as frozen entries.
fn frozen_tail(
    conn: &Connection,
    bid: i64,
    rows: &[(i64, BranchId, Seq, Seq)],
) -> Result<Vec<ParentRef>> {
    let (id, head): (Vec<u8>, Seq) = conn
        .prepare_cached("SELECT id, head_seq FROM branches WHERE bid = ?1")?
        .query_row([bid], |r| Ok((r.get(0)?, r.get(1)?)))?;
    let id = branch_uuid(&id)?;
    Ok(own_tail(bid, rows)
        .into_iter()
        .filter_map(|(lo, hi)| {
            let hi = hi.min(head);
            (lo <= hi).then(|| ParentRef::new(id, SeqRange::new(lo, Some(hi))))
        })
        .collect())
}

/// The interned key for an entity ID, if it has ever been written.
pub(crate) fn lookup_entity(conn: &Connection, ext_id: &str) -> Result<Option<i64>> {
    Ok(conn
        .prepare_cached("SELECT eid FROM entities WHERE ext_id = ?1")?
        .query_row([ext_id], |r| r.get(0))
        .optional()?)
}

/// The interned key for an entity ID, created on first use.
pub(crate) fn intern_entity(conn: &Connection, ext_id: &str) -> Result<i64> {
    if let Some(eid) = lookup_entity(conn, ext_id)? {
        return Ok(eid);
    }
    conn.prepare_cached("INSERT INTO entities (ext_id) VALUES (?1)")?
        .execute([ext_id])?;
    Ok(conn.last_insert_rowid())
}
