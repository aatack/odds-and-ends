#!/usr/bin/env bash
# A consistent daily copy of the YAWN store and the secrets that read it, kept
# locally. Run by workflows-backup.timer, or by hand as root.
#
#   sudo deploy/backup.sh
#
# What it keeps, per run, in /var/backups/workflows/workflows-<stamp>.tar.gz:
#   - store.sqlite            the store itself (a `sqlite3 .backup` snapshot, so it
#                             is consistent even while the backend writes)
#   - data/secret.key         the AES key WITHOUT WHICH THE SECRETS CANNOT BE READ
#   - data/bots/              the bots' working folders (sessions, worktrees)
#
# It keeps the last 14 and prunes older ones. This copy sits on the SAME box, so
# it survives a bad write, not a dead disk: copy it OFF the box too — see the note
# at the end.
set -euo pipefail

STORE=${WORKFLOWS_STORE:-/var/lib/workflows/store.sqlite}
DATA="$STORE.data"
DEST=${WORKFLOWS_BACKUP_DIR:-/var/backups/workflows}
KEEP=14
# No Date.now() worries here: the shell's clock is fine.
STAMP=$(date +%Y%m%d-%H%M%S)

echo "==> Backing up $STORE"
[ -f "$STORE" ] || { echo "No store at $STORE; nothing to back up."; exit 0; }
mkdir -p "$DEST"

# A scratch dir, cleaned up whatever happens.
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
mkdir -p "$WORK/data"

if command -v sqlite3 >/dev/null 2>&1; then
  # `.backup` takes a consistent snapshot while the backend keeps writing.
  echo "    store: sqlite3 .backup (hot, consistent)"
  sqlite3 "$STORE" ".backup '$WORK/store.sqlite'"
else
  # No sqlite3: stop the backend, copy the file, start it again, so the copy is
  # not a half-written page. install.sh installs sqlite3, so this is the rare path.
  echo "    store: sqlite3 missing — stop/copy/start"
  STOPPED=0
  if systemctl is-active --quiet workflows; then systemctl stop workflows; STOPPED=1; fi
  cp -a "$STORE" "$WORK/store.sqlite"
  [ "$STOPPED" -eq 1 ] && systemctl start workflows || true
fi

# The secrets' key and the bots' folders, from the data dir beside the store.
if [ -f "$DATA/secret.key" ]; then cp -a "$DATA/secret.key" "$WORK/data/secret.key"; else echo "    warning: no $DATA/secret.key"; fi
if [ -d "$DATA/bots" ]; then cp -a "$DATA/bots" "$WORK/data/bots"; else echo "    (no bots folder yet)"; fi

ARCHIVE="$DEST/workflows-$STAMP.tar.gz"
tar -czf "$ARCHIVE" -C "$WORK" store.sqlite data
chmod 0600 "$ARCHIVE"
echo "    wrote $ARCHIVE"

# Prune: keep the newest $KEEP, drop the rest.
mapfile -t OLD < <(ls -1t "$DEST"/workflows-*.tar.gz 2>/dev/null | tail -n +$((KEEP + 1)))
if [ "${#OLD[@]}" -gt 0 ]; then
  echo "    pruning ${#OLD[@]} old backup(s), keeping $KEEP"
  rm -f "${OLD[@]}"
fi

echo "==> Done. $(ls -1 "$DEST"/workflows-*.tar.gz 2>/dev/null | wc -l) backup(s) in $DEST"
echo
echo "NOTE: these sit on this box. Copy them OFF it so a dead disk is not the end:"
echo "  - rclone/scp the newest .tar.gz to another host or object store, or"
echo "  - take a Hetzner snapshot/backup of the volume in the console."
echo "The secret.key inside is REQUIRED to read the stored secrets; keep it safe."
