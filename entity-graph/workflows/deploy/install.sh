#!/usr/bin/env bash
# Install (or update) the YAWN workflows backend on a fresh Ubuntu 24.04 server.
# Idempotent: safe to run again. Run as root, from a checkout of the repo:
#
#   sudo deploy/install.sh                 first install
#   sudo deploy/install.sh --update        rebuild from the current checkout + restart
#   sudo deploy/install.sh --no-tailscale  skip installing Tailscale
#   sudo deploy/install.sh --no-claude     skip installing the Claude Code CLI
#
# It copies this checkout to /opt/workflows (if run from elsewhere), builds the
# app and the store's Rust binary, makes the `workflows` user and its state dir,
# writes /etc/workflows/env with a generated phone token (never overwritten), and
# installs + enables the systemd unit. It does NOT run `tailscale up`: it prints
# the manual steps at the end.
set -euo pipefail

# --- settings ---------------------------------------------------------------
PREFIX=/opt/workflows                       # the checkout lives here
APP="$PREFIX/entity-graph/workflows"        # the workflows app within it
STATE=/var/lib/workflows                    # the service's HOME and the store's home
ENVDIR=/etc/workflows
ENVFILE="$ENVDIR/env"
SVCUSER=workflows
UNIT_DIR=/etc/systemd/system

UPDATE=0 NO_TAILSCALE=0 NO_CLAUDE=0
for a in "$@"; do
  case "$a" in
    --update) UPDATE=1 ;;
    --no-tailscale) NO_TAILSCALE=1 ;;
    --no-claude) NO_CLAUDE=1 ;;
    -h|--help) grep '^#' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    *) echo "unknown option: $a (try --help)" >&2; exit 2 ;;
  esac
done

[ "$(id -u)" -eq 0 ] || { echo "Run as root: sudo deploy/install.sh" >&2; exit 1; }

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
SRCROOT=$(cd "$SCRIPT_DIR/../../.." && pwd)  # deploy -> workflows -> entity-graph -> repo root

[ "$UPDATE" -eq 1 ] && tag=" [update]" || tag=""
echo "==> YAWN install (prefix $PREFIX, user $SVCUSER)$tag"

# Run a command as the workflows user, with the env a build needs (its rustup
# cargo on PATH, Electron's binary download skipped — the server never starts it).
run_as_workflows() {
  sudo -u "$SVCUSER" -H env \
    ELECTRON_SKIP_BINARY_DOWNLOAD=1 \
    PATH="/usr/local/bin:/usr/bin:/bin:$STATE/.cargo/bin" \
    HOME="$STATE" \
    bash -c "$1"
}

# Build the app and the store binary from the checkout at $APP. Used by both a
# first install and --update.
build_app() {
  echo "==> Building (npm ci, build, egdb:build) — this needs ~2 GB RAM for the Rust build"
  run_as_workflows "cd '$APP' && npm ci"
  run_as_workflows "cd '$APP' && npm run build"
  run_as_workflows "cd '$APP' && npm run egdb:build"
}

# Copy this checkout to $PREFIX (unless we are already running from there),
# excluding build output and deps. Never deletes anything in the target, so a
# running install keeps its node_modules/out while the sources are refreshed.
# Used by both a first install and --update from a checkout elsewhere.
copy_checkout() {
  if [ "$SRCROOT" != "$PREFIX" ]; then
    echo "==> Copying checkout $SRCROOT -> $PREFIX (excluding node_modules/out/target)"
    mkdir -p "$PREFIX"
    rsync -a \
      --exclude 'node_modules/' --exclude 'out/' --exclude '.run/' \
      --exclude 'egdb/target/' \
      "$SRCROOT"/ "$PREFIX"/
  else
    echo "==> Running from $PREFIX; no copy needed"
  fi
  chown -R "$SVCUSER:$SVCUSER" "$PREFIX"
}

# Wait up to ~20 s for the phone listener's /health to answer ok, so the caller
# knows the restarted service really came up. Prints OK, or a journalctl hint.
wait_health() {
  command -v curl >/dev/null 2>&1 || { echo "    (curl missing; skipping /health check)"; return 0; }
  echo -n "==> Waiting for /health on 127.0.0.1:47392 "
  for _ in $(seq 1 20); do
    if curl -fsS "http://127.0.0.1:47392/health" 2>/dev/null | grep -q '"ok":true'; then
      echo "OK"; return 0
    fi
    echo -n "."; sleep 1
  done
  echo " not yet up."
  echo "    Check the logs: journalctl -u workflows -n 30"
}

# Install (or refresh) the systemd units from the checkout.
install_units() {
  echo "==> Installing systemd units"
  install -m 0644 "$SCRIPT_DIR/workflows.service"        "$UNIT_DIR/workflows.service"
  install -m 0644 "$SCRIPT_DIR/workflows-backup.service" "$UNIT_DIR/workflows-backup.service"
  install -m 0644 "$SCRIPT_DIR/workflows-backup.timer"   "$UNIT_DIR/workflows-backup.timer"
  systemctl daemon-reload
}

# --- --update: rebuild the checkout the user has already `git pull`ed, restart --
if [ "$UPDATE" -eq 1 ]; then
  # When run from a checkout elsewhere, refresh $PREFIX from it first (same copy
  # as a first install); when run from $PREFIX itself, this just fixes ownership.
  if [ "$SRCROOT" != "$PREFIX" ]; then
    copy_checkout
  else
    [ -d "$APP" ] || { echo "No install at $APP to update. Run without --update first." >&2; exit 1; }
    chown -R "$SVCUSER:$SVCUSER" "$PREFIX"
  fi
  build_app
  install_units
  echo "==> Restarting workflows.service"
  systemctl restart workflows
  systemctl --no-pager --lines=0 status workflows || true
  wait_health
  echo "==> Update done. Check it: sudo deploy/doctor.sh"
  exit 0
fi

# --- 1. apt packages --------------------------------------------------------
echo "==> apt: git curl rsync build-essential pkg-config ca-certificates sqlite3"
export DEBIAN_FRONTEND=noninteractive
apt-get update -y
apt-get install -y git curl rsync build-essential pkg-config ca-certificates sqlite3
# Firewall: no changes here. Tailscale is the gate (see DEPLOY.md). If you run ufw,
# keep inbound closed except SSH; nothing here needs an open port.
echo "    (no firewall changes made — Tailscale is the gate; see DEPLOY.md)"

# --- swap (the Rust build wants ~2 GB) --------------------------------------
mem_kb=$(awk '/^MemTotal:/{print $2}' /proc/meminfo)
swap_kb=$(awk '/^SwapTotal:/{print $2}' /proc/meminfo)
if [ "${mem_kb:-0}" -lt 3145728 ] && [ "${swap_kb:-0}" -eq 0 ]; then
  if [ ! -f /swapfile ]; then
    echo "==> RAM < 3 GB and no swap: adding a 2 GB swapfile for the build"
    fallocate -l 2G /swapfile || dd if=/dev/zero of=/swapfile bs=1M count=2048
    chmod 600 /swapfile
    mkswap /swapfile
    swapon /swapfile
    grep -q '^/swapfile ' /etc/fstab || echo '/swapfile none swap sw 0 0' >> /etc/fstab
  fi
fi

# --- 2. Node 22 (NodeSource apt repo) ---------------------------------------
node_major=0
command -v node >/dev/null 2>&1 && { nv=$(node -v); node_major=${nv#v}; node_major=${node_major%%.*}; }
if [ "${node_major:-0}" -lt 22 ]; then
  echo "==> Installing Node 22 (NodeSource setup_22.x)"
  curl -fsSL https://deb.nodesource.com/setup_22.x | bash -
  apt-get install -y nodejs
else
  echo "==> Node $(node -v) already >= 22"
fi

# --- user `workflows` and its state dir -------------------------------------
if ! id -u "$SVCUSER" >/dev/null 2>&1; then
  echo "==> Creating system user $SVCUSER (home $STATE, shell bash)"
  useradd --system --create-home --home-dir "$STATE" --shell /bin/bash "$SVCUSER"
else
  echo "==> User $SVCUSER already exists"
fi
mkdir -p "$STATE"
chown "$SVCUSER:$SVCUSER" "$STATE"
chmod 0750 "$STATE"

# --- copy the checkout to /opt/workflows ------------------------------------
copy_checkout

# --- 3. Rust via rustup, for the workflows user, for the build only ---------
# Scoped to the workflows user's home (~/.cargo): a system cargo may be too old
# for egdb's lockfile, and egdb-server.mjs prefers rustup's cargo when it is there.
if [ ! -x "$STATE/.cargo/bin/cargo" ]; then
  echo "==> Installing Rust (rustup, minimal) for $SVCUSER"
  run_as_workflows "curl -fsSL https://sh.rustup.rs | sh -s -- -y --profile minimal"
else
  echo "==> Rust already installed for $SVCUSER"
fi

# --- 4. build the app + store binary ----------------------------------------
build_app

# --- 5. Claude Code CLI (to /usr/local, so the service finds it on PATH) ----
if [ "$NO_CLAUDE" -eq 0 ]; then
  echo "==> Installing Claude Code CLI to /usr/local (npm -g)"
  npm_config_prefix=/usr/local npm install -g @anthropic-ai/claude-code
else
  echo "==> Skipping Claude Code CLI (--no-claude)"
fi

# --- 6. Tailscale -----------------------------------------------------------
if [ "$NO_TAILSCALE" -eq 0 ]; then
  if command -v tailscale >/dev/null 2>&1; then
    echo "==> Tailscale already installed"
  else
    echo "==> Installing Tailscale"
    curl -fsSL https://tailscale.com/install.sh | sh
  fi
else
  echo "==> Skipping Tailscale (--no-tailscale)"
fi

# --- 7. /etc/workflows/env (generate a phone token once; never overwrite) ---
mkdir -p "$ENVDIR"
if [ ! -f "$ENVFILE" ]; then
  echo "==> Writing $ENVFILE with a generated WORKFLOWS_PHONE_TOKEN"
  token=$(head -c 32 /dev/urandom | base64 | tr -d '\n')   # ~44 chars, well over the 24 minimum
  umask 027
  cat > "$ENVFILE" <<EOF
# YAWN backend environment. Mode 0640, root:workflows. Keep this file minimal:
# bots run with whatever is here. Edit, then: sudo systemctl restart workflows
WORKFLOWS_PHONE_TOKEN=$token
# Optional. Uncomment and set as needed (or run \`sudo -u workflows -H claude login\`):
#ANTHROPIC_API_KEY=
#GITHUB_TOKEN=
EOF
else
  echo "==> $ENVFILE already exists — left untouched (token not changed)"
fi
chown root:"$SVCUSER" "$ENVFILE"
chmod 0640 "$ENVFILE"

# --- 8. install + enable the unit -------------------------------------------
install_units
echo "==> Enabling + starting workflows.service and the daily backup timer"
systemctl enable --now workflows.service
systemctl enable --now workflows-backup.timer
systemctl --no-pager --lines=0 status workflows || true
wait_health

# --- final checklist --------------------------------------------------------
host=$(hostname)
cat <<EOF

================================================================================
Installed. The service is up on 127.0.0.1:47392 (loopback); the tailnet gate is
on. Finish these by hand:

1. Bring up Tailscale (opens an auth link in the terminal):
     sudo tailscale up

2. Serve the phone listener over HTTPS on the tailnet (needs MagicDNS + HTTPS
   certificates enabled in the Tailscale admin console):
     sudo tailscale serve --bg --https=443 http://127.0.0.1:47392

3. Let the bots run Claude (interactive login), OR put ANTHROPIC_API_KEY in
   $ENVFILE and restart:
     sudo -u $SVCUSER -H claude login

Your phone token is here (it is full owner access — treat it like a password):
     sudo grep PHONE $ENVFILE

Then open  https://$host.<your-tailnet>.ts.net/  on the laptop (Desktop app:
Source = that URL, Token = the token above) or the phone (paste the token, Add
to Home Screen). Full walkthrough: deploy/DEPLOY.md. Check it: sudo deploy/doctor.sh
================================================================================
EOF
