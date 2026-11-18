#!/usr/bin/env bash
# Read-only health checks for a YAWN headless host. Changes nothing; prints an
# OK/FAIL line per check and exits non-zero if any failed.
#
#   sudo deploy/doctor.sh      (sudo so it can look as the workflows user)
set -uo pipefail

APP=/opt/workflows/entity-graph/workflows
EGDB=/opt/workflows/egdb/target/release/egdb-server
PHONE_PORT=47392
USER=workflows

fails=0
ok()   { printf '  OK   %s\n' "$1"; }
fail() { printf '  FAIL %s\n' "$1"; fails=$((fails + 1)); }
note() { printf '  --   %s\n' "$1"; }

echo "==> YAWN doctor"

# node >= 22
if command -v node >/dev/null 2>&1; then
  v=$(node -v); major=${v#v}; major=${major%%.*}
  if [ "${major:-0}" -ge 22 ]; then ok "node $v (>= 22)"; else fail "node $v is older than 22"; fi
else
  fail "node is not on PATH"
fi

# egdb-server binary
if [ -x "$EGDB" ]; then ok "egdb-server built ($EGDB)"; else fail "egdb-server missing or not executable ($EGDB) — run install.sh"; fi

# built renderer (the phone's PWA)
if [ -f "$APP/out/renderer/index.html" ]; then ok "renderer built (out/renderer)"; else fail "renderer not built ($APP/out/renderer) — run \`npm run build\`"; fi

# claude on PATH for the workflows user, and that it runs (--version only; we do
# not probe login, which would hit the network / have side effects).
as_user() { if [ "$(id -u)" -eq 0 ]; then sudo -u "$USER" -H bash -lc "$1"; else bash -lc "$1"; fi; }
if as_user 'command -v claude >/dev/null 2>&1'; then
  if cv=$(as_user 'claude --version' 2>/dev/null); then
    ok "claude on PATH for $USER ($cv)"
    note "login is not checked here; if bots fail to start, run: sudo -u $USER -H claude login"
  else
    fail "claude is on PATH but \`claude --version\` failed for $USER"
  fi
else
  fail "claude is not on PATH for $USER (install it, or set ANTHROPIC_API_KEY in /etc/workflows/env)"
fi

# tailscale
if command -v tailscale >/dev/null 2>&1; then
  if tailscale status >/dev/null 2>&1; then ok "tailscale up"; else fail "tailscale installed but not up — run: sudo tailscale up"; fi
  if tailscale serve status 2>/dev/null | grep -q "127.0.0.1:$PHONE_PORT"; then
    ok "tailscale serve points at 127.0.0.1:$PHONE_PORT"
  else
    fail "tailscale serve does not point at 127.0.0.1:$PHONE_PORT — see DEPLOY.md step 3"
  fi
else
  note "tailscale not installed (ok if --no-tailscale was used)"
fi

# the service
if systemctl is-active --quiet workflows; then ok "workflows.service active"; else fail "workflows.service not active — journalctl -u workflows -e"; fi

# /health on the phone listener
if command -v curl >/dev/null 2>&1; then
  if curl -fsS "http://127.0.0.1:$PHONE_PORT/health" 2>/dev/null | grep -q '"ok":true'; then
    ok "/health on 127.0.0.1:$PHONE_PORT answers ok"
  else
    fail "/health on 127.0.0.1:$PHONE_PORT did not answer ok"
  fi
else
  note "curl not installed; skipped /health"
fi

echo
if [ "$fails" -eq 0 ]; then echo "All checks passed."; else echo "$fails check(s) FAILED."; fi
exit "$fails"
