# Claude

`modules/claude/` — `view.ts` (pure), `claude.ts` (sessions, prompts, `claude -p`).

## Items (all owned)

| Type | Is | Under |
|-|-|-|
| `claude.home` | the module's root; holds `cwds` (directories used, newest first) | the sidebar |
| `claude.session` | one Claude Code session; **its id is Claude's session id** | the item it was started from, and `claude` |
| `claude.prompt` | what I asked | the item I asked from, and its session |
| `claude.response` | Claude's answer (`running` while it works, `error` if it failed) | its prompt |

A session's data: `text` (its name), `cwd` (where it runs), `worktree` and
`branch` (if it made one), `repo`, `permissionMode`, `started`. The item a
session was started from gets `claudeSessionId`.

So: `[item + session] → [prompt] → [response]`.

## Keys

- **Shift+K** on a row: the new-session form. Name; directory (blank: a new
  temporary one; the last one used is filled in, earlier ones offered); in a
  new worktree (only with a directory). Enter starts it, Escape gives up.
- **k** on a row: a prompt, in the same box as a new note. Its session is the
  first item on the path from the view's root to the row, nearest first, that
  is a session or has `claudeSessionId`. None: a toast says so.

## Running

- **Directory.** None: `mkdtemp(<tmp>/claude-…)`. Given, no worktree: that
  directory. Worktree: `git worktree add -b claude/<name>-<id8>
  <data dir>/worktrees/<repo>-<id8>` from the directory's repo; the app's data
  directory is in no repo, so nothing commits it.
- **Prompting.** `claude -p <prompt> --output-format json --permission-mode
  <mode>`, with `--session-id <id>` the first time and `--resume <id>` after,
  in the session's directory. One prompt at a time per session, in order.
  The answer (`result`) fills the response; the cost is kept as `cost`.
- **Permissions.** `acceptEdits` by default: edits are allowed, anything else
  that would ask is refused, since `-p` has nobody to ask. Per session
  (`permissionMode`).
- **PRs.** After each prompt, the branch checked out in the session's
  directory is looked up on GitHub (a GraphQL query, read-only); if it has a
  PR, the PR is linked under the session, once.
- What Claude writes back is authored `claude`, so Ctrl+Z (my own edits)
  never takes an answer away.
