# Workflows — agent & contributor guide

An Electron + React + TypeScript app that gives the operator, at the press of a
button, the next thing that needs their decision, and hands everything else to
agents. What each workflow must achieve is in
[`workflow-descriptions/`](./workflow-descriptions): read the relevant one before
building anything for it.

## Never run the app

Do not start the app (`npm run dev`, `electron-vite`, `electron`) unless the user
asks. It opens the user's store and starts agent workers that spend real money.
Check work with `npm run typecheck`, `npm run build` and `npm test`.
`npx tsx test/smoke-claude.mts` does one real `claude -p` run, headless, against
an in-memory store; it costs a run, so use it only when the agent wiring changes.

## The paradigm

- An **item** is one thing that must be done. A **source** gives items.
- An open item always has exactly one **blocker**:
  - `operator`: it waits for the operator's decision.
  - `agent`: it is in the agent queue, or a worker runs it.
  - `external`: it waits for something outside the tool (a CI run, a reply, a time).
  - `items`: it waits for other items to close.
- Otherwise it is `closed`.
- **Next** shows the operator-blocked item that has waited longest.
- The operator dispatches it with an instruction, closes it, or records that they
  handled it outside the tool.
- A worker runs the item and must end its run by saying what blocks it now.
- An agent never does what other people can see. It asks for an **action**
  instead. The operator sees the form filled in, and then runs it, changes it, or
  does something else.
- Every item has a **state**: a short text, always kept current.

## Layout

```
src/engine     everything but the window; plain Node, so the tests drive it headlessly
  engine.ts      items, blockers, history, waits with backoff, the central loop
  mcp.ts         the MCP server the agents use, one URL per run (/mcp/<runId>)
  workers.ts     the pool of `claude -p` workers
  github.ts      GitHub via the gh CLI: reads, actions, waits, a source
  integrations.ts  the four kinds of integration: read, action, wait, source
  db.ts          the SQLite schema
src/main       Electron main: opens the store, starts the loop, pool and MCP, IPC
src/preload    the renderer's API (WorkflowsAPI in src/shared/types.ts)
src/renderer   the React UI
test/          engine.mts: the whole engine with a fake gh and fake agents
```

## Rules

- **The engine is the only writer.** Every change goes through `Engine`, which
  records an event for it and emits `change`. A blocker changes only in
  `setBlocker`.
- **Keep the whole history.** The `events` table is append-only, and `runs` keeps
  every run's prompt and full stream-json transcript. Agents read it back
  (`recent_items`, `item_history`, `run_transcript`) to find what to improve:
  slow steps, approvals the operator changed, work handled outside the tool.
  Record anything new that a review would want, and never delete or rewrite events.
- **The store is SQLite**, in `userData/workflows.db`. The app uses
  `better-sqlite3`, which is rebuilt for Electron on `postinstall`. The tests use
  `node:sqlite`. `db.ts` uses only the API both share, with positional parameters.
- **One process.** The Electron main process holds the store, the loop (every
  minute), the worker pool (four workers) and the MCP server (loopback, a free port).
- **Agents get only the MCP server.** Workers run `claude -p` with built-in tools
  off and only `mcp__workflows` allowed. A new capability for agents is a tool in
  `mcp.ts`, or an integration in the registry. Never add it to the agent's command line.
- **Adding an integration** means adding entries to the registry in `engine.ts`:
  - a read (an agent may call it at any time),
  - an action (needs the operator's approval),
  - a wait kind (`check` answers null while still waiting),
  - a source (polled every `everyMinutes`).
  Run CLIs through the `Exec` you are handed, so the tests can stand in for them.
- **Waits back off**: 1, 2, 5, then 10 minutes between checks.
