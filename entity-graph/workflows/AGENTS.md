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

[`docs/handoffs.md`](./docs/handoffs.md) is the long form; read it before changing the engine.

- An **item** has a **state** (where it is, amended by whoever last worked on it)
  and a **queue of handoffs** (what happens next, and so who does it).
- The first handoff is what the item waits for. When it resolves, its resolution
  is kept and the next one starts. That is how handoffs chain.
- Types: `human`, `agent`, `github`, `slack`, `items`, `script`.
- **Next** shows the item whose human handoff has waited longest.
- An agent's run ends by returning `{ state, next, close? }` as structured
  output. It has no tool for ending a run.
- A human handoff's `needed` is one direct question. An outward action is a human
  handoff's `approval`, which the human runs, changes, or turns down.

## Layout

```
src/engine     everything but the window; plain Node, so the tests drive it headlessly
  engine.ts      items, their queues of handoffs, history, the loop, agent output
  handoffs.ts    the handoff types
  actions.ts     the actions on items (github.ts and slack.ts add theirs)
  script.ts      script handoffs, in QuickJS
  schema.ts      JSON Schema from the zod shapes: forms, docs, the agents' output schema
  mcp.ts         the MCP server: the actions, in two scopes
  settings.ts    configuration in the store: Slack workspaces (tokens encrypted), the MCP port and token
  admin.ts       what the Integrations and MCP pages show and change
  workers.ts     the pool of `claude -p` workers
  github.ts      GitHub via the gh CLI: actions, the github handoff, a source
  slack.ts       Slack via the user tokens: actions, the slack handoff
  integrations.ts  the interfaces: Action, HandoffType, Source
  db.ts          the SQLite schema
src/main       Electron main: opens the store, starts the loop, pool and MCP, IPC
src/preload    the renderer's API (WorkflowsAPI in src/shared/types.ts)
src/renderer   the React UI
test/          engine.mts: the whole engine with a fake gh and fake agents
```

## Rules

- **The engine is the only writer.** Every change goes through `Engine` and emits
  `change`. Handoffs start in `advance`, and changes to one item run one at a time (`serial`).
- **Keep the whole history.** The `events` table is append-only, and `runs` keeps
  every run's prompt and full stream-json transcript. Resolved handoffs keep
  their resolutions, and the agent's own words are kept in the resolution's `output`. Agents read it back
  (`recent_items`, `get_item`, `run_transcript`) to find what to improve:
  slow steps, approvals the operator changed, work handled outside the tool.
  Record anything new that a review would want, and never delete or rewrite events.
- **The store is SQLite**, in `userData/workflows-v2.db`. The app uses
  `better-sqlite3`, which is rebuilt for Electron on `postinstall`. The tests use
  `node:sqlite`. `db.ts` uses only the API both share, with positional parameters.
- **One process.** The Electron main process holds the store, the loop (every
  minute), the worker pool (four workers) and the MCP server (loopback, a free port).
- **One list of actions** for both MCP scopes and for scripts. Every call goes
  through `Engine.callAction`, and an action checks who is calling:
  - Outward actions are for a human only.
  - An agent, or an agent's script, changes only its own item.
  - `hand_off` and `close_item` are for a human only.
  The scopes are `run` (a worker's agent, at `/mcp/<runId>` on a free port) and
  `operator` (anything the user connects, at `/mcp` on port 47390 by default,
  with a bearer token, acting as the human). Both bind to loopback only.
- **Agents get only the MCP server.** Workers run `claude -p` with built-in tools
  off, only `mcp__workflows` allowed, and the handoff schema as `--json-schema`.
  A new capability for agents is an action. Never add it to the agent's command line.
- **Adding an integration** means adding its actions (with `outward` where others
  will see them), a handoff type if it can be waited on, and a source if it
  produces items. Update `docs/handoffs.md` when a type changes. Run CLIs and
  HTTP through `engine.exec` and `engine.fetch`, so the tests can stand in for them.
- **Waits back off**: 1, 2, 5, then 10 minutes between checks.
