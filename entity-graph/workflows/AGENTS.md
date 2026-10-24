# Workflows — agent & contributor guide

An Electron + React + TypeScript app that gives the operator, at the press of a
button, the next thing that needs their decision, and hands everything else to
bots. What each workflow must achieve is in
[`workflow-descriptions/`](./workflow-descriptions): read the relevant one before
building anything for it.

## Never run the app

Do not start the app (`npm start`, `npm run dev`, `electron-vite`, `electron`)
unless the user asks. It opens the user's store and starts bot workers that spend real money.
Check work with `npm run typecheck`, `npm run build` and `npm test`.
`npx tsx test/smoke-claude.mts` does one real `claude -p` run, headless, against
an in-memory store; it costs a run, so use it only when the bot wiring changes.

## Running it (for the user)

`npm start` builds once and runs the built app, watching nothing: bots can edit
and build the code (in their worktrees) without restarting the app under
themselves. `npm run dev` hot-reloads, and restarts the app, and every bot in
it, when main-process code changes.

## The paradigm

[`docs/paradigm.md`](./docs/paradigm.md) is the idea, in a page.
[`docs/events.md`](./docs/events.md) is how it is built; read it before changing the engine.

- Everything is an **event**, items included: one tree. An item's children are
  its history. Some events resolve as they are added (a message); **blockers**
  wait until something happens (a bot answers, CI finishes, a child item closes).
- An item can have many blockers at once. It is the human's turn when nothing
  blocks it but approvals.
- `after` events chain: they wait for one event, then add others, as given or as
  a script decides.
- The chat input sends through presets (a plain message by default, and a bot when sent empty; its icon and hint
  show which); Tab
  steps through them.
- Each item has its own Claude session, kept in its params; a child item's
  first session is forked from its parent's. A bot is told what is new since it
  was last prompted, with little direction (the human's messages as written, anything else in [brackets], the
  item's id every ten lines; `promptFor` in `workers.ts`), and answers with `{ message?, events?, close? }` as structured output.
- Nothing about the running app is stored: what is running is held in memory.

## Terminology

- **Item** is the general word, for agents and in the code, for anything in the tree: every event is an item.
  *Item* and *event* name the same thing; *item* usually means the parent in a given view, *event* one of its children.
- **Task** is a particular kind of item, the `item` event type: a thing to be done, which is especially important.
  The UI says *task* for these; the code, store and MCP tools keep `item`.

## The unsafe rule

**An action that changes anything outside this app is `unsafe: true`, and no
agent is ever given one.** That covers posting, commenting, reviewing, merging,
sending, deploying and deleting, in any integration.

- `unsafe` is a required field on `Action`, so every new action has to declare it.
  If in doubt, it is unsafe.
- Unsafe actions are part of the toolset, but whenever an agent is given the
  tools they are left out: both MCP endpoints (a bot's, and the fixed one any
  other agent connects to) offer every action but the unsafe ones. Scripts that
  an agent wrote get none either. `Engine.callAction` refuses them to any caller
  that is not the human.
- Only the human runs one: through an `approval` event, in the app. An agent asks
  for one by adding an approval; `unsafe_actions` tells it what it can ask for.
- No event may act outside the app when an agent adds it. The `slack` event's
  message is an example: when an agent adds one, an approval goes in front of it.
- **Bots have a shell** (auto mode), and `gh` is logged in. The rule binds them
  there too: their instructions forbid doing anything unsafe with it, and the
  auto-mode classifier stands between them and it. That is a weaker guarantee
  than the MCP's, so give bots no new credentials lightly.
- `test/engine.mts` checks the rule. Keep those checks passing; never loosen them.

## Layout

```
src/engine     everything but the window; plain Node, so the tests drive it headlessly
  engine.ts      items, their events and blockers, the trace, the loop, bot output
  eventTypes.ts  the event types
  presets.ts     what the chat input can send
  actions.ts     the actions on items (github.ts and slack.ts add theirs)
  script.ts      after events' scripts, in QuickJS
  schema.ts      JSON Schema from the zod shapes: forms, docs, the bots' output schema
  mcp.ts         the MCP server: the actions, in two scopes
  settings.ts    configuration in the store: Slack workspaces (tokens encrypted), the MCP port and token
  admin.ts       what the Integrations and MCP pages show and change
  workers.ts     the pool of `claude -p` workers
  github.ts      GitHub via the gh CLI: actions, the github event, a source
  slack.ts       Slack via the user tokens: actions, the slack event
  integrations.ts  the interfaces: Action, EventType, Source
  db.ts          the SQLite schema
src/main       Electron main: opens the store, starts the loop, pool and MCP, IPC
src/preload    the renderer's API (WorkflowsAPI in src/shared/types.ts)
src/renderer   the React UI
test/          engine.mts: the whole engine with a fake gh and fake bots
```

## UI

- **The UI says *task*; everything else says *item*.** The words on screen call an item a task. The code, the
  store, the event type and the MCP tools (`list_items`, `get_item`, …) keep `item`.
- **Two kinds of form.**
  - The forms used all day are condensed: the new-item form, and the chat input
    (`Composer`: one growing line, its preset's icon and hint, Tab between presets,
    Enter to send, Ctrl+Enter twice to resolve the task; what is typed is kept per item in
    `draftsAtom`, in local storage, until it is sent). Keep them tight.
  - Everything else (approvals, settings) uses `SchemaForm`, through
    `Form` in `forms.tsx`, which adds the app's own editors (`secret` for tokens).
    `withHints` gives the engine's schemas text boxes for prose fields.
  - `SchemaForm` and `SchemaBuilder` are copied from entity-graph
    (`components/schema/`, with the `ui/` primitives they need and its `index.css`
    tokens). Keep them in step with entity-graph rather than editing them here.
- **The same design rules as entity-graph:** no motion, no pointer cursor,
  restrained colour, tone rather than borders. Tailwind v4 provides the tokens.
  The app's own CSS (`styles.css`) sits in `@layer components`, so the
  components' utilities win over it. Its buttons are `button[data-ui]`, so it
  never styles the components' buttons.

- **Styles on trial.** For now the menu switches between three themes (Linear, the default, dark; Notebook, light
  and warm; Terminal, dark and in Fira Code) and five serifs for what the human typed (their messages, the chat
  input, task titles). `appearance.ts` keeps the choice and sets `data-theme`/`data-serif` on `<html>`;
  `themes.css` holds the overrides. Code is Fira Code everywhere; weights lean light.
- **The content pane is a stack of views** (`views.ts`, a Jotai atom kept in
  local storage). The list and Next replace the stack; Open, Debug and links push
  onto it; Back pops it. A view is `{ kind, id }`: `event` shows an event as its
  type is shown (an item as its chat, any other event as `EventView`); `debug`
  shows its whole state as JSON. A new kind of view is a new `kind`.
- **There is no sidebar.** The content fills the window in a centred column.
  The bottom-left button opens the menu (bots busy; Tasks, Integrations, MCP,
  which are views on the stack like any other; poll sources), and the round
  bottom-right button is Next (Ctrl+D).
- **The items view** (`ItemsList`) is what shows when the stack is empty, and
  Ctrl+L puts it on top (or, when it is already showing, puts the cursor in its
  search line); a view is never pushed onto an identical one; Ctrl+W and Alt+Left take the top view off, and Alt+Right puts the
  last one taken off back on (`forwardAtom`, cleared by going anywhere new); the main process catches them in
  `before-input-event`, so the window's menu doesn't close the window. Typing filters titles fuzzily; the arrows
  move; Enter opens; Tab makes a new item with the typed title. What the items view opens *replaces* it on the stack
  (`open(view, { replace: true })`), so Back goes to where you were before it rather than to the list.
- **Text is markdown.** Messages, descriptions and code samples all go through `Markdown` (`components/ui/`, react-markdown
  with GFM; fences drawn by `CodeBlock`). Code that is not inside prose goes in as `fenced(code, language)`.
- **Picking an item** is `ItemPicker`: the items view itself in a `Modal` over the blurred window, calling back with
  the item picked (Enter or a click) or made (Tab). The paperclip at the top right of every event's view uses it to
  attach that event, whatever it is, to a task (`Engine.attach`, an `attachment` event in the task).
- **Ctrl+F finds in the view.** On the items view it is the search line; over a chat it opens `FindLine`, and the
  chat keeps only the events whose *displayed* text (`shows`, markdown stripped) has the phrase, marked in yellow
  (`Markdown`'s `highlight`). The phrase (`findAtom`) belongs to the view and is cleared when the view changes.
- **A child task is a card** (`TaskCard`): its title, with a tick once resolved, a cross if cancelled, a pending
  ring while open. While open it is among the blockers; once closed it is a chat bubble like any other event.
- **When a task stops needing you, the app moves on.** Whatever the human does to the task on screen (sends, approves,
  resolves, from the chat, header or menu) goes through `actOn(id)`: afterwards, if the task is resolved or blocked by
  more than approvals, the next one that needs them is *pushed* (so Back returns). A plain message leaves it be.
- **The Types page** (`TypesPage`) lists every event type with its actions. The app's own are in `eventActions.ts`;
  the human's own are **custom actions** (`engine/customActions.ts`, the `custom_actions` table): per type, a name, a
  description, hidden notes, an icon, *important* or not, and a script `async (context, actions) => null | string |
  { message?, events? }` run in QuickJS with the event's context and only the safe actions. **Generate** has Sonnet write
  the script from the description and notes. A run is an `action` event in the item, saying what it returned. Important
  actions are buttons in an event's header; the rest are in the ⋯ menu beside them; all are in the right-click menu.
  Agents list and run them with `custom_actions` and `run_custom_action`.
- **Ctrl+S summarises the task on screen**: a `summary` event, which Haiku (no tools, no session) answers in a few
  seconds with where the task stands now, in ASD-STE100; it is said in the chat like any event. Bots' prompts skip it.
- **Priority items** (a `priority` event; `Item.priority`) come first in Next, the most recently active first. Mark
  one from its actions.
- **Every event has a right-click menu** (`useEventMenu`): Open and Debug, then what can be done to it
  (`eventActions.ts`: Resolve, Done outside, Skip for an open task; Cancel for another open event; Attach). The view
  header draws the same list as icon buttons, so the two never differ. Spread
  it on anything that stands for an event. Clicking an event does nothing else.

## Rules

- **The engine is the only writer.** Every change goes through `Engine` and emits
  `change`. Events are added in `add` and resolved in `resolve`, and checks and
  starts on one item run one at a time (`serial`).
- **Trace everything.** The aim is complete traceability: what happened, when,
  and who or what did it.
  - The `trace` table records every moment, one row each: an item created; an
    event added, checked, resolved or cancelled; a view or skip; a bot
    run started and ended; every action called, by whom, in which run, with its
    arguments, its result (ok or error) and how long it took.
  - `ItemEvent.times` is read from the trace, never stored apart from it.
  - The `events` table is each item's history: rows are added and resolved,
    never otherwise changed or deleted, and a resolved event keeps its data (a
    bot's own words in `output`). `runs` keeps every run's prompt and full
    stream-json transcript.
  - The trace and the runs are append-only: never update or delete a row.
  - Anything new that changes, starts, ends or is seen goes through
    `Engine.mark`. Bots read the trace back (`recent_items`, `get_item`,
    `run_transcript`) to find what to improve.
- **The store is SQLite**, in `userData/workflows-v4.db`. The app uses
  `better-sqlite3`, which is rebuilt for Electron on `postinstall`. The tests use
  `node:sqlite`. `db.ts` uses only the API both share, with positional parameters.
- **One process.** The Electron main process holds the store, the loop (every
  minute), the worker pool (four workers) and the MCP server (loopback, a free port).
- **One toolset for every agent.** Both MCP endpoints serve every action but the
  unsafe ones, and every call goes through `Engine.callAction`. Tools about an
  item take its id, so an agent may act on any item, its own included. Agents
  are author `bot`, so the bots' rules (which event types, Slack approvals) apply.
  The endpoints are `run` (a worker's bot, at `/mcp/<runId>` on a free port; it
  only ties calls to the run, for the trace) and `operator` (any other agent, at
  `/mcp` on port 47390 by default, with a bearer token). Both bind to loopback only.
- **Bots run in auto mode, each worker in its own folder.** Workers run
  `claude -p --permission-mode auto` with the usual tools and the workflows MCP
  server, the output schema as `--json-schema`, and the item's own session
  (`--session-id`, then `--resume`; sessions are found by id from any folder).
  Each worker runs in `<tmpdir>/workflows-bots/worker-N` and makes git worktrees
  as it needs them. A new capability for bots that touches this app is an action.
- **Adding an integration** means adding its actions (each declaring `unsafe`), an event type if it can be waited on, a preset if the chat
  input should offer it, and a source if it produces items. Update `docs/events.md`
  when a type changes. Run CLIs and
  HTTP through `engine.exec` and `engine.fetch`, so the tests can stand in for them.
- **Waits back off**: 1, 2, 5, then 10 minutes between checks.
