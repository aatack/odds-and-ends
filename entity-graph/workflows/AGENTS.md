# Workflows — agent & contributor guide

An Electron + React + TypeScript app that gives the operator, at the press of a
button, the next thing that needs their decision, and hands everything else to
bots. What each workflow must achieve is in
[`workflow-descriptions/`](./workflow-descriptions): read the relevant one before
building anything for it.

## Never run the app

Do not start the app (`npm start`, `npm run dev`, `electron-vite`, `electron`)
unless the user asks. It opens the user's store and starts bots that spend real money.
The same goes for the backend on its own (`npm run backend`, `out/main/backend.js`)
on the user's source; on a temporary `--file` it is safe.
Check work with `npm run typecheck`, `npm run build` and `npm test` (`test/egdb.mts`
needs the egdb-server binary: `npm run egdb:build`; it is skipped without it).
`npx tsx test/smoke-claude.mts` does real `claude -p` runs, headless, against
an in-memory store; it costs runs, so use it only when the bot wiring changes.

## Running it (for the user)

`npm install`, then `npm start`: it builds egdb-server the first time (with
rustup's cargo where there is one; `npm run egdb:build` builds it again; or set
`EGDB_SERVER` to a binary), builds the app once and runs it, watching nothing:
bots can edit and build the code (in their worktrees) without restarting the app
under themselves. `npm run dev` hot-reloads the window.

The app asks first for a **username** and a **source**: a file path (`~/` is the
home folder), made if it is not there, which is the store (egdb's SQLite). Both
are kept in local storage, so the next start opens them at once; the menu signs
out of either. Beside the source, `<source>.data/` holds the rest: the secrets'
key, the bots' folders, and the running backend's `backend.json` and lock.

The backend is a process of its own, on one source. The app uses one already
running on the source (its `backend.json` says where); otherwise it starts one,
with Electron's own Node, and stops it as it leaves the source or quits.
`npm run backend -- --file <source>` runs one on its own (and `--port <n>`),
which lasts while apps come and go: then restarting the app restarts no bot.

The app's Electron profile is `workflows-egdb` (not the older app's `workflows`),
and its MCP endpoint's port is 47391 by default (the older app's is 47390), so
the two run side by side; it reads nothing of the older app's.

## The paradigm

[`docs/paradigm.md`](./docs/paradigm.md) is the idea, in a page.
[`docs/events.md`](./docs/events.md) is how it is built; read it before changing the engine.

- Everything is an **event**, items included: one tree. An item's children are
  its history. Some events resolve as they are added (a note); **blockers**
  wait until something happens (a script says it is done, an approval, a child item closes).
  There are six types: `item`, `note`, `approval`, `script`, `bot` and `user` (a person).
- **People** are items too (`user` events, in no item, whose id is their username); they sign in to the
  app by typing their username (no password: the app is local). Items are **assigned** to people and
  bots: an `assigned_to` link from the item, not an event (`Engine.setAssignee`; the items someone has are
  their links followed backwards). An item can have many blockers at once. It is a person's turn when they
  have it and nothing blocks it but approvals. Next goes through those, the least recently touched first.
- A **bot** is an item of its own (a `bot` event, in no item, its settings in its params): a name,
  instructions, a provider and model (opus by default), a debounce (a minute by default), and a folder and
  Claude session of its own, which last. Notes in it are its own chat. It wakes only when something happens
  to it: an item is assigned to it, its settings change, or someone writes in its chat (its debounce after);
  and every 15 minutes while its items have news (`Engine.wake`). It runs one thing at a
  time: a run is asked to action every item it has and give each a new assignee, and told what is new in
  its chat and in each (`promptFor` in `workers.ts`: people's notes as written, after their name; anything else in
  [brackets]; each item's id every ten lines). It answers with
  `{ updates: [{ item, message?, events?, assign?, close? }] }` as structured output. An item it says
  nothing of who has goes back to its owner, unless it is left waiting on something (`Engine.settle`).
- A `script` waits for anything: a function run on a schedule until it says it is done. Custom actions
  (the Types page) are scripts too, run on an event when asked.
- The chat input sends a note by default; `@` a bot or a person in it to hand them the task (and take it
  off the sender). With nothing typed it offers "Assign to @someone", the likeliest first. Tab steps through the
  choices; its icon and hint show which.
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
- No event may act outside the app when an agent adds it.
- For now the app has no unsafe actions of its own (the integrations are gone, to come back as types);
  the tests add one to check the rule.
- **Bots have a shell** (auto mode), and `gh` is logged in. The rule binds them
  there too: their instructions forbid doing anything unsafe with it, and the
  auto-mode classifier stands between them and it. That is a weaker guarantee
  than the MCP's, so give bots no new credentials lightly.
- `test/engine.mts` checks the rule. Keep those checks passing; never loosen them.

## Layout

```
src/engine     everything but the window; plain Node, so the tests drive it headlessly
  engine.ts      items, their events and blockers, assignments, bots' wakes and output, the trace, the loop
  store.ts       the store, in memory; every change goes on as egdb ops
  egdb.ts        the store's home: writing to egdb, reading it back, running egdb-server
  eventTypes.ts  the event types
  presets.ts     what the chat input can send
  actions.ts     the actions on items: the MCP tools
  script.ts      scripts, in QuickJS
  schema.ts      JSON Schema from the zod shapes: forms, docs, the bots' output schema
  mcp.ts         the MCP server: the actions, in two scopes
  settings.ts    configuration in the store: the MCP port and token
  admin.ts       what the Integrations and MCP pages show and change
  workers.ts     the bots' runner: one run at a time per bot, by its provider (`claude -p`)
  integrations.ts  the interfaces: Action, EventType
src/backend    the backend as a process: an HTTP API (api.ts) and a change stream for the app;
               info.ts, where a source keeps its things
src/main       Electron main: finds or starts the backend, passes the renderer's calls to it
src/preload    the renderer's API (WorkflowsAPI in src/shared/types.ts)
src/renderer   the React UI
test/          engine.mts: the whole engine with a fake gh and fake bots;
               egdb.mts: the store against a real egdb-server, the backend process on a fresh source
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
  and warm; Terminal, dark and in Fira Code) and five serifs (or none: the UI's own font) for what the human typed (their messages, the chat
  input, task titles). each in a light or a dark mode. `appearance.ts` keeps the choice and sets `data-theme`/`data-serif`/`data-mode` (and `.dark`) on `<html>`;
  `themes.css` holds the overrides. Code is Fira Code everywhere; weights lean light.
- **The content pane is a stack of views** (`views.ts`, a Jotai atom kept in
  local storage). The list and Next replace the stack; Open, Debug and links push
  onto it; Back pops it. A view is `{ kind, id }`: `event` shows an event as its
  type is shown (an item as its chat, any other event as `EventView`); `debug`
  shows its whole state as JSON. A new kind of view is a new `kind`.
- **There is no sidebar.** The content fills the window in a centred column.
  The bottom-left button opens the menu (who is signed in, on which source; bots working; Tasks, Bots,
  Integrations, Types, MCP, which are views on the stack like any other; Sign out; Close source). Next has no button: it is Ctrl+D, and the app moves on
  by itself once the task on screen no longer needs you.
- **The items view** (`ItemsList`) is what shows when the stack is empty, and
  Ctrl+L puts it on top (or, when it is already showing, puts the cursor in its
  search line); a view is never pushed onto an identical one; Ctrl+W and Alt+Left take the top view off, and Alt+Right puts the
  last one taken off back on (`forwardAtom`, cleared by going anywhere new); the main process catches them in
  `before-input-event`, so the window's menu doesn't close the window. Typing filters titles fuzzily; the arrows
  move; Enter opens; Tab makes a new item with the typed title; Escape with nothing typed goes Back. What the items view opens *replaces* it on the stack
  (`open(view, { replace: true })`), so Back goes to where you were before it rather than to the list.
- **The finder** (`ItemsList`) is the items view and every picker: items in **sections** (`sections.ts`: recently
  viewed, needs you, with bots, waiting, closed today), each instance choosing which, in what order,
  and how many rows each shows before an "n more" row (Enter or a click shows the rest). The Tasks view shows 12 a
  section; pickers in a modal, 6. Recently viewed comes from `Overview` (`Engine.inHand`: `viewed` trace rows). A
  row's grey text says only what its icon can't (what a script waits for); "needs you" and "waits for a task" are
  the icon's job.
- **Text is markdown.** Messages, descriptions and code samples all go through `Markdown` (`components/ui/`, react-markdown
  with GFM; fences drawn by `CodeBlock`). Code that is not inside prose goes in as `fenced(code, language)`.
- **Picking** is `ItemPicker`: the finder in a `Modal` over the blurred window, calling back with the item (or
  bot, or person) picked (Enter or a click) or made (Tab). With `multiple`, its `value` and `setValue` are a list:
  what is selected shows first, ticked; picking a row adds it, or takes it off.
- **Who has a task** is a pill at the top right of its view (`AssigneePill`): their name, or with more than one,
  their pictures. Clicking it opens the multiselect picker over people and bots.
- **`@` in the chat input refers to a bot, a person or a task**: typed at the start of a word, it opens the finder
  (`ItemPicker`), and what is picked (or made, with Tab) takes its place as `[id]()`, and the cursor goes back to
  the chat input. `Markdown` draws such a link, to nowhere with an
  item's id as its text, through `ItemRefContext`: the app gives `ItemRef`, the item's icon and title, which open it.
- **Ctrl+F finds in the view.** On the items view it is the search line; over a chat it opens `FindLine`, and the
  chat keeps only the events whose *displayed* text (`shows`, markdown stripped) has the phrase, marked in yellow
  (`Markdown`'s `highlight`). The phrase (`findAtom`) belongs to the view and is cleared when the view changes.
- **A child task is a card** (`TaskCard`): its title, with a tick once resolved, a cross if cancelled, a pending
  ring while open. While open it is among the blockers; once closed it is a chat bubble like any other event.
- **When a task stops needing you, the app moves on.** Whatever the human does to the task on screen (sends, approves,
  resolves, from the chat, header or menu) goes through `actOn(id)`: afterwards, if the task is resolved or blocked by
  more than approvals, the next one that needs them is *pushed* (so Back returns). A plain note leaves it be.
- **The Types page** (`TypesPage`) lists every event type: what it is for, whether it blocks, its fields (the
  Fields button: its params' schema and what it resolves with), and its actions. The app's own are in `eventActions.ts`;
  the human's own are **custom actions** (`engine/customActions.ts`, the `custom_actions` table): per type, a name, a
  description, hidden notes, an icon, *important* or not, and a script `async (context, actions) => null | string |
  { message?, events? }` run in QuickJS with the event's context and only the safe actions. **Generate** has Sonnet write
  the script from the description and notes. What a run says is a note in the item, by `script`. Important
  actions are buttons in an event's header; the rest are in the ⋯ menu beside them; all are in the right-click menu.
  Agents list and run them with `custom_actions` and `run_custom_action`.
- **Starting** (`Start`, before anything else): a username and a source, both needed. Opening the source
  starts (or finds) its backend; signing in makes the person if there is nobody by that username yet. Both
  are kept in local storage (`userAtom`, `sourceAtom`), and the preload sends the username with every call.
  The menu has Sign out and Close source (which stops the source's backend, if the app started it).
  Everything is as that person sees it: `withYou`, Next, the recent lists. Another source starts with an
  empty stack of views.
- **Bots** (`BotsPage`, in the menu) lists the bots, with what each has and whether it is working or when it
  wakes, and makes new ones (a name and instructions; the rest has defaults). A bot opens as an `event` view:
  `BotView` at the top (its model, debounce and state; Settings, folded, as a `Form`, saving them wakes it; Wake
  now; its last runs), then its own chat, like a task's. The header's Delete hands what it had back.
- **Every event has a right-click menu** (`useEventMenu`): Open and Debug, then what can be done to it
  (`eventActions.ts`: Resolve and Skip for an open task; Cancel (Delete, for a bot) for another open event; then the
  custom actions). The view header draws the same list as icon buttons (the less important in the ⋯ menu), so the
  two never differ. Spread
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
- **The store is egdb.** The backend runs an egdb-server of its own on the
  source's file (loopback, a token). The engine keeps the store in memory
  (`store.ts`) and reads it synchronously; every change is also queued as egdb ops
  and written in order (`EgdbSink`). The tests use the store with no sink.
- **The backend is one process.** It holds the store, the loop (every minute),
  the bots' runner and the MCP server; the app is a window onto it, over HTTP
  (`/rpc/<method>` and `/events`, loopback, a token). Every call the renderer
  makes is a WorkflowsAPI method listed in `API_METHODS` and answered in
  `src/backend/api.ts`, as the person in `x-workflows-user` (only signing in works without one). Secrets are encrypted under
  `<source>.data/secret.key`.
- **One toolset for every agent.** Both MCP endpoints serve every action but the
  unsafe ones, and every call goes through `Engine.callAction`. Tools about an
  item take its id, so an agent may act on any item, its own included. Agents
  are author `bot`, so the bots' rules (which event types they may add) apply.
  The endpoints are `run` (a bot's run, at `/mcp/<runId>` on a free port; it
  ties calls to the run and its bot, traced in the bot, and records what it adds as `bot:<id>`) and `operator` (any other agent, at
  `/mcp` on port 47391 by default, with a bearer token). Both bind to loopback only.
- **Bots run in auto mode, each in its own folder.** A Claude bot runs
  `claude -p --permission-mode auto` with the usual tools and the workflows MCP
  server, the output schema as `--json-schema`, its model, its own session
  (`--session-id`, then `--resume`), and the app's instructions and its own as
  its system prompt (`--append-system-prompt`), not in any prompt; a
  SessionStart `compact` hook says them again after a compaction. Its folder is
  `<data>/bots/<name>-<id>` unless it says otherwise, and lasts. Another provider
  is another `SpawnBot` in `PROVIDERS`. A new capability for bots that touches this app is an action.
- **Adding an integration** means adding its actions (each declaring `unsafe`), an event type if it can be waited on, a preset if the chat
  input should offer it, and a source if it produces items. Update `docs/events.md`
  when a type changes. Run CLIs and
  HTTP through `engine.exec` and `engine.fetch`, so the tests can stand in for them.
- **Waits back off**: 1, 2, 5, then 10 minutes between checks.
