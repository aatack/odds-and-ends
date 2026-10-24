# Workflows — agent & contributor guide

An Electron + React + TypeScript app that gives the operator, at the press of a
button, the next thing that needs their decision, and hands everything else to
bots. What each workflow must achieve is in
[`workflow-descriptions/`](./workflow-descriptions): read the relevant one before
building anything for it.

## Never run the app

Do not start the app (`npm run dev`, `electron-vite`, `electron`) unless the user
asks. It opens the user's store and starts bot workers that spend real money.
Check work with `npm run typecheck`, `npm run build` and `npm test`.
`npx tsx test/smoke-claude.mts` does one real `claude -p` run, headless, against
an in-memory store; it costs a run, so use it only when the bot wiring changes.

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
- The chat input sends through presets (a message plus a bot, by default); Tab
  steps through them.
- Each item has its own Claude session, kept in its params; a child item's
  first session is forked from its parent's. A bot is told what is new since it
  was last prompted, and answers with `{ message?, events?, close? }` as structured output.
- Nothing about the running app is stored: what is running is held in memory.

## The unsafe rule

**An action that changes anything outside this app is `unsafe: true`, and bots
never get it.** That covers posting, commenting, reviewing, merging, sending,
deploying and deleting, in any integration.

- `unsafe` is a required field on `Action`, so every new action has to declare it.
  If in doubt, it is unsafe.
- Bots, and scripts that bots wrote, never get an unsafe action. The run-scope MCP
  server leaves them out, and `Engine.callAction` refuses them to any caller that
  is not a human.
- A bot asks for an unsafe action by adding an `approval` event.
  `unsafe_actions` tells it what it can ask for.
- Nothing may give a bot another way round this: no shell, no HTTP tool, no
  built-in Claude Code tools, and no event that acts outside the app when a bot
  adds it. The `slack` event's message is an example: when a bot adds one, an
  approval goes in front of it.
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
  settings.ts    configuration in the store: Slack workspaces (tokens encrypted), the MCP and phone ports and tokens
  admin.ts       what the Integrations, MCP and Phone pages show and change
  workers.ts     the pool of `claude -p` workers
  github.ts      GitHub via the gh CLI: actions, the github event, a source
  slack.ts       Slack via the user tokens: actions, the slack event
  integrations.ts  the interfaces: Action, EventType, Source
  server.ts      the phone server: the PWA shell + a JSON API + an SSE feed, on loopback; its allowlist (PHONE_METHODS)
  db.ts          the SQLite schema
src/main       Electron main: opens the store, starts the loop, pool, MCP and the phone server
  handlers.ts    one named table of API methods, shared by IPC and the phone server
src/preload    the renderer's API (WorkflowsAPI in src/shared/types.ts)
src/renderer   the React UI
  src/transport/  the phone's HTTP+SSE backing for WorkflowsAPI (when window.workflows is absent)
  public/         the PWA shell copied verbatim: manifest, service worker, icons
test/          engine.mts: the whole engine with a fake gh and fake bots; server.mts: the phone server, headless
```

## UI

- **Every UI change must work on the phone too.** The phone PWA is this same
  renderer (see [The phone](#the-phone-mobile-pwa)), so anything you add has to
  hold up at phone widths (check ~375px) and by touch: no hover-only,
  right-click-only or keyboard-only affordance without a touch equivalent; 16px
  inputs on coarse pointers (so iOS does not zoom); safe-area insets on anything
  pinned to an edge. **The UI is shared:** new UI goes in the shared renderer,
  never a mobile-only copy. Branch on `isPhone` only for what truly differs —
  login, the HTTP transport, the service worker, and desktop-only pages.
- **Two kinds of form.**
  - The forms used all day are condensed: the new-item form, and the chat input
    (`Composer`: one growing line, its preset's icon and hint, Tab between presets,
    Enter to send). Keep them tight.
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

- **The content pane is a stack of views** (`views.ts`, a Jotai atom kept in
  local storage). The list and Next replace the stack; Open, Debug and links push
  onto it; Back pops it. Back is a visible header button, so it works by touch;
  on the phone the system Back pops the stack too (one guard history entry, armed
  while the stack is non-empty, so Android's back gesture stays in the app). A view is `{ kind, id }`: `event` shows an event as its
  type is shown (an item as its chat, any other event as `EventView`); `debug`
  shows its whole state as JSON. A new kind of view is a new `kind`.
- **There is no sidebar.** The content fills the window in a centred column.
  The bottom-left button opens the menu (bots busy; Items, Integrations, MCP,
  which are views on the stack like any other; poll sources), and the round
  bottom-right button is Next.
- **The items view** (`ItemsList`) is what shows when the stack is empty, and
  Ctrl+L puts it on top (or, when it is already showing, puts the cursor in its
  search line); a view is never pushed onto an identical one; Ctrl+W takes the top view off (the main process catches
  it in `before-input-event`, so the window's menu doesn't close the window) — a desktop-only shortcut, since the phone uses Back. Typing filters titles fuzzily; the arrows move; Enter
  opens; Tab makes a new item with the typed title.
- **Every event has a menu** (`useEventMenu`): Open and Debug, opened by
  right-click or, on touch, a long-press. Spread it on anything that stands for
  an event. Clicking an event does nothing else.

## The phone (mobile PWA)

The phone is this **same renderer**, served over HTTP with an SSE change feed in
place of IPC — no second app and no mobile-only copy. The main process serves it
(`server.ts`) on `127.0.0.1:47391`, **off by default**; turn it on from the
**Phone** page in the menu (desktop-only), which also shows the loopback URL, the
tailnet URL, the token, and the exact `tailscale serve` command. The renderer
picks the HTTP transport when `window.workflows` is absent, and `Login` collects
the token (kept in `localStorage`; a connect link carries it in the fragment).

Reached over Tailscale, which gives an HTTPS origin (so a secure context, so the
service worker registers and the app is installable). Setup:

- **On the laptop, once:** install Tailscale, `sudo tailscale up`, then
  `sudo tailscale set --operator=$USER` (without it every serve command needs
  `sudo` — reading the config never does, which is the confusing part). In the
  admin console enable **MagicDNS** and **HTTPS Certificates**. Then turn **Phone**
  on and run the shown `tailscale serve --bg --https=8443 http://127.0.0.1:47391`
  (or press **Run it**).
- **On the phone:** the Tailscale app, signed into the same account; open the
  connect link, or `https://<host>.<tailnet>.ts.net:8443/` and paste the token;
  then **Add to Home Screen** (iOS Safari) / **Install** (Android Chrome) —
  **reload once first**, or the install criteria aren't yet met and nothing is
  offered.

Quirks, carried over from `entity-graph/mobile/README.md`:

- Without the `--operator` setting, `tailscale serve status` works but every
  change fails with `Access denied: serve config denied`.
- The serve mount is its own HTTPS port (8443), so it sits beside entity-graph's
  mobile mount at `/` rather than replacing it. `--bg` keeps it across reboots.
- Turning Phone off stops the server; the mount stays and answers 502, which is
  harmless. To remove it, `tailscale serve --https=8443 off`, then check
  `tailscale serve status`: some versions ignore `off` and print their help.
  If yours does, `tailscale serve reset` and add back any other mounts.
- The service worker registers **only in a secure context**, so never under
  Electron or over plain HTTP. It caches the shell only, never `/api`.
- Regenerate the token on the Phone page to **cut a phone off**; it takes effect
  at once.

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
  minute), the worker pool (four workers), the MCP server (loopback, a free port)
  and the phone server (loopback, `47391` by default, off until turned on).
- **The phone server is the window's surface, no more.** It binds `127.0.0.1`
  only (reached from a phone over Tailscale, never bound beyond loopback), and
  dispatches over an explicit allowlist (`PHONE_METHODS` in `server.ts`) — exactly
  the `WorkflowsAPI` *data* methods the window already has, never a generic
  action-calling endpoint. Its bearer token is distinct from the MCP one
  (`phone.token`, not `mcp.token`), and a request that carries it acts as the
  human, just as the desktop window does — intended, and the reason the data
  methods are safe to expose. The phone-admin methods (`phone`,
  `setPhoneEnabled`, `setPhonePort`, `regeneratePhoneToken`, `runTailscaleServe`)
  are desktop-only: they are in the IPC handler table but NOT in the allowlist, so
  a remote client can neither run a CLI nor change its own access.
- **One list of actions** for both MCP scopes and for scripts. Every call goes
  through `Engine.callAction`, and an action checks who is calling:
  - Unsafe actions are for a human only (see the unsafe rule above).
  - A bot, or a bot's script, changes only its own item.
  - `hand_off` and `close_item` are for a human only.
  The scopes are `run` (a worker's bot, at `/mcp/<runId>` on a free port, with no unsafe actions) and
  `operator` (anything the user connects, at `/mcp` on port 47390 by default,
  with a bearer token, acting as the human). Both bind to loopback only.
- **Bots get only the MCP server.** Workers run `claude -p` with built-in tools
  off, only `mcp__workflows` allowed, the output schema as `--json-schema`, and
  the item's own session (`--session-id`, then `--resume`), always from the same directory.
  A new capability for bots is an action. Never add it to the bot's command line.
- **Adding an integration** means adding its actions (each declaring `unsafe`), an event type if it can be waited on, a preset if the chat
  input should offer it, and a source if it produces items. Update `docs/events.md`
  when a type changes. Run CLIs and
  HTTP through `engine.exec` and `engine.fetch`, so the tests can stand in for them.
- **Waits back off**: 1, 2, 5, then 10 minutes between checks.
