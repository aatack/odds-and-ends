# YAWN — agent & contributor guide

YAWN (Yet Another Workflow Nanny) is the app's name: on screen, and in what is said of it. The code keeps
`workflows` wherever a name is stored or connected to (the package, the folder, local storage keys, the
`workflows-egdb` profile, the `workflows` MCP server that bots' tools are named after), so nothing kept is lost.

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

## Seeing the UI (expensive: only to check a specific change)

`npx tsx scripts/screenshot.mts [view ...]` builds the app, renders views in headless Chromium on a throwaway
in-memory engine (never the user's source), and saves PNGs at desktop and phone widths to `./screenshots`, to look
at with Read. **Treat it as expensive**: it costs a build, a browser and large images in your context. Run it only
to check a UI change you just made (a layout, a theme, an empty state), for the one or two views it touches, and
not as a routine step. Views: `tasks`, `task`, `file`, `bot`, `bots`, `integrations`, `types`, `log`,
`item:<id>`; `--desktop-only` / `--phone-only` halve it, `--no-build` skips the build, `--seed <module>` seeds your own
fixture, `--press <keys>` presses keys before each shot (see the script's header).

## Running it (for the user)

`npm install`, then `npm start`: it builds egdb-server and egdb's client whenever
egdb's sources are newer than them (with rustup's cargo where there is one;
`npm run egdb:build` builds them again; or set `EGDB_SERVER` to a binary), builds the app once and runs it from a snapshot of that build (`.run/`, made by `scripts/start.mjs`), watching nothing:
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

## The phone

The same app reaches a phone as a PWA, served by the backend, over Tailscale. Nothing of it runs in the app's
window: there `window.workflows` is the preload's, as always.

- **Two listeners, one handler** (`src/backend/http.ts`). The app's: loopback, a new port each start, the token in
  `backend.json`, every method. The phone's: loopback too, on a port that lasts (`phone.port`, 47392 by default, for
  `tailscale serve` to point at), behind the phone token (`phone.token`, kept as a secret, read on every request so a
  new one takes at once, compared in constant time). It also serves the built app (`out/renderer`, beside
  `out/main/backend.js`) to whoever asks, and refuses `PHONE_REFUSED` (the Phone page's own calls): the phone may not
  change how it is reached. A port that is taken is said on the Phone page, and the backend goes on without it.
- **Loopback only, unless let in.** `tailscale serve` forwards to loopback, so a bind can't tell the tailnet apart;
  the handler does: a request that is forwarded (`X-Forwarded-For`, `Forwarded`, `Tailscale-User-Login`) or names a
  host that is not loopback is refused (403) while `phone.tailnet` is off, which it is by default. The Phone page
  turns it on.
- **The transport** is `src/renderer/src/transport.ts`: WorkflowsAPI over `POST /rpc/<method>` and `GET /events`,
  with the token and the person signed in on each call, and no DOM, so `test/phone.mts` drives it. `api()` uses it
  wherever there is no preload (`phone.ts`, which adds the browser's own Back: a phone's back gesture takes the top
  view off the stack). `openSource` asks the backend which source it is (`source`), and so checks the token;
  `closeSource` forgets the token; `setToken`/`token` exist only here. A refused token sends the phone back to
  signing in.
- **Signing in on the phone** is `Start` with a token instead of a source. The Phone page's QR code (and its link)
  is the phone address with `#token=…` after it, which the browser never sends: opened, it fills the token in and
  signs in. `helpers/qr.ts` draws it (no dependency; checked against `qrencode` in the tests, where there is one).
- **The PWA's files** are in `src/renderer/public` (copied as they are into the build): the manifest, the icons
  (`node scripts/make-icons.mjs` makes them), and `sw.js`, which keeps the app's files so it opens offline and never
  touches `/rpc` or `/events`. It registers only in a browser, in a secure context.
- **`mobile`** is decided once, in `Root` (`useMobile`: a screen at most 640px wide, or the app installed), and passed
  down as a prop to each main view (the search page, the chat and its input, the bot's page, the settings pages, the
  header, the menu). Each lays itself out for a phone when it is true, through a `mobile` class and the `.mobile`
  rules at the end of `styles.css`: one column, targets of 40px and more, 16px inputs (no zoom on focus), the safe
  areas kept clear, and the menu at the top right with Next in it (a phone has no Ctrl+D). Views also act
  differently where a phone needs it: nothing is focused for the person (it would raise the keyboard), the chat
  input's icon is tapped to change what is sent and Enter is a new line, and the header's actions are all in its ⋯
  menu. Without `mobile` nothing changes: a new view takes the prop and keeps the desktop as it was.

**Setting it up** (the person does this; agents never run `tailscale`'s writes: the app runs them only on the person's
own press on the Phone page, which the phone itself may not call):

1. Tailscale on this machine and on the phone, on the same tailnet, with MagicDNS and HTTPS certificates on. If
   `tailscale serve` wants root, once: `sudo tailscale set --operator=$USER`.
2. In the app: menu → Phone. Tick *Serve it on the tailnet*: the app runs `tailscale serve --bg --https=<tailnet port>
   http://127.0.0.1:<port>` (and `… off` when unticked), and lets what comes through it in. Both ports are set there
   (8443 on the tailnet by default, leaving 443 to whatever else this machine serves).
3. On the phone: scan the QR code (or open `https://<machine>.<tailnet>.ts.net:8443/` and paste the token), sign in,
   then Add to Home Screen.

## Special rules

[`docs/special-rules.md`](./docs/special-rules.md) lists every place the app treats an item type, a value, a link
or an id specially, and whether the backend or the window does it. **When you add, change or remove such a rule,
update that list in the same commit.** Prefer making behaviour general over adding a row; the list is meant to
shrink.

## The paradigm

[`docs/paradigm.md`](./docs/paradigm.md) is the idea, in a page.
[`docs/items.md`](./docs/items.md) is how it is built; read it before changing the engine.

- Everything is an **event**, topics included: one tree, held in egdb as entities joined by `comments` and `notes`
  links. A **topic** is the one type people write (a comment, a task, a note: the old `task` and `note`): `text` (its
  first line its title), `open` (true open, false closed, absent no status: a comment; a note's checkbox and a task's
  open/closed are the same), `section`, and `display` (which view it opens on: see **Views** under UI). Its comments are its history; its notes a tree of topics. Closing writes
  `open: false`; topics never write `resolved`. Some items resolve as they are added (a comment); **blockers** wait
  until something happens (an approval). Topics don't depend on, or attach to, each other: an open topic in another is
  a card in its chat, and blocks nothing. There are nine types: `topic`, `approval` (the only way a person runs an
  unsafe action a bot asked for), `script`, `link`, `file`, `slack_message`,
  `slack_channel`, `bot` and `user` (a person). An older build's `widget`, `process` and `search` items are left out on load.
- **People** are items too (`user` items, in no item, whose id is their username); they sign in to the
  app by typing their username (no password: the app is local). Anything can be **assigned** to people, bots and
  scripts: an `assignees` link from the item, not an item (`Engine.setAssignee`; what someone has is their links
  followed backwards). Topics, bots and scripts have an **owner**, a person or bot: an `owns` link to it, set as it is
  made (whoever made it if a person, else the owner of what it is in, else the owner of the bot that made it; the
  `set_owner` tool changes it). Who made an item, and when, is who set its `type` value; a move changes neither. An
  item can have many blockers at once. It is a person's turn when they have it and nothing blocks it but approvals.
- A **bot** is an item of its own (a `bot` item, in no item, its settings in its params): a name,
  instructions, a provider and model (opus by default), a `cycle_minutes` (30), and a folder and
  Claude session of its own, which last. Its comments are its own chat; writing there assigns the bot item to the bot.
  **Bots and scripts run by one rule** (`Engine.due`), **on one item at a time**: the runner checks each second; for
  each one not running, it takes the first item assigned to it (the longest held first) that it hasn't run on in the
  last `cycle_minutes` (`CYCLE_MINUTES`), unless someone else assigned it to it again since; a person writing in an
  item while a run on it went on makes it due at once (without making its whole history news again). The marks are its
  `last_seen` value. Changing its settings doesn't wake it; Wake now runs it on its first held item at once. A run may
  end with a `handoff` (optional, in its output): done for
  now, so its next run starts a fresh session told the handoff once (else it carries on, and Claude's auto-compaction
  keeps the session in bounds). Due more than `bots.idle_minutes` (30) after its last run ended, it starts a fresh
  session too, since the prompt cache is cold by then; a fresh session also gets a recap from the edit log
  (`Engine.recap`: topics given to or taken from it, what it wrote, each named beside its id). A run is asked to action
  its item and give it a new assignee, and told what is new in it since it last ran on it (`promptFor` in
  `workers.ts`: people's comments as written, after their name; anything else in [brackets]; `[current topic ID: …]`
  every ten lines). It answers with
  `{ updates: [{ item, message?, items?, assign?, close? }] }` as structured output. An item it says
  nothing of who has goes back to its owner, unless it is left waiting on something (`Engine.settle`).
  It is told never to close a topic unless someone explicitly asked it to. Its `last_run` value says how
  its last run went (egdb keeps the earlier ones, as old values); `seen` lists the items its session has seen.
- A `script` is a bot without a model: it can sit on its own (with a `name`), is given topics as a bot is (`@` it,
  or assign it), runs by the same rule, and runs its JavaScript on one topic a run (`context.topics` holds exactly
  one), keeping its state on those topics (`set_value`, duck typed). Like a bot, it lasts till
  someone cancels it: it can't end itself, a run that throws says so in its chat (once per new error) and it runs
  again on its cycle, and closing a topic it sits in leaves it be. `mentions(topic)` and
  `refs(item, type)` help it read what it was asked. See `SCRIPT_DOC` in `itemTypes.ts`. Useful scripts (Snooze, Slack wait, Merge) are
  made in the app, not written into the code; the default actions hand topics to them by name. Custom actions (the Types page) are scripts too,
  run on an item when asked (one kept for type `task` or `note` reads as `topic`'s); `run_script` runs one once.
- The chat input sends a comment by default; `@` a bot or a person in it to hand them the item (and take it
  off the sender). With nothing typed it offers the default actions, a curated list in code
  (`renderer/src/defaultActions.ts`): assign to the last bot, and, when scripts by these names exist, "Slack wait" for each recent Slack message and "Snooze" for an hour or a day. Tab steps through the
  choices; its icon and hint show which.
- Nothing about the running app is stored: what is running is held in memory.

## Terminology

- **Item** is the general word for a thing stored in the database (an egdb entity): every event is an item.
- **Topic** is the `topic` item type: what people write. Its items sit in it (`comments` and `notes` links).
  The MCP tools' names (`list_items`, `get_item`, `item_id`, …) still say *item*; a bot's prompt says *topic*
  (`[current topic ID: …]`, `[topic: …]`).

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
- Only the human runs one: through an `approval` item, in the app. An agent asks
  for one by adding an approval; `unsafe_actions` tells it what it can ask for.
- No item may act outside the app when an agent adds it.
- **Scripts get unsafe tools only when a person presses play.** A script (a script item, a custom action, any
  code the app runs) is handed the same tools as the MCP. When anything but a person's own press runs it (an
  agent, a bot, its own schedule, another script), the unsafe ones are left out, whoever wrote it. This holds for
  every tool added later: declaring `unsafe` is what keeps it out. (Custom actions are not given them yet.)
- Slack (`slack.ts`) is the app's one integration with tools: its reads are safe (through a cache in the backend's
  memory: names a day, channels an hour, messages five minutes), and `slack_post` is unsafe. A Slack message's link
  in a comment (or pasted alone) is pulled in as a `slack_message`, whose card reads the message as it is shown.
- **Bots have a shell** (auto mode), and `gh` is logged in. The rule binds them
  there too: their instructions forbid doing anything unsafe with it, and the
  auto-mode classifier stands between them and it. That is a weaker guarantee
  than the MCP's, so give bots no new credentials lightly.
- `test/engine.mts` checks the rule. Keep those checks passing; never loosen them.

## Pins

An item sits in at most one topic (its `comments` or `notes` link: `parent_id`, `via`). Anything can also be **pinned** to any item: a `pins` link
from it, saying the pinned item is useful in actioning it (a subtask, a pull request, a preview link, a file). Followed
backwards, pins say where an item is referenced. `ItemInfo.pins` / `pinnedIn` list the ids; `ItemInfoDetail.pins` /
`pinnedIn` the items. Pinning moves nothing and counts for nothing else (blockers, runs, closing).

- **Made**: the `pin` tool (`link_items`, its old name, still answers; `unpin: true` takes them off), `pin: true` on an
  item in `add_items`, an open topic as it is made in another's comments (pinned to it), a person's right-click: *Pin to
  <the item on screen>* (or *Unpin from*) and *Pin to…* (a picker), and the + beside either list in the right pane.
- **Shown**: in the right pane of every item's view, as pills in two lists, *Pinned* and *Pinned in* (`Pins`); on a
  phone, in the debug view. A pin's × takes it off; each list's + opens the picker, to pin what is picked, or (Tab) to
  make a set of notes there.
- **Older stores**: a `contains` link later than the one an item was made with (the old `link_items`) is read in by
  the migration (`migrate` in `egdb.ts`) as a `pins` link, written as its author; the old link stays as it was.
- A URL mentioned in a topic's text is still pulled in as an item (its id the URL), but in no topic: it shows in no chat
  until someone pins it.

## Notes

An item's Notes view (a set of notes, made from the right pane's +, opens on it: `display: notes`) is a dense tree (`notes/NotesView.tsx`), worked as
entity-graph's outliner is, with its pins in the right pane as every view's are. Everything of it is in `src/renderer/src/notes/`
(the view, its store, its CSS); what it shares it takes from the rest (the picker, the text box, the find
line, markdown). A note is typed in the app's one text box (`TextInput`, as the chat input is: `@` inserts a reference)
and drawn by the one `Markdown`. Its structure is only `notes` links: the items in each topic's notes (`subtree` walks
the root's notes, then topics' notes below it; anything else is a leaf; added there by `addItems` with `notes`). Typing a title in the
picker of the right pane's + and pressing Tab makes a topic showing its notes (`newNotes`), in no item, pinned there. Every change shows at once and is sent after (`notes/store.ts`: pending ops over what was read, in order, a new note
on a temporary id until the backend gives its own), so nothing waits on the backend. It is set in the theme and serif
chosen in the menu, as the app's lists are.

Keys: `w`/`s` (or the arrows) move, `a` to the parent, Home/End; ←/→ fold; `d` opens the row (a note as its own tree,
anything else as itself) and `Shift+A` (or Alt+←) goes back; Enter adds a note in the row, `/` a section, `?` a
checkbox, `e` edits (Enter, or pressing another row, writes; Escape doesn't; leaving the window or the view keeps the
draft, per note, to finish); `Ctrl+/` toggles a section, `>` steps the checkbox (none, open, ticked); `x` then select
where it goes and `x` again moves the row there, `r` then select what and `r` again puts it in the row (either across
trees reached with `d`; the corner says what waits, and its Pick… opens the app's picker); Delete/Backspace takes the
row out of its note (into none); `q` sections only, `Shift+Q` open ones only; Ctrl+F finds (Enter hands the keys back),
Escape clears a find or the filters. Where each tree was left (the row, what is folded) is kept per note. The wheel
anywhere on the page scrolls the tree.

Bots read a topic's notes at its focus, as text (see the four sizes below): `get_item` gives them as `focus`, an
indented tree with ids, cut at 100 lines with the ids to ask for the rest; a run's prompt tells its item's pins so.

## The search page

Home (Ctrl+L, the items view, or nothing on the stack) is the person signed in's own item, shown as any item is
(`selected` in `useWorkflows` is them): its title is their name, it opens on their Assignments (the finder: see The
finder, under UI), and its header, right pane and views are every item's. Only Home's finder makes topics with Tab and
replaces itself with what it opens. A bot's or another person's home is their item. There are no configurable sections
any more: an older build's `search` item (its `sections`) is left out on load, as `widget` and `process` are.

## Integrations

**This is how every integration works** (aatack's direction, Oct 2026). Each is an `Integration`
(`src/engine/integrations.ts`), drawn by the Integrations page from that alone. `github.ts` (over `gh`) is the model.

- **Auth is written once and reused**: a CLI's own login, a token, or OAuth. A new integration picks one rather than
  building its own.
- **Tools are a registry**: thin wrappers round the service's calls, each marked **read** (safe) or **write**
  (`unsafe`: only a person runs it, through an approval; see the unsafe rule). Their tools join the actions, so bots,
  scripts and the MCP see them as any other. (Later: who may run each tool, per automation: allowed, not, with a
  person's approval, or for some users; for now, every write needs a person.)
- **Setup** is custom where it must be, under rules: each value it is set up with (a token, a default repo, a channel
  allowlist and other app-specific permissions) is its own field, with how to get it beside it. A value is always
  shown, unless it is a secret: then only that it is set, with a way to remove it. Values are settings
  (`integration.<id>.<key>`), secrets encrypted.
- **Triggers** (webhooks, or what the service can tell us has happened) are listed last. Until one is built, a script
  polling the read tools stands in.
- On the page, each integration has four sections, each folding: **setup, read, write, triggers**.

Slack (`slack.ts`) predates this and is still to be moved into it; Deepgram (a key for the window, no tools) is
setup alone.

## Layout

```
src/engine     everything but the window; plain Node, so the tests drive it headlessly
  engine.ts      topics, their items and blockers, assignments and owners, what bots and scripts run on, the loop
  store.ts       the store, in memory; every change goes on as egdb ops; older items read as topics
  egdb.ts        the store's home: writing to egdb, reading it back (and an older build's links in), running egdb-server
  itemTypes.ts   the item types
  presets.ts     what the chat input can send
  actions.ts     the actions on items: the MCP tools
  script.ts      scripts, in QuickJS
  schema.ts      JSON Schema from the zod shapes: forms, docs, the bots' output schema
  mcp.ts         the MCP server: the actions, in two scopes
  settings.ts    configuration in the store: the MCP port and token, integration secrets (not used yet)
  admin.ts       what the Integrations and MCP pages show and change
  workers.ts     the bots' runner: one run at a time per bot, on one item, by its provider (`claude -p`)
  integrations.ts  the interfaces: Action, ItemType
src/backend    the backend as a process: an HTTP API (api.ts, answered by http.ts) and a change stream for the app,
               the phone's listener (http.ts); info.ts, where a source keeps its things
src/main       Electron main: finds or starts the backend, passes the renderer's calls to it
src/preload    the renderer's API (WorkflowsAPI in src/shared/types.ts)
src/renderer   the React UI; public/ the PWA's files; src/transport.ts and src/phone.ts the app in a browser
test/          engine.mts: the whole engine with a fake gh and fake bots;
               phone.mts: the two listeners, the phone token, the tailnet's gate and the HTTP transport;
               egdb.mts: the store against a real egdb-server, the backend process on a fresh source
```

## UI

- **One of each control.** Menus and pickers are the app's `Dropdown` (`components/ui/Dropdown.tsx`, with
  `menuClassName="popover"`), not a native `<select>` .
  Tooltips are `title=` as ever: `Tooltips` (mounted once in `main.tsx`) moves each to `data-tip` on hover and draws it
  as a small popover, so the browser's own never shows. Don't add another tooltip component.
- **Two kinds of form.**
  - The forms used all day are condensed: the new-item form, and the chat input
    (`Composer`: one growing line, its preset's icon and hint, Tab between presets,
    Enter to send, Ctrl+Enter twice to close the topic; what is typed is kept per item in
    `draftsAtom`, in local storage, until it is sent). Keep them tight.
  - Everything else (approvals, settings) uses `SchemaForm`, through
    `Form` in `forms.tsx`, which adds the app's own editors (`secret` for tokens).
    `withHints` gives the engine's schemas text boxes for prose fields.
  - `SchemaForm` and `SchemaBuilder` are copied from entity-graph
    (`components/schema/`, with the `ui/` primitives they need and its `index.css`
    tokens). Keep them in step with entity-graph rather than editing them here.
- **Four sizes.** Anything is drawn as an avatar (its picture in the chat), a pill (inline in text, as `[id]()` is),
  a card (in a chat or list, by its type: `CARDS`) or its view (filling the window: see Views).
  They are all in `sizes.tsx`: a new type's views go there. Each but the avatar has a text version, for bots
  (`engine/views.ts`: `pillText`, `cardText`, `focusText`, each built from the smaller ones; `get_item` with `size`, and
  the prompt's lines, which are cards). What both share (`displayOf`, `focusKind`, the pill's name) is in
  `shared/views.ts`: a type's text goes in step with how it is drawn.
- **Views.** Every item, whatever its type, has the same views, chosen from its header (pressed anywhere but a button: `viewMenu`) and
  kept as its `display` value; without one, its type's (`DISPLAY` in `shared/views.ts`): **Focus** (its type's own: a
  file itself, a bot's settings and runs (`BotView`), anything else's details), **Comments** (its chat: any item can
  be commented in), **Notes** (its tree: any item can have notes), **Assignments** (what is assigned to it, then what
  it owns, and so on: the finder for that item; a person's or bot's home), and **Debug** (pushed, the same
  for everything). People and bots open on Assignments; topics and scripts on Comments; the rest on Focus.
- **The same design rules as entity-graph:** no motion, no pointer cursor,
  restrained colour, tone rather than borders. Tailwind v4 provides the tokens.
  The app's own CSS (`styles.css`) sits in `@layer components`, so the
  components' utilities win over it. Its buttons are `button[data-ui]`, so it
  never styles the components' buttons.

- **Styles on trial.** For now the left pane's menu switches between three themes (Linear, the default, dark; Notebook, light
  and warm; Terminal, dark and in Fira Code) and five serifs (or none: the UI's own font) for what the human typed (their messages, the chat
  input, topic titles). each in a light or a dark mode. `appearance.ts` keeps the choice and sets `data-theme`/`data-serif`/`data-mode` (and `.dark`) on `<html>`;
  `themes.css` holds the overrides. Code is Fira Code everywhere; weights lean light.
- **The content pane is a stack of views** (`views.ts`, a Jotai atom kept in
  local storage). The list and Next replace the stack; Open, Debug and links push
  onto it; Back pops it. A view is `{ kind, id }`: `item` shows an item in the view its `display` says (see Views); `debug`
  (`DebugView`) shows the item as egdb holds it (`Engine.inspect`): its id, who made it and when (who set its
  `type`), its values (a comment's are just `type` and `text`), and its links, each with when and by whom. A new kind of view is a new `kind`.
- **Three columns, the same for every view** (on a desktop; a phone has the middle alone, and its menu at the top
  right). **Left** (`SideNav`): the pages (Home, Bots, with how many are working, Integrations, Types, MCP, Phone
  (only in the app), Edit log), views on the stack like any other; below them, a menu with Sign out, Close source and
  the look (style, mode, serif). **Middle**: the view, under its header: Back, the item as its pill, and Next; pressed
  anywhere else, an item's header opens its views (and Debug, and Rename for a topic). **Right** (`side-pane`): for an
  item, its assignees and owner (`AssigneePill`, `OwnerPill`) and its actions as buttons, then its pins
  (`Pins`); for Home, whose it is. On a phone the header has the assignees and actions instead.
  **Next** (a button in every header on the desktop, in the menu on the phone, and Ctrl+D)
  goes to the next item assigned to you that you haven't looked at in the last half hour, never the one on screen; the
  app also moves on by itself once the topic on screen closes.
- **The items view** (`ItemsList`) is what shows when the stack is empty, and
  Ctrl+L puts it on top (or, when it is already showing, puts the cursor in its
  search line); a view is never pushed onto an identical one; Ctrl+W and Alt+Left take the top view off, and Alt+Right puts the
  last one taken off back on (`forwardAtom`, cleared by going anywhere new); the main process catches them in
  `before-input-event`, so the window's menu doesn't close the window. Typing filters titles fuzzily; the arrows
  move; Enter opens; Tab makes a new item with the typed title; Escape with nothing typed goes Back. What the items view opens *replaces* it on the stack
  (`open(view, { replace: true })`), so Back goes to where you were before it rather than to the list.
- **The finder** (`ItemsList`) is one view: home, an item's Assignments, and every item picker (`ItemPicker`, the
  same list in a modal, picking rather than opening: one item, or with `multiple` toggling each) show the same
  **sections** (`FINDER_SECTIONS` in `sections.ts`), for whoever it is for (`recent.who`, else you): **Assigned**,
  **Owned**, **Pinned** (what is pinned to them), **Recently unassigned** (what was assigned to them and no longer is,
  the latest 16, from egdb's history of `assignees` links), then, while something is typed, **Everything else** that
  matches. Pinned and Recently unassigned come from the store (`Engine.finder`). An item shows in every section
  that has it (Everything else only has what none above does). Given `act` (home, Assignments), Assigned, Owned and Pinned show even when empty, each with a + that
  opens the picker and links what is picked. People pickers offer people and bots alone (`sections={[]}`). Each
  section shows a number of rows before an "n more" row (Enter or a click shows the rest). Home shows 12 a
  section; pickers in a modal, 6. Recently viewed is kept by the app, in local storage (`viewedAtom`). A
  row's grey text says only what its icon can't (what a script waits for); "needs you" and "waits for a bot" are
  the icon's job.
- **Text is markdown, always through `Markdown`.** Rule: any text that is or could be markdown-like, anywhere in the
  app (messages, notes, descriptions, bot output, script results, code samples, new views too), is drawn by
  `Markdown` (`components/ui/`, react-markdown with GFM; fences drawn by `CodeBlock`), never as a raw string or a
  second renderer. Code that is not inside prose goes in as `fenced(code, language)`. `[id]()` in it is drawn as that
  item's pill (`ItemRefContext`, `Pill` in `sizes.tsx`).
- **Picking** is `ItemPicker`: the finder in a `Modal` over the blurred window, calling back with the item (or
  bot, or person) picked (Enter or a click); Tab only where a caller gives `onTab` (the Tasks view starts a topic). With `multiple`, its `value` and `setValue` are a list:
  what is selected shows first, ticked; picking a row adds it, or takes it off.
- **Who has an item** (`AssigneePill`, in the right pane) shows their name, or with more than one, their pictures;
  clicking it opens the multiselect picker over people and bots. Its owner (`OwnerPill`) is one, or nobody.
- **`@` in the chat input refers to a bot, a person or an item**: typed at the start of a word, it opens the finder
  (`ItemPicker`), and what is picked (or made, with Tab) takes its place as `[id]()`, and the cursor goes back to
  the chat input. `Markdown` draws such a link, to nowhere with an
  item's id as its text, through `ItemRefContext`: the app gives `ItemRef`, the item's icon and title, which open it.
- **Ctrl+F finds in the view.** On the items view it is the search line; over a chat it opens `FindLine`, and the
  chat keeps only the items whose *displayed* text (`shows`, markdown stripped) has the phrase, marked in yellow
  (`Markdown`'s `highlight`). The phrase (`findAtom`) belongs to the view and is cleared when the view changes.
- **A topic with a status is a card** (`TaskCard`): its first line, with a tick once closed, a pending ring while
  open. Clicking it opens it.
- **A topic's chat is its comments** (`Chat`: its `comments` links, in the order they were added), each as its
  card: a comment its words; an open or closed topic its `TaskCard`; a script (`ScriptCard`) what it waits for, then a tick and
  its output once done, a play button, and when it runs next (counting down);
  an approval its form while open. There is no separate blockers panel. Agents are told that what should show
  in the chat must be an item in the topic.
- **When a topic closes, the app moves on.** Whatever the human does to the topic on screen (sends, approves,
  closes, from the chat, header or menu) goes through `actOn(id)`: afterwards, if it has closed, the next one
  that needs them is *pushed* (so Back returns). Handing it to someone else stays on it, for now.
- **A view's title opens its item's menu** when right-clicked: Open, Debug, Rename (for the topic on
  screen: its first line becomes a text box, `renameAtom`) and its actions. `[id]()` references in text (topics, bots,
  people) have the same menu on right-click.
- **The Types page** (`TypesPage`) lists every item type: what it is for, whether it blocks, its fields (the
  Fields button: its params' schema and what it resolves with), and its actions. The app's own are in `itemActions.ts`;
  the human's own are **custom actions** (`engine/customActions.ts`, the `custom_actions` table): per type, a name, a
  description, hidden notes, an icon, *important* or not, and a script `async (context, actions) => null | string |
  { message?, items? }` run in QuickJS with the item's context and only the safe actions. **Generate** has Sonnet write
  the script from the description and notes. What a run says is a comment in the item, by `script`. Important
  actions are, as all an item's actions, buttons in the right pane, and in the right-click menu.
  Agents list and run them with `custom_actions` and `run_custom_action`.
- **Starting** (`Start`, before anything else): a username and a source, both needed. Opening the source
  starts (or finds) its backend; signing in makes the person if there is nobody by that username yet. Both
  are kept in local storage (`userAtom`, `sourceAtom`), and the preload sends the username with every call.
  The left pane's menu has Sign out and Close source (which stops the source's backend, if the app started it).
  Everything is as that person sees it: `withYou`, Next, the recent lists. Another source starts with an
  empty stack of views.
- **Bots** (`BotsPage`, in the left pane) lists the bots, with what each has and whether it is working or when it
  runs next, counting down), and creates new ones (a name and instructions; the rest has defaults). A bot opens on its
  Assignments; its Focus is `BotView` (its model, cycle and state; Wake now; its last run in a line; Settings,
  folded, as a `Form`, with Delete there, asking first, which hands what it had back); its Comments are its own chat.
  Its header has no Delete.
- **Every item has a right-click menu** (`useItemMenu`, `ItemMenu`), the same for every type, in sections; a row with
  a submenu does its own thing when pressed and shows the submenu on hover. **Open** (its focus; hover: Comments,
  Notes, Assignments, Debug, Edit log; a view picked is kept as its `display`), Rename for the topic on screen, and
  **Copy** (its card text; hover: pill, card or focus text, as bots read it (`viewText`), or its id). Then the
  links: **Pin to**, **Assign to**, **Set owner**, each pressed opening Choose… (the picker, a multiselect but for the
  owner) and on hover offering a few suggestions (for pins, the item on screen and what was viewed lately; for people,
  you, whoever things went to lately, the bots), Choose…, and under Unlink the links it has, each taken off when
  pressed. Last, what can be done to it (`itemActions.ts`: Close for anything open, Reopen for anything closed; its
  type's own, `TYPE_ACTIONS`, none yet; then the custom actions). The right pane
  draws the same list as buttons (on a phone, the header's ⋯ menu), so they never differ. There are no thumbs-ups:
  stored `reactions` values are left as they are, and read by nothing. Closing anything sets its
  `open` false and takes its owner and assignees off (kept as `last_owner`, `last_assignees`), so it floats free;
  Reopen sets it true and puts them back. Spread
  it on anything that stands for an item. Clicking an item does nothing else.

## Rules

- **The engine is the only writer.** Every change goes through `Engine` and emits
  `change`. Items are added in `add` and resolved in `resolve`, and checks and
  starts on one item run one at a time (`serial`).
- **egdb is the history.** Every write is an egdb event, with its author and time, and egdb keeps every value an
  entity had and every state a link had. So the app keeps no trace, runs or transcripts of its own:
  - An item's values are its params (a topic's status is its `open`); a blocker's also `resolved` (false, then
    true), and `cancelled`, `data` and `state` once they apply. When it resolved (or a topic closed), and by whom,
    is egdb's event for that value (`value_events`, read as a store loads; the debug view shows each value's). Who
    made it and when is egdb's event for its `type`; who has it, its `assignees` links; who owns it, its `owns` link.
  - A bot keeps only its `last_run` (`{ id, startedAt, endedAt, items, costUsd, turns, error, output }`); the
    earlier runs are its old values. Claude Code keeps its own session files.
  - What happened in an item since a bot last ran on it is told by when things were written (`Store.moment()` in
    memory, times after a restart), against the bot's mark on that item, set as each run starts and kept as its
    `last_seen`.
- **The store is egdb.** The backend runs an egdb-server of its own on the
  source's file (loopback, a token). The engine keeps the store in memory
  (`store.ts`) and reads it synchronously; every change is also queued as egdb ops
  and written in order (`EgdbSink`). The tests use the store with no sink.
- **The backend is one process.** It holds the store, the loop (every minute; scripts checked each second),
  the bots' runner and the MCP server; the app is a window onto it, over HTTP
  (`/rpc/<method>` and `/events`, loopback, a token), and so is the phone (its own listener and token: see The phone). Every call the renderer
  makes is a WorkflowsAPI method listed in `API_METHODS` and answered in
  `src/backend/api.ts`, as the person in `x-workflows-user` (only signing in works without one). Secrets are encrypted under
  `<source>.data/secret.key`.
- **One toolset for every agent.** Both MCP endpoints serve every action but the
  unsafe ones, and every call goes through `Engine.callAction`. Tools about an
  item take its id, so an agent may act on any item, its own included. Agents
  are author `bot`, so the bots' rules (which item types they may add) apply.
  The endpoints are `run` (a bot's run, at `/mcp/<runId>` on a free port; it
  ties calls to the run and its bot, and records what it adds as `bot:<id>`) and `operator` (any other agent, at
  `/mcp` on port 47391 by default, with a bearer token). Both bind to loopback only.
- **Bots run in auto mode, each in its own folder.** A Claude bot runs
  `claude -p --permission-mode auto` with the usual tools and the workflows MCP
  server, the output schema as `--json-schema`, its model, its own session
  (`--session-id`, then `--resume`), and the app's instructions and its own as
  its system prompt (`--append-system-prompt`), not in any prompt; a
  SessionStart `compact` hook says them again after a compaction. Its folder is
  `<data>/bots/<name>-<id>` unless it says otherwise, and lasts. Another provider
  is another `SpawnBot` in `PROVIDERS`. A new capability for bots that touches this app is an action.
- **Adding an integration** means adding its actions (each declaring `unsafe`), an item type if it can be waited on, a preset if the chat
  input should offer it, and a source if it produces items. Update `docs/items.md`
  when a type changes. Run CLIs and
  HTTP through `engine.exec` and `engine.fetch`, so the tests can stand in for them.
- **Waits back off**: 1, 2, 5, then 10 minutes between checks.
