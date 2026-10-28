# Special rules

Every place the app treats an item, a value, a link or an id specially: what the rule is, and where it lives
(**B** the backend, `src/engine`; **F** the window, `src/renderer`). Generic behaviour (store a value, add an item,
follow a link) is not listed. Keep this list current: see AGENTS.md. Each rule is a candidate for simplification.

## Item types

| Type | Special behaviour | Where |
|---|---|---|
| `task` | The only type that can be assigned (`setAssignee`), has a chat view, a title, Next, `withYou`, Reopen. Its first items (`items`) and `assignees` are params taken out on add. A new one goes to its `assignees`, else its maker if a person, else nobody. `key` makes it add once. `detached` puts it in no item. A task in another blocks nothing and is not closed with it. | B `addNow`, `toItem`, `close`; F `FOCUS`, `itemActions`, `App` |
| `bot` | Sits in no item; its chat holds only notes. Settings are params; the app keeps `session_id`, `seen`, `last_run`, `handoff` on it. Assignable. Cancelling deletes it and hands its tasks back. | B `agent`, `saveAgent`, `deleteAgent`, `addNow`; F `BotView`, `FOCUS` |
| `user` | Sits in no item; its id is its username. Signing in makes one. Assignable. | B `signIn`, `saveUser`, `addNow` |
| `script` | Never a blocker. Assignable (it "owns" tasks: `owns` param, taken out on add). Wakes by the bot rule (`scriptDue`); with its own `cycle_minutes` it is a timer, running every cycle even owning nothing; `last_wake` kept. Done or cancelled, hands its tasks back. Unsafe tools only on a person's press. Counted against `scripts.max_running`. Older `wake_at` / `schedule` honoured. | B `addNow`, `scriptDue`, `checkOne`, `resolve`, `itemTypes.ts`; F `ScriptCard` |
| `approval` | A blocker that leaves the task `withYou`. Its `action` must be unsafe. Resolved only by a person (`approve`). | B `prepare`, `waitsOnHuman`, `approve`; F `Approval` |
| `note` | Plain. In the chat its picture is whoever wrote it, not its type; it is Ctrl+F's text. | F `itemIcon`, `Chat` |
| `widget` | `widget_state` is set through `setWidgetState` / `set_value`; drawn in a sandboxed frame. | B `setWidgetState`; F `Widget` |
| `search` | One per store, in no item; its `sections` configure every finder. Read and set through its own calls. | B `searchSections`, `setSearchSections`; F `SearchSectionsContext` |
| `slack_message`, `slack_channel` | Fetch nothing on add; their card and view read Slack (cached) as shown. A Slack link in a person's note is added as one. | B `slack.ts`, `presets.ts`; F `SlackView`, `Composer` (paste) |
| `process` | Switched off: can't be added; kept so stored ones read. | B `prepare` |

## Values with special meaning

| Value | Meaning | Where |
|---|---|---|
| `type`, `resolved`, `cancelled`, `data`, `state`, `reactions` (and old `resolved_at`, `resolved_by`) | How an item stands, not its params (`STATE_KEYS`). Not settable with `set_value`. | B `store.ts`, `setValue` |
| `session_id`, `seen`, `last_run`, `last_wake`, `handoff`, `key` | Kept by the app for itself (`OWN_KEYS`); not settable with `set_value`. | B `engine.ts` |
| `title` | A task's name; a bot's summary is its `@name` instead. | B `title`, `toInfo` |
| `debounce_minutes`, `cycle_minutes` | The wake rule's two numbers, on bots and scripts (defaults 1 and 15). | B `wakes.ts`, `wake`, `scriptDue` |
| `owns` | On a script, as it is added: assigns it those tasks; not kept. | B `addNow` |
| `wake_at`, `schedule`, `timeout_minutes` | Older scripts' timing; still read. | B `scriptDue`, `checkOne` |
| `widget_state` | A widget's state. | B `setWidgetState`; F `Widget` |
| `sections` | On the search item: the finders' sections. | B, F as above |
| `derives_from` | egdb: an entity reads its source's values under its own (not yet used by the app). | egdb `read.rs` |
| `reactions` | Who gave a thumbs-up; set by `react`. | B `react`; F `Chat` |
| `wf.setting`, `wf.custom_action`, `wf.root` types | The app's own entities, not items: settings, custom actions, the root. | B `egdb.ts`, `store.ts` |

## Links

| Link | Meaning | Where |
|---|---|---|
| `contains` | From an item (or the root, `@root`) to what is in it. The **first** one is where an item sits (`parent_id`); later ones put it in other items too (`link_items`), shown there and as reply pills, but they count for nothing else (blockers, wakes, closing). | B `store.ts` (`linkedIn`), `egdb.ts` loader, `detail`; F `Chat` pills |
| `assigned_to` | From a task to a person, bot or script: who has it. Drives `withYou`, Next, the bots' wakes and prompts, scripts' `owns`, hand-backs. | B `setAssignee`, `assignees`, `assignedTo`, `wake`, `settle` |

## Ids and actors

| Id | Meaning | Where |
|---|---|---|
| `bot:<id>` as an author | A bot's run. Its own writes don't wake it; the chat names it `@name`; the finder's "assign to last bot" reads it. | B `wake`, `actorName`; F `people.ts`, `Chat`, `defaultActions`, `App.likeliest` |
| `script`, `script:bot`, `system`, `mcp`, `bot` | Other authors: scripts, the app, outside agents. `system` notes are the app's (failures, reopen). | B `authorOf`, `callAction`; F `icons.whoIcon` |
| A username | A person, signed in by it; `me()` is the first person when nobody is said. | B `user`, `me` |
| `@root` | The root of the tree. | B `store.ts` |

## Behaviour rules

| Rule | Where |
|---|---|
| **withYou**: open, assigned to the viewer, and blocked by nothing but approvals. | B `toItem` |
| **Next**: the viewer's withYou tasks, most recently touched first; skipped ones last. | B `next`; F `useWorkflows.next` |
| **Owner**: the maker if a person, else the parent's owner. Hand-backs go there. | B `owner`, `handBack` |
| **Hand-back after a run** (`settle`): a task the bot said nothing about goes back to its owner, unless the run changed who has it, it waits on a non-person blocker the bot added, or news came in during the run. An update can't hand away a task a person wrote in during the run. A failed run hands everything back with a note. | B `endRun`, `settle` |
| **Wakes** (`dueAt`, one rule for bots and scripts): affected since the last wake and settled for the debounce, or a cycle since the last wake while holding tasks; never while running. Limits `bots.max_running` (6) and `scripts.max_running` (8). | B `wakes.ts`, `wake`, `scriptDue`, `checkDue`; `workers.fill` |
| **Prompts**: a task's whole history the first time a session sees it (`seen`), then only what is new; a handoff starts a fresh session. | B `claim`, `endRun`; `workers.promptFor` |
| **Sending** with an `@` (or a default action that assigns) hands the task to them and takes it off the sender. | B `send` |
| **Bots never close** a task unless asked (instruction only). | `workers.ts` SYSTEM |
| **Unsafe**: an unsafe action runs only for a person (an approval, a pressed script, `/`). | B `callAction`, `itemTypes.ts` script |
| **Closing** a task cancels its open blockers; tasks in it stay open. Reopen undoes the close only. | B `close`, `reopen` |
| **Finder sections**: Needs you = withYou; With bots = held by a bot (not a script); Waiting = the rest open; Closed today. Most recently touched first. | F `sections.ts`; B `overview` (order) |
| **Default actions** with an empty input: assign to the last bot, wait for Slack, snooze. | F `defaultActions.ts` |
| **History**: closed tasks leave the back/forward stacks; Forward with nothing ahead goes where you last went from here. | F `useWorkflows.ts` |
| **Old tool names** (`add_events`, `event_id`, …) are hidden aliases. | B `actions.ts` `LEGACY_*`, `callAction` |
