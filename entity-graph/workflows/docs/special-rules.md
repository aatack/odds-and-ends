# Special rules

Every place the app treats an item, a value, a link or an id specially: what the rule is, and where it lives
(**B** the backend, `src/engine`; **F** the window, `src/renderer`). Generic behaviour (store a value, add an item,
follow a link) is not listed. Keep this list current: see AGENTS.md. Each rule is a candidate for simplification.

## Item types

| Type | Special behaviour | Where |
|---|---|---|
| `topic` | The one type people write (old `task` and `note` are added as one; stored ones read as one: `legacyValues`). Its title is its `text`'s first line (`rename` changes that line). `open` is its status (true, false, absent: none), set through `close`/`reopen` (`set_value` on `open` goes there too), never `resolved`; closed reads as resolved. With a status it is a card that opens (`TaskCard`); with none, its chat card is its words. Holds `comments` and `notes` (as any item may); opens on Comments unless its `display` says. Its first `comments`/`notes` and `assignees` are params taken out on add; a new open one goes to its `assignees`, else its maker if a person. `key` makes it add once; `detached` puts it in no item. An open one made in another's comments is pinned there and blocks nothing. Answers to `type: task`/`note` and has `title` in search queries. Ctrl+F's text; in the chat its picture is whoever wrote it. | B `addNow`, `prepare` (`asTopic`), `close`, `setValue`, `valuesOf`, `store.ts` (`setOpen`, `legacyValues`); F `CARDS`, `displayOf`, `itemActions`, `ItemMenu`, `Chat` |
| `bot` | Sits in no item; its chat holds only topics, and writing in it assigns the bot item to the bot. Settings are params; the app keeps `session_id`, `seen`, `last_seen`, `last_run`, `handoff` on it. Cancelling deletes it and hands its items back. Its Focus view is `BotView`. | B `agent`, `saveAgent`, `deleteAgent`, `send`, `addNow`; F `BotView`, `DISPLAY` |
| `user` | Sits in no item; its id is its username. Signing in makes one. | B `signIn`, `saveUser`, `addNow` |
| `script` | A bot without a model: may sit in no topic, with a `name` (a holder by it, offered in pickers). Never a blocker. Runs by the bot rule (`due`), on one topic: its context's `topics` is that one; `mentions` / `refs` helpers. Cancelled, hands its topics back. Unsafe tools only on a person's press. Counted against `scripts.max_running`. | B `addNow`, `holder`, `due`, `runScriptItem`, `itemTypes.ts`, `script.ts`; F `ScriptCard`, `defaultActions` (Snooze) |
| (notes) | A topic's notes are a tree (`NotesView`, from `subtree`, which walks into topics' notes only); `section` and `open` make a row a heading or a checkbox there. `newNotes` makes a topic showing its notes, in no item, pinned where it was made (Tab in the right pane's pin picker). | B `newNotes`, `subtree`; F `notes/` (`NotesView`), `Pins` |
| `slack_message`, `slack_channel` | Fetch nothing on add; their card and view read Slack (cached) as shown. A Slack link in a topic is added as one (see `link`). | B `slack.ts`, `presets.ts`; F `SlackView`, `Composer` (paste) |
| `link` | A web page. Made when a topic's text mentions a URL (bare, `[label](url)` or `[url]()`), as are Slack messages and channels, in no topic (shown in no chat until pinned): its id **is** the URL, so it is made once. A URL in rendered text is drawn as that item's pill. | B `addNow`, `pulledIn`; F `Markdown` Link, `ItemRef` |
| `file` | Its params are prepared by `Files.prepare` (a path checked; `copy` kept under its hash and dropped; `name`, `mime` filled in). Its bytes are read through the backend, never by the window. Only a file it refers to may be edited (`saveFileText`); a kept copy stays as given. A file dropped or pasted in a topic's chat is uploaded as one. Its view is the file itself (`FileFocus`), not its details. | B `files.ts`, `prepare`, `uploadFile`, `fileData`, `fileText`, `saveFileText`; F `FileView`, `App` (focus, drop, paste) |

## Values with special meaning

| Value | Meaning | Where |
|---|---|---|
| `type`, `resolved`, `cancelled`, `data`, `state`, `reactions` (an older build's thumbs-ups, read by nothing now), and old `resolved_at`, `resolved_by` | How an item stands, not its params (`STATE_KEYS`). Not settable with `set_value`; anything else is (duck typed: a type's params are advice). | B `store.ts`, `setValue` |
| `session_id`, `seen`, `last_run`, `last_seen`, `last_wake` (old), `handoff`, `key` | Kept by the app for itself (`OWN_KEYS`); not settable with `set_value`. `last_seen`: where a bot or script last ran on each item it holds (its per-item marks). | B `engine.ts` (`setMark`) |
| `open`, `display` | A topic's status (see `topic`); and, on any item, which view it opens on (focus, comments, notes, assignments; without one, its type's: `DISPLAY`). | B `setValue`, `setOpen`; F `displayOf`, `ViewPicker` |
| `cycle_minutes` | On bots and scripts: how long before it runs on an item it still holds again (30). An old `debounce_minutes` is ignored. | B `cycleMinutes`, `due` |
| `migration.links_until` (a setting) | Up to when an older build's links have been read in (`migrate`). | B `egdb.ts` |
| `derives_from` | egdb: an entity reads its source's values under its own (not yet used by the app). | egdb `read.rs` |
| `wf.setting`, `wf.custom_action`, `wf.root` types | The app's own entities, not items: settings, custom actions, the root. | B `egdb.ts`, `store.ts` |
| `task`, `note` as a custom action's type | Read as `topic`'s. | B `customActions.ts` |

## Links

| Link | Meaning | Where |
|---|---|---|
| `comments`, `notes` | From a topic (or a bot's or script's own chat) to what is in it, sorted by place: its chat and its tree. An item has at most one, where it sits (`parent_id`, `via`); none in no item. A move (`moveItem`) removes it and adds another; who made the item, and when, stay (they are who set its `type`). | B `store.ts` (`insertItem`, `move`), `egdb.ts` loader |
| `pins` | From an item to what is pinned to it: artefacts useful in actioning it. Followed back, where an item is referenced. An open topic is pinned to the topic it is made in; `pin: true` on an added item pins it where it is added. Counts for nothing else. | B `store.ts` (`pin`, `unpin`), `egdb.ts` loader, `addNow`, `pin`; F `Pins`, `ItemMenu` |
| `assignees` | From any item to a person, bot or script: who has it. Drives `withYou`, Next, the search page, what bots and scripts run on, hand-backs. | B `setAssignee`, `assignees`, `assignedTo`, `due`, `settle` |
| `owns` | From a person or bot to the topics, bots and scripts they own: who an item goes back to, and the finder's *Owned*. | B `ownerFor`, `setOwner`, `handBack`; F `sections.ts` |
| `contains`, `assigned_to` | An older build's. Read once into the above by `migrate` (added links, nothing rewritten), past `migration.links_until`. | B `egdb.ts` |

## Ids and actors

| Id | Meaning | Where |
|---|---|---|
| `bot:<id>` as an author | A bot's run. Its own writes are not news to it, and its own reassignments don't make an item due; the chat names it `@name`; the finder's "assign to last bot" reads it. | B `due`, `claim`, `actorName`; F `people.ts`, `Chat`, `defaultActions`, `App.likeliest` |
| `script`, `script:bot`, `system`, `mcp`, `bot` | Other authors: scripts, the app, outside agents. `system` comments are the app's (failures, reopen). | B `authorOf`, `callAction`; F `icons.whoIcon` |
| A username | A person, signed in by it; `me()` is the first person when nobody is said. | B `user`, `me` |
| `@root` | The root an older build linked items in no task from; skipped by the loader and `migrate`. | B `egdb.ts` |

## Behaviour rules

| Rule | Where |
|---|---|
| **withYou**: open, assigned to the viewer, and blocked by nothing. | B `toItem` |
| **Next**: the viewer's withYou items, most recently touched first, never the one on screen nor what they looked at in the last half hour. | B `next`; F `useWorkflows.next` |
| **Owner** (`owns`): the maker if a person, else the owner of what it is in, else the owner of the bot that made it; set as it is made. Hand-backs go there. | B `ownerFor`, `owner`, `handBack` |
| **Hand-back after a run** (`settle`): the item the bot said nothing about goes back to its owner, unless the run changed who has it, it waits on a non-person blocker the bot added, or a person wrote in it during the run (then the bot keeps it and is due on it again). An update can't hand away an item a person wrote in during the run. A failed run hands it back with a note. | B `endRun`, `settle` |
| **Runs** (`due`, one rule for bots and scripts): one item a run; the first item it holds (longest held first) it hasn't run on in its `cycle_minutes`, or that someone else assigned it again since; Wake now runs its first at once; never while running. Limits `bots.max_running` (6) and `scripts.max_running` (8). | B `due`, `claim`, `checkDue`; `workers.fill` |
| **Usage limits**: a run stopped by one is put back as if it never began (the bot keeps its item, news stays new), and no bot of that provider runs until it resets (`limitedUntil`; held in memory). | B `endRun`, `pause`; `workers.fill` |
| **Prompts**: an item's whole history the first time a session sees it (`seen`), then only what is new since the bot last ran on it; its pins (all the first time, then those pinned since), each at its focus as text (a chat at its pill). A handoff starts a fresh session, as does waking more than `bots.idle_minutes` (30) after the last run ended (the cache is cold by then); a fresh session is told its handoff and a recap from the edit log (topics given to or taken from it, what it wrote, names beside ids). | B `claim`, `recap`, `endRun`; `workers.run`, `promptFor` |
| **Sending** with an `@`, `[id]()` or `[label](id)` of a bot or person (or a default action that assigns) hands the item to them and takes it off the sender. | B `send` |
| **Bots never close** a topic unless asked (instruction only). | `workers.ts` SYSTEM |
| **Unsafe**: a label for now, not a gate: anyone may run an unsafe action; bots are told to only when asked. | B `callAction`; `workers.ts` SYSTEM |
| **Closing** anything sets its `open` false and takes its owner and assignees off (kept as `last_owner`, `last_assignees`): it floats free. A topic's open blockers are cancelled (topics in it stay open); a bot is deleted; a script or other blocker cancelled. Reopen sets `open` true and puts the owner and assignees back (but bots and scripts gone since); what the close cancelled stays cancelled. `cancel` is close's old name. | B `close`, `reopen`, `isClosed` |
| **Finder sections**: every finder (home, an item's Assignments, every item picker) shows Assigned (open, assigned to whoever it is for), Owned (open topics, bots and scripts they own, not assigned to them), Pinned (pinned to them), Recently unassigned (latest 16 whose `assignees` link to them was removed), and while typing Everything else; an item shows in every section that has it, but Everything else only has what none above does. For the item whose Assignments it is; home is the person signed in's. | F `sections.ts`, `ItemsList`; B `finder`, `overview` |
| **Default actions** with an empty input: assign to the last bot, wait for Slack, snooze. | F `defaultActions.ts` |
| **History**: closed topics leave the back/forward stacks; Forward with nothing ahead goes where you last went from here. | F `useWorkflows.ts` |
| **Text views**: an item as a bot reads it, at the sizes it is drawn at (`views.ts`): pill (type, name, id), card (a prompt's line), focus by `focusKind` (a chat as its latest 100 cards; a topic showing its notes as its tree, topics' notes walked into, anything else as its pill, 100 lines at most with the ids to ask for the rest; anything else as its values). `get_item` gives `focus` for anything without a chat, and for a topic that shows its notes; any size with `size`. | B `views.ts`, `view`, `get_item`, `claim`, `promptFor` |
| **Old tool names** (`add_events`, `event_id`, …) are hidden aliases. | B `actions.ts` `LEGACY_*`, `callAction` |
