# Items

Everything is an **event**, topics included. Each is an item in egdb (an entity),
and *item* is the general word for one; *topic* is the `topic` type.

- A **topic** is what people write: something said, a thing to be done, a note in
  a tree; one type for all of them (an older build's `task` and `note`). Its
  `text`'s first line is its title. Its status, `open`, is true while open, false
  once closed, and left out for none (a comment). A note's checkbox and a task's
  open/closed are the same value.
- A topic holds two lists: its **comments** (its chat, its history: `comments`
  links) and its **notes** (a tree of topics: `notes` links). Its `display` says
  which its view shows (`comments`, the default, or `notes`). A bot's or script's
  own chat is its comments too. The items form one tree.
- A plain item resolves as it is added: a comment.
- A **blocker** is an item that stays unresolved until something happens: a
  person approves. Topics don't block each other: an open topic in another is a
  card in its chat, and blocks nothing.
- Anything can be **assigned** to people, bots and scripts (`assignees` links,
  not items). A person is a `user` item and a bot a `bot` item, each in no item.
  Topics, bots and scripts also have an **owner** (an `owns` link). See
  [People, and who has what](#people-and-who-has-what) and [Bots](#bots).
- An item can have any number of blockers at once: its open blockers among its
  comments.
- An open item is **a person's turn** when it is assigned to them, and nothing
  blocks it, or only things people resolve do: approvals.
- There are no top-level items as such. An item may sit in none, but every list
  shows every item, at any depth.

A blocker either resolves itself, because the loop runs its `check` with
backoff (at once, then after 1, 2, 5 and then every 10 minutes), or is resolved
from outside: by a person approving, or by a cancel.
When it resolves, it keeps its **data**.

## The interface

Every item has the same shape, whatever its type: `ItemInfo`, in
`src/shared/types.ts`. A type decides only what its `params` and `data` hold,
how it reads (`summary`, `message`), and whether and how it waits.

### Every item (`ItemInfo`)

| field | what it is |
| --- | --- |
| `id` | Its id: its egdb entity. |
| `parentId`, `via` | The item (or bot or script) it sits in, and the link it sits there by (`comments` or `notes`). Null for an item that sits in none. |
| `owner` | Who owns it (its `owns` link): a person or a bot, or null. |
| `type` | Its type: one of the table below. |
| `params` | What it was given, as its type's params schema says. |
| `createdBy`, `createdAt` | Who added it, and when (the author and time of the write that set its `type`; a move changes neither): a person's username, `bot:<id>` (a bot's run), `bot` (an agent acting as a bot), `system`, `script`, `script:bot`, or `mcp`. |
| `resolved`, `resolvedAt`, `resolvedBy` | Whether it has happened, when, and who resolved it. A plain event is resolved by whoever added it, as it is added; a topic is resolved while it is closed (`open` false). Only `resolved` (a topic's `open`) is stored; when and by whom are egdb's event for it. |
| `cancelled` | Resolved without happening. |
| `data` | What it resolved with, as its type's data says. Null while it is open. |
| `actor` | Who made it happen: `resolvedBy` for a blocker, `createdBy` for a plain item. The chat puts the signed-in person's on the right. |
| `summary` | One line saying what it is, from its type's `describe`. This is a blocker's row while it is open, and how prompts name it. |
| `message` | What it says in the chat once resolved, from its type's `said`, or `Cancelled: …` when cancelled. Null while open, or when it says nothing. A topic's is its text. |
| `nextCheckAt`, `lastError`, `runs` | For blockers the loop checks: when the next check is (null for a script run by hand), what went wrong on the last one, and how many checks since the app started. |

Items are ordered within their topic by when they were added. Items added
together keep the order they were given in.

An item's summary as the lists read it (`Item`) is read from its values: its title, its
status (`open`) and whether it has closed, its open blockers, who has it (`assignees`) and who owns it (`owner`),
and whether it is the turn of the person looking (`withYou`). Which bot is running on it is held in
memory, not stored, so a crash leaves nothing to clean up.

**For agents:** `src/engine/paradigm.ts` is the paradigm in a few lines: the MCP
server's instructions, and the `paradigm` action. Keep it that short.

**Scripts for a type:** a script owned by an item type (`set_owner` to its name)
is listed on the Types page and offered in the right pane of every item of that
type, which runs it on the item. (They replace custom actions.)

**Reads:** `get_item` (an item in full, with its owner), `get_children` (the items
in an item's comments; with `notes`, its notes; with `open`, its blockers),
`list_items` (every open topic, anything else someone has, and the topics
closed in the last day), `items_waiting_on`
(the open items waiting on a person, any bot, one bot, or one blocking type),
`next_for_human` (what a person's Next would show: of the items that are their
turn, the least recently touched), `list_people` and `list_bots`.

### Every type (`ItemType`)

Defined in `src/engine/integrations.ts` and written in `src/engine/itemTypes.ts`.

| member | what it is |
| --- | --- |
| `type`, `description` | Its name, and what it is for (shown to bots by `item_types`, and on the MCP page). |
| `params` | A zod shape. Params are checked against it as the item is added. |
| `data` | What it resolves with, in words. |
| `blocking` | False: it resolves as it is added. True: it is a blocker. |
| `human` | A blocker a person resolves. While only these are open, it is the turn of whoever has the item. |
| `bots` | A bot may add it in its output. |
| `describe(params)` | Its `summary`. |
| `said(params, data)` | Its `message` once resolved; null for nothing. Left out, it says its summary. Not called when the item is cancelled. |
| `expand(params, author)` | Optional: turns it into the items actually added. |
| `start(params, ctx)` | Optional: runs as it is added. It may change the params, or resolve the item at once. |
| `check(params, ctx)` | Optional, for waits: answers the data once the item has happened, or null while it hasn't. Polled with backoff. |

**To add a type:** write it in `itemTypes.ts` and add its name to `ItemKind`.
Give it an icon in the renderer's `icons.tsx`, and a row in the table below. If
the chat input should offer it, add a preset in `presets.ts`.

## Types

| type | blocks | params | data |
| --- | --- | --- | --- |
| `topic` | no (its status is `open`) | `text` (its first line its title), `open?` (true open, false closed, absent none), `section?` (a heading, in a tree), `display?` (`comments`, the default, or `notes`), `comments?` and `notes?` (its first items, added in it), `assignees?` (who has it from the start; links, not kept in its params), `detached?` (sits in no item, wherever it is added), `key?` (so it is added once) | — |
| `approval` | a person's | `action` (unsafe), `inputs`, `reason?` | `{ ran, inputs, changed, output }` |
| `script` | while it lasts; it blocks nothing (a topic waits on it by being assigned to it) | `source`, `name?`, `description?`, `cycle_minutes?` (30); the app sets `last_seen` | `{ result, runs, logs }` or `{ error }` |
| `bot` | while the bot lasts (sits in no item) | `name`, `instructions?`, `provider?`, `model?` (opus), `cycle_minutes?` (30), `folder?`; the app sets `session_id`, `seen`, `last_seen` and `last_run` | `{ cancelled }`, once deleted |
| `file` | no | `path` (absolute, a file on this machine, read as it is when shown), or `copy: true` with it (a copy kept in `<source>.data/files/<sha256>`, as `hash`; `copy` and `path` not kept); `name?`, `title?`; the app sets `mime` (from the name), and `size` for a copy | — (its card and view read it through the backend: `fileData`, `fileText`) |
| `slack_message` | no | `link`, or `channel` and `ts` (`thread_ts?`), `workspace?` | — (its card and view read Slack, through the cache, as shown) |
| `slack_channel` | no | `channel` (id or #name), `oldest`, `latest?`, `workspace?` | — (as `slack_message`) |
| `link` | no | `url`, `title?`: a web page; its id is its URL | — |
| `search` | no (one, in no item) | `sections`: every finder's configured sections | — |
| `user` | while the person lasts (sits in no item) | `username` (its id), `name?` | `{ cancelled }`, once removed |

An older build's `task` and `note` are added as topics (a task's `title` its
text, `items` its comments, open); stored ones read as topics (see The store).
An older build's `widget` and `process` items are left out as a store loads.

- **`script`** is a bot without a model (`SCRIPT_DOC` in `itemTypes.ts`, which
  agents read): it may sit on its own with a `name`, is given topics as a bot
  is (assigned, or `@`-ed), and runs by the bot rule (`Engine.due`), on one
  topic at a time. Each run: `context` (the script's values, `state`, `runs`,
  `topics`: the one topic it is run on, `now`, `trigger`) and `tools` in scope,
  with `mentions` and `refs`; it returns `{ messages, assign, state }`.
  It keeps its state on the topics it works on (`set_value`). It never ends on
  its own: throwing, it says so in its chat (once per new error) and runs again
  on its cycle; cancelled, it hands back what it holds. Where it last ran on
  each topic is its `last_seen`.
  - Unsafe tools only when a person presses play (`runNow`, by them); on a
    schedule never, whoever wrote it.
  - `run_script` runs a body once, now, with the caller's tools.

## People, and who has what

- A **person** is a `user` item, in no item, whose id is their **username**.
  People sign in to the app by typing it (no password: the app is local); every
  call the app makes says who it is from (`x-workflows-user`). Signing in as a
  username nobody has yet makes that person.
- **Assignment is a link, not an event**: an `assignees` link in egdb from any
  item to the person, bot or script (`Store.setAssigned`), sorted by when; taking
  it off removes the link. The items someone has are their `assignees` links
  followed backwards (`Engine.assignedTo`, the longest held first). egdb keeps
  the link's history: who assigned it, and when.
- **Ownership is a link too**: an `owns` link from a person or bot to each
  topic, bot and script they own (`Store.setOwner`; `set_owner` changes it). A
  new one is owned by whoever made it if a person; else by the owner of what it
  is in; else by the owner of the bot that made it (`Engine.ownerFor`).
- **Pinning is a link too**: a `pins` link from an item to what is pinned to it
  (`Store.pin` / `unpin`), sorted by when; followed backwards, where an item is
  referenced. An open topic is pinned to the topic it is made in; an item added with
  `pin: true` (beside `type` and `params`) to the item it is added in. See
  AGENTS.md, Pins.
- A new topic goes to its `assignees` param if it has one; else, if it is open
  and a person made it, to them; else (a bot or a source made it) to nobody: a
  bot decides who has what it makes. An item a bot lets go of goes back to its
  **owner** (`Engine.owner`), if nobody else has it.
- Agents assign with the `assign` action; a bot's update may say `assign` too.

## Bots

- A **bot** is a `bot` item, in no item: a name (people `@` it by that),
  instructions of its own, a provider (`claude`, Claude Code, for now: another is
  another entry in `PROVIDERS`, `workers.ts`) and model (opus, unless it says
  otherwise), a cycle (30 minutes), and a folder and a Claude session of its own,
  which last. Its settings are its params: changing them writes them anew (egdb
  keeps the old values). Its comments are its **own chat**; writing in it
  assigns the bot item to the bot.
  Deleting it cancels it, and what it had goes back to each item's owner. People make and edit bots on the Bots page; agents may make
  them too (`add_items`).
- **It runs by the one rule** (`Engine.due`), as a script does: on **one item
  at a time**. The runner checks each second; for each bot not running, it takes
  the first item assigned to it (the longest held first) that it has not run on
  in the last `cycle_minutes` (`CYCLE_MINUTES`, 30), unless someone else assigned
  it to the bot again since (then at once). A person writing in an item while a
  run on it went on makes it due again at once, without making its whole history
  news again. Where it last ran on each item is its `last_seen` value. Woken by
  hand (Wake now), its first held item runs at once.
- **One run at a time**, in **one session**
  (`--session-id`, then `--resume`), in its own folder. Its instructions are its
  system prompt (`--append-system-prompt`), not part of any prompt; a
  SessionStart hook on `compact` says them again into the conversation after a
  compaction.
- **Prompt:** "Please action this topic, assigned to you, and give it a new
  assignee to carry it on", then the item's comments new since the bot last ran
  on it (its mark there, set as the run starts), less what its own runs did, or
  "nothing new"; the item's whole history the first time its session sees it
  (`seen`). People's comments are given as they wrote them, after their name;
  anything else in brackets; `[current topic ID: …]` every ten lines.
- **Output:** `{ updates: [{ item, message?, items?, assign?, close? }] }`, as
  structured output: for its item, what to say, items to add, who has it from now on
  (usernames, bots' names; itself to keep it), and whether it is done. An update
  for its own id is its reply in its own chat.
- **After a run**, the item it said nothing of who has goes back to its owner,
  unless the bot left it waiting on something that is not a person (a wait it
  added, in this run or before), or a person wrote in it while it ran. A run that
  fails says so in the item, and gives it back to its owner. A bot
  deleted while it runs lands nothing.
- The bot's `last_run` value says how its last run went: when it started and
  ended, its item, cost, turns, error and the bot's own words. egdb keeps the
  earlier ones as old values. A run cut short (the app stopped) is marked ended
  when the app starts again, and its item goes back to its owner.
- **It is told never to close a topic unless someone explicitly asked it to**;
  finishing the work is not being asked, so it hands the topic back instead.

## The chat input

What a person types goes through a **preset**, which turns the text into items
(`src/engine/presets.ts`). Tab steps through the presets.

| preset | adds |
| --- | --- |
| Comment (the default) | the comment (a topic with no status) |

- `@` refers to a bot, a person or an item: `[id]()`. A bot or person referred to
  in what is sent is assigned the item, and the sender is taken off it.
- With nothing typed, the choices are "Assign to @someone" instead (preset
  `assign:<id>`), the likeliest first: whoever else has this item, then
  whoever the sender assigned things to last, then the bots, then the people.

## The store

- The engine holds the store in memory (`store.ts`) and reads it synchronously.
  Every change is also queued as egdb ops and written, in order, to an
  egdb-server (`egdb.ts`, `EgdbSink`), each write with its author and time: an
  entity per item, setting and custom action. An item's values are its `type`
  and its params (a comment's are just `type` and `text`; a topic's status is its
  `open`); a blocker also has `resolved` (false, then true: when it turned true,
  and by whom, is egdb's event for that value, read with `value_events`), and
  `cancelled`, `data` and `state` once they apply. It is linked from the topic it
  sits in by a `comments` or `notes` link (sorted by its place; an item in none
  has none), and who made it and when is the author and time of its `type`. Who
  has an item is an `assignees` link per assignee, and its owner an `owns` link
  to it. egdb keeps every value an entity had, and
  every state a link had, so nothing is lost by a change: there is no trace,
  run or transcript store of the app's own.
- A source made by an earlier build (values `params` and `created_by`) is
  refused: start a new source.
- A source the older build of this app wrote (`task` and `note` items, `contains`
  and `assigned_to` links) is read, never rewritten, so that build still reads it
  as before. A stored task or note reads as a topic, keeping its id
  (`legacyValues` in `store.ts`: a task's `title` is its text, open till its
  `resolved` turned true; a note shows its notes). Its links are read in by
  `migrate` (`egdb.ts`) as the store loads, adding links only: an item's first
  `contains` becomes a `comments` link (`notes` if the parent was a note), a later
  one (the old `link_items`) a `pins` link, each `assigned_to` an `assignees`
  link, each written as the original's author; and each topic, bot and script
  gets an `owns` link (by `system`) by the old owner rule. The setting
  `migration.links_until` (the newest old link read) keeps it idempotent: a
  later load reads only what the older build added since. The older build doesn't
  see items this build makes (they have no `contains` link).
- When a blocker is next checked, its backoff step and its last error are held
  in memory only: after a restart every wait is checked at once.
- A source starts empty: nothing is brought in from the older app's store.

## Who may do what

There is one list of **actions**, and every call goes through `Engine.callAction`.

- **Unsafe** actions change something outside this app, and only the human runs
  one, through an `approval` item. No agent gets them: both MCP endpoints leave
  them out, a script gets them only when a person presses its play button, and `callAction` refuses them to
  anyone but the human. See the unsafe rule in AGENTS.md.
- Every other action is the same for every agent, a bot or anything connected:
  `add_items`, `cancel_item` and `close_item` included, on any item. A bot can
  also add items through its output, and a script by what it returns.
- Closing a topic cancels its open blockers; topics in it are left open.
