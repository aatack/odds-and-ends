# Items

Everything is an **event**, tasks included. Each is an item in egdb (an entity),
and *item* is the general word for one.

- A **task** is an item that stands for one thing to be done. Other items sit
  **in** it (their parent, a `contains` link), and so can other tasks: the items
  form one tree. Its children are its history.
- A plain item resolves as it is added: a note.
- A **blocker** is an item that stays unresolved until something happens: a
  a person approves. Tasks don't block each other.
- An item is **assigned** to people and to bots (`assigned_to` links, not items).
  A person is a `user` item and a bot a `bot` item, each in no item. See
  [People, and who has what](#people-and-who-has-what) and [Bots](#bots).
- An item is itself a blocker: it blocks the item it sits in until it closes.
  So the deepest items are dealt with before the items they sit in.
- An item can have any number of blockers at once: its open children.
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
| `parentId` | The item (or bot) it sits in. Null for an item, a bot or a person that sits in none. |
| `type` | Its type: one of the table below. |
| `params` | What it was given, as its type's params schema says. |
| `createdBy`, `createdAt` | Who added it, and when (the author and time of the `contains` link to it): a person's username, `bot:<id>` (a bot's run), `bot` (an agent acting as a bot), `system`, `script`, `script:bot`, or `mcp`. |
| `resolved`, `resolvedAt`, `resolvedBy` | Whether it has happened, when, and who resolved it. A plain event is resolved by whoever added it, as it is added. Only `resolved` is stored; when and by whom are egdb's event for it. |
| `cancelled` | Resolved without happening. |
| `data` | What it resolved with, as its type's data says. Null while it is open. |
| `actor` | Who made it happen: `resolvedBy` for a blocker, `createdBy` for a plain item. The chat puts the signed-in person's on the right. |
| `summary` | One line saying what it is, from its type's `describe`. This is a blocker's row while it is open, and how prompts name it. |
| `message` | What it says in the chat once resolved, from its type's `said`, or `Cancelled: …` when cancelled. Null while open, or when it says nothing. |
| `nextCheckAt`, `lastError`, `runs` | For blockers the loop checks: when the next check is (null for a script run by hand), what went wrong on the last one, and how many checks since the app started. |

Items are ordered within their task by when they were added. Items added
together keep the order they were given in.

A task's summary (`Item`) is read from its values: its title, whether it
has closed and how, its open children (`blockers`), who has it (`assignees`),
and whether it is the turn of the person looking (`withYou`). Which bot is running on it is held in
memory, not stored, so a crash leaves nothing to clean up.

**For agents:** `src/engine/paradigm.ts` is the paradigm in a few lines: the MCP
server's instructions, and the `paradigm` action. Keep it that short.

**Custom actions:** `src/engine/customActions.ts`. People define them per item
type on the Types page: each a script. `custom_actions` lists them and
`run_custom_action` runs one on an item. What a run says is a note in the item,
by `script`.

**Reads:** `get_item` (an item in full), `get_children` (the items in an item;
with `open`, its blockers), `list_items` (every item), `items_waiting_on`
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
| `task` | yes | `title`, `items?` (its first children, added in it), `assignees?` (who has it from the start; links, not kept in its params), `detached?` (sits in no item, wherever it is added), `key?` (so it is added once) | `{ how: closed \| cancelled }` |
| `note` | no | `text` | — |
| `approval` | a person's | `action` (unsafe), `inputs`, `reason?` | `{ ran, inputs, changed, output }` |
| `script` | while it lasts; it blocks nothing (a topic waits on it by being assigned to it) | `source`, `name?`, `description?`, `debounce_minutes?`, `cycle_minutes?` | `{ result, runs, logs }` or `{ error }` |
| `bot` | while the bot lasts (sits in no item) | `name`, `instructions?`, `provider?`, `model?` (opus), `debounce_minutes?` (1), `folder?`; the app sets `session_id`, `seen` and `last_run` | `{ cancelled }`, once deleted |
| `process` | — | switched off for now: kept so stored ones read; none can be added | — |
| `widget` | no | `html`, `title?`, `widget_state?` (its state, set again as it changes) | — |
| `slack_message` | no | `link`, or `channel` and `ts` (`thread_ts?`), `workspace?` | — (its card and view read Slack, through the cache, as shown) |
| `slack_channel` | no | `channel` (id or #name), `oldest`, `latest?`, `workspace?` | — (as `slack_message`) |
| `user` | while the person lasts (sits in no item) | `username` (its id), `name?` | `{ cancelled }`, once removed |

- **`script`** is a bot without a model (`SCRIPT_DOC` in `itemTypes.ts`, which
  agents read): it may sit on its own with a `name`, is given topics as a bot
  is (assigned, or `@`-ed), and wakes by the bot rule: when something happens to
  it (a topic given to it, a note in its own chat), its debounce after; else
  every `cycle_minutes` while it holds topics. Each run: `context` (the script's
  values, `state`, `runs`, `topics`, `now`, `trigger`) and `tools` in scope,
  with `mentions` and `refs`; it returns `{ messages, assign, state, done, data }`.
  It keeps its state on the topics it works on (`set_value`). Done, or
  throwing, it ends and hands back what it holds. It keeps `last_wake`.
  - Unsafe tools only when a person presses play (`runNow`, by them); on a
    schedule never, whoever wrote it.
  - `run_script` runs a body once, now, with the caller's tools.
## People, and who has what

- A **person** is a `user` item, in no item, whose id is their **username**.
  People sign in to the app by typing it (no password: the app is local); every
  call the app makes says who it is from (`x-workflows-user`). Signing in as a
  username nobody has yet makes that person.
- **Assignment is a link, not an event**: an `assigned_to` link in egdb from the
  item to the person or bot (`Store.setAssigned`), sorted by when; taking it off
  removes the link. The items someone has are their `assigned_to` links followed
  backwards (`Engine.assignedTo`). egdb keeps the link's history: who assigned
  it, and when.
- A new item goes to its `assignees` param if it has one; else, if a person made
  it, to them; else (a bot or a source made it) to nobody: a bot decides who has
  what it makes. A task's **owner** (`Engine.owner`), whom a bot hands it back to,
  is the person who made it, else its parent's owner.
- Agents assign with the `assign` action; a bot's update may say `assign` too.

## Bots

- A **bot** is a `bot` item, in no item: a name (people `@` it by that),
  instructions of its own, a provider (`claude`, Claude Code, for now: another is
  another entry in `PROVIDERS`, `workers.ts`) and model (opus, unless it says
  otherwise), a debounce (a minute), and a folder and a Claude session of its own,
  which last. Its settings are its params: changing them writes them anew (egdb
  keeps the old values) and wake it. Notes in it are its **own chat**.
  Deleting it cancels it, and what it had goes back to each item's owner. People make and edit bots on the Bots page; agents may make
  them too (`add_items`).
- **It wakes by the one rule** (`Engine.wake`, `dueAt` in wakes.ts), as a script
  does: once anything affected it since it last woke (an item assigned to it,
  even one it has; anything added or resolved in its items, not by its own runs;
  someone writing in its chat; its settings changing), its debounce after; and
  every `cycle_minutes` (`PERIODIC_MINUTES`, 15) after its last run while it
  has items, news or not. Woken by hand, at once. With no items, only news wakes it.
- **One run at a time**, for **every item it has**, in **one session**
  (`--session-id`, then `--resume`), in its own folder. Its instructions are its
  system prompt (`--append-system-prompt`), not part of any prompt; a
  SessionStart hook on `compact` says them again into the conversation after a
  compaction.
- **Prompt:** "Please action your assigned items, and give each a new assignee
  to carry it on", then its own chat (when someone wrote in it), then per item the
  items new since the bot was last prompted
  (its mark, set as each run starts), less what its own runs did, or "nothing
  new"; the task's whole history the first time its session sees it (`seen`).
  People's notes are given as they wrote them, after their name; anything else
  in brackets. Items are separated by `---`, each led by its id.
- **Output:** `{ updates: [{ item, message?, items?, assign?, close? }] }`, as
  structured output: per item, what to say, items to add, who has it from now on
  (usernames, bots' names; itself to keep it), and whether it is done. An update
  for its own id is its reply in its own chat.
- **After a run**, each item it has and said nothing of who has goes back to its
  owner, unless the bot left it waiting on something that is not a person (a wait
  it added, in this run or before), or something new came in while it ran. A run
  that fails says so in each item it has, and gives each back to its owner. A bot
  deleted while it runs lands nothing.
- The bot's `last_run` value says how its last run went: when it started and
  ended, its tasks, cost, turns, error and the bot's own words. egdb keeps the
  earlier ones as old values. A run cut short (the app stopped) is marked ended
  when the app starts again, and its tasks go back to their owners.
- **It is told never to close a task unless someone explicitly asked it to**;
  finishing the work is not being asked, so it hands the task back instead.

## The chat input

What a person types goes through a **preset**, which turns the text into items
(`src/engine/presets.ts`). Tab steps through the presets.

| preset | adds |
| --- | --- |
| Note (the default) | the note |

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
  and its params (a note's are just `type` and `text`); a blocker also has `resolved` (false, then true: when it turned true, and by whom, is egdb's event for that value, read with `value_events`), and `cancelled`, `data` and `state` once they apply. It is linked
  from its task, or from `@root`, by a `contains` link (sorted by its place), and
  who made it and when is that link's author and time. Who has a task is an
  `assigned_to` link per assignee. egdb keeps every value an entity had, and
  every state a link had, so nothing is lost by a change: there is no trace,
  run or transcript store of the app's own.
- A source made by an earlier build (values `params` and `created_by`) is
  refused: start a new source.
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
- Closing an item cancels its open blockers; tasks in it are left open.
