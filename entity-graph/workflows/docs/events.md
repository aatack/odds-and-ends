# Events

Everything is an **event**, items included.

- An **item** is an event that stands for one thing to be done. Other events sit
  **in** it (their parent), and so can other items: the events form one tree.
  Its children are its history.
- A plain event resolves as it is added: a note.
- A **blocker** is an event that stays unresolved until something happens: a
  script says it is done, a person approves, a child item closes.
- An item is **assigned** to people and to bots (`assigned_to` links, not events).
  A person is a `user` event and a bot a `bot` event, each in no item. See
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

Every event has the same shape, whatever its type: `ItemEvent`, in
`src/shared/types.ts`. A type decides only what its `params` and `data` hold,
how it reads (`summary`, `message`), and whether and how it waits.

### Every event (`ItemEvent`)

| field | what it is |
| --- | --- |
| `id` | Its id. The trace refers to it. |
| `parentId` | The item (or bot) it sits in. Null for an item, a bot or a person that sits in none. |
| `type` | Its type: one of the table below. |
| `params` | What it was given, as its type's params schema says. |
| `createdBy`, `createdAt` | Who added it, and when: a person's username, `bot:<id>` (a bot's run), `bot` (an agent acting as a bot), `system`, `script`, `script:bot`, or `mcp`. |
| `resolved`, `resolvedAt`, `resolvedBy` | Whether it has happened, when, and who resolved it. A plain event is resolved by whoever added it, as it is added. |
| `cancelled` | Resolved without happening. |
| `data` | What it resolved with, as its type's data says. Null while it is open. |
| `actor` | Who made it happen: `resolvedBy` for a blocker, `createdBy` for a plain event. The chat puts the signed-in person's on the right. |
| `summary` | One line saying what it is, from its type's `describe`. This is a blocker's row while it is open, and how prompts name it. |
| `message` | What it says in the chat once resolved, from its type's `said`, or `Cancelled: …` when cancelled. Null while open, or when it says nothing. |
| `nextCheckAt`, `lastError` | For blockers the loop checks: when the next check is, and what went wrong on the last one. |
| `times` | Its timeline from the trace: added, first and last checked, bot run started and finished, resolved. |

Events are ordered within their item by when they were added. Events added
together keep the order they were given in.

An item's view of itself (`Item`) is read from its event: its title, whether it
has closed and how, its open children (`blockers`), who has it (`assignees`),
and whether it is the turn of the person looking (`withYou`). Which bot is running on it is held in
memory, not stored, so a crash leaves nothing to clean up.

**For agents:** `src/engine/paradigm.ts` is the paradigm in a few lines: the MCP
server's instructions, and the `paradigm` action. Keep it that short.

**Custom actions:** `src/engine/customActions.ts`. People define them per event
type on the Types page: each a script. `custom_actions` lists them and
`run_custom_action` runs one on an event. What a run says is a note in the item,
by `script`; the trace has the run.

**Reads:** `get_item` (an item in full), `get_children` (the events in an item;
with `open`, its blockers), `list_items` (every item), `items_waiting_on`
(the open items waiting on a person, any bot, one bot, or one blocking type),
`next_for_human` (what a person's Next would show: of the items that are their
turn, the least recently touched), `list_people` and `list_bots`.

### Every type (`EventType`)

Defined in `src/engine/integrations.ts` and written in `src/engine/eventTypes.ts`.

| member | what it is |
| --- | --- |
| `type`, `description` | Its name, and what it is for (shown to bots by `event_types`, and on the MCP page). |
| `params` | A zod shape. Params are checked against it as the event is added. |
| `data` | What it resolves with, in words. |
| `blocking` | False: it resolves as it is added. True: it is a blocker. |
| `human` | A blocker a person resolves. While only these are open, it is the turn of whoever has the item. |
| `bots` | A bot may add it in its output. |
| `describe(params)` | Its `summary`. |
| `said(params, data)` | Its `message` once resolved; null for nothing. Left out, it says its summary. Not called when the event is cancelled. |
| `expand(params, author)` | Optional: turns it into the events actually added. |
| `start(params, ctx)` | Optional: runs as it is added. It may change the params, or resolve the event at once. |
| `check(params, ctx)` | Optional, for waits: answers the data once the event has happened, or null while it hasn't. Polled with backoff. |

**To add a type:** write it in `eventTypes.ts` and add its name to `EventKind`.
Give it an icon in the renderer's `icons.tsx`, and a row in the table below. If
the chat input should offer it, add a preset in `presets.ts`.

## Types

| type | blocks | params | data |
| --- | --- | --- | --- |
| `item` | yes | `title`, `events?` (its first children, added in it), `assignees?` (who has it from the start; links, not kept in its params), `detached?` (sits in no item, wherever it is added), `key?` (so it is added once) | `{ how: closed \| cancelled }` |
| `note` | no | `text` | — |
| `approval` | a person's | `action` (unsafe), `inputs`, `reason?` | `{ ran, inputs, changed, output }` |
| `script` | yes | `source`, `description?`, `schedule?` (seconds), `timeout_minutes?` | `{ result, message?, runs, logs }` or `{ error, logs }` |
| `bot` | while the bot lasts (sits in no item) | `name`, `instructions?`, `provider?`, `model?` (opus), `debounce_minutes?` (1), `folder?`, `sessionId` (set by the app) | `{ cancelled }`, once deleted |
| `user` | while the person lasts (sits in no item) | `username` (its id), `name?` | `{ cancelled }`, once removed |

- **`script`** waits for anything. Its `source` is
  `async (ctx, actions) => result`, run on its `schedule`.
  - The schedule is seconds: the wait before the first run, then between runs,
    with the last repeating. `[1, 5, 10]` runs after 1 second, then 5 more, then
    every 10. The default is `[1, 5, 10, 30, 60]`.
  - Runs are at least a second apart. A run never starts while the last is still
    going. Every blocker's checks work this way; each type has a schedule, and
    the default is at once, then 1, 2, 5 and then every 10 minutes.
  - `ctx` is `{ item, event: { id, description, createdAt, runs }, now, state }`.
  - It returns `null` or `{ done: false, state? }` to keep blocking. The `state`
    is handed to its next run.
  - It returns `{ done: true, data?, message? }` to finish. It resolves with
    `{ result: data, message, runs }`, and `message` (if given) is what the chat says.
  - A run that throws, or returns anything else, ends it with `{ error }`.
  - It runs in QuickJS, with nothing but `actions` and `console.log`. A script a
    bot wrote gets no unsafe actions.
## People, and who has what

- A **person** is a `user` event, in no item, whose id is their **username**.
  People sign in to the app by typing it (no password: the app is local); every
  call the app makes says who it is from (`x-workflows-user`). Signing in as a
  username nobody has yet makes that person.
- **Assignment is a link, not an event**: an `assigned_to` link in egdb from the
  item to the person or bot (`Store.setAssigned`), sorted by when; taking it off
  removes the link. The items someone has are their `assigned_to` links followed
  backwards (`Engine.assignedTo`). Each change is traced twice, as `assigned` or
  `unassigned`: in the item (`data.to`) and in the assignee (`data.item`). The
  chat shows them as notes, and "Handed lately" and `recently_assigned` read them.
- A new item goes to its `assignees` param if it has one; else, if a person made
  it, to them; else (a bot or a source made it) to nobody: a bot decides who has
  what it makes. An item's **owner** (`Engine.owner`), whom a bot hands it back to,
  is the person who last had it, else the person who made it, else its parent's owner.
- Agents assign with the `assign` action; a bot's update may say `assign` too.

## Bots

- A **bot** is a `bot` event, in no item: a name (people `@` it by that),
  instructions of its own, a provider (`claude`, Claude Code, for now: another is
  another entry in `PROVIDERS`, `workers.ts`) and model (opus, unless it says
  otherwise), a debounce (a minute), and a folder and a Claude session of its own,
  which last. Its settings are its params: changing them writes them anew (egdb
  keeps the old values) and traces `configured`. Notes in it are its **own chat**.
  Deleting it cancels it, and what it had goes back to each item's owner. People make and edit bots on the Bots page; agents may make
  them too (`add_events`).
- **It wakes when something happens to it** (`Engine.wake`), its debounce after:
  an item is assigned to it (assigning one it has already writes the link again,
  so it wakes), its settings change, or someone writes in its chat; or at once
  when woken by hand. What its
  own runs do is not something happening to it. And while something has
  happened in the items it has since its last run (an event added, resolved or
  cancelled, not by its own runs; a child item closing always counts), it wakes
  every `PERIODIC_MINUTES` (15) after its last run. With no items, it never wakes.
- **One run at a time**, for **every item it has**, in **one session**
  (`--session-id`, then `--resume`), in its own folder. Its instructions are its
  system prompt (`--append-system-prompt`), not part of any prompt; a
  SessionStart hook on `compact` says them again into the conversation after a
  compaction.
- **Prompt:** "Please action your assigned items, and give each a new assignee
  to carry it on", then its own chat (when someone wrote in it), then per item the
  events new since the bot was last prompted
  (its place in the trace is its last run's `trace_mark`), less what its own runs
  did, or "nothing new"; the item's whole history the first time the bot sees it.
  People's notes are given as they wrote them, after their name; anything else
  in brackets. Items are separated by `---`, each led by its id.
- **Output:** `{ updates: [{ item, message?, events?, assign?, close? }] }`, as
  structured output: per item, what to say, events to add, who has it from now on
  (usernames, bots' names; itself to keep it), and whether it is done. An update
  for its own id is its reply in its own chat.
- **After a run**, each item it has and said nothing of who has goes back to its
  owner, unless the bot left it waiting on something that is not a person (a wait
  it added, in this run or before), or something new came in while it ran. A run
  that fails says so in each item it has, and gives each back to its owner. A bot
  deleted while it runs lands nothing.
- Runs (`runs`) keep the bot, the items, the prompt and, once over, the whole
  stream-json transcript.

## The chat input

What a person types goes through a **preset**, which turns the text into events
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
  egdb-server (`egdb.ts`, `EgdbSink`): an entity per event (its `type` the
  event type, linked as a `child` of its item, or of `@root`), per trace row
  (`trace:<n>`), run (`run:<id>`), setting and custom action, and per finished
  transcript (`transcript:<id>`, in parts, read only when asked for); and an
  `assigned_to` link per assignment. egdb keeps every value an entity had, and
  every state a link had, so nothing is lost by a change.
- When a blocker is next checked, its backoff step and its last error are held
  in memory only: after a restart every wait is checked at once.
- A source starts empty: nothing is brought in from the older app's store.

## Who may do what

There is one list of **actions**, and every call goes through `Engine.callAction`.

- **Unsafe** actions change something outside this app, and only the human runs
  one, through an `approval` event. No agent gets them: both MCP endpoints leave
  them out, scripts an agent wrote get none, and `callAction` refuses them to
  anyone but the human. See the unsafe rule in AGENTS.md.
- Every other action is the same for every agent, a bot or anything connected:
  `add_events`, `cancel_event` and `close_item` included, on any item. A bot can
  also add events through its output, and a script by what it returns.
- Closing an item cancels its open children, closing child items as cancelled,
  all the way down.
