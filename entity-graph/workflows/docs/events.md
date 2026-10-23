# Events

Everything is an **event**, items included.

- An **item** is an event that stands for one thing to be done. Other events sit
  **in** it (their parent), and so can other items: the events form one tree.
  Its children are its history.
- A plain event resolves as it is added: a message, an action run.
- A **blocker** is an event that stays unresolved until something happens: a bot
  answers, CI finishes, somebody replies, the human approves, a child item closes.
- An item is itself a blocker: it blocks the item it sits in until it closes.
  So the deepest items are dealt with before the items they sit in.
- An item can have any number of blockers at once: its open children.
- An open item is **the human's turn** when nothing blocks it, or only things the
  human resolves do: approvals, and afters waiting on them.
- There are no top-level items as such. An item may sit in none, but every list
  shows every item, at any depth.

A blocker either resolves itself, because the loop runs its `check` with
backoff (at once, then after 1, 2, 5 and then every 10 minutes), or is resolved
from outside: by a bot's run ending, by the human approving, or by a cancel.
When it resolves, it keeps its **data**.

## The interface

Every event has the same shape, whatever its type: `ItemEvent`, in
`src/shared/types.ts`. A type decides only what its `params` and `data` hold,
how it reads (`summary`, `message`), and whether and how it waits.

### Every event (`ItemEvent`)

| field | what it is |
| --- | --- |
| `id` | Its id. `after` events and the trace refer to it. |
| `parentId` | The item it sits in. Null only for an item that sits in none. |
| `type` | Its type: one of the table below. |
| `params` | What it was given, as its type's params schema says. |
| `createdBy`, `createdAt` | Who added it, and when: `human`, `bot`, `system`, `script`, `script:bot`, `mcp`, or a source's id. |
| `resolved`, `resolvedAt`, `resolvedBy` | Whether it has happened, when, and who resolved it. A plain event is resolved by whoever added it, as it is added. |
| `cancelled` | Resolved without happening. |
| `data` | What it resolved with, as its type's data says. Null while it is open. |
| `actor` | Who made it happen: `resolvedBy` for a blocker, `createdBy` for a plain event. The chat puts the human's (and an MCP client's) on the right. |
| `summary` | One line saying what it is, from its type's `describe`. This is a blocker's row while it is open, and how prompts name it. |
| `message` | What it says in the chat once resolved, from its type's `said`, or `Cancelled: …` when cancelled. Null while open, or when it says nothing. |
| `nextCheckAt`, `lastError` | For blockers the loop checks: when the next check is, and what went wrong on the last one. |
| `times` | Its timeline from the trace: added, first and last checked, bot run started and finished, resolved. |

Events are ordered within their item by when they were added. Events added
together keep the order they were given in.

An item's view of itself (`Item`) is read from its event: its title, whether it
has closed and how, its open children (`blockers`), whether it is the human's
turn, and its Claude session. Which worker is running a bot on it is held in
memory, not stored, so a crash leaves nothing to clean up.

**Reads:** `get_item` (an item in full), `get_children` (the events in an item;
with `open`, its blockers), `list_items` (every item), `items_waiting_on`
(the open items waiting on the human, a bot, or one blocking type), and
`next_for_human` (what Next would show).

### Every type (`EventType`)

Defined in `src/engine/integrations.ts` and written in `src/engine/eventTypes.ts`.

| member | what it is |
| --- | --- |
| `type`, `description` | Its name, and what it is for (shown to bots by `event_types`, and on the MCP page). |
| `params` | A zod shape. Params are checked against it as the event is added. |
| `data` | What it resolves with, in words. |
| `blocking` | False: it resolves as it is added. True: it is a blocker. |
| `human` | A blocker the human resolves. While only these are open (and afters waiting on them), it is the human's turn. |
| `bots` | A bot may add it in its output. |
| `describe(params)` | Its `summary`. |
| `said(params, data)` | Its `message` once resolved; null for nothing. Left out, it says its summary. Not called when the event is cancelled. |
| `expand(params, author)` | Optional: turns it into the events actually added. For example, a bot's Slack message becomes an approval and a wait. |
| `start(params, ctx)` | Optional: runs as it is added. It may change the params (such as finding a PR by its title), or resolve the event at once. |
| `check(params, ctx)` | Optional, for waits: answers the data once the event has happened, or null while it hasn't. Polled with backoff. |

**To add a type:** write it in `eventTypes.ts` and add its name to `EventKind`.
Give it an icon in the renderer's `icons.tsx`, and a row in the table below. If
the chat input should offer it, add a preset in `presets.ts`.

## Types

| type | blocks | params | data |
| --- | --- | --- | --- |
| `item` | yes | `title`, `events?` (its first children, added in it), `detached?` (sits in no item, wherever it is added), `key?` (so a source adds it once), `sessionId` (set by the app) | `{ how: closed \| handled_outside \| cancelled }` |
| `message` | no | `text` | — |
| `attachment` | no | `event`, `title`, `item` (filled in by `attach_event`) | — |
| `action` | no | `name`, `inputs`, `output` | — |
| `approval` | the human's | `action` (unsafe), `inputs`, `reason?` | `{ ran, inputs, changed, output, detail }` |
| `bot` | yes | `instructions?` (it reads as "Bot" without) | `{ runId, message?, costUsd?, output }` or `{ error }` |
| `github` | yes | `repo`+`number`, or `query`; `until`: `checks` \| `activity` \| `merged`; `timeout_minutes?` | `{ outcome, detail, failed? }` |
| `slack` | yes | `channel`, `workspace?`, `message?`, `thread_ts?`, `thread_of?`, `timeout_minutes?` | `{ outcome, replies, detail }` |
| `after` | yes | `event`, then `then` or `script` | `{ added, logs? }` or `{ error }` |
| `script` | yes | `source`, `description?`, `schedule?` (seconds), `timeout_minutes?` | `{ result, message?, runs, logs }` or `{ error, logs }` |

- **`after`** is how events chain. It waits for one event to resolve, then adds
  `then`, or whatever its `script` returns.
  - A script is `async (input, actions) => events`, where `input` is
    `{ item, event }`: the event it waited for, with its data.
  - The script runs in QuickJS, with no access to anything but `actions` and
    `console.log`.
- **`script`** waits for anything the other types don't cover. Its `source` is
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
  - Like an `after`'s script, it runs in QuickJS. A script a bot wrote gets no
    unsafe actions.
- **`attachment`** gathers another event in an item, wherever that event sits:
  ideas to build together, say. It only points at it (`event`, and `item`, the
  item to read it in); the event stays where it was. The human attaches from the
  paperclip at the top right of any event's view; agents use `attach_event`.
- **A `slack` event with a message that a bot adds** becomes an approval of
  `slack_post`, followed by an after that starts the wait in the thread the
  message posted.

## Bots

- Every item has **its own Claude session**, its id kept in the item's params
  (`sessionId`) once made. Every later run resumes it (`--resume`), so a bot
  remembers the item.
- An item's first run starts a new session. If an item it sits in (at any depth)
  has a session, the new one is **forked** from the nearest such session
  (`--resume <it> --fork-session --session-id <new>`). The new session is the
  item's own from then on, and its bot knows how the item came about.
- At most four bots run at once, and only one runs on an item at a time.
- A run answers every bot blocker that was waiting on its item when it started.
- A bot added while one runs on its item waits for that run to end, then is told
  everything since it started: messages can be queued freely. While a bot is
  queued (open, not yet taken up by a run), another bot with no new
  instructions is not added in its item; a ref to it stands for the queued one.
- **Prompt:** the item and its events on the first run. After that, every event
  added or resolved since the bot was last prompted. Each run keeps its place in
  the trace (`trace_mark`) to know what is new. What this item's own runs did is
  left out, because the session already holds it. What other bots did, including
  a child item's, is included.
- **Output:** `{ message?, events?, close? }`, as structured output.
  - `message` is to the human: what the bot did, or one direct question.
  - `events` are events to add in the item: blockers, and child items to split
    work off (briefed with their first `events`).
  - With no events, and the item still open, it is the human's turn.
  - Whatever the bot wrote along the way is kept in its bot event's data, as `output`.

## The chat input

What the human types goes through a **preset**, which turns the text into events
(`src/engine/presets.ts`). Tab steps through the presets.

| preset | adds |
| --- | --- |
| Bot (the default) | the message, then a bot blocker |
| Message | the message only |
| PR merged | a `github` wait on the pull request the text names, until merged |
| PR checks | a `github` wait on the pull request the text names, until its checks finish |

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
