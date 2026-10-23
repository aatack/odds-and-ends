# Events

An **item** is one thing that must be done. Its history is a list of **events**.

- A plain event resolves as it is added: a message, an action run, the item closing.
- A **blocker** is an event that stays unresolved until something happens: a bot
  answers, CI finishes, somebody replies, the human approves.
- An item can have any number of blockers at once.
- An open item is **the human's turn** when nothing blocks it, or only things the
  human resolves do: approvals, and afters waiting on them.

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
| `itemId` | The item it belongs to. |
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
| `message` | no | `text` | — |
| `action` | no | `name`, `inputs`, `output` | — |
| `closed` | no | `how`: `closed` \| `handled_outside` | — |
| `approval` | the human's | `action` (unsafe), `inputs`, `reason?` | `{ ran, inputs, changed, output, detail }` |
| `bot` | yes | `instructions?` (it reads as "Bot" without) | `{ runId, message?, costUsd?, output }` or `{ error }` |
| `github` | yes | `repo`+`number`, or `query`; `until`: `checks` \| `activity` \| `merged`; `timeout_minutes?` | `{ outcome, detail, failed? }` |
| `slack` | yes | `channel`, `workspace?`, `message?`, `thread_ts?`, `thread_of?`, `timeout_minutes?` | `{ outcome, replies, detail }` |
| `items` | yes | `item_ids`, `timeout_minutes?` | `{ outcome, items }` |
| `after` | yes | `event`, then `then` or `script` | `{ added, logs? }` or `{ error }` |

- **`after`** is how events chain. It waits for one event to resolve, then adds
  `then`, or whatever its `script` returns.
  - A script is `async (input, actions) => events`, where `input` is
    `{ item, event }`: the event it waited for, with its data.
  - The script runs in QuickJS, with no access to anything but `actions` and
    `console.log`.
- **A `slack` event with a message that a bot adds** becomes an approval of
  `slack_post`, followed by an after that starts the wait in the thread the
  message posted.

## Bots

- Every item has **its own Claude session**. The first bot run on the item starts
  it (`--session-id`), and every later run resumes it (`--resume`), so a bot
  remembers the item.
- At most four bots run at once, and only one runs on an item at a time.
- A run answers every bot blocker that was waiting on its item when it started.
- **Prompt:** the item and its events on the first run. After that, every event
  added or resolved since the bot was last prompted. Each run keeps its place in
  the trace (`trace_mark`) to know what is new. What the bot's own runs did is
  left out, because the session already holds it.
- **Output:** `{ message?, events?, close? }`, as structured output.
  - `message` is to the human: what the bot did, or one direct question.
  - `events` are blockers to add.
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

- **Unsafe** actions change something outside this app, and only a human may run
  one. Bots, and scripts that bots wrote, do not get them: their MCP tools leave
  them out, and `callAction` refuses them. A bot asks for one with an `approval`
  event. See the unsafe rule in AGENTS.md.
- `add_events`, `cancel_event` and `close_item` are only for a human. A bot adds
  events through its output, and a script by what it returns.
