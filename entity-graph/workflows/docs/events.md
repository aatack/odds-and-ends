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

```ts
interface EventSpec { type: EventKind; params: Record<string, unknown>; ref?: string }  // what is added

interface EventType {                                      // src/engine/integrations.ts
  type; description; params: ZodRawShape; data: string
  blocking: boolean                                        // false: resolves as it is added
  human?: boolean                                          // a blocker the human resolves
  bots?: boolean                                           // a bot may add it
  expand?(params, author): EventSpec[]                     // as it is added, e.g. an approval in front
  start?(params, ctx): Promise<{ params?, resolve? }>      // as it is added: may change params, or resolve
  check?(params, ctx): Promise<data | null>                // a wait: polled with backoff
}
```

`ref` names an event within the list it is added with. An `after` in the same list
can then wait for it, using the ref in place of an id.

## Types

| type | blocks | params | data |
| --- | --- | --- | --- |
| `message` | no | `text` | — |
| `action` | no | `name`, `inputs`, `output` | — |
| `closed` | no | `how`: `closed` \| `handled_outside` | — |
| `approval` | the human's | `action` (unsafe), `inputs`, `reason?` | `{ ran, inputs, changed, output, detail }` |
| `bot` | yes | `instructions?` | `{ runId, message?, costUsd?, output }` or `{ error }` |
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
