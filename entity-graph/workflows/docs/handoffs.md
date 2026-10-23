# Handoffs

An **item** is one thing that must be done. It has a **queue of handoffs**:
what has to happen to it next, and so who does it.

The first handoff in the queue is **active**: it is what the item waits for.
When it **resolves**, it keeps its **resolution**, and the next one becomes active.
When nothing is queued, a human handoff is added that says what just resolved.
The item's history is its handoffs in order, with the actions run and checks
failed between them. The app draws it as a chat: what the human set or did on
the right, everything else on the left.

## The interface

```ts
interface HandoffSpec { type: HandoffKind; params: Record<string, unknown> }  // what is set

interface HandoffType {                                     // src/engine/integrations.ts
  type; description; params: ZodRawShape; resolution: string
  expand?(params, author): HandoffSpec[]                    // as it is set, e.g. to put an approval in front
  start?(params, ctx): Promise<extra params | void>         // as it becomes active
  check?(params, ctx): Promise<resolution | null>           // a wait: polled with backoff
  run?(params, ctx): Promise<{ resolution, next }>          // instant: resolves as it starts
}
```

Human and bot handoffs have no hooks. They resolve when the human acts, or when
the bot's run ends. A wait resolves on `check`, or as `{ outcome: 'timed_out' }`
after `timeout_minutes`. Waits are checked as they start, then after 1, 2, 5, then
every 10 minutes.

`next` handoffs go at the **front** of the queue, ahead of what was queued
already. That is how a chain is built and then extended.

## Types

| type | params | resolves | resolution |
| --- | --- | --- | --- |
| `human` | `needed`: one direct question or request; `approval?`: `{ action, inputs }` | when the human hands off, approves, or closes | `{ decision: handed_off \| approved \| closed \| handled_outside, action?: { name, proposed, inputs, changed, output } }` |
| `bot` | `instructions` | when its `claude -p` run ends | `{ output, runId, costUsd, closed? }`, or `{ error, output }` |
| `github` | `repo`, `number`, `until`: `checks \| activity \| closed`, `timeout_minutes?` | on `check` | `{ outcome: passed \| failed \| activity \| merged \| closed \| timed_out, detail, failed? }` |
| `slack` | `channel`, `workspace?`, `message?`, `thread_ts?`, `timeout_minutes?` | on somebody else's reply in the thread | `{ outcome: replied \| timed_out, channel, thread_ts, replies, detail }` |
| `items` | `item_ids` | when every one has closed | `{ outcome: closed \| timed_out, items: { id, title }[] }` |
| `script` | `source`, `description?` | at once | `{ next, logs }` |

- **A slack handoff with a message that a bot sets** is expanded into a human
  approval of `slack_post`, then the wait. The wait takes its thread from what was posted.
- **A script** is a function expression, `async (input, actions) => handoffs`:
  - `input` is `{ item, previous, history }`. `previous` is the handoff resolved
    just before it, with its resolution.
  - It returns a handoff, a list of handoffs, or `null` to go on with the queue.
  - It runs in QuickJS, with no access to anything but `actions` and `console.log`.

## Who may do what

There is one list of **actions** (`src/engine/actions.ts`, `github.ts`,
`slack.ts`), and every call goes through `Engine.callAction`.

- An **unsafe** action changes something outside this app. Only a human may run
  one. Bots, and scripts that bots wrote, do not get it at all: their MCP tools
  leave it out, and `callAction` refuses it. A bot asks for one as a human
  handoff's `approval`; `unsafe_actions` lists them. See the unsafe rule in AGENTS.md.
- A **bot**, or a script a bot wrote, may change only the item it is
  working on.
- `hand_off` and `close_item` are only for a human. A bot hands an item on by
  returning `{ next, close? }` as its structured output. The schema it is
  given is every handoff type's params, so it cannot return one that doesn't
  exist. A script hands on by returning handoffs.

## Example

A bot pushes a fix and returns:

```json
{ "next": [
    { "type": "github", "params": { "repo": "o/r", "number": 7, "until": "checks" } },
    { "type": "script", "params": { "source": "async ({ previous }) => previous.resolution.outcome === 'failed' ? { type: 'bot', params: { instructions: 'Fix: ' + previous.resolution.failed.join(', ') } } : { type: 'human', params: { needed: 'Merge o/r#7?', approval: { action: 'github_pr_merge', inputs: { repo: 'o/r', number: 7, method: 'squash' } } } }" } }
  ] }
```
