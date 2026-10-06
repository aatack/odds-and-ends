# Refetch strategies

Written in ASD-STE100.

## What the app does now

- Each module gives a freshness time for each part of an entity (`ModuleView.foreign`).
- The cache loads a part when it is on screen and older than its freshness time.
- Each 20 s, the session examines the entities on screen again (`revisit`).
- Each load gets all of the data again. No load uses a cursor.
- Nothing loads when the entity is not on screen.

## Strategies

Use these strategies. One item type can use more than one.

1. **Load once.** The data does not change. Load it one time. Load it again only when the cache store is cleared.
2. **Freshness time.** Load the part again when it is on screen and older than its freshness time. This is the default.
3. **Cursor.** Keep the newest timestamp that you have on the entity. Ask the service only for data newer than this timestamp. Do not load old data again.
4. **Change signal.** Get a small value that shows a change (for example, `updatedAt` or `latest_reply`). Do the large load only when this value changed.
5. **Watch.** Load the part on a timer while the app is open, also when it is not on screen.
6. **Fast while busy.** Use a short interval while the item is in a busy state. Use a long interval, or stop, when the item is in a final state.

## Item types

| Item | Part | Strategies | Interval |
|-|-|-|-|
| `slack.home` | children | Freshness time | 10 min |
| `slack.conversation` | self | Freshness time, watch (some) | 5 min on screen; 1 min for watched |
| `slack.conversation` | children | Cursor, freshness time | 30 s when open |
| `slack.message` | self | Load once | Never again |
| `slack.message` | children | Change signal, cursor | 30 s when open |
| `slack.user` | self | Freshness time | 7 days |
| Slack images | blob | Load once | Never again |
| `github.home` | children | Watch, change signal | 2 min |
| `github.pr` | self | Change signal, fast while busy, load once when final | 30 s to 5 min |
| `github.pr` | children | Change signal | With `self` |
| `github.check`, `github.item` | self | None. They come with their PR. | None |
| `task`, `tasks.home`, `github.localApproval` | none | None. They are owned. | None |

### `slack.home`

- The list of conversations changes almost never.
- Load it again after 10 min.
- Also load it again when a message refers to a conversation that is not in the list.

### `slack.conversation`, self (unread count)

- Slack has no call that gives all unread counts. Each conversation needs one call.
- Load the counts for conversations on screen after 5 min. Keep these calls behind urgent calls. The app does this now.
- Watch the conversations that are most important: DMs, group DMs, and conversations with unread messages. Load these each 1 min.
- Do not watch all conversations. 600 conversations at 90 calls each minute use the full rate limit.

### `slack.conversation`, children (messages)

- Use a cursor. Keep `latestTs` (the newest message that you have) on the conversation. The app keeps this value now.
- Call `conversations.history` with `oldest = latestTs`. Slack then sends only new messages.
- A cursor does not show edits, deletions or new reactions on old messages. Thus, each 10 min, load the newest 100 messages again while the conversation is open.
- When the cache store is cleared, `latestTs` is also cleared. The next load is then a full load. This is correct.

### `slack.message`, self

- A message comes with its conversation history. It has a date and almost never changes.
- Load it alone only when a link points to it and the cache store does not have it.

### `slack.message`, children (thread replies)

- Use a change signal. The parent message has `latestReply` and `replyCount`. The conversation history updates these values.
- Load the thread only when `latestReply` is newer than the newest reply that you have.
- Use a cursor: call `conversations.replies` with `oldest` set to the newest reply that you have.

### `slack.user`

- Names change almost never. Load a user again after 7 days.

### Slack images

- An image is fixed by its file id. Load it one time.

### `github.home`

- Watch this list. Load it each 2 min while the app is open.
- The list gives `updatedAt` for each PR. Use this value as the change signal for the PR.

### `github.pr`

- The full PR query is large. Do it only when a change signal shows a change:
  - The `updatedAt` from the list changed.
  - The PR is open on screen and older than its freshness time.
- Use a short interval while the checks are pending: 30 s. Use 5 min when no check is pending.
- When the PR is merged or closed, it is final. Do not load it again unless I ask for it (`r`).
- `gh` cannot get push events from GitHub. Thus, a timer is necessary. A webhook needs a public server. Do not use one.
- Do not use conditional requests (`If-None-Match`). They work only on the REST API. The app uses GraphQL.

### `github.check`, `github.item`

- These come with their PR. Do not load them alone.
- `github.item` (description, comment, review) has a date. A cursor is possible but not necessary, because the full PR query is one call.

### Owned items

- `task`, `tasks.home` and `github.localApproval` are in the owned store. The app does not load them from a service.

## Changes to make

1. Add `ModuleView.watch(entity)`. It gives an interval, or null. The core keeps a timer for each watched entity while the app is open.
2. Let the freshness time come from the entity, not only from its type. Then a merged PR is final, and a PR with pending checks is fast.
3. Give each loader the entity that it loads. Then the loader can read its cursor (`latestTs`, the newest reply) and send `oldest`.
4. Add a change signal to the cache: a load of a list compares `updatedAt` and `latestReply`, and marks only the changed items as stale.
5. Each 10 min, do a full load of the newest window of an open conversation, to get edits and reactions.
