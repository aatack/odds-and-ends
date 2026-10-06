# Refetch strategies

Written in ASD-STE100.

## What the app does now

- Each module gives a freshness time for each part of an entity (`ModuleView.foreign`).
- The cache loads a part when it is on screen and older than its freshness time.
- Each 20 s, the session examines all entities that the frontend cache has read in this session (`revisit`). Thus, an entity stays current after it goes off screen.
- `children` loads only for an entity that was opened.
- Each load gets all of the data again. No load uses a cursor.
- All entities use the same freshness time, on screen or not.

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

## Watch and catch up

Polling each item costs many calls. Where a service can tell the app about a change, use that.
A watch works only while the app runs. Thus, each watch needs a catch-up. A catch-up finds the changes that occurred while the app was closed.

Keep the time of the last event that each watch received (`watch.<service>.at`). Keep it in the cache store. When the cache store is cleared, this time is also cleared. The next start then does a full load. This is correct.

On start, do these steps in this sequence:

1. Read the time of the last event.
2. Open the watch. Keep the events that come in.
3. Do the catch-up from the time of the last event.
4. Apply the events that came in during step 3.

Open the watch before the catch-up. If not, the app can lose an event that occurs between the two steps.

An event or a catch-up does not write data to the screen. It marks entities as stale in the cache store, or it writes the data to the cache store. The frontend cache then reads it as usual.

### Slack

- **Watch:** Socket Mode. Slack sends events through a WebSocket. Subscribe to these user events: `message.channels`, `message.groups`, `message.im`, `message.mpim`, `reaction_added`, `reaction_removed`, `channel_marked`.
- Socket Mode needs an app-level token (`xapp-`, scope `connections:write`). Socket Mode must be on in the Slack app settings. You must do these steps in the Slack app settings. The app cannot do them.
- `apps.connections.open` is not on the read list in `api.ts`. Socket Mode needs it. It writes nothing.
- **An event:** a new message is a dated event. Write it to the cache store at its ts. Then increase the unread count of its conversation. `channel_marked` sets the read position. Thus, the app does not need to call `conversations.info` for each conversation all the time.
- **Catch-up:** `search.messages` with `after:<date of last event>`. One query gives the new messages in all conversations. Load only the conversations that it names. This needs the `search:read` user scope. The search gives days, not times. Thus, the catch-up can get some messages again. This is safe, because a cache write of the same event changes nothing.
- **Without a watch:** if there is no app-level token, use the polling in "Item types".

### GitHub

- **Watch:** GitHub has no WebSocket for a user, and a webhook needs a public server. Thus, poll. But poll the notifications, not each PR.
- Poll `GET /notifications` through `gh api`. Send `If-Modified-Since` with the `Last-Modified` value from the last response. When nothing changed, GitHub sends `304`, and the call does not count against the rate limit. Obey `X-Poll-Interval` (usually 60 s).
- **An event:** a notification names a PR and its `updated_at`. Mark that PR stale, so the cache loads it again.
- Notifications do not show changes to CI checks. Thus, keep the fast poll for a PR with pending checks (30 s), and the list poll of `github.home` (2 min).
- Notifications do not show pushes to a branch that is not in a PR. To watch a branch, poll its head commit with a conditional REST request (`GET /repos/{owner}/{repo}/branches/{branch}` with `If-None-Match`). A `304` is free.
- **Catch-up:** `GET /notifications?all=true&since=<time of last event>`. Also load `github.home` one time.

## Changes to make

1. Add `ModuleView.watch(entity)`. It gives an interval, or null. The core keeps a timer for each watched entity while the app is open.
2. Let the freshness time come from the entity, not only from its type. Then a merged PR is final, and a PR with pending checks is fast.
3. Give each loader the entity that it loads. Then the loader can read its cursor (`latestTs`, the newest reply) and send `oldest`.
4. Add a change signal to the cache: a load of a list compares `updatedAt` and `latestReply`, and marks only the changed items as stale.
5. Each 10 min, do a full load of the newest window of an open conversation, to get edits and reactions.
6. Add watches to the core: a Slack Socket Mode client and a GitHub notifications poll. Each keeps its last event time and does a catch-up on start.
7. While a watch is open, make the freshness times of the items that it covers long. The watch then does the work, and the polls are only a fallback.
