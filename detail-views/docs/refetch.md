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
| `slack.home` | children | Freshness time | 60 min |
| `slack.conversation` | self | Load once, then the watch | Never again |
| `slack.conversation` | children | Load once, then the watch | Never again |
| `slack.message` | self | Load once | Never again |
| `slack.message` | children | Load once, then the watch | Never again |
| `slack.user` | self | Freshness time | 7 days |
| Slack images | blob | Load once | Never again |
| `github.home` | children | Watch, change signal | 2 min |
| `github.pr` | self | Change signal, fast while busy, load once when final | 30 s to 5 min |
| `github.pr` | children | Change signal | With `self` |
| `github.check`, `github.item` | self | None. They come with their PR. | None |
| `task`, `tasks.home`, `github.localApproval` | none | None. They are owned. | None |

### Slack: load once, then watch (done)

- A conversation, its unread count and a thread load one time, when they are first looked at.
- After that, the watch (`Slack.poll`) keeps them current. The app does not load them again, unless I refresh one (`r`).
- Each 15 s, the watch does one `search.messages` call: all messages after the newest message that it saw, newest first. It reads pages until it gets to a message that it saw before.
- For each new message, the watch:
  - writes the message at its ts, as the history load does;
  - links it under its conversation, or under its thread parent when it is a reply;
  - sets the `latestTs` of the conversation, and adds 1 to its unread count (not for replies, and not for my own messages);
  - when the message is mine, sets the read position of the conversation to it;
  - adds a conversation that is not in the list (a new DM, for example).
- The watch ignores a message that the cache store has. Thus, two looks at the same message do not count it two times.
- The watch keeps the ts of the newest message that it saw on the Slack root (`watch.at`), in the cache store.
- **Catch-up:** on start, the first poll reads everything after `watch.at`. This finds the messages that came while the app was closed.
- If the catch-up needs more than 10 pages (1000 messages), the watch marks all conversations as not loaded. Each then loads whole when it is next looked at.
- If the cache store is new, there is no `watch.at`. The first poll only sets it. Each conversation then loads whole when it is first looked at.
- Search indexes a message a short time after it is sent. Thus, each poll looks again at the 120 s before `watch.at`.

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

- The watch is a poll of `search.messages`. See "Slack: load once, then watch".
- Socket Mode can replace the poll later. It sends events at once, but it needs an app-level token (`xapp-`) and changes to the Slack app settings. The catch-up stays the same.

### GitHub

- **Watch:** GitHub has no WebSocket for a user, and a webhook needs a public server. Thus, poll. But poll the notifications, not each PR.
- Poll `GET /notifications` through `gh api`. Send `If-Modified-Since` with the `Last-Modified` value from the last response. When nothing changed, GitHub sends `304`, and the call does not count against the rate limit. Obey `X-Poll-Interval` (usually 60 s).
- **An event:** a notification names a PR and its `updated_at`. Mark that PR stale, so the cache loads it again.
- Notifications do not show changes to CI checks. Thus, keep the fast poll for a PR with pending checks (30 s), and the list poll of `github.home` (2 min).
- Notifications do not show pushes to a branch that is not in a PR. To watch a branch, poll its head commit with a conditional REST request (`GET /repos/{owner}/{repo}/branches/{branch}` with `If-None-Match`). A `304` is free.
- **Catch-up:** `GET /notifications?all=true&since=<time of last event>`. Also load `github.home` one time.

## Changes to make

1. Add `ModuleView.watch(entity)` for GitHub. It gives an interval, or null. The core keeps a timer for each watched entity while the app is open.
2. Let the freshness time come from the entity, not only from its type. Then a merged PR is final, and a PR with pending checks is fast.
3. Give each loader the entity that it loads. Then the loader can read its cursor (`latestTs`, the newest reply) and send `oldest`.
4. Add a change signal to the cache: a load of a list compares `updatedAt` and `latestReply`, and marks only the changed items as stale.
5. Each 10 min, do a full load of the newest window of an open conversation, to get edits and reactions.
6. Add a GitHub notifications poll. It keeps its last event time and does a catch-up on start. (The Slack watch is done.)
7. While a watch is open, make the freshness times of the items that it covers long. The watch then does the work, and the polls are only a fallback.
