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
| `slack.home` | children (the lists) | Freshness time | 60 min |
| `slack.home` | history | One batch, then the watch; more on demand | 15 s (watch) |
| `slack.conversation` | history | The watch; more on demand | Never alone |
| `slack.message` | self | Load once, only when no load or search got it | Never again |
| `slack.message` | children | The watch; the full thread on demand | Never alone |
| `slack.user` | self | With the lists; alone only for a user that `users.list` does not have | 60 min / 7 days |
| Slack images | blob | Load once | Never again |
| `github.home` | children | Watch, change signal | 2 min |
| `github.pr` | self | Change signal, fast while busy, load once when final | 30 s to 5 min |
| `github.pr` | children | Change signal | With `self` |
| `github.check`, `github.item` | self | None. They come with their PR. | None |
| `task`, `tasks.home`, `github.localApproval` | none | None. They are owned. | None |

### Slack: lists, one batch, then the watch (done)

- The app does not keep unread counts.
- On its own, the app loads only these:
  - The lists of the workspace, each in a few calls: my conversations (`users.conversations`), all public channels (`conversations.list`) and all users (`users.list`). Load them again after 60 min.
  - The first batch of history: the newest 1000 messages in all conversations, by one search (about 10 calls).
  - The watch (`Slack.poll`), each 15 s.
- The app does not load a conversation or a thread alone, unless I ask for it (`o`).
- The workspace lists the conversations (channels, DMs, group DMs) and the threads in them.
- A reply adds its thread to the workspace. A message from history that has replies also adds its thread.
- The list shows the item with the newest event first (`updatedAt`). For a conversation, this is the link to its newest message. For a thread, this is the link to its newest reply. A load writes at timestamp 0. Thus, a load does not move an item. A conversation with no message in the batch stays at the bottom.
- The load of the lists (each 60 min) replaces only the conversation links of the workspace. It keeps the thread links.
- The header of the workspace, of a conversation and of a thread shows where the cached history starts (`from`), and a button (**Older**) that loads further back. The button goes when nothing is older (`history.complete`).

#### Cursors

All cursors are values in the cache store, at timestamp 0. When the cache store is cleared, all cursors are cleared together.

| Cursor | On | Meaning | Moves |
|-|-|-|-|
| `watch.at` | workspace | The newest message that the watch saw | Each poll |
| `history.oldest` | workspace | All messages after this ts are in the cache store, in all conversations | Each batch back (`o` on the list) |
| `history.query`, `history.page` | workspace | The search that the last batch used, and its next page | Each batch back |
| `history.oldest` | conversation | All messages of this conversation after this ts are in the cache store | Each `o` in the conversation |
| `history.complete` | workspace, conversation | There is nothing older | When a load gets to the start |

#### The watch

- Each 15 s, one `search.messages` call gets all messages after `watch.at`, newest first.
- For each new message, the watch:
  - writes the message at its ts;
  - links it under its conversation at its ts, or under its thread parent when it is a reply. The link moves the conversation to the top of the list. A reply does not move it;
  - adds a new DM, group DM or private channel to the list. Search also finds public channels that I am not in. Thus, a new public channel comes only with the next load of the list.
- The watch ignores a message that the cache store has. Thus, two looks at the same message do not add it two times.
- Search indexes a message a short time after it is sent. Thus, each poll looks again at the 120 s before `watch.at`.
- **Catch-up:** on start, the first poll gets all messages after `watch.at`, up to 100 pages (10000 messages). If there are more, the cached messages are not continuous. Then the workspace `history.oldest` moves to the oldest message of the catch-up, and the cursor of each conversation is cleared.

#### Going further back (on demand)

- **All conversations (`o` on the list):** one more batch of 1000 messages before the workspace `history.oldest`. The batch continues the search of the last batch, from its next page. Thus, it does not get messages again. When the search gets to page 100, the next batch starts a new search from the cursor.
- **One conversation (`o` in it):** `conversations.history` gets the 100 messages before its start. Its start is the older of its own `history.oldest` and the workspace `history.oldest`, because all messages after the workspace cursor are in the cache store.
- **One thread (`o` in it):** `conversations.replies` gets the full thread. Then the thread is complete.
- A message from search has no reactions or replies. A message from history has them. When history gets a message that search got, it writes the full message.

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
