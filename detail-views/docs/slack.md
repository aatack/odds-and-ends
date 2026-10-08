# Slack

`modules/slack/` — `view.ts` (pure), `slack.ts` (loads, watch), `api.ts`
(transport, read allowlist, rate limiter).

## The problem (a possible rethink)

Keeping track of Slack, and not missing anything, is hard:

- What I need to track is spread across DMs, group DMs, channels and threads.
  No one place in Slack's UI shows all of them; threads never show beside
  channels and chats.
- I often action a message and come back to it later.
- I sometimes want to look at something without marking it read.
- Slack can't put only unread things at the top and hide read channels.
- I work across two workspaces.

**Proposal.** Accumulate the channels, DMs, group DMs and threads I haven't
read into one list, as a tree. Treat them all alike: each is a *topic* that
holds *messages*. Topics and messages are items like any other: I can link
them to other things, write notes on them and make them tasks. When I'm ready,
I mark them read.

### What Slack's API allows (checked October 2026)

Public column: checked against the [method reference](https://docs.slack.dev/reference/methods).
Undocumented column: from memory, unverified; none of it is in the reference.

| Need | Public API | Undocumented (the web client's) |
|-|-|-|
| Unread conversations | `conversations.info` gives `last_read` (my read point) for one conversation. Unread = newest message ts > `last_read`. `conversations.list` and `users.conversations` say they include `unread_count` for DMs only (untested). No call lists every conversation's read state: `rtm.start` did, but has behaved like `rtm.connect` (no unreads) since September 2022. | `client.counts`: unread flags and mention counts for every channel, DM, group DM and threads, in one call |
| Unread threads (the Threads / Activity view) | Per thread only: `conversations.replies` documents the parent as carrying `subscribed` (I follow it), `last_read` and `unread_count` (untested). Nothing lists the threads I follow; `conversations.history` and `search.messages` don't carry these fields. | `subscriptions.thread.getView`, `activity.feed` |
| Mark read | `conversations.mark` (`channel`, `ts`): "sets the read cursor in a channel". User scopes `channels:write`, `groups:write`, `im:write`, `mpim:write`; tier 3 (50+/min). No thread equivalent. | `subscriptions.thread.mark` for threads |
| Hear read markers move (read elsewhere) | `channel_marked`, `im_marked`, `group_marked` events, but only over the legacy RTM API, which new apps can't use |  |

- The undocumented methods need a browser session token (`xoxc` and its `d`
  cookie), not an app's token; they can change or break without notice, and a
  workspace may not allow them.
- `conversations.info` per conversation is the ~600 calls ruled out above,
  but it need only run for a conversation the watch has just seen a message
  in, which keeps it to a few calls a minute.
- Threads have no public read state at all, so for threads the app would have
  to keep its own.
- `conversations.mark` is a write: it needs `slackWrites` and my say-so.
- A second workspace needs its own token. From May 2025 Slack limits
  `conversations.history` and `conversations.replies` to 1 call a minute
  (15 messages) for non-Marketplace apps that aren't internal; an internal
  app installed in its own workspace is exempt.

## Constraints

- **Read-only.** `api.ts` refuses any method not on its read list
  (`slackWrites` is off). Adding a read method needs my say-so; it is how
  `search.messages`, `users.list` and `conversations.list` got there.
- **Read state is mine** (below): an owned `lastRead` per conversation, set
  with `m`. Slack's own read state is never read.
- **No per-conversation loading on its own.** Slack has no public call for
  every channel's latest message; one call per conversation (~600) took
  minutes. Everything automatic is a list call or a search.

## Shape

Two levels, not three: threads read flat in their conversations.

- `slack` (`slack.home`) is the workspace. Its children are conversations
  (channels, DMs, group DMs) only; no message is ever linked under it.
- **Every message is linked under its conversation**, at its ts, a reply too.
  A reply is also linked under its thread's parent message, so `d` on a
  parent still shows the thread alone.
- A reply carries `thread` (its parent's id, from `present`); shown under a
  conversation, its row starts with that parent's pill: hover to peek at the
  thread, click to go to it.
- **Hiding a chat is unlinking it** from the workspace: Backspace (or Delete)
  on its row writes an owned unlink (`Slack.unlink`, via
  `Core.actions.unlink`). The unlink is later than the cached link (at 0), so
  the hourly list reload never brings it back. There is no way to relink from
  the UI yet.
- **Hiding a chat from a message**: Shift+Backspace on a message (or **Hide
  chat** in its header) unlinks the message's conversation, worked out from
  the message id, so it works for a channel that was never listed.
- Each poll prunes cached message links from the workspace (`pruneThreads`),
  left from when threads were listed there.
- `slack:watch` (`slack.watch`) holds the last poll's `polledAt` and `found`,
  on its own entity so a poll every 15 s re-reads only that.

## Read state

- **`lastMessageReceived`** (cache, at 0) on a conversation: the newest
  message in it from someone else, a reply included, raised by every load
  that brings messages in (`receivedEvents`). My own messages never raise it.
- **`m`** (`Slack.markRead`) on a conversation, or on a message (its
  conversation), writes an owned `lastRead` equal to `lastMessageReceived`,
  authored `me`, so Ctrl+Z takes it back. Not the time now: a message sent
  before I pressed `m` but not yet found by the watch would then be taken as
  read and never shown.
- A conversation is **unread** when `lastMessageReceived` is later than
  `lastRead`, or it has no `lastRead`. There is no default: everything anyone
  has written in starts unread, and I go through it.
- **The workspace lists only unread conversations**, by `lastMessageReceived`,
  newest first (`order`). Read ones aren't shown there.
- **In the workspace, a conversation shows only messages after its
  `lastRead`** (`order` is given the view's root for this). Its own view
  (`d`) shows all of it.
- Unread conversations are tinted `accent` (`slackTint`).
- A conversation is marked in Slack too (`conversations.mark`) only while
  `slackWrites` is on.

## What loads on its own

1. **The lists** (workspace `children`, fresh for 60 min), in parallel, a few
   calls each: `users.conversations` (mine, linked under the workspace),
   `conversations.list` (every public channel, named but not linked, so
   mentions resolve) and `users.list` (every user, marked loaded). The reload
   replaces only `slack:conv:` links.
2. **The first batch**: on a cache with no `watch.at`, the first poll searches
   back from now for 1000 messages (about 10 calls).
3. **The watch**: `Slack.poll` every 15 s (started by `Core.start`). Each poll
   first makes sure the lists are loaded (`load(slack, children)`, a no-op
   while fresh), so the lists always land before the first batch and before
   any thread is checked against them.
4. Rare single loads: a message seen only by id (`self`, once), a user
   `users.list` didn't have (`users.info`, 7 days).

## The watch (`poll`)

- One `search.messages` query `after:<cursor day − 2>`, sorted newest first,
  reading pages until a match is at or before `watch.at − 120 s` (search
  indexes a few seconds late). Up to 100 pages.
- Each new match becomes message events (`partial`: search has no reactions or
  reply counts, so those keys are not written), linked under its conversation
  or, for a reply (`thread_ts` from the permalink ≠ ts), under its parent, with
  the parent linked under the workspace. Already-cached and repeated matches
  are skipped.
- New DMs, group DMs and private channels join the workspace. Public channels
  don't (search also returns channels I'm not in).
- **Catch-up** is the same poll on start. If it runs out of pages before
  reaching `watch.at`, the cache has a hole: the global `history.oldest` moves
  up to the catch-up's oldest message and every conversation's cursor is
  dropped.

## Cursors (cache store, timestamp 0)

| Value | On | Meaning |
|-|-|-|
| `watch.at` | workspace | Newest message the watch has seen |
| `history.oldest` | workspace | Every conversation is cached from here to now |
| `history.query`, `history.page` | workspace | The last batch's search and its next page |
| `history.oldest` | conversation | This conversation is cached from here to now |
| `history.complete` | workspace, conversation, thread | Nothing older exists |

Presented as `from` (where cached history starts) and `complete`. A
conversation's `from` is the older of its own cursor and the workspace's.

## Going further back (only when asked: `o` or **Older**)

- **Workspace**: the next 1000 messages before `history.oldest`, by search.
  It resumes the last batch's query from its next page, so no page is fetched
  twice (10 calls per batch); after page 100 it starts a new `before:` query
  from the cursor. Search dates are whole days, so a new query starts two days
  past the cursor and skips matches at or after it.
- **Conversation**: `conversations.history` with `latest` = its `from`, 100
  messages. Full messages, overwriting thinner search ones.
- **Thread**: `conversations.replies`, the whole thread; then `complete`.

## Rate limiting

`RateLimiter`: one token bucket for all methods, 90/min with a burst of 10,
honouring `Retry-After`. Callers using `api.urgent` (opened things, searches,
user lookups) queue ahead of the rest.

## Known gaps

- Edits, deletions and new reactions on cached messages are never seen.
- Reading in Slack changes nothing here; marking here changes nothing in
  Slack while it is read-only.
- A read conversation can't be reached from the workspace; only through a
  link to it (a mention, a pill, Recents).
- A conversation's own backlog (`o`) is `conversations.history`: top-level
  messages and their reply counts, not the replies themselves (`o` on a
  thread loads those).
- Join/leave messages aren't in search.
- Messages made only of blocks or attachments show empty text.
- Messages from public channels I'm not in are cached (not shown) and counted
  in the poll's `found`.
- Socket Mode would make the watch instant and see edits, but needs an
  app-level token and Slack app settings; the catch-up would stay as is.
