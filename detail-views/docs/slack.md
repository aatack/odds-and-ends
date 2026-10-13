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

- **Read-only, but for one write.** `api.ts` refuses any method not on its read list
  (`slackWrites` is off). Adding a read method needs my say-so; it is how
  `search.messages`, `users.list` and `conversations.list` got there.
- **The one write: a message I send from a topic's box** (`docs/topics.md`
  19–21). `chat.postMessage` goes through only with `byMe`, which only
  `Slack.send` gives, which only `Core.topicSlackSend` calls. That is not in
  `Core.actions`, so the phone, the MCP server and Claude cannot reach it; only
  the window's own IPC channel (`slackSend`) does, straight after a key press or
  click of mine. Never call `Slack.send` from anything a script, an
  automation or an agent can start.
- **Read state is mine** (below): an owned `lastRead` per item, set with
  `m`. Slack's own read state is never read.
- **No per-conversation loading on its own.** Slack has no public call for
  every channel's latest message; one call per conversation (~600) took
  minutes. Everything automatic is a list call or a search.

## Shape

- `slack` (`slack.home`) is the workspace. Its children are conversations
  (channels, DMs, group DMs) only; no message is ever linked under it.
- A conversation's children are its top-level messages, linked at their ts.
  A thread's first message has its replies under it, linked at their ts.
- **A reply moves its thread to the end of its conversation**
  (`relinkThread`): the first message is linked there again at the reply's
  ts, which replaces the cached link. Not if I have taken the thread out of
  the conversation and put it somewhere else (`threadStays`); an owned unlink
  with nowhere else to go is undone by the next reply, so nothing new is lost.
- **Removing a chat is an ordinary unlink**: Backspace on its row writes an
  owned unlink from the workspace, later than the cached link (at 0), so the
  hourly list reload never brings it back. Nothing special to Slack. There
  is no way to relink from the UI yet.
- Each poll prunes cached message links from the workspace (`pruneThreads`),
  left from when threads were listed there.
- **Every row starts open** (`openByDefault`), so a view reads to its full
  depth: conversations, messages, threads' replies. A thread has no "N
  replies" line; its replies are the rows under it.
- `slack:watch` (`slack.watch`) holds the last poll's `polledAt` and `found`,
  on its own entity so a poll every 15 s re-reads only that.

## Read state

- **`lastMessageReceived`** (cache, at 0): the newest message from someone
  else under an item. Every load that brings messages in raises it
  (`receivedEvents`) on a reply's thread, on the conversation (unless the
  thread has been moved out of it), and on everything those sit under,
  however I organised them (up to 4 levels, never a module's root). My own
  messages never raise it.
- **`activity`** of an item is its `lastMessageReceived`, or for a message
  its own ts if that is later.
- **`readTime`** of an item is its `activity`, or, when nothing has come
  into it (a note of mine), when it was made. Never now: a message sent
  before I pressed `m` but not yet found by the watch would then be taken as
  read and never shown.
- **`m`** (`Slack.markRead`) takes the path to the selected row (or the
  view's root) and writes an owned `lastRead` equal to that row's
  `readTime` on **every item on the path**, the row included, except a
  module's root (so `m` on a conversation in the workspace reads only that
  conversation). Authored `me`, so Ctrl+Z takes it back. Pressing it on a
  reply reads the thread up to that reply and the conversation up to then:
  older messages around it go, newer ones (in the conversation or the
  thread) stay. Pressing it on an old row in a conversation's own view moves
  its marks back.
- **The workspace shows, at every level, only what is new** (`shows`, which
  `viewOf` asks of the root's module for every row): an item shows if its
  `readTime` is later than its own `lastRead` and every `lastRead` above it
  in the view. A conversation nothing has come into isn't shown. There is no
  default `lastRead`: everything starts unread.
- The workspace lists conversations by `activity`, newest first.
- A conversation's own view (`d`), or anything else not rooted at the
  workspace, shows everything.
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

- **Pause / Resume** (a button on the workspace's header, and a tool with no
  key): `watch.paused`, module data on the workspace, so it lasts across
  restarts and undo never touches it. While paused, `poll` does nothing.
  Resuming polls at once, which catches up from `watch.at` as on start.

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
  thread loads those). A thread's newest reply there is taken as someone
  else's (history doesn't say whose).
- Join/leave messages aren't in search.
- Messages made only of blocks or attachments show empty text.
- Messages from public channels I'm not in are cached (not shown) and counted
  in the poll's `found`.
- Socket Mode would make the watch instant and see edits, but needs an
  app-level token and Slack app settings; the catch-up would stay as is.
