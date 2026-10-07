# Slack

`modules/slack/` — `view.ts` (pure), `slack.ts` (loads, watch), `api.ts`
(transport, read allowlist, rate limiter).

## Constraints

- **Read-only.** `api.ts` refuses any method not on its read list
  (`slackWrites` is off). Adding a read method needs my say-so; it is how
  `search.messages`, `users.list` and `conversations.list` got there.
- **No unread counts.** Nothing tracks read state. Lists are ordered by
  recency instead.
- **No per-conversation loading on its own.** Slack has no public call for
  every channel's latest message; one call per conversation (~600) took
  minutes. Everything automatic is a list call or a search.

## Shape

- `slack` (`slack.home`) is the workspace. Its children are conversations
  (channels, DMs, group DMs) **and threads** (parent messages), ordered by
  `updatedAt`: a conversation's newest message link, a thread's newest reply.
- A conversation's children are its top-level messages, linked at their ts.
  A thread parent's children are its replies, linked at their ts.
- A reply never touches its conversation; it bumps its thread, which is listed
  in the workspace.
- `slack:watch` (`slack.watch`) holds the last poll's `polledAt` and `found`,
  on its own entity so a poll every 15 s re-reads only that.

## What loads on its own

1. **The lists** (workspace `children`, fresh for 60 min), in parallel, a few
   calls each: `users.conversations` (mine, linked under the workspace),
   `conversations.list` (every public channel, named but not linked, so
   mentions resolve) and `users.list` (every user, marked loaded). The reload
   replaces only `slack:conv:` links.
2. **The first batch**: on a cache with no `watch.at`, the first poll searches
   back from now for 1000 messages (about 10 calls).
3. **The watch**: `Slack.poll` every 15 s (started by `Core.start`).
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
- Reading elsewhere changes nothing here (no read state at all).
- Join/leave messages aren't in search.
- Messages made only of blocks or attachments show empty text.
- Messages from public channels I'm not in are cached (not shown) and counted
  in the poll's `found`.
- Socket Mode would make the watch instant and see edits, but needs an
  app-level token and Slack app settings; the catch-up would stay as is.
