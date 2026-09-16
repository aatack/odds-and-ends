# Tracking what happens elsewhere

Two nodes on the sources graph do something rather than *are* something:
**`slackEvents`** and **`githubEvents`**. Each watches one service and writes
what it finds into whatever is plugged into it, as ordinary notes under
`@inbox`, to be read and filed later or not at all.

They are nodes rather than integrations because everything about them is a
decision the user makes and then looks at: which store the messages land in,
which account they are read as, whether it is running. `src/main/events/` is the
whole of it — `writer.ts`, `feed.ts`, one file per service, and `feeds.ts`
keeping them in step with the drawing.

## The one property everything rests on

**An entity's id is made from the thing's own id.** A Slack message is its
permalink, a pull request is `owner/repo#123`, a comment is
`issuecomment-2412345678` — the fragment GitHub's own URL ends in. So reading the
same message a second time writes to the same entity rather than to a new one,
and `EntityWriter` compares what it is about to write with what is already there
and writes only the difference.

A permalink earns the job twice over: it names the workspace, the channel and the
timestamp in one string, and it is a thing that can be clicked. It is always
*built* rather than taken from the search hit that came with one — one way of
arriving at the string means a message found twice is one note, where two ways
that usually agree would be two notes on the day they didn't. What is stripped is
the `?thread_ts=…&cid=…` a reply's link carries: learning that a message is a
reply must not move it to another entity.

Reading a stretch twice therefore costs nothing, and every catch-up rule is an
application of that:

- **The cursor is wound back a minute before every request**, not only the first.
  Slack's search runs off an index and an index lags; a comment written on the
  cursor's own second would otherwise fall in the gap.
- **The cursor moves after the entities are written, never before.** A crash
  between the two reads the same minute again on the next start, which is free.
  The other order loses it, silently.
- **A cursor more than a week behind is walked a day at a time**, so a first run
  over a year asks for a day per request rather than for a year at once.
- **A new node's cursor is now**, written when the node is added. Switching a
  feed on is a decision about what happens next, not a request to import what has
  already been said; winding it back is something somebody can choose.

Two other rules follow from the writer rather than from the cursor:

- **A value nobody found out is left alone.** A draft's `undefined` means "I did
  not learn this", which is not "this is empty" — the sweep for pull requests
  nobody notified us about knows no `github/reason`, and must not blank the one a
  notification put there.
- **Only a brand new entity goes in the inbox.** A note read and filed somewhere
  by hand is not dragged back the next time the thing it names is mentioned.

## What gets written

Each carries its own `type`, so a row reads as the thing it is. Nothing writes
the type *notes* — a schema for `slack/message` under `@types` is yours to write
if you want one, and everything here works without it.

| type | id | values | hangs under |
| --- | --- | --- | --- |
| `slack/message` | its permalink | `text`, `slack/ts`, `slack/user`, `slack/permalink` | its thread, or its channel |
| `slack/channel` | the channel id | `text` (the name), `slack/channel`, `slack/kind` | — |
| `github/pullRequest` | `owner/repo#123` | `text` (the title), `github/url`, `github/state`, `github/author`, `github/repo`, `github/reason`, `github/checks` | — |
| `github/comment` | `issuecomment-…`, `discussion_r…`, `pullrequestreview-…` | `text`, `github/author`, `github/url`, `github/reviewState` | its pull request |

Everything new is also linked under **`@inbox`** — which the first write names
`Inbox` and hangs under `@index`, since an id nobody has written to is an entity
in the sense that links to it work and in no other: no text, nowhere in the
outline, and so a feed writing into it faithfully looks exactly like a feed doing
nothing. Once only, and only while it has nothing written on it at all, so
renaming it or filing it somewhere else is the last word.

Neither the channel nor the thread is written on a message. The channel is in the
permalink and the message hangs under the channel's own note; the thread is the
note it hangs off. A value saying either again is a second copy to keep in step
with the first.

**A message is never written blank.** A thread parent older than the cursor is
not in the batch that turns up its replies, so it is *fetched* — once per thread
per run — rather than stubbed: a note with children and nothing written on it is
the one thing nobody can act on. Where even that fails, `text` is left unwritten
rather than written empty, so a later reading fills it in instead of confirming a
blank.

A **channel** is made when the first message in it arrives, never by listing
conversations: a channel nothing has been said in is not news.

Nothing is written about **reactions, edits or deletions**. A search hands back
the message as it now stands and says nothing about what happened to it; the only
way to hear about those is Socket Mode, and that is a second token and, in most
workspaces, an administrator's approval — see [Socket Mode](#socket-mode).

A **notification** makes no entity either. It says that something happened on a
thread and why you were told, not what happened — so it decides where to look and
nothing else.

The pull request entity is **independent of changesets**. The same pull request
may be the subject of a changeset elsewhere in the store; these two notes are not
the same note and neither knows about the other.

## What each node polls

### Slack

One `search.messages` call covers everywhere — channels, DMs, group DMs and
thread replies alike:

```
query:    "after:<the day before the cursor>"   (plus "before:…" while catching up)
sort:     "timestamp"
sort_dir: "desc"
count:    100
page:     1, 2, 3, …
```

There is no search text at all. Slack requires a non-empty query but not a
*term*, so a query of bounds alone filters the lot, and `sort: timestamp` turns
"everything" into "the most recent of everything". The pages are read until one
comes back older than the cursor — to there rather than to a count, since how
many messages a day holds is not something to guess at.

`conversations.history` and `conversations.replies` are the fallback, not the
route: each is one channel's top-level messages, so a feed built on them would be
a call per conversation and would still miss every thread reply.

A search hit is **not a message**: it carries no `thread_ts`. Whether it is a
reply survives only in its `permalink`, which ends `?thread_ts=…` when it is one,
so that is what is parsed. Search is Tier 2 — twenty requests a minute — and
needs a user token; a bot token cannot search under any scope.

A message's permalink is *built* rather than asked for — Slack's own form is the
workspace URL, the channel, and the timestamp with its dot taken out — which is
what makes it usable as an id even for a message no search hit came with.

#### Socket Mode

Not implemented, and the node has one field because of it. Socket Mode would make
the same entities appear within seconds rather than within the minute, and would
be the only way to hear about a reaction, an edit or a deletion. It costs a
second token (`xapp-…` with `connections:write`), a second set of event
subscriptions under *on behalf of users*, and in most workspaces an
administrator's approval of the app — for promptness, and for three kinds of
event that are not what an inbox is for. It was built once and taken out again;
`git log` has it if it is wanted back.

### GitHub

```
GET /notifications?since=<cursor>&all=true
```

One call covers every repository. `all=true` keeps the threads already read. The
last `Last-Modified` goes back as `If-Modified-Since`, and a `304` costs no rate
limit at all — which is why this is raw HTTP rather than `gh api`, which treats a
304 as a failure. `X-Poll-Interval` is obeyed; it is about a minute.

Then, per repository the notifications named:

```
GET /repos/{owner}/{repo}/issues/comments?since=   the comments on issues and pull requests
GET /repos/{owner}/{repo}/pulls/comments?since=    the comments on a line of the diff
GET /repos/{owner}/{repo}/pulls/{n}/reviews        the reviews, filtered by time here
```

and, for each notified thread, the thread itself — a notification carries a title
and nothing else worth keeping. A pull request's checks are rolled into one word
from its head commit's check runs; "still going" outranks a failure that has
already happened.

Comments belonging to a thread nobody was notified about are dropped. A
repository's comments since a timestamp are broader than its notifications, and
the inbox is for what arrived rather than for everything that happened.

GitHub has no Socket Mode and no equivalent: its push side is a webhook, which
needs an address on the internet. So this node only polls.

**A notification only comes for a thread you are subscribed to**, so your own
open pull requests in repositories you do not watch never arrive. They are swept
up separately through `github.listMyPullRequests` every quarter of an hour, which
is the only part of this that goes through the app's own integrations.

## Credentials

They live on the node, in the app's graph database, because that file is the
app's own and not a pensive: it already holds the bearer tokens a broadcast
issues, and **a secret copied into a store is a secret published with it**.
Nothing here is ever written into a note.

Each node's help icon says where each field comes from — the Slack scopes, the
app-level token, `gh auth refresh --scopes notifications` — so the questions that
are actually answered in a browser tab are answered beside the field that needs
them.

| | |
| --- | --- |
| `slackEvents` | `userToken` (`xoxp-…`), `cursor` |
| `githubEvents` | `token` (empty for `gh auth token`), `cursor`, `lastModified` |

Nothing is filtered. Your own messages, channels you are not a member of and
conversations you have muted are all written: what is worth reading later is a
different question from what is worth a red dot now.

## Pausing, and the cursor

**Pausing a feed stops it.** This is where the feeds differ from the rest of the
graph: a paused broadcast keeps its server and answers 403, because "somebody
switched this off" is worth telling apart from "there is nothing here". A feed
has nobody to tell, the point of it is that it is doing something, and the socket
it holds open is exactly what should not survive being switched off. Pressing
play starts it again, beginning with the catch-up, so nothing said in the
meantime is lost.

The cursor is written straight onto the node rather than through the IPC handler
that ordinarily edits one. That handler rebuilds every pensive downstream and
re-syncs the servers — for a value that moves once a minute it would be the most
expensive thing the app does, and it would restart the very feed that wrote it.
For the same reason `feedSignature` leaves the cursor out: a cursor that moved is
the feed working, not a reason to throw away the connection it moved with.

A feed's own line about itself — how far it has read, what refused it — reaches
the page over `feeds:changed`, which is its own channel because nothing about the
graph has changed. `pensive:changed` means "you are looking at a different store
now", which a cursor moving is not.

## Being able to see what it is doing

**A feed polling happily and finding nothing looks exactly like a feed that is
not polling at all.** Both draw a quiet node. So every feed keeps a ring of the
last forty things it did — what it asked for, how much came back, what it wrote,
what refused it — with the raw answer under each line, and the node has a
terminal icon that shows them.

The ring lives on `EventFeeds` rather than on the feed itself, so it survives a
restart: a token edited *because* the old one was refused restarts the feed, and
the line saying it was refused is the one still worth reading. `feeds:log` is its
own IPC call rather than part of `graph:read`, since it is only ever wanted while
somebody has the panel open and it is the one answer on that page that is a page
long.

If nothing is arriving, that panel answers the questions in order: did it ask
(a `Searched …` line), did Slack hand anything back (the raw matches under it),
did any of it survive the cursor, and did the write land (`Wrote N notes`). A
feed that has never said anything has never run — which means nothing is plugged
into it, or it is switched off.
