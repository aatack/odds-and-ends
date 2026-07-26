# Integrations

The server can reach three things outside itself: **GitHub**, **Slack**, and
**Claude Code in the cloud**. They live in `src/integrations/`, they are listed
in one registry, and there is exactly one way to invoke one:

```
GET  /tools     → the list, each with a JSON Schema for its arguments
POST /runTool   → { "tool": "<id>", "args": { … } }
                  → { "status": "success", "result": … }
                  → { "status": "error", "message": "…" }
```

Both answer to the **admin token** (`Authorization: Bearer $ADMIN_TOKEN`), not to
a source token: integrations are the server's own hands, not part of any source.
They are not exposed over MCP.

`stripNulls` applies as it does everywhere else — passing `null` for an argument
means "use the default", and `null` for a required argument is an error.

The Electron app fetches `/tools` from whichever server hosts the open source and
folds them into the command palette, so every tool here is runnable from ⌘P — and
from a `type: code` entity, which calls one by the camel case of its name:
`tool.sendSlackMessage(channel, text)`.

---

## Secrets

Everything is configured through **`server/.env`**, which is gitignored. Copy
[`.env.example`](../.env.example) to `.env` and fill in what you need. Variables
already present in the environment win, so the app's `PORT`/`ADMIN_TOKEN`/
`CONFIG_DB` are never clobbered by a stray line.

Restart the server after editing it — the file is read once, at startup.

---

## GitHub

Everything goes through the [`gh` CLI](https://cli.github.com), which must be on
the server's `PATH`. Arguments are passed as a vector, never a command line, so
no shell is involved.

### Authentication

Either is fine:

```sh
gh auth login            # writes ~/.config/gh/hosts.yml; nothing to put in .env
```

or a token in `.env`:

```
GH_TOKEN=ghp_…
```

The token needs the `repo` scope (and `read:org` for private organisations).
Approving, merging and closing all need write access to the repository.

### Tools

| id | what it does |
|----|--------------|
| `github.listMyPullRequests` | your open PRs across every repo you can see |
| `github.getPullRequest`     | one PR in full — body, reviews, mergeability, diff size |
| `github.approvePullRequest` | submit an approving review, optionally with a comment |
| `github.markPullRequestReady` | take it out of draft |
| `github.mergePullRequest`   | squash / merge / rebase, optionally `auto` (merge when green) |
| `github.closePullRequest`   | close without merging, optionally with a comment |
| `github.listRepoPullRequests` | a repo's open PRs with yours filtered out, paged by `offset` |

A pull request is named by its **URL** or by **`owner/repo#123`**. A bare number
is refused: there is no working directory here for `gh` to resolve it against.

`github.listRepoPullRequests` pages by asking GitHub for `offset + limit + 1`
results and taking the window, since `gh` has no offset of its own. `hasMore`
says whether the extra one came back.

---

## Slack

Direct calls to the Web API (`https://slack.com/api/…`), form-encoded, which
every method accepts.

### Authentication

```
SLACK_TOKEN=xoxp-…
```

`SLACK_USER_TOKEN` and `SLACK_BOT_TOKEN` are also read, in that order of
preference, if `SLACK_TOKEN` is unset.

A **user token** (`xoxp-`) is what you want: it sees everything you see,
including DMs and private channels, and posts as you. To get one:

1. <https://api.slack.com/apps> → **Create New App** → *From scratch*, pick your
   workspace.
2. **OAuth & Permissions** → **User Token Scopes**, add:
   - `channels:history`, `groups:history`, `im:history`, `mpim:history` — reading
     messages in public channels, private channels, DMs and group DMs
     respectively
   - `channels:read`, `groups:read`, `im:read`, `mpim:read` — listing
     conversations, and resolving `#name` and permalinks
   - `search:read` — the recent-messages feed, which is one search
   - `chat:write` — sending
   - `users:read` — *optional*: naming the other side of a DM. Without it the
     lookup is skipped and a DM lists as its raw id (`D0123ABCD`) instead of
     `@someone`; nothing else changes
3. **Install to Workspace**, then copy the *User OAuth Token* (`xoxp-…`).

`auth.test`, which is how the feed learns your handle to exclude it, needs no
scope of its own.

A bot token (`xoxb-`) works for most of this, but only for conversations the bot
has been invited to, and it posts as the bot. **`slack.recentMessages` will not
work on one at all**: Slack's search is user-token only, under any scope.

### Tools

| id | what it does |
|----|--------------|
| `slack.recentMessages` | the last N messages from anywhere you can see, newest first |
| `slack.listChannels` | every conversation you're in — DMs, groups, channels — paged |
| `slack.getChannelMessages` | one conversation's messages, paged by `offset` |
| `slack.readMessage` | the text of one message, optionally with its thread |
| `slack.sendMessage` | post to any conversation, or reply in a thread |

**There is one notion of "where":** a conversation id. DMs (`D…`), group DMs
(`G…`), public and private channels (`C…`) are all conversations and are all read
and written the same way. A user id (`U…`) works as a destination too — Slack
resolves it to the DM.

Threads fold into the same thing rather than needing their own tools. A
reference to a *message* — what Slack's **⋮ → Copy link** gives you — carries the
thread it sits in, so:

- `slack.readMessage` with a link to a reply returns that reply (and, with
  `includeThread`, the whole thread around it).
- `slack.sendMessage` pointed at a **message** link replies *in that message's
  thread*; pointed at a **conversation** (a bare id, or a channel link with no
  message in it) it posts to the conversation. Passing `threadTs` explicitly
  overrides either.

Accepted references, everywhere a conversation or message is asked for:

```
https://acme.slack.com/archives/C0123ABCD/p1712345678000100?thread_ts=…
C0123ABCD:1712345678.000100
C0123ABCD
#general
U0123ABCD
```

### The recent-messages feed

`slack.recentMessages` is as close to a notifications feed as the Web API gets.
Slack has no "what's new for me" endpoint — unread is per-conversation state — so
this is one `search.messages` call standing in for one:

```
query:    "after:<N days ago> -from:@you"
sort:     "timestamp"      ← not "score", which is the default
sort_dir: "desc"
count:    <limit>
```

The query carries **no search text at all**. Slack requires a non-empty `query`,
but it doesn't have to contain a term — a query of modifiers alone filters the
lot, and `sort: timestamp` is then what turns "everything" into "the most recent
of everything". Left on the default `score` sort there is nothing to be relevant
to, and the top N would be arbitrary.

Two things the query is doing deliberately. `-from:@you` makes it a feed of what
*arrived* rather than an activity log. And `after:` earns its place twice over:
it keeps the search shallow, and it is a **positive** term, without which the
query would be nothing but a negation — which search engines commonly reject.

Caveats: `after:` is day-granular, hence a day count rather than a timestamp
(default 2, so today is covered whether or not the bound counts the day it
names). Compare `ts` against your own cursor to get finer resolution. Search runs
off an index, so expect it to trail real-time by seconds.

#### What it leaves out

Search is broader than your sidebar: it sees **every public channel in the
workspace**, joined or not, and knows nothing about muting. Left alone, that is a
much noisier feed than the thing it stands in for. So two filters run by default,
each with an escape hatch:

| left out | why | keep it with |
|---|---|---|
| public channels you aren't a member of | there is no `is:member` search modifier, so this is an intersection with `users.conversations` | `includeUnjoined: true` |
| muted conversations | see below | `includeMuted: true` |

Both are applied **after** the search, since neither can be said in the query. So
the tool over-fetches — four times `limit`, capped at Slack's 100 — and takes the
window from what survives. The result reports `scanned` alongside `count`, which
is the ratio: if they're close together on a busy day, raise `limit`.

The two sets are cached for five minutes, because this is a tool you poll and
re-deriving them per call would be the most expensive part of it.

**Mute is the unsupported half.** No documented Web API method reports it — mute
is a user preference, and `users.prefs.get` (`prefs.muted_channels`), which the
Slack clients themselves use, is not part of the public API. So the call is
best-effort: if it's refused, nothing counts as muted and the rest of the feed is
unaffected. `SLACK_MUTED` in `server/.env` is the way to say it by hand, and
works whether or not the endpoint does:

```
SLACK_MUTED=C0123ABCD,C0456EFGH
```

Anything it names is merged with whatever `users.prefs.get` managed to return.

Membership is *not* best-effort by contrast: a failure there means a missing
`*:read` scope, and quietly handing back an unfiltered feed would read as the
filter not working.

### Paging

`listChannels` and `getChannelMessages` take `offset` + `limit`, like the GitHub
listings, though Slack itself pages by cursor: the walk happens server-side, and
`hasMore` comes from asking for one past the end of the window. `listChannels`
resolves a DM's counterpart to a display name — a list of `D0123ABCD` is no use
to pick from — but only for the window it returns, so a big workspace costs a
handful of lookups rather than one per conversation.

---

## Claude

Cloud Claude Code sessions, through the **routines** (remote-trigger) API at
`https://api.anthropic.com/v1/code/…`.

> ⚠ **This one is a stopgap, and it is unverified.** The routines API is
> undocumented and was implemented from how the Claude Code CLI calls it. The
> request shapes here are a best guess; expect to adjust
> `src/integrations/claude.ts` the first time you run it. `github.*` and
> `slack.*` are on documented, stable interfaces and are not in the same boat.

### Authentication

```
CLAUDE_CODE_OAUTH_TOKEN=sk-ant-oat…
CLAUDE_ENVIRONMENT_ID=env_…
CLAUDE_DEFAULT_REPO=aatack/odds-and-ends
CLAUDE_DEFAULT_MODEL=claude-sonnet-5      # optional
CLAUDE_CODE_API_BASE_URL=…                # optional; defaults to api.anthropic.com
```

Get the token with:

```sh
claude setup-token
```

which mints a long-lived OAuth token tied to your claude.ai account. The
environment id is the cloud environment a session runs in — list yours at
<https://claude.ai/code/routines> (creating a routine there shows them), or ask
Claude Code to `/schedule` and read the ids it offers.

### Tools

| id | what it does |
|----|--------------|
| `claude.startSession` | start a cloud session on a repo with a prompt |
| `claude.followUpSession` | send another turn to a running session |
| `claude.listSessions` | recent sessions — where a session id comes from |

**Starting a session creates a routine.** The API has no "just run a session"
call, so `claude.startSession` creates a one-off routine, leaves it **disabled**
so it can never fire on its own, and runs it immediately. It returns the routine
id and a link to <https://claude.ai/code/routines/…>. Routines cannot be deleted
from the API, so these accumulate — prune them in the web UI.

**Following up does not go through a routine**, though starting does. Re-running
a routine starts a *fresh* session on a fresh checkout, which is not a follow-up
in any sense that matters — it drops everything the session had learnt. So
`claude.followUpSession` posts the turn to `/v1/code/sessions/{id}/events`
instead, which is the only call that continues a conversation. Change it if the
routines API grows a real follow-up.

Sessions get `Bash, Read, Write, Edit, Glob, Grep`; change `ALLOWED_TOOLS` in
`src/integrations/claude.ts` if that isn't right.

---

## Adding another

1. Write the tool as a `ToolDef` (`src/core/source/types.ts`) — a zod schema for
   its arguments, `safety: 'dangerous'`, and a handler. Reach for
   [`exec.ts`](../src/integrations/exec.ts) for anything with a CLI and
   [`http.ts`](../src/integrations/http.ts) for anything without.
2. Export it from a file in `src/integrations/` and add that list to
   `INTEGRATION_TOOLS` in `src/integrations/index.ts`.
3. That is all: `/tools`, `/runTool`, and the app's command palette pick it up
   from the registry, and the palette builds its argument prompts from the zod
   schema.
