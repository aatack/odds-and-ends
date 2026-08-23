# Integrations

The server can reach four things outside itself: **GitHub**, **Slack**, **Claude
Code**, and the **git repositories on this machine** — the last two locally rather
than over a network. They live in `src/integrations/`, they are listed in one
registry, and there is exactly one way to invoke one:

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
| `github.pullRequestForBranch` | the open PR raised from a branch, or `null` |
| `github.createPullRequest`  | open one from the branch a checkout is on |

A pull request is named by its **URL** or by **`owner/repo#123`**. A bare number
is refused: there is no working directory here for `gh` to resolve it against.

### The two that are told *where*

`pullRequestForBranch` and `createPullRequest` take a **checkout** instead of a
repo, and they are the exception that proves the rule above. A worktree knows its
own remote and its own branch, so `gh` run inside one can work out the
`owner/repo` that every other tool here has to be handed — and the caller that
has just pushed a branch is holding a path, not a URL.

Between them they answer "has this been raised, and if not, raise it" without a
pull request reference existing yet. Nothing found by `pullRequestForBranch` is
an ordinary answer rather than an error. `createPullRequest` needs the branch
pushed first, passes the body over **standard input** (`--body-file -`) so a
description as long as an exported set of notes never reaches an argument
vector, and reads the number back out of the URL `gh` prints, that being all it
prints.

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
   - `users:read` — putting a name to a user id: `slack.getUser`, and the naming
     of the other side of a DM in `slack.listChannels`. The listing degrades
     without it — the lookup is skipped and a DM shows its raw id (`D0123ABCD`)
     instead of `@someone` — but `slack.getUser` has nothing to fall back to and
     will say so
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
| `slack.getUser` | put a name to a user id — the one thing that reads `U0123ABCD` |

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

### Putting a name to an id

Every message these tools hand back names its author as a bare id — `user:
"U0123ABCD"` — because that is all Slack puts on a message. `userName` is
filled in only for the messages carrying one of their own, which in practice
means apps posting under a chosen name. So a feed read straight out of
`slack.recentMessages` is a wall of ids, and `slack.getUser` is what turns one
into somebody:

```
POST /runTool  { "tool": "slack.getUser", "args": { "user": "U0123ABCD" } }
→ { "id": "U0123ABCD", "name": "Alex", "handle": "@alex",
    "realName": "Alex Atack", "isBot": false, "deleted": false }
```

`name` is the answer to the question, in Slack's own order of preference: the
display name somebody chose, then their real name, then the handle. Which of
those an account has filled in varies, so all three come back and a caller
wanting a different rule can apply it.

Three forms of the argument, all the same lookup: the plain id, an `@`-prefixed
one, and `<@U0123ABCD>`, which is how a mention appears inside a message's
*text* and therefore the form most likely to be copied out of one. A **handle**
or a name is refused rather than attempted — no Web API method takes either,
so there is nothing to fall back to.

A **bot id** (`B…`) is answered too, from `bots.info`, in the same shape. The
message tools fall back to `bot_id` where a message has no `user`, so an id that
came from one of them can be either, and `users.info` has never heard of the
second kind.

Lookups are remembered for the lifetime of the server: a workspace's people
rarely change, and naming the authors of a hundred messages should not be a
hundred calls.

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

Headless Claude Code sessions **on this machine**, through the
[`claude` CLI](https://claude.com/claude-code), which must be on the server's
`PATH`.

### Authentication

None to configure: whatever `claude` is already signed in as on this machine is
what runs. Nothing goes in `.env`.

### Tools

| id | what it does |
|----|--------------|
| `claude.runPrompt` | run a session in a directory and wait for its JSON |

```
POST /runTool  { "tool": "claude.runPrompt", "args": {
  "path": "~/repos/local-helpers",
  "prompt": "Summarise what changed this week",
  "sessionId": "weekly-summary"
}}
```

It runs `claude --print --output-format json` in `path`, with the prompt on
standard input — so a prompt can be as long as you like, and never reaches an
argument vector or `ps`. The result is the CLI's JSON verbatim: `result` is what
Claude said, `session_id` the conversation it said it in, plus the turn count,
duration, cost and token usage.

`systemPrompt` is the one argument that *does* go in the vector, because
`--append-system-prompt` takes it no other way. It is read only on the turn that
starts a conversation — a resumed session already has the system prompt it was
started with — so send it with the first prompt or not at all. Keep it to rules
and ids; anything long belongs in the prompt.

**`~` is expanded**, since these paths are named by hand. A relative path
resolves against the *server's* working directory, which is rarely what anyone
means, so an error names the absolute path it went looking for.

#### The session id names a conversation

Pass one you have used before and the session carries on where it left off; pass
an unused one and a new session starts. That is the whole of the contract, and it
is deliberately the caller's to keep — the caller is usually a `type: code`
entity, which has somewhere to put a session id and knows which conversation it
means.

Two things it does not have, hence two accommodations:

- **It need not be a UUID.** The CLI takes nothing else, so anything that isn't
  one is hashed into one. The same string always yields the same UUID, so a name
  — `"weekly-summary"`, an entity id — works as a session id. The `session_id`
  that comes back is the derived UUID, not the name.
- **A session id is scoped to its directory.** Sessions are stored per project,
  so the same id in two paths is two conversations. The CLI has no "resume it, or
  start it if it isn't there", so both are tried in turn: resume first, because
  being wrong about it is free — it looks for the transcript before it reaches
  the API, and says so in a line and an exit code.

#### It runs with permissions bypassed, and it blocks

`--permission-mode bypassPermissions`: there is nobody here to answer a prompt,
and a session that can only read is not worth starting. **This is arbitrary code
execution on this machine, on purpose** — that is what `safety: 'dangerous'`
means here, more literally than for the other integrations.

The call does not return until the session does, and **there is no time limit**.
There was a half-hour ceiling here, on the theory that a session past it was
wedged; no length actually tells a wedged session apart from one grinding through
a large change, so all it ever killed was the second kind. What interrupts a run
is the app's Stop, which is a decision rather than a guess. Two consequences
worth knowing:

- The app's main process does not use `fetch` for `/runTool`, because it cannot:
  undici abandons a response whose headers haven't arrived within five minutes
  and the fetch API has no way to say otherwise. It goes over `node:http`.
- A call in flight shows as **Running** in the app's activity log, and settles
  in place when it answers. From a `type: code` entity the call is synchronous
  like any other, and the script simply waits.

A session that fails part-way still prints its JSON and can still exit cleanly;
`is_error` in that JSON is treated as a failed call, with the CLI's own words.

---

## Git

The repositories on this machine, through the `git` CLI, which must be on the
server's `PATH`. Nothing to configure, and no secrets: git runs as whoever the
server does, with whatever remotes and credentials that user already has.

**Every tool takes the directory to run in**, because that is what names the
repository — there is no "current" one here, and two worktrees of the same
repository are two different places to be standing. `~` is expanded, as
everywhere else.

### Tools

| id | what it does |
|----|--------------|
| `git.createWorktree` | a fresh worktree under `~/.pensive-worktrees`, path returned |
| `git.removeWorktree` | delete one, and its branch if that is safe |
| `git.pull` | fast-forward a checkout to its upstream |
| `git.commitAll` | stage everything in a checkout and commit it |
| `git.push` | push to `origin` with tracking, optionally onto a new branch |
| `git.checkout` | switch to an existing branch |

### Worktrees

`git.createWorktree` is the only tool here that decides *where*: it makes the
worktree under **`~/.pensive-worktrees`** with a six-character name from
`a-zA-Z0-9`, gives the branch that same name, and returns the full path along
with the id and branch.

```
POST /runTool  { "tool": "git.createWorktree", "args": {
  "path": "~/repos/pensive", "from": "origin/master" }}
→ { "path": "/home/you/.pensive-worktrees/aB3xY9", "id": "aB3xY9",
    "branch": "aB3xY9", "from": "origin/master" }
```

A fresh worktree is a fresh branch sharing nothing with the checkout it came from
but the repository. That is the point: something about to change a repository
gets a checkout of its own, works there, pushes a branch, and hands the path
back.

**`from` is what it branches off**, and the checkout's default remote is fetched
first — a remote-tracking ref is only worth branching from if it is current, and
that is the one moment it needs to be. Leave it out and the branch starts from
whatever the given checkout has at `HEAD`, which is what a checkout sitting on a
feature branch will quietly hand you.

The branch is created with `-b` rather than by letting git name it after the
directory. With a start-point that is the only way to land on a branch instead of
a detached `HEAD`; without one the result is what it always was.

The id is drawn from `randomBytes`, rejecting the bytes above the last whole
multiple of 62 rather than folding them back in with `%` — which would make the
first handful of letters likelier than the rest. Names already taken on disk are
redrawn. Git creates the parent directory itself, so nothing has to exist first.

**Removal is the destructive one**, so it is the one that can be refused:

- A worktree with changes or untracked files in it is kept, unless `force`.
- The **main checkout of a repository is always refused** — git knows the
  difference, and says so.
- The branch outlives the worktree, so a directory of one-shot worktrees would
  leave one behind every time. It is deleted with `git branch -d`, which refuses
  a branch that isn't fully merged: work that hasn't gone anywhere is never what
  gets tidied away, and `branchDeleted` in the result says which happened. A
  refusal there is not a failure of the removal, which has already happened.

Removal runs from the repository's **main** worktree, found through
`git rev-parse --git-common-dir`, rather than from inside the directory being
deleted.

### Pulling, committing, pushing, switching

- **`git.pull` is `--ff-only`.** There is nobody here to resolve a merge, and a
  merge commit is not something a tool should invent; a branch that has diverged
  says so and leaves the checkout alone.
- **`git.commitAll` finding nothing is the good outcome.** It is the sweep that
  runs behind an agent which was supposed to commit its own work, so an empty
  checkout is an ordinary answer rather than an error; `committed` says which
  happened. It stages with `add --all` — untracked files included — and reads
  `status --porcelain` afterwards, so the answer doesn't depend on git's version
  or the server's locale.
- **`git.push` sets upstream**, always: `git push --set-upstream origin HEAD`.
  `HEAD` rather than the branch's name so it reads the same whatever the branch is
  called, and tracking so that a later pull on it means something. Naming a
  `branch` makes one with `checkout -b` and switches to it first, so what is
  pushed and what is checked out don't part company.
- **`git.checkout` only ever switches to an existing branch.** Making one belongs
  to `git.push`, where there is somewhere to put it.

**A branch can only be checked out in one worktree at a time.** So asking a
worktree for `master` is refused while the main checkout has it — git names the
worktree holding it. Switch the checkout that owns the branch instead. There is no
way to have it in both and nothing here pretends there is.

Two variables are set on every call, and they matter more than they look:
`GIT_TERMINAL_PROMPT=0` and `GIT_EDITOR=true`. Without them, a push to a remote
wanting a password — or anything that would open an editor — waits for an answer
that is never coming, and is killed on the timeout, which reports the wait rather
than the reason for it. Use SSH or a credential helper. Pull and push get five
minutes; the local operations get the usual one.

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
