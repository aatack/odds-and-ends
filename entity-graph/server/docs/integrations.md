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
folds them into the command palette, so every tool here is runnable from ⌘P.

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
   - `chat:write` — sending
   - `channels:read`, `groups:read`, `im:read`, `mpim:read` — resolving `#name`
     and permalinks
3. **Install to Workspace**, then copy the *User OAuth Token* (`xoxp-…`).

A bot token (`xoxb-`) works too, but only for conversations the bot has been
invited to, and it posts as the bot.

### Tools

| id | what it does |
|----|--------------|
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
