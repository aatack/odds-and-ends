# Looper

Keeps an agent working on one long-running task in the background. You go to a
git repo, run `looper`, and it wakes a Claude agent over and over: the agent reads
its task from a note on your notes server, does a piece of work, commits it,
writes down where it got to, and stops. Then Looper waits a bit and wakes it
again.

It messages you on Telegram when it has something worth saying or something it
genuinely can't get past, and whatever you reply is in the prompt at its next
wake. Most of the time it says nothing.

Nothing to install: it is TypeScript with no runtime dependencies, and Node runs
it directly.

## Setup

You need [Node.js](https://nodejs.org/) v22.18+ (it runs TypeScript as-is), the
[`claude` CLI](https://claude.com/claude-code) installed and logged in, and a
notes server to keep the task in.

### 1. A bot to talk to

Message [@BotFather](https://t.me/BotFather), send `/newbot`, follow the prompts,
and keep the token it gives you.

### 2. A note that says what to do

Write the task as a note, with whatever detail you have, and keep its id. The
agent reads that note and everything under it at every wake, and writes its
findings, decisions and next steps back underneath. Every wake is told to read
first, act, then write — the notes are where its memory of record lives, whatever
the session carries over.

### 3. Run it

```bash
cd ~/repos/the-idea      # a git repo; `git init` if it's new
looper
```

The first run asks for the bot token, the chat, the notes server and the task
note, and saves them. Leave the chat blank and it will ask you to message the bot,
then take the chat id from the message.

- Secrets go in `~/.config/looper/env` — one bot and one notes server serve every
  task.
- The task and any settings go in `<repo>/.looper/env`, beside the work.

To run it from anywhere, either `npm link` in this directory (which gives you a
`looper` command) or call it by path: `node /path/to/looper/src/index.ts`.

### Which Claude account it uses

By default, whichever account `claude` is logged into. To give a repo its own
account — a personal subscription for background work, kept apart from the one you
use for everything else — point it at its own config directory:

```bash
mkdir -p ~/.config/claude-looper
CLAUDE_CONFIG_DIR=~/.config/claude-looper claude auth login   # once, interactively
echo 'LOOPER_CLAUDE_CONFIG_DIR=~/.config/claude-looper' >> .looper/env
```

`CLAUDE_CONFIG_DIR` moves the whole of Claude Code's configuration — credentials,
settings and saved sessions — so one directory is one account, and the setting
lives in `.looper/env`, which makes it a property of the repo. Looper checks the
account before the loop starts and logs who it is running as; if that account
isn't logged in it says so, with the command to fix it, rather than failing every
wake.

### Trying it out

```bash
looper --once      # one wake, then stop
looper --dry-run   # print the account, the prompt and the command; run nothing
looper --help      # every setting, with its default
```

## How it works

- **`src/loop.ts`** — the loop. Wake the agent, see how the wake ended, decide
  how long to leave it, go again. All the interesting behaviour is in that
  decision, and it is the file to read first.
- **`src/prompt.ts`** — what the agent is told: the standing brief, plus what
  happened last wake, what you have said since, and where the repo stands.
- **`src/claude.ts`** — one wake: `claude --print` in the repo with the notes
  server and the notify tool wired in, its event stream read as it goes.
- **`src/notify.ts`** — the tool the agent reaches you with. A small MCP server
  over stdio, exposing `tell_user` and `ask_user`.
- **`src/telegram.ts`** — the Bot API over `fetch`, long polling for your replies.
- **`src/state.ts`** — everything remembered between wakes, in `<repo>/.looper`.
- **`src/config.ts`** — the two env files, and asking for what's missing.

### The timings

Every wake ends, and what happens next depends on how it ended. All of these can
be set per repo (see `looper --help`):

| How the wake ended | What happens | Default |
| --- | --- | --- |
| Normally | Short gap, then the next wake | 5m |
| Normally, but it used no tools | The long gap — a wake that did nothing is not worth repeating every five minutes | 30m |
| It asked you something | Waits for your answer, then carries on anyway if it doesn't come | 6h |
| The API was overloaded | A short gap, doubling — a 529 is capacity, not a fault, so it doesn't count as a failure | 2m, capped at the failure gap |
| It failed | Backs off, doubling with each failure in a row | 30m, capped at a day |
| A usage cap | Sleeps until the cap resets, or a fixed gap if it isn't told when | 3h |

A message from you cuts the waiting short — but not the instant you send it. It
waits until you have been quiet for 90 seconds, so three messages in a row arrive
as one thought.

Three failed wakes in a row and it tells you; six overloads in a row, by which
point the API has been refusing work for over an hour, and it tells you that too.
A missing `claude` or a rejected login it doesn't retry at all: it says so and
stops.

A wake the API never took is treated as a wake that never happened: anything you
had sent goes back in the queue for the next one rather than being lost with it,
and the session is kept. `LOOPER_FALLBACK_MODEL` gives Claude a second model to
try before it gets that far.

### What it keeps

Everything is in `<repo>/.looper`, which holds a `.gitignore` that ignores itself
— so the agent committing its work can never commit Looper's:

```
.looper/
  env            settings for this repo
  state.json     wake count, what you've said, how the last wake ended
  looper.log     one line per event, and every tool call the agent made
  inbox.jsonl    everything you've sent
  sent.jsonl     everything the agent has sent you
  runs/          the full event stream of every wake, one file each
```

Each wake is a real Claude session, so `claude --resume <id>` opens up what
happened; the id is in `state.json` and in the run's log.

### Sessions

By default each wake **resumes** the last session, and auto-compaction handles the
growth. The notes are still the memory of record — the prompt says so, and each
wake is told to read them first — but continuity between wakes is worth having on
top. A session that can't be resumed (deleted, or left half-written by a kill) is
dropped after one failed attempt, so the next wake starts a new one rather than
retrying the same dead id forever.

Set `LOOPER_SESSION_MODE=fresh` to start every wake from nothing but the notes.

## Deliberate omissions

- It does not push, publish, or touch anything outside the repo — the agent is
  told not to, and the notify tool refuses to attach a file from outside it.
- It only reads text you send. Voice notes and photos are consumed and dropped.
- There is one task per directory. Two tasks means two directories.

## Type-checking and tests

```bash
npm install     # only needed for these two
npm run typecheck
npm test
```

The test stands up a fake Bot API and a fake `claude` on `PATH`, then runs a whole
wake through the real loop.
