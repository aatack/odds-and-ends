# Looper

Keeps an agent working on one long-running task in the background. You go to a
git repo — or any folder inside one — run `looper`, and it wakes a Claude agent
over and over: the agent reads its task from a note in that folder, does a piece
of work, commits it, writes down where things stand, and stops. Then Looper
waits a bit and wakes it again.

The folder doubles as an [Obsidian](https://obsidian.md/) vault: the task and the
agent's notes are markdown files beside the work, linked with `[[wikilinks]]`, so
you can open it in Obsidian and read along. The notes say where things stand —
findings, todos, open questions — rather than logging what each wake did; git is
the log.

It messages you on Telegram when it has something worth saying or something it
genuinely can't get past, and whatever you reply is in the prompt at its next
wake. Every wake is shown the whole conversation — everything you have sent, and
what it said to you — with your new messages marked, so a reply never loses the
question it answers. Most of the time it says nothing.

Everything the agent is told is written in
[ASD-STE100 Simplified Technical English](https://www.asd-ste100.org/), and it is
told to write its notes, commits and messages to you the same way.

Wakes run on Opus 5.5 in auto mode (see [Model and permissions](#model-and-permissions)).
It is TypeScript with no runtime dependencies, and Node runs it directly.

## Setup

### 1. Install

You need [Node.js](https://nodejs.org/) v22.18+ (it runs TypeScript as-is) and
the [`claude` CLI](https://claude.com/claude-code). Logging in can wait: the first
run sorts out the account.

Then, once, in this directory:

```bash
npm link
```

That puts a `looper` command on your PATH — on Windows too, in PowerShell, cmd and
Git Bash — which links back to this checkout, so a `git pull` takes effect with
nothing to reinstall; `npm unlink -g looper` takes it away. Link it rather than
installing a copy: Node will not run TypeScript from inside `node_modules`, and a
link is followed back to the real files. Or skip this and call it by path:
`node /path/to/looper/src/index.ts`.

### 2. A bot to talk to

Message [@BotFather](https://t.me/BotFather), send `/newbot`, follow the prompts,
and keep the token it gives you.

### 3. A note that says what to do

Write the task in `TASK.md` in the folder Looper will run in, with whatever detail
you have — or let the first run ask you for a line and write it for you. The agent
reads it at every wake and leaves it as you wrote it; its own notes go in
`notes/`, starting from `notes/Index.md`. `LOOPER_TASK` and `LOOPER_NOTES_DIR`
move either.

### 4. Run it

```bash
cd ~/repos/the-idea      # a git repo; `git init` if it's new
looper
```

It also runs in any folder inside a repo — `~/repos/odds-and-ends/the-idea`, say.
That folder is then the task's own: its task note, its notes and `.looper/` all
live there, the agent may read the rest of the repo but changes nothing outside
its folder, and it commits only what is inside it, so other work in the same repo
is left alone.

The first run asks for what it needs and saves it:

- **The bot token and the chat**, once per computer, in `~/.config/looper/env` —
  one bot serves every task. Leave the chat blank and it will ask you to message
  the bot, then take the chat id from the message.
- **The task**, if there is no `TASK.md` yet.
- **Which Claude account to use**, once per folder — see below.

Anything else for one folder goes in its `.looper/env`; `looper --help` lists
every setting with its default.

On Windows, do the first run of a folder in PowerShell or Windows Terminal rather
than Git Bash's own window: there Node does not see a terminal, so the questions
about the task and the account are skipped.

### Which Claude account it uses

The first run in a folder shows which account `claude` is logged into on this
computer and asks whether the wakes should use it. Say no and it sets up another
one for you — a personal subscription for background work, say, kept apart from
the one you use for everything else. It asks for:

- the account's email address, to fill in on the login page (or leave it blank
  and choose in the browser);
- a short name for it, which becomes its directory,
  `~/.config/looper/claude/<name>`;
- whether its usage is paid by a Claude subscription or by Anthropic Console API
  credit.

Then it runs `claude auth login` in that directory — a browser window opens — and
writes `LOOPER_CLAUDE_CONFIG_DIR=<that directory>` into the folder's
`.looper/env`. An account set up once can be picked again by name for another
folder, without logging in again. Saying yes writes `LOOPER_CLAUDE_ACCOUNT=default`
instead, so the question is not asked again.

`CLAUDE_CONFIG_DIR` moves the whole of Claude Code's configuration — credentials,
settings and saved sessions — so one directory is one account. Looper checks the
account before the loop starts and logs who it is running as; if that account has
been logged out, it offers to log it in again there and then (or, without a
terminal, says how to), rather than failing every wake.

### Model and permissions

Wakes run on `claude-opus-5-5` (Opus 5.5), pinned by its full name so a new release
doesn't change the model under a task part way through; `LOOPER_MODEL` picks
another, and `LOOPER_EFFORT` and `LOOPER_FALLBACK_MODEL` are there too.

They run with `--permission-mode auto`: a classifier approves or refuses each
action, so an agent nobody is watching can work without prompts, but without the
blanket approval of `--dangerously-skip-permissions`, which Looper never uses.
`LOOPER_PERMISSION_MODE` changes it.

A wake is also run with `--strict-mcp-config`, so it gets Looper's own notify tool
and nothing else: whatever MCP servers the Claude account has configured are
deliberately not there. Its notes are files, so it needs no server for them.

### Trying it out

```bash
looper --once      # one wake, then stop
looper --dry-run   # print the account, the notes, the prompt and the command;
                   # run nothing
looper --help      # every setting, with its default
```

### Coming from the notes server

Looper used to keep the task and notes on a notes MCP server. That is gone: any
`NOTES_MCP_URL` and `NOTES_MCP_TOKEN` lines are now ignored, and a `LOOPER_TASK`
that holds a note id will be read as a file path — delete it from `.looper/env`, or
point it at the task's markdown file.

## How it works

- **`src/loop.ts`** — the loop. Wake the agent, see how the wake ended, decide
  how long to leave it, go again. All the interesting behaviour is in that
  decision, and it is the file to read first.
- **`src/prompt.ts`** — what the agent is told: the standing brief, plus what
  happened last time, the whole Telegram conversation, and where its folder
  stands in git. It is kept short, and never mentions wakes or numbers them:
  told it was on wake 314, the agent wrote about wake 314 everywhere.
- **`src/claude.ts`** — one wake: `claude --print` in the folder with the notify
  tool wired in, its event stream read as it goes.
- **`src/notify.ts`** — the tool the agent reaches you with. A small MCP server
  over stdio, exposing `tell_user` and `ask_user`.
- **`src/telegram.ts`** — the Bot API over `fetch`, long polling for your replies.
- **`src/state.ts`** — everything remembered between wakes, in `.looper/`.
- **`src/config.ts`** — the two env files, and asking for what's missing.
- **`src/account.ts`** — which Claude account the wakes use, and logging in a
  different one when asked.
- **`bin/looper.js`** — the `looper` command: a two-line Node launcher, so the
  command npm makes from it works on Windows as well.

### The timings

Every wake ends, and what happens next depends on how it ended. All of these can
be set per folder (see `looper --help`):

| How the wake ended | What happens | Default |
| --- | --- | --- |
| Normally | Short gap, then the next wake | 5m |
| Normally, but it used no tools | The long gap — a wake that did nothing is not worth repeating every five minutes | 30m |
| It asked you something | Waits for your answer, then carries on anyway if it doesn't come | 6h |
| The API was overloaded | A short gap, doubling — a 529 is capacity, not a fault, so it doesn't count as a failure | 2m, capped at the failure gap |
| It failed | Backs off, doubling with each failure in a row | 30m, capped at a day |
| A usage cap | Sleeps until the cap resets, or a fixed gap if it isn't told when | 3h |

A cap is a 429, or any refusal that says so in words; the reset it names — an
epoch, or a clock time in a named zone like `resets 9:50am (America/Los_Angeles)`
— is what it waits for, and nothing shortens that wait, since nothing you can say
will lift a cap.

A message from you cuts the waiting short — but not the instant you send it. It
waits until you have been quiet for 90 seconds, so three messages in a row arrive
as one thought. It also only happens once per message: one handed to a wake that
died before reading it goes back in the queue, but waits its turn with the rest,
so a wake that fails in two seconds can't be woken straight back into the same
failure by the same message.

Three failed wakes in a row and it tells you; six overloads in a row, by which
point the API has been refusing work for over an hour, and it tells you that too.
A missing `claude` or a rejected login it doesn't retry at all: it says so and
stops.

A wake the API never took is treated as a wake that never happened: anything you
had sent is marked new again for the next one rather than being lost with it, and
the session is kept. `LOOPER_FALLBACK_MODEL` gives Claude a second model to try
before it gets that far.

### What it keeps

Everything is in `.looper/` in the folder Looper runs in, which holds a
`.gitignore` that ignores itself — so the agent committing its work can never
commit Looper's:

```
.looper/
  env            settings for this folder
  state.json     wake count, messages not yet shown, how the last wake ended
  looper.log     one line per event, and every tool call the agent made
  inbox.jsonl    everything you've sent — the conversation each wake is shown
  sent.jsonl     everything the agent has sent you — the other half of it
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

### Tidying the notes

Every wake is told to keep the notes to where things stand, but notes edited in a
hurry from inside one session drift towards a log regardless. So every 20 wakes
(`LOOPER_TIDY_EVERY`; 0 turns it off) one wake is spent in a new session, doing no
work on the task, only reading the notes cold and putting them back in order:
stale findings and finished todos deleted, duplicates merged, links fixed, the
index kept short. A tidy-up never takes the place of a wake you have just
messaged; it waits for the next quiet one. The wakes after it resume its session,
so they start from one that has just read everything afresh.

## Deliberate omissions

- It does not push, publish, or change anything outside its folder — the agent is
  told not to, and the notify tool refuses to attach a file from outside it.
- It only reads text you send. Voice notes and photos are consumed and dropped.
- There is one task per folder. Two tasks means two folders — which can be two
  subdirectories of one repo.

## Type-checking and tests

```bash
npm install     # only needed for these two
npm run typecheck   # src/ and test/
npm test
```

The test stands up a fake Bot API and a fake `claude` on `PATH`, then runs a whole
wake through the real loop. The fake `claude` is a shell script, so the tests run
on macOS and Linux (or WSL) but not on Windows itself; the typecheck runs
anywhere.
