# conswap

A place to put down anything I am waiting for, and a key that takes me to the next
thing that needs me. Work spreads over Slack, GitHub, Claude and my own head;
this keeps one thread per piece of work and wakes it up when something happens.

Electron + React + TypeScript, one SQLite file, and a small HTTP backend that the
desktop app starts for itself.

## Running it

```bash
npm install
npm run dev
```

That is one command and one window. The app starts the backend as a child
process, so there is nothing else to run.

`dev` passes `--no-sandbox`, because Electron's setuid sandbox helper is not set
up on this machine. To run with it instead, give the helper its permissions once
per `npm install` and use `npm run dev:sandboxed`:

```bash
sudo chown root:root node_modules/electron/dist/chrome-sandbox
sudo chmod 4755 node_modules/electron/dist/chrome-sandbox
```

## The idea

- A **topic** is a collection of related items, and each item is itself a topic.
  A note, a Slack message, a reply from Claude and a subtopic are all topics; the
  only difference is their type.
- A **link** says one topic sits under another. A `watch` link — the default —
  carries activity upward: something landing in a child wakes every ancestor
  watching it. A `reference` link is inert, for grouping.
- A topic is **open** when it needs me. It is **resolved** when I have signed it
  off. The two are separate: a topic I finished can wake up again when somebody
  replies.
- A **blocker** is what I am waiting for. A topic with a live blocker is put down
  and stays out of the queue; when the blocker clears, the topic opens again.
  Blockers wait on a reply in Slack, a Claude run, CI, a review, a merge, every
  subtopic being signed off, the clock, a shell command, or an agent's answer.

Pressing `.` goes to the next open topic. That is the whole loop.

## Keys

`?` shows all of them, read from the same registry the command palette uses.

| | |
| --- | --- |
| `.` | next open topic |
| `⏎` | write a note; `⏎` again keeps it |
| `s` / `c` / `t` | Slack reply / ask Claude / start a subtopic |
| `b` | put this down until… |
| `r` | sign it off |
| `p` | take a subtopic on as work of its own |
| `j` `k` | up and down the feed, `space` expands |
| `o` | look at a subtopic on its own |
| `l` | associate another topic with this one |
| `i` | show the topic's metadata |
| `d` | light or dark; the palette has "follow the machine" |
| `⌃k` | everything I can do |

## Packages

- **`packages/backend`** — the logic. SQLite, the HTTP API, the blocker
  scheduler, and the Slack, GitHub and Claude integrations. No UI code.
- **`packages/common`** — everything a client needs: wire types, the HTTP client,
  the cache with optimistic writes, the latent app state, the tool registry, the
  key dispatcher, and the views. Nothing in here is desktop-specific, so a
  `mobile` package can use all of it.
- **`packages/desktop`** — Electron. Starts the backend, opens a window, renders
  `common`.

## Configuration

Everything is environment variables; nothing is required to start.

| | |
| --- | --- |
| `SLACK_USER_TOKEN` | a user token (`xoxp-…`). Without it Slack is off. |
| `CONSWAP_HOME` | where the database and worktrees live. Default `~/.conswap`. |
| `CONSWAP_DATABASE` | the sqlite file itself. |
| `CONSWAP_PORT` | the backend port. Default `4319`. |
| `CONSWAP_SERVER` | talk to a backend somewhere else, and do not start one. |
| `CONSWAP_GITHUB` | `off` to stop polling GitHub. |
| `CONSWAP_SLACK_POLL` | seconds between Slack polls. Default `15`. |
| `CONSWAP_SLACK_RATE` | requests a minute allowed to Slack. Default `45`. |
| `CONSWAP_GITHUB_POLL` | seconds between GitHub polls. Default `180`. |
| `CONSWAP_CLAUDE_MODEL` | default `opus`. |
| `CONSWAP_CLAUDE_PERMISSIONS` | `ask` to stop Claude accepting its own edits. |

GitHub goes through the `gh` CLI, so it works as soon as `gh auth status` does.
Claude goes through `claude -p`.
