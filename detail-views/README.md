# detail-views

A local desktop app for my work, one workflow per module. Pick a module on the
left; the rest of the window is the focused entity and what sits under it.

```bash
npm install
npm run dev
```

## Keys

| | |
|-|-|
| `1`–`9` | module |
| `w` `s` / arrows | up, down |
| `d` | focus the selected item |
| `Shift+A` / `Backspace` / `Alt+←` | back: pop the focus off the trail |
| `Alt+→` | forward |
| `Enter` | write; `Enter` again sends (or closes when empty), `Shift+Enter` new line, `Esc` closes |
| `r` | refresh |
| `o` | load further back: all of Slack, this conversation, or this thread |
| `Backspace` `Delete` | on the Slack list: hide the selected chat (a conversation hides its threads too) |
| `x` `Space` | tick a task |
| `a` | approve a PR |
| `X` | close my PR and delete its branch |

## Slack

**Read-only for now.** `slackWrites` in `src/core/modules/slack/api.ts` is off,
so the app has no message box in Slack and refuses any method not on its read
list. Turn it on to send messages and mark conversations read (`m`).

Paste a user token (`xoxp-…`) into the Slack view; it is checked, then kept in
the local database. The Slack app it belongs to needs these user scopes:

`channels:read groups:read im:read mpim:read channels:history groups:history
im:history mpim:history users:read files:read search:read chat:write channels:write groups:write
im:write mpim:write`

On a new cache the app loads Slack's lists (my conversations, every public
channel, every user: a few calls each) and the newest 1000 messages across
every conversation, by one search. The list is ordered by each conversation's
newest message; one with nothing in that batch sits at the bottom. After that,
one search every 15 seconds brings in whatever is new, and catches up on start
with what came while the app was closed.

The list holds every thread too: a reply puts its thread there, ordered by its
newest reply, even when the message it started from is older than anything
loaded.

Nothing older loads on its own. The header of the Slack list, a conversation
and a thread shows where what is loaded starts, with **Older** to go further
back (also `o`): on the list, 1000 messages further back across everything; in
a conversation, its 100 before that; in a thread, the thread whole.

## GitHub

Uses the `gh` CLI as it is signed in; nothing to configure. The list is my open
pull requests. A PR shows the checks that need attention (passing and skipped
ones are counted), then its description, comments and reviews in order, with
inline review comments and the diff lines they point at. Any link to a PR, in
Slack or elsewhere, peeks at the PR itself.

Inside a PR, `a` approves (someone else's: an approving review, with an
optional comment; mine: marked approved here and auto-merge turned on) and `X`
closes mine and deletes the branch, with an optional comment. Both open a
prompt; Enter confirms, Esc cancels.

## Data

Two files in `~/.config/detail-views/` (or `DETAIL_VIEWS_DIR`):

- `detail-views.owned.sqlite`: what I make, as an event log, and settings.
- `detail-views.cache.sqlite`: what was loaded from Slack and GitHub. Emptied
  every week, and safe to delete any time: it all loads again as it is looked
  at.

The first database, `detail-views.sqlite`, is read once to bring tasks and
settings over, and otherwise left alone.
