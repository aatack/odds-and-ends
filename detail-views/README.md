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
| `d` / `Enter` | focus the selected item |
| `Shift+A` / `Backspace` / `Alt+←` | back: pop the focus off the trail |
| `Alt+→` | forward |
| `c` `i` `/` | write; `Enter` sends, `Shift+Enter` new line, `Esc` leaves |
| `r` | refresh |
| `x` `Space` | tick a task |

## Slack

**Read-only for now.** `slackWrites` in `src/core/modules/slack/api.ts` is off,
so the app has no composer in Slack and refuses any method not on its read
list. Turn it on to send messages and mark conversations read (`m`).

Paste a user token (`xoxp-…`) into the Slack view; it is checked, then kept in
the local database. The Slack app it belongs to needs these user scopes:

`channels:read groups:read im:read mpim:read channels:history groups:history
im:history mpim:history users:read chat:write channels:write groups:write
im:write mpim:write`

Unread counts come one conversation at a time (Slack has no public call for all
of them), so the list fills in over the first minute or two and is cached after.

## Data

`~/.config/detail-views/detail-views.sqlite`, or `DETAIL_VIEWS_DB`. Anything
from Slack expires after a day (users after a week) and is swept every ten
minutes; what I type stays.
