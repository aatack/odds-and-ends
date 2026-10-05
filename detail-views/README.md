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
| `j` `k` / arrows | move |
| `Enter` `l` | open |
| `h` `Backspace` / `Alt+←` | back |
| `Alt+→` | forward |
| `c` `i` `/` | write; `Enter` sends, `Shift+Enter` new line, `Esc` leaves |
| `r` | refresh |
| `m` | mark a Slack conversation read |
| `x` `Space` | tick a task |

## Slack

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
