# The phone (PWA over Tailscale)

## What is needed

1. **A server in the desktop app.** The core lives in the Electron main
   process. To reach it from elsewhere, main serves `Core.actions` over HTTP,
   next to the IPC it already serves to the window: the same actions, the same
   change notifications.
2. **A way to it from the phone that isn't the internet.** Tailscale: the
   server listens on `127.0.0.1` only, and `tailscale serve` publishes it on
   this machine's `https://<name>.<tailnet>.ts.net`, reachable from my devices
   only, with a real certificate (which a PWA needs to install).
3. **A token anyway.** Anything on the tailnet could otherwise drive the core,
   which includes approving and closing PRs. Every `/api` call carries a
   bearer token, kept in the owned settings (`phone.token`).
4. **The app itself, as a PWA**: a manifest and a service worker so it
   installs to the home screen and opens full-screen, served from the same
   origin as the API, so there is no CORS to arrange.
5. **The phone's UI**: the same views, but touch first: no hover (so no
   peeks), no keyboard (so buttons for what keys do), a narrow screen.

## What is shared (nearly everything)

The renderer is already split so that this is a second *entry*, not a second
app:

| Layer | Desktop | Phone |
|-|-|-|
| `Api` (the seam to the core) | `electronApi`: IPC | `httpApi`: `fetch` + Server-Sent Events |
| `Session`, `state.ts` | shared | shared |
| `EntityCache`, `viewOf`, module views | shared | shared |
| views (`views/*.tsx`) | shared | shared, plus `PhoneBar` |
| entry | `main.tsx` | `phone.tsx` |
| images | `slack-image://` protocol | `/api/slack-image/<ref>` |

So loading (what is fetched, when, from Slack and GitHub), caching (the
frontend entity cache, the owned and cache stores) and presentation are one
code path. The phone's cache is its own copy in its own tab; the stores are
the desktop's.

## The HTTP API (`src/main/phone.ts`)

- `POST /api/<action>`, a JSON body of the action's arguments, a JSON answer.
  `<action>` is any of `Core.actions`. `Authorization: Bearer <token>`.
- `GET /api/changes?token=…`: Server-Sent Events, one `data:` line per change
  notification (the ids that changed, or `null`), as the window gets over IPC.
  The token is in the query because `EventSource` can't set headers.
- `GET /api/slack-image/<ref>?token=…`: a Slack image, through the core's blob
  cache.
- `GET /` and the rest: the built PWA (`out/phone`), no token (no data in it).

## Setting it up

```bash
npm run build:phone                      # the PWA into out/phone
npm run dev                              # the desktop app; serves on 127.0.0.1:47821
tailscale serve --bg 47821               # once: https://<name>.<tailnet>.ts.net → it
```

The desktop app writes the phone's link, with the token in its hash, to
`<userData>/phone-link.txt` on start. Open that on the phone once (the app
keeps the token and drops it from the address), then *Add to home screen*.

`tailscale serve` may need `sudo tailscale set --operator=$USER` first.

## The simple version, and what it leaves out

- Tap a row to select it; tap it again to push its view. A bar at the bottom:
  back, add a note, edit, load older. Sidebar becomes a row of tabs.
- No peeks (no hover), and links open in the browser.
- No offline: the service worker caches the app shell, not data. The phone
  needs the desktop app running.
- No push notifications.
- One token, no per-device revoke: changing `phone.token` signs every device
  out.
