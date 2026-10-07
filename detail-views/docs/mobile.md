# Mobile: detail-views on my phone

The same app on the phone, as a PWA (an installable web app), reaching the
desktop app over Tailscale. Everything below is what exists today.

## Using it

### Once

1. Build the phone app (again after any change to the app):
   ```bash
   npm run build:phone
   ```
2. Publish the desktop app's phone server on the tailnet:
   ```bash
   tailscale serve --bg 47821
   ```
   If that says permission denied, first run
   `sudo tailscale set --operator=$USER`. `tailscale serve status` shows what
   is published; `tailscale serve reset` undoes it.
3. Start (or restart) the desktop app. It writes the phone's link, with the
   sign-in token in it, to `~/.config/detail-views/phone-link.txt`
   (`$DETAIL_VIEWS_DIR/phone-link.txt` if set). It looks like
   `https://xps-laptop.<tailnet>.ts.net/#token=…`.
4. On the phone, with Tailscale connected, open that link once. The app keeps
   the token and drops it from the address bar.
5. Browser menu → *Add to home screen* (Android: *Install app*). From then on
   it opens full screen from its icon.

### Day to day

- The desktop app must be running: the phone has no data of its own.
- **Tap** a row to select it; **tap it again** to open its view.
- The bar along the bottom stands in for keys: **Back** (Shift+A), **Open**
  (d), **Note** (Enter), **Edit** (e), and **More**: every other action that
  can be done right now (remove, move, link, tick, fold, find, undo, older,
  hide chat, approve…). These come from the same tool registry as the keys,
  so the phone can always do what the desktop can.
- The tabs along the top are the modules (the desktop's sidebar).
- Links open in the phone's browser (there is no hover, so no peeks).
- Edits, notes, PR actions: everything does what it does on the desktop, to
  the same data, and shows up on both at once.

### If something is wrong

| Symptom | Likely cause |
|-|-|
| "Open the link in phone-link.txt…" | The phone has no token yet: open the link once. |
| Page doesn't load | Desktop app not running, Tailscale off on either side, or `tailscale serve` not set up. |
| "The phone app is not built" | Run `npm run build:phone`. |
| Everything says "not signed in" | The token changed (see Security). Open the new link. |
| Old version of the app | Close and reopen it; the service worker fetches the new one first. |

## The rule

The content and the actions are the same on both; only how views are
organised differs (tabs for the sidebar, a bottom bar, no floating windows).
Every action is a tool (`src/renderer/src/tools.ts`); a tool with a `label`
is an action everywhere, and the phone's buttons are `actionsNow`: the
labelled tools enabled right now. A new key gets a label, and so a phone
button, unless it is only cursor movement or typing.

## How it works

```
phone (PWA in its browser)                       desktop (Electron main)
  same Session / EntityCache / views              Core (owned + cache stores)
  httpApi ── HTTPS, tailnet only ──► tailscale serve ──► 127.0.0.1:47821 (src/main/phone.ts)
```

1. **A server in the desktop app** (`src/main/phone.ts`). The core lives in
   the Electron main process, which already serves `Core.actions` to the
   window over IPC. It serves the same actions over HTTP too:
   - `POST /api/<action>`: JSON arguments in, JSON answer out.
   - `GET /api/changes`: Server-Sent Events, one message per change
     notification (the ids that changed), as the window gets them over IPC.
   - `GET /api/slack-image/<ref>`: Slack images through the core's blob
     cache (the desktop uses a `slack-image://` protocol instead).
   - Anything else: the built PWA from `out/phone`, so app and API share one
     origin and nothing needs CORS.
2. **Tailscale** publishes `127.0.0.1:47821` as `https://<machine>.<tailnet>.ts.net`:
   reachable from my devices only, with a real certificate (a PWA needs HTTPS
   to install). Nothing listens on the LAN.
3. **The phone app is the renderer again.** `src/renderer/src/phone.tsx` is a
   second entry beside the desktop's `main.tsx`; both mount the same root
   (`root.tsx`). The only difference underneath is the `Api`:

   | | Desktop | Phone |
   |-|-|-|
   | `Api` | `electronApi`: IPC | `httpApi`: `fetch` + `EventSource` |
   | `Session`, `state.ts`, tools | shared | shared |
   | `EntityCache`, `viewOf`, module views | shared | shared |
   | views | shared | shared, plus the bottom bar and phone CSS |
   | images | `slack-image://` | `/api/slack-image/…` (`images.ts`) |
   | hover | peeks | none; links open in the browser |

   So loading (what is fetched from Slack and GitHub, when), caching (the
   frontend entity cache over the owned and cache stores) and presentation
   are one code path. The phone keeps its own copy of the frontend cache; the
   stores are the desktop's.
4. **PWA bits**: `src/renderer/phone.html`, `src/renderer/phone-public/`
   (`manifest.webmanifest`, `icon.svg`, `sw.js`), built by
   `vite.phone.config.ts`. The service worker caches the app shell (network
   first), never data.

## Security

- The server binds `127.0.0.1` only; Tailscale is the only way in from
  elsewhere, and only from my devices.
- Even so, every `/api` call needs the token (`Authorization: Bearer`, or
  `?token=` for the event stream and images, which can't send headers):
  anything on the tailnet could otherwise drive the core, which includes
  approving and closing PRs.
- The token is in the owned database's settings as `phone.token`, made on
  first start. To sign every device out, delete it (the next start makes a
  new one and a new link).
- The PWA's files carry no data, so they are served without the token.

## Files

| File | What |
|-|-|
| `src/main/phone.ts` | The HTTP server, the token |
| `src/main/index.ts` | Starts it; writes `phone-link.txt` |
| `src/renderer/src/api.ts` | `httpApi` beside `electronApi` |
| `src/renderer/src/phone.tsx` | The phone's entry: token, images, service worker |
| `src/renderer/src/root.tsx` | The app's root, for both entries |
| `src/renderer/src/views/App.tsx` | `PhoneBar`, its More list |
| `src/renderer/src/tools.ts` | Every action, with its key and its label (`actionsNow`, `runTool`) |
| `src/renderer/src/styles.css` | `.app.phone` rules, at the end |
| `src/renderer/phone.html`, `phone-public/` | The PWA's page, manifest, icon, service worker |
| `vite.phone.config.ts` | Its build (`npm run build:phone`) |
| `src/main/phone.test.ts` | A headless test: the phone's Session over HTTP to a Core |

## Not done yet

- **Offline.** The phone needs the desktop app running; nothing is cached on
  the phone between sessions except the app itself.
- **Notifications.** No push; the app updates while it is open.
- **Per-device sign-out.** One token for all devices.
- **Turning it on from the app.** `tailscale serve` is a command I run; it
  could be a switch in the app (entity-graph has the code for that).
- **Touch gestures.** No swipe back, no long-press menus; the bar does it.
- **Peeks.** Could become a long-press or a sheet.
- **The phone server always runs** with the desktop app. It could be off by
  default and turned on in settings.
