# entity-graph mobile

A phone client for one entity-graph source: read the tree, write to it, navigate it.
A separate app from the Electron one — its own install, its own dependencies, nothing
of the desktop renderer in its build — sharing only the server it talks to.

It is a **progressive web app**, not React Native, because the point was to be using
it on a phone the same day: no SDK, no signing, no store, no build step between an
edit and the phone reloading. The cost is the things a browser can't do (no share
target, no camera, no notifications), none of which the basics need.

## Getting it on the phone

The phone needs to reach the source server, so both need to be on the same network.

```sh
# 1. the source server, if it isn't already running (from entity-graph/)
ADMIN_TOKEN=secret PORT=4000 HOST=0.0.0.0 npm run --prefix server start

# 2. this app
npm install          # first time only
npm run dev          # prints a Network: URL — that is the one to open on the phone
```

`HOST=0.0.0.0` matters: the server defaults to `127.0.0.1`, which the phone cannot
reach. So does the port being open — on most Linux setups it already is, but a
firewall will silently swallow the connection.

Then, on the phone, open the `Network:` URL vite printed and fill in:

| | |
|---|---|
| **Server** | the laptop's address and the server's port, e.g. `http://192.168.1.20:4000` |
| **Source** | the source's id |
| **Token** | a token issued for that source (`POST /admin/sources/:id/tokens`, or the admin console at `/admin`) |
| **Author** | recorded against everything written from the phone; `mobile` by default |

The details are kept in the phone's `localStorage` and nowhere else.

### Without typing the token

Typing a 48-character token with a thumb is miserable, so a connection can be handed
over in the URL fragment instead. From the repo root:

```sh
node -e '
const c = { baseUrl: "http://192.168.1.20:4000", sourceId: "flow", token: "…", author: "phone" }
const hash = Buffer.from(JSON.stringify(c)).toString("base64").replace(/=+$/, "")
console.log(`http://192.168.1.20:5180/#connect=${hash}`)
'
```

Open that on the phone — send it to yourself, or turn it into a QR code — and the app
connects on load. The fragment is stripped from the address bar immediately, and being
a fragment it never reaches a server or a log on the way in.

### Add to home screen

Chrome's menu → *Add to Home screen* gives it an icon and a launcher entry. A full
install (its own window, no browser chrome) additionally needs a secure context, which
plain HTTP over a LAN is not; serve it over HTTPS — `tailscale serve` is the least
work — and the manifest and service worker already here will make it installable.

### Away from the laptop's network

Anything that gives the server a reachable URL works, since the app holds its own
token and the server allows cross-origin calls to the source API:

- **Tailscale** on both devices — then the "laptop's address" is its tailnet address,
  and `tailscale serve` gives HTTPS as a bonus.
- **A tunnel** (`cloudflared tunnel --url http://localhost:4000`) — but the source's
  token is then the only thing between the internet and the store, so issue one for
  the occasion and revoke it after.

## How it differs from the desktop app

Same three layers — state, tools, views — and the same rule that they only depend
downwards. What changed is everything above one view, and the gestures.

- **One screen, one navigation stack.** No tab groups, no tabs, no side-by-side
  frames. A level is what a frame was: an entity, a direction, a selection. Drilling
  in pushes one; the header shows the trail and the system back gesture pops it.
- **Tap to select, tap again to edit.** The first tap has to be free to mean "this
  row" for every other control on screen.
- **Long-press is right-click.** It opens the action sheet, which is the command
  palette: the same registry, filtered to what applies.
- **The bar under the thumb** is where hotkeys went. Open, add below, add child, edit,
  more — and while typing, cancel / done / "+ another", which is the flow that makes a
  list bearable to type. It sits above the keyboard rather than behind it.
- **Adding is the primary act, and the app says so.** "Child" is the tinted button, in
  the middle of the bar where either thumb reaches without shifting grip; "+ another"
  rather than "done" is the primary while typing; and the action sheet leads with
  Create, then Edit, then Structure.
- **Folding is client-side.** The desktop makes the folded set part of the query, so
  folding refetches. Here a level fetches its subtree in pages and folding filters the
  rows, so a tap costs nothing — the right way round when the network is the slow part.
- **Ordering is done in one write.** Adding a line below a row, indenting, outdenting
  and reordering all go out as a single `writeEvents` batch, so one undo takes the
  whole action back rather than half of it.
- **No integrations, no code entities, no admin.** A code entity is shown as code but
  not run; the server's GitHub/Slack/Claude tools aren't fetched; sources are not
  configured from here. The app reads and writes one source and does nothing else.
- **Markdown renders, less maths and highlighting.** Bold, links, lists, quotes and
  tables come out as they do on the desktop, through the same stylesheet written the
  other way up (a line with no markup in it is indistinguishable from plain text).
  KaTeX and Prism are left out — a stylesheet and a set of font files each, for
  something you are unlikely to be reading on a phone. They cost about 48 KB gzipped
  between them, which is most of the difference between a 62 KB bundle and a 110 KB
  one; over a LAN, nothing.

## Tests

```sh
npm test        # two headless runs, no browser
```

- `test/smoke.ts` drives the state and tools layers against an in-memory source over
  real HTTP, with nothing stubbed but `localStorage`. That it runs at all is the
  architecture's claim (no React, no DOM below the views); what it checks is where a
  new line lands, that chained entry keeps its order, that indent and outdent move
  what they say, and that one undo is one action.
- `test/render.tsx` renders the whole view layer to a string and asserts the outline,
  the header, the bar and each sheet say what they should — including that undo
  disappears on a source that can't pop events.

```sh
npm run typecheck
npm run build    # dist/ — a static bundle, servable from anywhere
```
