# entity-graph mobile

A phone client for one pensive: read the tree, write to it, navigate it. A separate app
from the Electron one — its own install, its own dependencies, nothing of the desktop
renderer in its build — sharing only the broadcast it talks to.

It is a **progressive web app**, not React Native, because the point was to be using
it on a phone the same day: no SDK, no signing, no store, no build step between an
edit and the phone reloading. The cost is the things a browser can't do (no share
target, no camera, no notifications), none of which the basics need.

## What it talks to

A **broadcast node** on the desktop app's Sources page: add one, drag a pensive into it,
and the app runs a small HTTP server over that pensive on a port it keeps. The node's
panel shows the address to copy and issues the bearer tokens — one per person, and a
write that arrives with a token is recorded as whoever it was issued to, whatever this
app asks for. [`../docs/sources.md`](../docs/sources.md) is the whole of it.

There is nothing else on that server: no admin surface, no reach outside the store. The
token is the entire access decision, and the two things to know about it are that
revoking one is immediate, and that the app's own window issues itself nothing — a
broadcast with no tokens answers nobody.

There is no standalone server any more, so nothing to start by hand and no config DB to
pick between: the pensive the phone reads is whichever one the drawing says.

## Getting it on the phone: Tailscale

The route worth taking. One HTTPS origin serves both this app and the broadcast, so the
phone reaches the laptop **from any network**, the token stops crossing the wire in
cleartext, and — because a real certificate means a secure context — the app becomes
genuinely *installable* rather than a home-screen bookmark. Same-origin also means CORS
stops mattering.

**Once, on the laptop:**

```sh
curl -fsSL https://tailscale.com/install.sh | sudo sh
sudo tailscale up
sudo tailscale set --operator=$USER    # see below — the desktop app needs this
```

Then in the [admin console](https://login.tailscale.com/admin/dns) enable **MagicDNS**
(the `*.ts.net` name) and **HTTPS Certificates**. Without the latter there is no
certificate, so no secure context, so no install. `tailscale status --json` should then
list the name under `CertDomains`.

The `--operator` line is what lets anything other than root change the serve config.
*Reading* it needs no privilege, which is the confusing part: `tailscale serve status`
answers happily while every command that would change something fails with `Access
denied: serve config denied`. The desktop app runs as you, so without it every switch
below is a permission error.

**Once, on the phone:** install Tailscale from the store, sign in with the same account,
turn it on. `tailscale ping <phone>` from the laptop confirms the two can see each
other.

### From the desktop app

The **Sources** page has the two mounts as switches, which is the shortest route and the
one that keeps the connect link below within reach.

- **Phone access**, at the top of the page, serves this app's build at `/`. It shows the
  `.ts.net` name, the directory being served, and warns when nothing has been built into
  it yet — the served files are whatever `mobile/dist` last held, so run `npm run build`
  in `mobile/` at least once. Nothing needs restarting after a rebuild.
- **Phone access** again, inside a broadcast node's panel, serves that node at
  `/api/<nodeId>`, and — once it is on — will build a **connect link** for it: a QR
  code and a URL that carry the whole connection, token included. That is the last
  manual step gone; see [Without typing the token](#without-typing-the-token).

The switches read Tailscale's own config rather than remembering anything, so a mount
made by hand shows as on, and one made from the app can be taken away with the CLI.
Two consequences worth knowing:

- A mount pointing somewhere else is a **conflict**, not an off switch. The app won't
  quietly take over a path something else holds.
- Turning a mount **off** is the destructive direction, because of the missing removal
  below: it clears the serve config and rebuilds it without that one mount. If the
  config holds something the app can't put back — Funnel, a service, a foreground
  `tailscale serve`, a second host, a raw TCP forwarder — it refuses and says which,
  rather than resetting away something it can't restore. Adding is always just one
  command and never resets anything.

### By hand

The same two mounts. Note this serves `dist/` — a static build read off disk by
`tailscaled` — not the vite dev server, so nothing of yours has to keep running:

```sh
npm run build        # in mobile/ — the served files are whatever dist/ last held

tailscale serve --bg /abs/path/to/entity-graph/mobile/dist
tailscale serve --bg --set-path=/api/<nodeId> http://127.0.0.1:<port>
```

`--set-path` **strips the prefix before proxying**, which is the whole trick: a request
for `/api/<nodeId>/tools` arrives at the broadcast as `/tools`. That is what lets the
app's base URL be `https://<host>.<tailnet>.ts.net/api` while the broadcast sees the
paths it expects — and a broadcast ignores any path in front of its own routes anyway,
so the id in the middle is harmless either way.

The mount carries the node's id so that a second broadcast is a second `--set-path`
rather than a collision.

`tailscale serve status` shows the result:

```
https://<host>.<tailnet>.ts.net (tailnet only)
|-- /          path  /abs/path/to/entity-graph/mobile/dist
|-- /api/flow  proxy http://127.0.0.1:36901
```

`--bg` stores the config in `tailscaled`, so it survives reboots — set up once, not per
session, and nothing needs starting after one.

To *change* it, note there is no per-path removal: `off` is not a target `tailscale
serve` accepts (1.98 prints its help and does nothing, which reads like success). Clear
everything and re-add the handlers you want:

```sh
tailscale serve reset
tailscale serve --bg /abs/path/to/entity-graph/mobile/dist
tailscale serve --bg --set-path=/api/<nodeId> http://127.0.0.1:<port>
```

(Without `sudo tailscale set --operator=$USER`, every line above needs `sudo`.)

### Filling it in on the phone

Then open `https://<host>.<tailnet>.ts.net` on the phone and fill in:

| | |
|---|---|
| **Server** | `https://<host>.<tailnet>.ts.net/api` — no node id, no port; the app appends the id itself |
| **Source** | the broadcast node's id, which its mount already names |
| **Token** | a token issued on that node — see below |
| **Author** | recorded against everything written from the phone; `mobile` by default |

A connect link from the desktop app fills all four, so this table is what you need when
setting one up by hand.

The token is issued on the node itself: **Sources → the broadcast node → the key
button**, then a name and **Create**. The name is not a label — every write that arrives
with that token is recorded as that author — and it can be paused or revoked from the
same list, which is how the phone is cut off without touching anything else.

The details are kept in the phone's `localStorage` and nowhere else — which is
per-origin, so a connection saved against an old `http://<lan-ip>:5180` does **not**
carry over to the `.ts.net` name. Expect to set it up once more after switching.

### Installing it properly

Over HTTPS the service worker registers (`src/main.tsx` only does so in a secure
context) and Chrome will offer a real install rather than a shortcut.

**Reload once before looking for it.** On the first visit to an origin the worker
registers *after* `load`, so Chrome has not yet re-evaluated the install criteria and
the menu offers nothing. This is the usual reason "it should be installable" and "there
is no install option" are both true at the same time.

Then, in Chrome on Android: **⋮ → Add to Home screen**. The sheet that appears is the
tell — if its primary button says **Install**, you get a real PWA: its own window, no
address bar, its own entry in the app switcher. If it only offers *Add to Home screen*
/ *Create shortcut*, a criterion failed and you'd get a bookmark that opens in a tab.
Some builds put **Install app** straight in the ⋮ menu instead; either is the real
thing.

To check the service worker registered, visit `chrome://serviceworker-internals` on the
phone and look for the origin — it should read `ACTIVATED`. After installing, the
absence of an address bar is the simplest confirmation it launched as an app.

To debug the criteria rather than guess at them, ask Chrome on the laptop over CDP:
`Page.getAppManifest` reports manifest parse errors and `Page.getInstallabilityErrors`
lists what is blocking an install. Both empty means the phone should offer it.

One quirk worth knowing: `tailscale serve` serves `.webmanifest` as `text/plain`,
because Go's MIME table has no entry for the extension. Chrome parses it anyway
(`getAppManifest` reports no errors), so it is left alone rather than renamed.

An installed app shares Chrome's storage for the same origin, so a connection set up in
the browser carries into the installed app — no need to open the `#connect` link twice.

### What is on the LAN

A broadcast binds every interface, so with or without Tailscale it is reachable from the
network — the token is what makes that safe rather than the bind address, and a node
with no tokens answers nobody. `tailscaled` proxies to `127.0.0.1` regardless, so the
Tailscale route neither needs nor avoids the LAN one.

## Without typing the token

Typing a 48-character token with a thumb is miserable, so a connection can be handed
over in the URL fragment instead — for either route.

The desktop app builds one: **Sources → a broadcast node → the key button → Phone
access → Make connect link**, once that node is served. It shows the link as a QR code to point the phone's camera
at and as text to send yourself, and issues (or reuses) a token labelled `phone` to put
in it. The **Author** beside the button is what every write from that phone is recorded
under, `phone` by default.

By hand, from the repo root:

```sh
node -e '
const c = {
  baseUrl: "https://<host>.<tailnet>.ts.net/api",
  sourceId: "<nodeId>",
  token: "…",
  author: "phone",
}
const hash = Buffer.from(JSON.stringify(c)).toString("base64").replace(/=+$/, "")
console.log(`https://<host>.<tailnet>.ts.net/#connect=${hash}`)
'
```

Open that on the phone — send it to yourself, or turn it into a QR code — and the app
connects on load. The fragment is stripped from the address bar immediately, and being
a fragment it never reaches a server or a log on the way in.

Because `localStorage` is per-origin, this is also how you move a connection to a new
origin: the link has to be opened at the origin it names.

## Fallback: plain LAN, no Tailscale

Same wifi only, no HTTPS, and therefore no install and no service worker — the app runs
the same, it just can't leave the network or leave the browser. Worth knowing for when
there's no Tailscale.

```sh
# 1. the desktop app, with a broadcast node over the pensive you want
npm run dev

# 2. this app, in another shell
npm install          # first time only
npm run dev          # prints a Network: URL — that is the one to open on the phone
```

A broadcast binds every interface, so it is reachable from the phone without anything
extra; the address to type is the one on the node itself, which already names the
laptop rather than loopback. The port has to be open — on most Linux setups it already
is, but a firewall will swallow the connection silently.

### Away from the laptop's network, without Tailscale

A tunnel (`cloudflared tunnel --url http://localhost:36901`) also gives the broadcast a
reachable URL, and the app holds its own token, so it works. But the tunnel is public:
the token becomes the only thing between the internet and the store. Issue one for the
occasion and revoke it after.

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
- **Reading and querying are the desktop's, exactly.** Both clients keep the same
  event cache and run the same traversal over it (`../src/core`), so folding, drilling
  in and every edit redraw with no round trip, and a new line is on screen before the
  write is answered. This used to be the one place the two apps deliberately parted
  company; now it is the place they are most alike, which matters more here than
  there — on mobile data the network is the slow part of everything.
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
