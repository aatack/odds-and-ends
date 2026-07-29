# Entity Graph mobile — agent & contributor guide

A progressive web app for reading and writing one entity-graph source from a phone.
Vite + React + TypeScript, its own `package.json` and its own `node_modules`. See
[`README.md`](./README.md) for running it and for the list of deliberate departures
from the desktop client.

## It is a separate app, sharing one model

This app has its own `package.json`, its own `node_modules`, its own build. What it
shares with the desktop client is **`../src/core`, and only that**: the entity, the
rollup, the traversal, the entity cache, the atom the cache is built on, and the
markdown an outline is exported as. Nothing from
`../src/main`, `../src/preload` or `../src/renderer` is imported, and nothing shared
pulls in Electron, node or zod — the line is drawn at what a browser can run.

That line moved. These shapes used to be *copies*, on the reasoning that the only
thing crossing the wire was a query result. It stopped holding when both clients
started keeping their own event cache and running the traversal themselves: at that
point the fold from events to an entity has to agree exactly, or the two apps
disagree about what the store says. A shared type is one thing; a shared fold is
another, and once that is shared the type may as well come with it.

Practically: `vite.config.ts` opens `fs.allow` one level up, and `src/core/types.ts`
is a re-export shim so the app's own imports still read `from '../core/types'`.

The server is shared too, and the one change made to it for this app's sake is CORS
on the source-scoped endpoints (`server/src/app.ts`) — deliberately *not* on the
admin surface, which is open when `ADMIN_TOKEN` is unset.

On the Tailscale route that CORS is no longer load-bearing: one origin serves both the
app and `/api/<sourceId>`, so the calls are same-origin. It still matters for the
plain-LAN fallback, where the app is on vite's port and the server on its own, and it
is what keeps a tunnel workable. Don't remove it on the strength of the tunnel setup.

Those two mounts are switches on the desktop app's Sources page (`src/main/tailscale.ts`
over there), which also builds the `#connect=` link this app reads in `main.tsx`. Nothing
here depends on that — a mount set up in a shell is the same mount — but if the link
format changes, `encodeConnection` / `connectionFromHash` in `src/source/connection.ts`
and the desktop's `tailscale:phoneLink` handler are the two ends of it.

## Don't start the desktop app

The same rule as `../AGENTS.md`: **never** run `npm run dev` in `entity-graph/` — it
seizes the source server's port and fights the Electron window the user has open. This
app's own `npm run dev`, `npm test`, `npm run build` and `npm run typecheck` are all
fine; the tests bring up their own in-memory source on a free port and touch nothing
of the user's.

## Design language

The desktop app's, at phone scale: the same tokens (`src/index.css`), the same two
fonts (Geist for chrome, Lora for entity text), tone and spacing over borders, one
muted indigo accent, gray first.

**Hard rules, carried over:**

- **No motion**, with exactly one exception: a bottom sheet slides up (`.sheet-in`).
  On a touch screen there is no cursor to explain where a sheet came from, and the
  movement is that explanation. Everything else is instantaneous, and a global
  override in `index.css` keeps it that way if a `transition-*` class slips in.
- **Prefer no borders.** Tone, a whisper of shadow, or spacing.
- **No garish colours.**
- **Focus rings only for the keyboard** (`focus-visible:`).

And rules that are this app's own, because a finger isn't a cursor:

- **Nothing smaller than 44px** in its tappable direction. `min-h-11` is the floor.
- **No hover states.** Use `active:` — a phone has no hover, and a sticky `:hover`
  after a tap reads as a stuck button.
- **16px in every text field.** Below that, mobile browsers zoom the page on focus.
  Set once in `index.css`, not per component.
- **Respect the insets.** Pad by `var(--inset-top)` / `var(--inset-bottom)`, never
  assume the screen is rectangular.
- **The keyboard is half the screen.** The bottom bar is `fixed` and stays above the
  keyboard because of `interactive-widget=resizes-content` in `index.html`. Don't
  touch that meta tag without knowing what it does.
- **No secure-context APIs**, still. The usual route now *is* a secure context — served
  over HTTPS by `tailscale serve`, which is what makes the app installable — but the
  plain-LAN fallback in [`README.md`](./README.md) is not, and the app has to run the
  same on both. So `crypto.randomUUID` and `navigator.clipboard` stay off limits: see
  `helpers/uuid.ts` and `helpers/clipboard.ts` for what to use instead. The one place
  that may branch on `window.isSecureContext` is the service-worker registration in
  `main.tsx`, because installability is exactly the thing that differs.

## State, tools and views

Three layers, each depending only on the ones above it — the desktop app's rule, and
the reason `npm test` can drive the app in node.

- **`state/` holds all of it, and holds nothing else.** Atoms, pure reducers, pure
  derivations. No React, no DOM. Latent state only: the navigation stack, the folded
  set, the selection *path*, the edit draft. Anything derivable (the row list, the
  resolved selection, a crumb's label) is a function in `state/derive.ts`; anything
  cached (entities, resources) is a runtime atom that is never persisted.
- **Nothing on screen waits on the network.** Every event the app has read is kept
  per entity in `core/cache`, read synchronously through `useGetEntities()`, and
  asking for something is what fetches it. The rows are then a derivation: `core/tree`
  steps the traversal over the cache, so folding, going in and out of a level, and
  every edit redraw with no round trip — which on a phone is most of the point. The
  only read of the store is `scanEvents`, which fetches a couple of layers ahead, and
  a write goes into the cache on its way out so the line is on screen before it lands.
  `docs/frontend-state.md` in the desktop project is the long form; it applies here
  unchanged, because it is the same code.
- **Never write derived state back.** The resolved selection is computed from the
  latent path against the visible rows; storing it would lose the original when a
  folded ancestor is reopened.
- **`tools/` is the only way the user does anything.** Every command is one `ToolSpec`
  declaring its arguments and whether it writes. The bottom bar, the action sheet and
  the long-press menu all go through `runTool`, so a button and a sheet entry can't
  drift, and a button greys itself out by asking the same `enabled` predicate the
  sheet filters by. New commands go in a tool file, never inline in a handler.
- **Arguments are asked for uniformly.** Anything a tool still needs after the context
  is folded in either opens a form sheet, or — for an argument that is *pointed at* —
  puts the app into picking mode, where the outline stays live and the next row tapped
  supplies it. Navigation during a pick is allowed and is the point.
- **Views render and forward.** `Outline.tsx` takes rows and callbacks and holds no
  state; the decisions are in `useOutline.ts`. Direct manipulation (tapping a row,
  folding, typing) calls a named action in `state/actions.ts`; anything invocable is a
  tool.
- **Errors surface as call results.** A tool throws, `runTool` settles it, a toast
  shows it. Components never raise toasts.
- **Rows are virtualised by the browser**, not by us: `.row-virtual` is
  `content-visibility: auto`. Don't add a windowing library — and note that this is
  what lets the row being edited stay mounted wherever it is. Row props are passed one
  by one rather than spread, so a memoised row doesn't re-render on every state change.
- **Adding a line is the thing this app is for.** Weight the interface accordingly:
  "Child" is the bar's one tinted button, "+ another" rather than "done" is the
  primary while typing, and the action sheet's groups are ordered by `GROUP_ORDER` in
  `tools/registry.ts` rather than by which file a tool happens to live in.
- **Entity text renders as markdown** (`components/ui/Markdown.tsx`, with the
  typography under `.markdown` in `index.css`). The rules are written the other way up
  from a prose stylesheet — no typography by default — so a line with no markup in it
  comes out identical to a plain span. Maths and syntax highlighting are deliberately
  not loaded; don't add them back without a reason bigger than parity.

## Layout

```
src/
  core/types.ts   a re-export of the shared model in ../src/core (see above)
  source/         the connection, and typed wrappers over the source's tools
  state/          latent state, pure derivations, how far a level is unrolled
  tools/          the registry, the dispatcher, the tool sets
  components/ui/  Button, Field, Sheet, TextEditor
  views/          the shell, the outline, the sheets, the gestures
test/             an in-memory source, and two headless runs over it
```
