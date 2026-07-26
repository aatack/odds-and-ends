# Entity Graph mobile — agent & contributor guide

A progressive web app for reading and writing one entity-graph source from a phone.
Vite + React + TypeScript, its own `package.json` and its own `node_modules`. See
[`README.md`](./README.md) for running it and for the list of deliberate departures
from the desktop client.

## It is a separate app

Nothing here imports from `../src` — not even a type. The handful of wire shapes it
needs are copied into `src/core/types.ts` and labelled as copies. This is on purpose:
this app must build and deploy without the Electron project in its graph, and a
type-only import would still tie the two tsconfigs together. If the server's contract
changes, both clients change, which is the honest situation either way.

The one thing shared is the server, and the one change made to it for this app's sake
is CORS on the source-scoped endpoints (`server/src/app.ts`) — deliberately *not* on
the admin surface, which is open when `ADMIN_TOKEN` is unset.

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
- **No secure-context APIs.** Served over plain HTTP on a LAN, `crypto.randomUUID`
  and `navigator.clipboard` are absent — see `helpers/uuid.ts` and
  `helpers/clipboard.ts` for what to use instead.

## State, tools and views

Three layers, each depending only on the ones above it — the desktop app's rule, and
the reason `npm test` can drive the app in node.

- **`state/` holds all of it, and holds nothing else.** Atoms, pure reducers, pure
  derivations. No React, no DOM. Latent state only: the navigation stack, the folded
  set, the selection *path*, the edit draft. Anything derivable (the row list, the
  resolved selection, a crumb's label) is a function in `state/derive.ts`; anything
  cached (query pages, summaries, resources) is a runtime atom that is never persisted.
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
  what lets the row being edited stay mounted wherever it is.

## Layout

```
src/
  core/types.ts   the wire shapes (a copy — see above)
  source/         the connection, and typed wrappers over the source's tools
  state/          latent state, pure derivations, the query engine, runtime caches
  tools/          the registry, the dispatcher, the tool sets
  components/ui/  Button, Field, Sheet, TextEditor
  views/          the shell, the outline, the sheets, the gestures
test/             an in-memory source, and two headless runs over it
```
