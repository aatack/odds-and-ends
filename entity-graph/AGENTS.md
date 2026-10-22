# Entity Graph — agent & contributor guide

An Electron + React + TypeScript app over a store of notes — a **pensive**. The
renderer holds no backend of its own: it calls the open pensive's tools over IPC
(`window.entityGraph`) — `scanEvents`, `writeValue`, `writeLink`, … — and the main
process is where the stores actually are. The main outliner edits the tree rooted
at `@index`.

There is no separate server. Which pensive is open, where it lives and who else
can reach it are all one graph the user draws on the Sources page; see
[Pensives and the sources graph](#pensives-and-the-sources-graph).

This guide is the *desktop* app's. There is a second one in [`mobile/`](./mobile) —
a phone-shaped PWA over one pensive, with its own dependencies, its own guide and no
imports from `src/`. Work on one does not touch the other; a broadcast node is the
only thing they share.

## Never run the app

**Do not start, launch, or run the app yourself under any circumstances unless
the user explicitly asks you to** — no `npm run dev`, `dev:no-sandbox`,
`electron-vite dev`, `preview`, or any other command that spawns the Electron
process. A local instance takes over the ports the user's own window is
broadcasting on, and opens a second handle on every SQLite store it holds. To
check your work, use `npm run typecheck`, `npm run build` and `npm test` — the
last drives the state, tool and pensive layers headlessly against an in-memory
pensive, so most of what you would want to click through can be asserted
instead. If you believe the app really
needs to be run, ask the user to run it.

## Design language

Clean, Linear-/Apple-esque: generous whitespace, restrained colour, one muted
accent, hairlines and tone over heavy chrome. Design tokens live in
`src/renderer/src/index.css` (Tailwind v4 `@theme`); standard utilities
(`bg-brand-600`, `text-gray-700`, `shadow-xs`, …) resolve to them.

**Hard rules:**

- **No motion.** There are **no transitions and no animations, anywhere.** State
  changes (hover, selection, opening a menu/modal) are instantaneous. A global
  rule in `index.css` disables `transition`/`animation` on every element, so the
  ban holds even if a utility class slips in — but don't add `transition-*`,
  `animate-*`, `duration-*`, or `@keyframes` in the first place.
- **Prefer no borders.** Separate surfaces with background tone (white panel on a
  gray body), a whisper of `shadow-xs`, or spacing. Where a divider is genuinely
  needed, keep it soft (`gray-100`).
- **No garish colours.** The accent is a muted indigo (`brand`), used sparingly;
  status hues are desaturated. Reach for gray first.
- **Never change the cursor to a pointer.** No `cursor-pointer`; the cursor stays
  the default arrow on clickable elements.
- **Focus rings only for the keyboard** (`focus-visible:`, with
  `focus:outline-none`).

## Typography

Two self-hosted fonts (bundled, no network), split by authorship:

- **UI chrome → Geist** (`--font-sans`, the default body font): everything the
  app draws — buttons, headers, labels, menus, config, badges.
- **User-entered text → Lora** (`--font-serif`), applied via the `font-serif`
  utility, at **weight 450**. Base size is **14px** and primary-text ink is
  **`#292929`** (the `gray-900` token).

Use `font-serif` for anything the user typed (entity content); leave chrome on
the default sans. Set this at the token/utility level — don't hard-code font
families per element.

## State, tools and views

Three layers, each depending only on the ones above it. `docs/frontend-state.md`
is the long form; the rules that matter day to day:

- **`state/` holds all of it, and holds nothing else.** One atom store, pure
  reducers, pure derivations. No React, no DOM — the app should in principle run
  headlessly, and `npm test` does exactly that. Latent state only: selection
  *paths*, collapsed sets, edit drafts. Anything derivable (the row list, the
  resolved selection, tab labels) is a function in `state/derive.ts`, and
  anything cached (entities, code output) lives in a runtime atom that is never
  persisted.
- **Never write derived state back.** The resolved selection path is computed
  from the latent one against the visible rows; storing it would lose the
  original when a collapsed ancestor is re-expanded.
- **Nothing on screen waits on the network.** The client keeps every event it
  has read in one entity cache (`core/cache.ts`); `useGetEntities()` reads it
  synchronously and always answers, and asking is what sets off the fetch. The
  rows are then a *derivation*: `core/query.ts` steps a depth-first traversal
  over whatever is cached and `core/tree.ts` turns that into rows, so folding,
  depth caps and edits redraw without a round trip and the tree fills in as
  events arrive. The only read of the store is `scanEvents`, which fetches a
  couple of layers ahead. **A write says what it changed** — the events it is
  making, or failing that the ids it touched — so that is all that is read again,
  and what is marked keeps what it has (`stale`) rather than turning back into a
  loading row. Reading the whole cache again is for a change made where this side
  cannot see it. See `docs/frontend-state.md` for the rest — types
  and how writes and undo reach the cache.
- **`src/core` is shared with the main process *and* the phone.** Anything put
  there is imported by three builds, so the parts the renderer and the phone
  reach must stay free of Electron, node and zod — which in practice means they
  import types from `core/pensive/types` and never the tool definitions.
  The entity and its rollup, the traversal, the tree, the markdown an outline is
  exported as, the cache and the atom under it all live there precisely because
  both clients have to agree on them — and in the markdown's case because the
  same outline goes to an agent over MCP; see
  [`mobile/AGENTS.md`](./mobile/AGENTS.md) for where that line is drawn.
- **Custom types are entities.** An entity's `type` value names another
  entity; one saying `type: type` is a custom type, its id is its name, and it
  holds a `schema` (JSON Schema for its instances) and optionally an `onLoad` (a
  function, as source, run in the sandbox once per instance per session as the
  instance loads, returning events for the cache only — never the store).
  `core/builtins.ts` serves the `type` entity itself, self-typed, whose schema
  describes those fields; `core/cache.ts` runs `onLoad` (`startLazy`) through
  `helpers/onLoad.ts`. [`docs/types.md`](./docs/types.md) is the long form, and
  MCP serves it as `docs://types`. How a type is *drawn* is still chosen in code
  (`components/kinds/`): a type it has no entry for draws as a note.
- **Schema components** (`components/schema/`, imported through its
  `index.ts`): `SchemaForm` (value, setValue, schema) edits a value its schema
  describes; `SchemaBuilder` (value, setValue) writes a schema. Both draw lines
  under hairlines and take `bare` inside another surface — only the outermost
  draws a border. `kindOf` in `schema.ts` is the one reading of what a schema is,
  and `parts.tsx` the pieces the two share. The folder depends on nothing of the
  app but three `../ui` primitives (Dropdown, Modal, TextEditor); keep it
  that way. Extension keywords: `x-multiline`, `x-monospace`, `x-popout`,
  `x-editor`. **Custom editors** (`editors.tsx`): a schema names one with
  `x-editor`, and `editors` on the form or builder (or `EditorsProvider`) says
  what the name means; it replaces a line's control in place, and the builder
  offers the ones that suit a field's kind. Every input in either component goes
  through the form — the builder's own options are a form — so add a control
  to the form rather than writing one into the builder.
- **The editor library** (`components/editors/`) is where custom editors live:
  `basic.tsx` for ones that need only their value, `tabs.tsx` and the like for
  ones that read the app. `EDITORS` in its `index.ts` is all of them. It sits
  beside `components/schema/` and is never imported by it.
- **`tools/` is the only way the user does anything.** Every command — moving the
  selection, opening a tab, writing a value — is one `ToolSpec` declaring its
  arguments, its scope, and how far it reaches. A tool is that and its `run`:
  there are no buttons on rows, no named calls a note watches, and no argument
  that answers itself from somewhere else. Hotkeys and the command palette
  both dispatch through that declaration, so they cannot drift. New commands go
  here, never inline in a keydown handler.
- **Every tool is the app's own.** The registry is fixed at build time; read it
  through `allTools()`. The store defines no tools, and **the app has no
  integrations**: nothing in the code reaches Slack, GitHub, Claude or any other
  service, and a note naming one is an ordinary note.
- **A script is another caller.** `runScript` runs JavaScript in a QuickJS worker
  (`helpers/codeRunner*`) whose only globals are `console`, `context` — the call
  context it was handed — and `tool`, which reaches the whole registry by
  the camel case of a tool's label (`tool.createChildOfEntity(parentId, text)`) or
  by its id (`tool['entity.create'](…)`). Arguments go positionally in the order
  the tool declares them, or as one object naming them. The calls are
  **synchronous**: the worker blocks on a `SharedArrayBuffer` while the main
  thread runs the call through the same machine a hotkey does, so a script's calls
  reach the cache and fail the same way. They are *not* kept in the call log,
  which records what the user did — `callToolByName` is the one way into the
  machine that isn't a gesture, and it says so (`origin: 'code'`).
- **One key listener, at the top.** `tools/dispatch.ts` owns it and resolves
  through the focus chain (focused frame → its tab group → the app), not through
  whatever has DOM focus. **A dialog owns the keyboard:** every `Modal` and open
  `Dropdown` registers itself (`openModal` in `state/ui.ts`), and while one is up
  the router hands keys to it rather than to the frames behind — Escape closes the
  innermost, ⌘P still opens the palette. A modal takes focus when it opens and
  hands it back when it closes.
- **Views render and forward.** A dumb component takes rows and callbacks; state
  comes from `state/hooks.ts`. Direct manipulation (clicking a row, typing in the
  in-place editor) calls a named action in `state/actions.ts`; anything invocable
  or worth recording is a tool.
- **Errors surface as call results.** A tool throws; the call machine settles it
  and the toast layer shows it. Components don't raise toasts themselves. Both the
  toast and the call log are a record of what the *user* did, so a script's calls
  reach neither — its errors belong where the script is.

## Pensives and the sources graph

**A pensive is an interface, not an implementation.** `core/pensive/types.ts` is
the whole contract: read events, write events, pop events, read a resource, write
a resource, list tools, call a tool. The first five are the store; the last two
are the *vocabulary* — `query`, `createEntity` and the rest — which
`core/pensive/tools.ts` builds over those five, so a
pensive gains them by being one. `BasePensive` is that assembly; an
implementation says how events are stored and nothing else.

Four of them exist:

| | what it is | inputs |
| --- | --- | --- |
| `SqlitePensive` | one file on this machine — the only one that holds notes | — |
| `CombinedPensive` | its inputs read as one store, written to one of them | many |
| `ConnectPensive` | a pensive on another machine, over HTTP | — |
| `AttributedPensive` | any pensive, with every write recorded as one person | one |

Plus `PausedPensive`, which is what a switched-off node *is*: every call fails
with a sentence saying who is paused. A combiner reads *around* one of those —
pausing one store of five takes its notes out of the outline rather than taking
the outline down — but writing to it still fails, having one place to go.

**The user draws the arrangement.** The Sources page is a graph of nodes — a
node per pensive, plus the two that publish one (`broadcast` over HTTP,
`mcp` for an agent) and one fixed node standing for this window. An edge names the
pensive at the other end of it, so a combiner's children are edges rather than
configuration, and what the outliner shows is whatever has been dragged into the
desktop node.
The graph is a SQLite file of the app's own under `userData`; `src/core/client.ts`
holds the shapes both ends read, `NODE_KINDS` included — the page draws its
handles from it and the main process refuses an edge that disagrees with it.
A node of a kind this build doesn't know (the Slack and GitHub feeds an older
build drew) stays in the file and is left out of every read.

Three rules hold on the main-process side rather than in the page, because the
page is not the only caller:

- **A loop is refused** — while an edge is written (`wouldCycle`) and again while
  a pensive is built. It is a node downstream of *itself along one path*, so the
  second check threads the path down the recursion; a shared "being built" set
  cannot tell that from two callers asking for the same node at once, which is
  ordinary and must wait for the build in flight instead.
- **A paused node yields a `PausedPensive`**, so pausing works for a broadcast's
  callers too, which get a 403.
- **A token is an identity.** A token on a published node is issued to somebody
  by name, and every write that arrives with it is recorded as that author,
  whatever the client asks for. That is `AttributedPensive`, wrapped on per
  request.

**`better-sqlite3` has to match whoever is loading it.** The stores are in the
Electron main process now, so it must be built against Electron's ABI:
`npm run rebuild:electron` after an install. The tsx tests deliberately never
touch it — `test/source.mts` is an in-memory pensive for exactly that reason — but
`npm run migrate:v2` does, and needs `npm run rebuild:node` first (and
`rebuild:electron` again afterwards, or the app won't start).

`src/main/pensive/` is the rest: `graph.ts` the file, `registry.ts` the building
of pensives from it, `http.ts` one small server per published node, `mcpServer.ts`
what an agent sees, `servers.ts` keeping the listeners in step with the drawing.
**A published node binds loopback only** — a copied URL has to outlive the wifi
that was up when it was copied, so reaching one from another machine is a node
kind yet to be written, not a wider bind.
[`docs/sources.md`](./docs/sources.md) is the long form — the interface, every
node's type, and what a broadcast serves.

## Phone access

**Unwired at the moment.** `tailscale serve` — the app's build at `/` and one
broadcast node at `/api/<nodeId>`, on the tailnet's HTTPS name — is how the phone
client in [`mobile/`](./mobile) reaches this machine, and it will come back as a
*node* on the sources graph rather than as a panel on the page. Everything below
is still in the tree and still works; what is gone is the UI that called it
(`components/PhoneAccess.tsx` and `views/useTailscale.ts` are unrendered, and the
IPC handlers are unreached). Plumbing it back in is wiring, not writing.

`src/main/tailscale.ts` is the whole of it, and is plain Node — the app root is
passed in rather than read from `electron`, so it runs outside the app. It holds
**no state**: Tailscale's own config is read back on every refresh, so a mount
made from a shell and one made from a switch are indistinguishable, and a switch
can't drift from the truth. Two rules follow from `tailscale` having no per-path
removal:

- **Adding is one idempotent command; removing is `reset` plus a rebuild of
  everything else.** So removal is the only destructive operation, the only one
  that can be refused, and the only one that needs to check first.
- **A mount pointing elsewhere is a conflict, not an off state.** Don't take over
  a path something else holds.

Changing the serve config needs `sudo tailscale set --operator=$USER` once;
reading it doesn't, which is why a permission failure only ever shows up on a
write. `failed()` spots that case and gives the fix rather than passing
tailscaled's wording through.

## Reusable components

Primitives live in `src/renderer/src/components/ui/` — `Button`, `Badge`,
`Input`, `Select`, `Field`, `Modal`, `Dropdown`, `ContextMenu`, `IconButton`,
`TextEditor` (the one free-text editing control). Domain-agnostic helpers go in
`src/renderer/src/helpers/`; check there before adding one. If a
component/helper appears twice, factor it out.

`Modal` takes a `size`: `md` and `wide` are dialogs that grow with their contents
and scroll the backdrop; `large` is a panel — nearly the whole window, `flex`
column, its body scrolling *inside* the card so whatever the child pins to the top
stays there. `EntityInspector` is the one of those, and its tabs are why.

**Every kind of entity has three views** (`components/kinds/`), by how much room
it is given — 0-, 1- and 2-dimensional:

- **Pill** — inline, in prose or beside other things: a name and a mark.
- **Card** — a row to itself: the gist and its key actions. A row draws its
  kind's card and nothing else — no type pill.
- **Focus** — a page to itself, which is the root row of a frame. Optional, and a
  note's is its card: its page is its text with its children as the rows below.
  A file's focus is the whole file (a large image, text read out, media playing).

`kindOf(type)` in `components/kinds/index.ts` is the one place a kind's views are
chosen — rows, pills and frame roots all ask there — and a type nothing is
registered for is drawn as a note. Two kinds exist: the note and `file`. Adding
a kind is adding an entry, not a branch in the editor.

**Peeks** (`layout/PeekLayer.tsx`, `state/peeks.ts`) are floating windows onto
whatever a pill or a link refers to, opened by hovering it for a moment and closed
a moment after the pointer leaves — unless the window has been moved or resized,
which pins it until its ×. An entity's peek is a tab of its own that belongs to no
group, so its frame is an ordinary frame: it draws the entity's focus with its
children below, and pressing in it gives it the keys (`peekFocus`), which points
the focus chain at its frame — its own selection and stack, the same tools, and
no group tools, since it has no group. A page's peek is a locked-down `<webview>`
(no preload, no node, its own partition; see `will-attach-webview` in
`src/main/index.ts`). Peeks stack: hovering inside one opens another on top, and
leaving closes everything above where the pointer went. Only pinned peeks survive
a reload. No key opens or closes one.

**Links are pills.** A link in an entity's text is drawn as `LinkPill` — its
words behind a globe — and hovering a web page peeks at it; a plain click still
does nothing and ⌘/Ctrl-click opens it in the browser. Another entity is
mentioned as `[@entity:id](label)`, which draws that entity's pill. `peekHandlers` in
`components/peekHover.ts` is what anything peekable spreads on; `PillWrapper`
takes `peek` to do so (`EntityPill` always does; a tab doesn't, being what is
already on screen).

**Entity pills** (`components/EntityPill.tsx`) are how an entity appears anywhere
it is referenced in passing rather than shown in full — a tab title, a crumb, a
mention inside another entity's text. Three parts, usable separately:
`PillContent` (the entity's kind's pill), `PillWrapper` (the entity
gestures, around any child — it publishes `data-entity-id`, which is all the
global right-click and middle-click handlers need), and `PillBackground` (a
surface for a pill that has to be told apart from the text around it).
`EntityPill` is all three. Reach for these rather than writing out an entity's
label: a bare label can't be right-clicked, and it won't follow the entity.

**Custom fields** (`components/ui/markdownFields.ts`) are how a row's text holds
something other than text: `[@type:arg](text)` renders as a component instead of
as markup. There is one: `[@entity:id](label)`, another entity as a pill
mid-sentence — the label a fallback for while that entity has no text of its own.
A link is still a link; only that label shape, and only for a type the caller can
render, is taken. `Markdown` takes the renderers and knows nothing else about
them; `EntityMarkdown` is where they meet the app.

## Layout

```
mobile/         a separate phone client (PWA) for one pensive — its own install, own guide
scripts/        one-off tools run outside the app with tsx (the v2 migration, the sources one)
src/main       Electron main — window, tailscale serve, and:
  pensive/        the graph of nodes, the pensives built from it, and the servers publishing them
src/preload     contextBridge exposing the typed EntityGraphAPI
src/core        the shared model: entity + rollup, traversal, tree, markdown, cache
  pensive/        what a pensive is, the tools it wears, and the four kinds of one
test/           the state, tool and pensive layers driven headlessly (`npm test`)
src/renderer/src  React app
  state/          latent state, pure derivations, the entity cache
  source/         the transport seam onto the open pensive
  tools/          the tool registry, pending-call machine, key router
  helpers/        domain-agnostic utilities (cn, code runner)
  components/     ui/ primitives + feature components
  layout/         tab groups, tabs, frames
  views/          top-level screens
```
