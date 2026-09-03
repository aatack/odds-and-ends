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
  cannot see it. See `docs/frontend-state.md` for the rest — types,
  `events` scripts, and how writes and undo reach the cache.
- **`src/core` is shared with the main process *and* the phone.** Anything put
  there is imported by three builds, so the parts the renderer and the phone
  reach must stay free of Electron, node and zod — which in practice means they
  import types from `core/pensive/types` and never the tool definitions.
  The entity and its rollup, the traversal, the tree, the markdown an outline is
  exported as, the cache and the atom under it all live there precisely because
  both clients have to agree on them — and in the markdown's case because the
  same outline goes to an agent over MCP; see
  [`mobile/AGENTS.md`](./mobile/AGENTS.md) for where that line is drawn.
- **A type describes its instances; it does not lend them values.** An entity's
  `type` names another entity, whose `schema` says which values an entity of that
  kind holds, whose `actions` name the tools every row of it wears as a button,
  and whose `events` script is run once per instance. Nothing of it is inherited — what an
  entity holds is what was written to it. `core/schema.ts` is the reading of a
  type and `core/builtins.ts` the one type the store supplies (`type` itself,
  schema and all, whether or not anybody wrote it);
  [`docs/types.md`](./docs/types.md) is the long form.
- **`tools/` is the only way the user does anything.** Every command — moving the
  selection, opening a tab, writing a value — is one `ToolSpec` declaring its
  arguments, its scope, and how far it reaches. Hotkeys and the command palette
  both dispatch through that declaration, so they cannot drift. New commands go
  here, never inline in a keydown handler.
- **The registry isn't all built at build time.** The *integrations* (GitHub,
  Slack, Claude, git, a terminal — `src/main/integrations/`, documented in
  [`docs/integrations.md`](./docs/integrations.md)) are read from the main process
  when the app starts and folded into the same registry, with their argument
  prompts derived from the JSON Schema they publish. So read the list through
  `allTools()`, never a constant, and add a new integration in `src/main` rather
  than here. **They are the app's own hands and belong to no pensive** — nothing
  a broadcast or an MCP node serves can reach them.
- **The store defines tools too.** A note under `@tools` saying `type: tool`
  becomes a tool of the app, called whatever its text says — palette entry,
  optional key, callable from other scripts. Its `execute` value
  is an expression evaluating to a function, applied to the declared arguments
  positionally and run in the same sandbox a code entity uses, so it reaches the
  whole registry. See [`docs/user-tools.md`](./docs/user-tools.md) for the fields.
  `tools/declared.ts` holds what these have in common with the integrations, since
  both arrive described from outside; `tools/userTools.ts` reads them, and
  "Reload your tools" re-reads them after an edit. **Changesets are the largest
  thing written this way** — a worktree, a Claude session and a pull request held
  open as one entity — and [`docs/changesets.md`](./docs/changesets.md) is the
  worked example: what the definitions are, and which tools here exist for them
  to be written in terms of. When something a definition needs is missing, the
  answer is a tool it can reach, not a feature in `tools/`.
- **A diagram is a note that holds a drawing.** `type: diagram` draws a pannable
  canvas above the note's own text, and its shapes are the entity's own values —
  one per key, under a key beginning `diagram/`, so a diagram still has ordinary
  notes under it and moving one rectangle is one small event rather than a rewrite
  of the picture. `core/diagram.ts` is the reading of them, `components/Diagram.tsx`
  the canvas, and [`docs/diagrams.md`](./docs/diagrams.md) the long form. React
  Flow does the viewport, the dragging and the arrow heads; it does not do the
  state, and it holds no keys — its own key handling is off, since there is one key
  listener and it is at the top.
- **A code entity is another caller.** `type: code` runs in a QuickJS worker
  (`helpers/codeRunner*`) whose only globals are `console`, `context` — the folded
  call context of the entity it is on, so `context.channel` is whatever an
  ancestor said the channel was — and `tool`, which reaches the whole registry by
  the camel case of a tool's label (`tool.sendSlackMessage(channel, text)`) or by
  its id (`tool['slack.sendMessage'](…)`). Arguments go positionally in the order
  the tool declares them, or as one object naming them. The calls are
  **synchronous**: the worker blocks on a `SharedArrayBuffer` while the main
  thread runs the call through the same machine a hotkey does, so a script's calls
  reach the cache and fail the same way. They are *not* kept in the call log,
  which records what the user did — `callToolByName` is the one way into the
  machine that isn't a gesture, and it says so (`origin: 'code'`).
- **One key listener, at the top.** `tools/dispatch.ts` owns it and resolves
  through the focus chain (focused frame → its tab group → the app), not through
  whatever has DOM focus.
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
are the *vocabulary* — `query`, `createEntity`, whatever the user has written
under `@tools` — which `core/pensive/tools.ts` builds over those five, so a
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
with a sentence saying who is paused, so a combiner one of whose inputs is paused
is broken exactly that far.

**The user draws the arrangement.** The Sources page is a graph of nodes — a
node per pensive, plus the two that publish one (`broadcast` over HTTP,
`mcp` for an agent) and one fixed node standing for this window. An edge means
"read that one", so a combiner's children are edges rather than configuration,
and what the outliner shows is whatever has been dragged into the desktop node.
The graph is a SQLite file of the app's own under `userData`; `src/core/client.ts`
holds the shapes both ends read, `NODE_KINDS` included — the page draws its
handles from it and the main process refuses an edge that disagrees with it.

Three rules hold on the main-process side rather than in the page, because the
page is not the only caller:

- **A loop is refused** — while an edge is written (`wouldCycle`) and again while
  a pensive is built.
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
[`docs/sources.md`](./docs/sources.md) is the long form — the interface, every
node's type, and what a broadcast serves.

## Phone access

The Sources page also drives `tailscale serve`, which is how the phone client in
[`mobile/`](./mobile) reaches this machine: the app's build at `/`, and one
broadcast node at `/api/<nodeId>`, on the tailnet's HTTPS name.

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

**Entity pills** (`components/EntityPill.tsx`) are how an entity appears anywhere
it is referenced in passing rather than shown in full — a tab title, a crumb, a
mention inside another entity's text. Three parts, usable separately:
`PillContent` (the entity at pill size, by type), `PillWrapper` (the entity
gestures, around any child — it publishes `data-entity-id`, which is all the
global right-click and middle-click handlers need), and `PillBackground` (a
surface for a pill that has to be told apart from the text around it).
`EntityPill` is all three. Reach for these rather than writing out an entity's
label: a bare label can't be right-clicked, and it won't follow the entity.

**Type pills** (`components/TypePill.tsx`) are how a row shows what it *is*. Any
entity with a `type` value gets one at the head of its row, in the tone of a
secondary button with nothing to press. On a row drawing prose it is *floated into
the text*: the first line begins after it and every line below runs the full
width, so the type reads as the entity's first word rather than as a column in
front of it. A code block, a file's bytes and a box being typed into can't flow
around a float, so on those the pill sits beside the mark instead. `code` and
`file` already look like themselves; the pill is what stops every other type the
user invents from reading as a plain bullet. It takes a `label` rather than a
type, because a row's type is not its only caller: the `[@pill](text)` field puts
the same shape mid-sentence, for a word no value on the entity accounts for.

**Custom fields** (`components/ui/markdownFields.ts`) are how a row's text holds
something other than text: `[@type:arg](text)` renders as a component instead of
as markup — `[@button:tool](label)` is the Actions button inline, naming a tool
the way a script does (its id, or the camel case of its label),
`[@codeEditor:key](hint)` a code box over one of the entity's values,
`[@pill](text)` the type pill's shape around a word the sentence wants set apart,
and `[@tool:callId](label)` how one call is getting on — counting while it runs,
and how it ended after. The `:arg` is optional, as the pill shows: a form that
points at nothing is written without one. A link is still a link; only that label
shape, and only for a type the caller can render, is taken. `Markdown` takes the
renderers and knows nothing else about them;
`EntityMarkdown` is where they meet the app, and is what the outliner renders a
row with. A field acts on the entity whose text it appears in and not on the
selection: its calls are born along that row's path (`runTool`'s `within`, the
gesture counterpart of `contextWithin`), so a button in a row's prose is a button
on *that* row wherever the cursor happens to be.

`[@button:…]` is also how a **type's actions** are drawn: `views/Editor` appends
one per action to the row's text before rendering it, so there is one way a
button gets beside a row rather than two. See [`docs/types.md`](./docs/types.md).

**A call can be named.** `$callId` alongside a script's arguments —
`tool['claude.runPrompt']({ …, $callId: id })` — makes that string the call's id
instead of a fresh uuid, and a named call is kept in the activity log however it
came about, where a script's calls otherwise are not. Naming one is only ever
done in order to point at it afterwards, which is what `[@tool:callId]` does:
write the field into a note before making the call, and the note watches the
call.

## Layout

```
mobile/         a separate phone client (PWA) for one pensive — its own install, own guide
scripts/        one-off tools run outside the app with tsx (the v2 migration, the sources one)
src/main       Electron main — window, tailscale serve, and:
  pensive/        the graph of nodes, the pensives built from it, and the servers publishing them
  integrations/   the app's reach outside itself (GitHub, Slack, Claude, git, terminal)
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
