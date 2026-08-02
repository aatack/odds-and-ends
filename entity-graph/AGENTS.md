# Entity Graph — agent & contributor guide

An Electron + React + TypeScript desktop client for an entity-graph source
server. The renderer holds no backend of its own: it opens a source over IPC
(`window.entityGraph`) and drives it through the source's tools (`scanEvents`,
`writeValue`, `writeLink`, …). The main outliner edits the tree rooted at
`@index`.

This guide is the *desktop* client's. There is a second one in [`mobile/`](./mobile) —
a phone-shaped PWA over one source, with its own dependencies, its own guide and no
imports from `src/`. Work on one does not touch the other; the server is the only thing
they share.

## Never run the app

**Do not start, launch, or run the app yourself under any circumstances unless
the user explicitly asks you to** — no `npm run dev`, `dev:no-sandbox`,
`electron-vite dev`, `preview`, or any other command that spawns the Electron
process. A local instance seizes the source server's port and conflicts with the
Electron window the user already has open. To check your work, use `npm run
typecheck`, `npm run build` and `npm test` — the last drives the state and
source layers headlessly against an in-memory source, so most of what you would
want to click through can be asserted instead. If you believe the app really
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
  couple of layers ahead. See `docs/frontend-state.md` for the rest — type
  defaults, `events` scripts, and how writes and undo reach the cache.
- **`src/core` is shared with the server *and* the phone.** Anything put there
  is imported by three builds, so it must stay free of Electron, node and zod.
  The entity and its rollup, the traversal, the tree, the markdown an outline is
  exported as, the cache and the atom under it all live there precisely because
  both clients have to agree on them — and in the markdown's case because the
  server hands the same outline to an agent over MCP; see
  [`mobile/AGENTS.md`](./mobile/AGENTS.md) for where that line is drawn.
- **`tools/` is the only way the user does anything.** Every command — moving the
  selection, opening a tab, writing a value — is one `ToolSpec` declaring its
  arguments, its scope, and how far it reaches. Hotkeys and the command palette
  both dispatch through that declaration, so they cannot drift. New commands go
  here, never inline in a keydown handler.
- **The registry isn't all built at build time.** The server's *integrations*
  (GitHub, Slack, Claude, git — `server/src/integrations/`, documented in
  `server/docs/integrations.md`) are fetched when a source opens and folded into
  the same registry, with their argument prompts derived from the JSON Schema the
  server publishes. So read the list through `allTools()`, never a constant, and
  add a new integration on the server rather than here.
- **The store defines tools too.** A note under `@tools` becomes a tool of the app
  — palette entry, optional key, callable from other scripts. Its `execute` value
  is an expression evaluating to a function, applied to the declared arguments
  positionally and run in the same sandbox a code entity uses, so it reaches the
  whole registry. See [`docs/user-tools.md`](./docs/user-tools.md) for the fields.
  `tools/declared.ts` holds what these have in common with the integrations, since
  both arrive described from outside; `tools/userTools.ts` reads them, and
  "Reload your tools" re-reads them after an edit.
- **A code entity is another caller.** `type: code` runs in a QuickJS worker
  (`helpers/codeRunner*`) whose only globals are `console`, `context` — the folded
  call context of the entity it is on, so `context.channel` is whatever an
  ancestor said the channel was — and `tool`, which reaches the whole registry by
  the camel case of a tool's label (`tool.sendSlackMessage(channel, text)`) or by
  its id (`tool['slack.sendMessage'](…)`). Arguments go positionally in the order
  the tool declares them, or as one object naming them. The calls are
  **synchronous**: the worker blocks on a `SharedArrayBuffer` while the main
  thread runs the call through the same machine a hotkey does, so a script's calls
  refresh the frames and fail the same way. They are *not* kept in the call log,
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

## Phone access

The Sources page also drives `tailscale serve`, which is how the phone client in
[`mobile/`](./mobile) reaches this machine: the app's build at `/`, and one source
at `/api/<sourceId>`, on the tailnet's HTTPS name.

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
user invents from reading as a plain bullet.

**Custom fields** (`components/ui/markdownFields.ts`) are how a row's text holds
something other than text: `[@type:arg](text)` renders as a component instead of
as markup — `[@button:tool](label)` is the Actions button inline, naming a tool
the way a script does (its id, or the camel case of its label), and
`[@codeEditor:key](hint)` a code box over one of the entity's values. A link is
still a link; only that label shape, and only for a type the caller can render,
is taken. `Markdown` takes the renderers and knows nothing else about them;
`EntityMarkdown` is where they meet the app, and is what the outliner renders a
row with. A field acts on the entity whose text it appears in and not on the
selection: its calls are born along that row's path (`runTool`'s `within`, the
gesture counterpart of `contextWithin`), so a button in a row's prose is a button
on *that* row wherever the cursor happens to be.

## Layout

```
server/         the HTTP server: sources, and the integrations (GitHub, Slack, Claude, git)
mobile/         a separate phone client (PWA) for one source — its own install, own guide
src/main       Electron main — window, servers, config store, tailscale serve
src/preload     contextBridge exposing the typed EntityGraphAPI
src/core        the shared model: entity + rollup, traversal, tree, markdown, cache, sources
test/           the state layer driven headlessly (`npm test`)
src/renderer/src  React app
  state/          latent state, pure derivations, the entity cache
  source/         the transport seam onto the open source
  tools/          the tool registry, pending-call machine, key router
  helpers/        domain-agnostic utilities (cn, code runner)
  components/     ui/ primitives + feature components
  layout/         tab groups, tabs, frames
  views/          top-level screens
```
