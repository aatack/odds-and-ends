# Entity Graph — agent & contributor guide

An Electron + React + TypeScript desktop client for an entity-graph source
server. The renderer holds no backend of its own: it opens a source over IPC
(`window.entityGraph`) and drives it through the source's tools (`query`,
`writeValue`, `writeLink`, …). The main outliner edits the tree rooted at
`@index`.

## Never run the app

**Do not start, launch, or run the app yourself under any circumstances unless
the user explicitly asks you to** — no `npm run dev`, `dev:no-sandbox`,
`electron-vite dev`, `preview`, or any other command that spawns the Electron
process. A local instance seizes the source server's port and conflicts with the
Electron window the user already has open. To check your work, use `npm run
typecheck` and `npm run build` only. If you believe the app needs to be run to
verify something, ask the user to run it.

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
  headlessly. Latent state only: selection *paths*, collapsed sets, edit drafts.
  Anything derivable (the row list, the resolved selection, tab labels) is a
  function in `state/derive.ts`, and anything cached (query results, code output)
  lives in a runtime atom that is never persisted.
- **Never write derived state back.** The resolved selection path is computed
  from the latent one against the visible rows; storing it would lose the
  original when a collapsed ancestor is re-expanded.
- **`tools/` is the only way the user does anything.** Every command — moving the
  selection, opening a tab, writing a value — is one `ToolSpec` declaring its
  arguments, its scope, and how far it reaches. Hotkeys and the command palette
  both dispatch through that declaration, so they cannot drift. New commands go
  here, never inline in a keydown handler.
- **One key listener, at the top.** `tools/dispatch.ts` owns it and resolves
  through the focus chain (focused frame → its tab group → the app), not through
  whatever has DOM focus.
- **Views render and forward.** A dumb component takes rows and callbacks; state
  comes from `state/hooks.ts`. Direct manipulation (clicking a row, typing in the
  in-place editor) calls a named action in `state/actions.ts`; anything invocable
  or worth recording is a tool.
- **Errors surface as call results.** A tool throws; the call machine settles it
  and the toast layer shows it. Components don't raise toasts themselves.

## Reusable components

Primitives live in `src/renderer/src/components/ui/` — `Button`, `Badge`,
`Input`, `Select`, `Field`, `Modal`, `Dropdown`, `ContextMenu`, `IconButton`,
`TextEditor` (the one free-text editing control). Domain-agnostic helpers go in
`src/renderer/src/helpers/`; check there before adding one. If a
component/helper appears twice, factor it out.

## Layout

```
src/main       Electron main — window, servers, config store
src/preload     contextBridge exposing the typed EntityGraphAPI
src/core        source client + query wrapper (shared types)
src/renderer/src  React app
  state/          latent state, pure derivations, the query engine
  source/         the transport seam onto the open source
  tools/          the tool registry, pending-call machine, key router
  helpers/        domain-agnostic utilities (cn, code runner)
  components/     ui/ primitives + feature components
  layout/         tab groups, tabs, frames
  views/          top-level screens
```
