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

## State & actions

- **Render/logic separation.** A dumb component renders and forwards events; a
  sibling hook (`useApp`, `useEditor`, `useServers`) owns the logic. Components
  never own domain state.
- **Latent vs derived state.** Hooks hold only the minimal latent state
  (selection path, collapsed set, edit/pending mode) and derive everything else
  (the flat row list) from it plus the query results.
- **Action registry.** Every editor command the user triggers is defined once in
  `src/renderer/src/actions/editorActions.ts`, separately from the state it
  mutates. Both the keyboard handler and the command palette dispatch through it
  (via `useEditor`'s stable `runAction` and an `EditorController`), so a hotkey
  and its palette entry can't drift. New user-triggered commands go here, not
  inline in a keydown handler.

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
  actions/        the editor action registry
  helpers/        domain-agnostic utilities (cn, useTheme)
  components/      ui/ primitives + feature components
  views/          top-level screens + their logic hooks
```
