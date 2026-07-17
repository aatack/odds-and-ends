# Orchestrator — agent & contributor guide

A local tool for managing the things I work on — GitHub PRs, agents, tickets —
and the relationships between them. Electron + React + TypeScript + SQLite.

It lives in the `local-helpers` monorepo **next to `pr-review-helper`** but is a
**separate project** with its own dependencies and lifecycle. Don't conflate the
two: the PR helper is a Vite web app driven by the `gh` CLI; this is an Electron
desktop app with a local database.

## Design language

Clean, Linear-/Apple-esque: generous whitespace, restrained colour, one muted
accent, hairlines and tone over heavy chrome, immediate feedback for every
interaction. Tokens live in `src/renderer/styles.css`. Consistency is a feature:
if an interaction (e.g. right-click, drag-to-link) works on an item in one place,
it works everywhere that item appears.

**Hard rules (apply to every new surface):**

- **Prefer no borders.** Separate surfaces with background tone (e.g. white
  panel on a `gray-50` body), a whisper of shadow (`shadow-xs`), or spacing.
  Where a divider is genuinely needed, make it soft — `gray-100`, never darker.
- **No garish colours.** The accent is a muted indigo, used sparingly; status
  hues are desaturated. Reach for gray first; add colour only when it carries
  meaning, and keep it quiet.
- **Never change the cursor to a pointer.** No `cursor-pointer` on clickable
  elements — the cursor stays the default arrow everywhere.
- **Compact, flat controls.** Small heights, `font-medium`, `rounded-md`, no drop
  shadows on buttons, quiet focus rings. Segmented controls over rows of pills.
- **Status is a dot + neutral text**, not a filled chip — use `<Badge dot>`.
  Reserve filled `<Badge>` for labels that aren't status (e.g. an item's type).
- **One radius scale.** `md` for controls (buttons, chips, inputs), `lg` for
  cards / rows / nested panels, `xl` for large surfaces and modals. A container
  wrapping `md` children may step up one (e.g. an `lg` segmented track). Reserve
  `rounded-full` for dots and avatars only.
- **Consistent interaction states.** Hover fill is `bg-gray-100/70`; the selected
  state is a solid `bg-gray-100`. Same treatment for cards, list rows, and menu
  items alike.
- **Motion is subtle and fast.** ~150ms `transition-colors` on hover/state.
  Enter animations (`animate-fade-in`, `animate-pop-in`, defined in `styles.css`)
  are reserved for overlays/popovers — never bouncy or slow. Respect
  `prefers-reduced-motion` (handled globally).
- **Focus rings only for the keyboard.** `focus-visible:` never bare `focus:`;
  pair with `focus:outline-none`.
- **Quiet empty states.** A single muted line (`text-gray-400`), centred, with
  padding — no boxes, dashed borders, or illustrations.
- **Icons: one set** (`@untitledui/icons`), `text-gray-500` (or `gray-400` when
  de-emphasised). Standalone/control icons are 16px; icons sitting inline with
  text match the adjacent text size.

**Type scale (keep it small).** UI default `text-[13px]`; secondary / meta
`text-xs`; micro labels `text-[11px]` uppercase. User prose is serif at
`text-[15px]`, titles serif `text-2xl`. Secondary text is `gray-500`, tertiary
`gray-400`. Don't introduce ad-hoc sizes outside this set.

**Typography.** Two self-hosted fonts (bundled via `@fontsource-variable/*`, no
network). UI chrome is **Geist** (`--font-sans`, the default). User-written text
— anything the user typed: item titles, descriptions, chat — is **Source Serif 4**
via the `font-serif` utility. This split is the core of the visual voice: keep
chrome sans and content serif. Fonts are imported in `main.tsx` before
`styles.css`.

**Typography.** Two self-hosted fonts (bundled via `@fontsource-variable/*`, no
network). UI chrome is **Geist** (`--font-sans`, the default). User-written text
— anything the user typed: item titles, descriptions, chat — is **Source Serif 4**
via the `font-serif` utility. This split is the core of the visual voice: keep
chrome sans and content serif. Fonts are imported in `main.tsx` before
`styles.css`.

## Programming principles

- **Functional style.** Return new values; don't mutate. Prefer `map`/`filter`/
  `reduce`. State transitions happen through actions, not in-place edits.
- **Reuse.** If a component/helper appears twice, factor it out. Domain-agnostic
  helpers go in `src/renderer/helpers/` — check there before adding a new one.
- **Minimal comments.** Only document (a) an unusual technical decision, with the
  reason, or (b) an invariant on inputs. Never narrate what the code does.
- **State is separate from rendering.** Components read state and dispatch
  actions; they never own domain state.

## State (three levels)

1. **Local** — trivial UI state in `useState` (e.g. "is this card expanded").
   Doesn't need an action.
2. **Session** — global-ish, non-persisted: Jotai atoms in `src/renderer/state/`
   (e.g. the loaded DB snapshot, the action log).
3. **Persisted**
   - **True persisted state** → SQLite, mutated only by **async actions** over
     IPC. The renderer mirrors it in `snapshotAtom` and refreshes after writes.
   - **User-specific throwaway state** (e.g. which item is selected) → a Jotai
     atom persisted to `localStorage` (`atomWithStorage`), mutated by a
     **synchronous** action, applied optimistically.

## Actions

All state changes flow through the action registry (`src/renderer/actions/`).
Each action has a unique `name`, a Zod `params` schema, and a `run` function.
Same registry is reachable from the UI, hotkeys, the command palette, and
(later) the AI.

- `runAction(name, params)` validates params, logs to the audit trail (last
  ~200), and — for async actions — tracks pending/failed status. Failed actions
  keep their arguments so they can be re-run verbatim (`rerunAction`).
- Set `log: false` to keep an action out of the trail (e.g. `selection.select`).
- Set `palette` to expose an action in ⌘K.
- Integration work (fetching/mutating PRs, launching agents, …) must also go
  through actions.

## Data model

- **`items`** — every item, keyed by `(type, id)`. `id` is unique within a type
  (repo+branch for branches, URL for PRs, session id for agents). Common columns:
  `created_at`, `archived_at`. Archival is a shared behaviour across all types.
- **`links`** — arbitrary directional links between two items (`source`, `dest`).
  Directional by default; the UI usually treats them as bidirectional.
- **Per-type tables** — e.g. `tasks`. May branch to further tables as needed
  (e.g. `agent_messages`).

Everything type-specific about rendering lives behind `ItemView` in
`src/renderer/components/items/itemViews.tsx`. Add a new item type by adding a
DB table + repository functions + IPC handlers + an `itemViews` entry; the
generic Pill/Card/Detail shells then work for it automatically.

## Reusable components

- **`TextEditor`** — the only text input. `value`/`setValue`, `eager` flag
  (commit per keystroke vs on blur/Enter), autosizes.
- **`ItemDetail`** — full detail view; per-type body + common header, archival,
  debug (links as pills, metadata), and drop-to-link.
- **`ItemCard`** — compact "inbox row"; draggable, drop target, click to open (or
  expand in place via a flag).
- **`ItemPill`** — one-line overview; draggable, click to open, truncates.

## Layout

```
src/
  main/       Electron main process — window, SQLite (better-sqlite3), IPC
  preload/    contextBridge: exposes the typed OrchestratorApi to the renderer
  shared/     types + the IPC API contract, imported by main and renderer
  renderer/   React app
    state/    Jotai atoms + the shared store
    actions/  action registry + action definitions
    helpers/  domain-agnostic utilities
    components/  ui/ primitives, items/ (Pill/Card/Detail + view registry)
    views/    top-level screens
```

## Note on `task`

`task` is a **placeholder** item type used to build and demo the generic
machinery. Real item types (PR, agent, ticket) come later and slot into the same
`items`/`links`/`ItemView` structure.
