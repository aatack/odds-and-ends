# detail-views

A local desktop app for managing my work. It will grow over time; these are the
constraints every change must keep.

## Invariants

### Product

- **Modules are workflows.** Each one (Slack, tasks, later calendar, …) is a
  module with a root entity. The sidebar lists the modules and nothing else; it
  is how I enter the app.
- **One entity is focused at a time.** Everything is an entity: a Slack
  conversation, a message, a task. The bulk of the window is the focus view of
  that one entity.
- **Links are directional, parent → child.** Focusing an entity shows its
  children. Any entity may link to any other, across modules.
- **Navigation is a trail.** Opening a child pushes it; back and forward walk
  the trail, like a browser.
- **Desktop app, not a web page.** Electron, run locally.

### Data

- **One local SQLite file** (`node:sqlite`, no native modules).
- **Owned vs cached.** What I type is owned: `expires_at` is null and it is
  never deleted by the app. Data fetched from another service is a cache: it
  carries an `expires_at` and the sweeper deletes it once it has passed.
- **A cache write never downgrades owned data**, and an owned link keeps both
  of its ends alive past their expiry, so my own notes never dangle.
- **Cached ids are namespaced by service** (`slack:conv:C123`), so they cannot
  collide with owned ids (uuids) or with each other.
- **Slack is read-only during development.** `slackWrites` in
  `modules/slack/api.ts` gates an allowlist of read methods; do not turn it on
  or widen the list unless I ask.
- **GitHub goes through `gh`.** Reads are `gh api graphql` queries, never a
  mutation. The only writes are the PR actions I asked for, allowlisted in
  `checkWrite` (`modules/github/github.ts`): approve someone else's PR; on my
  own, approve locally (an owned entity) and turn on auto-merge; close mine and
  delete the branch. Each is started by me and confirmed with Enter. `gh` runs
  from the temp dir so `--delete-branch` never touches a local checkout.
- **Actions are generic.** A module offers `actions(entity)` and does them in
  `perform`; the UI shows each as a button and a key (`tools.ts`), opens a
  prompt, and only Enter confirms. A PR is an entity keyed by its URL
  (`prEntityId` in `core/types.ts`), so a link to one anywhere is that item;
  `Module.materialise` makes the entity the first time an unseen one is looked at.
- **Secrets and settings are local**, in the `settings` table of the same file.
- Migrations are append-only in `src/core/db.ts`. Never edit a shipped one.

### UI

- **Minimal.** No descriptive headers, no labels that restate what is obvious,
  no information shown twice. If a row already says it, nothing else does.
- **Keyboard first.** Every key press goes through one listener
  (`renderer/src/dispatch.ts`) that runs a tool from the single registry
  (`renderer/src/tools.ts`). Never add a `keydown` listener anywhere else.
- **Navigation keys are a fixed rule** across every focus view: `w` up, `s`
  down, `d` focuses the selected item, `Shift+A` pops the focus (back). Other
  bindings may be added beside them, never in their place.
- **A click highlights, it never focuses.** Clicking an item moves the cursor
  to it; only `d` focuses. The exceptions are deliberate shortcuts: a person
  or mention pushes their conversation, and a thumbnail opens the image.
- **Peeks.** Anything that refers to something else is peekable: hovering it
  opens a floating window onto a URL (a locked-down `<webview>`) or an entity
  (its normal focus view). Use `Link` for URLs and `usePeek` for anything else
  (`views/primitives.tsx`). Moving or resizing a peek pins it (with an `×`);
  pinned peeks persist. `Open` sends a URL to the browser or pushes an entity.
- **Every item type has three views**, registered in `views/kinds.tsx`:
  - **full**: the item focused, filling the view (or a peek);
  - **row** ("normal"): the item as a child in another item's view, some detail
    but not all;
  - **pill**: the item in a small space: named in text, heading its own full
    view, or in a peek's bar.
  `itemTypes` in `core/types.ts` lists every type, `Store.put` only takes those,
  and the registry is typed against the list, so `npm run type-check` fails if
  any type lacks any of the three. Adding a type means adding it to
  `itemTypes` and giving it all three views.
- **Pills are one component.** The same pill names an item everywhere: in
  text (`ItemPill`: peek on hover, push on click), heading a full view and in a
  peek's bar (`HeaderPill`: inert, and without the pill's frame). A full view inside a peek drops its own
  header (`headed={false}`) because the bar already names it. Badges are worked
  out by the module (`Badge` in `core/types.ts`).
- No animations. The cursor never becomes a pointer.

### Code

- **State, logic and views are separate, and the app must be drivable with no
  UI.**
  - `src/core/` is plain Node: database, store, modules, integrations. It
    never imports Electron or React. Everything it can do is an entry in
    `Core.actions`, which is the only surface the UI reaches it through.
  - `src/main/` only hosts the core and forwards IPC.
  - `src/renderer/src/state.ts` is latent UI state (the trail, cursors,
    drafts) plus *pure* derivations. Derived values are never written back.
  - `src/renderer/src/session.ts` holds state and runs effects through the
    `Api` seam (`api.ts`); `environment.ts` is the only place touching
    `localStorage`.
  - `src/renderer/src/views/` are dumb: props in, gestures out.
- **Ordering and shaping of children is core's job** (`Module.order`,
  `Module.present`) so a headless caller sees what the UI sees.

## Adding things

- **A module**: a `Module` in `src/core/modules/`, registered in `core.ts`. It
  declares its root entity, refreshes what it caches, and orders children.
- **An item type**: add it to `itemTypes` in `core/types.ts`, then give it a
  full view, a row and a pill in `views/kinds.tsx`.
- **A key or command**: a tool in `tools.ts`. Nothing else.

## Checking work

```bash
npm test              # core, headless, in plain node
npm run type-check
npm run dev           # the real thing (passes --no-sandbox; needed here)
```
