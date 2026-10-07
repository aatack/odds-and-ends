# detail-views

A local desktop app for managing my work. It will grow over time; these are the
constraints every change must keep.

How it is built, and why, is in `docs/` (start at `docs/README.md`). Read the
doc for the area you change, and keep it up to date in the same commit.

## Invariants

### Product

- **Modules are workflows.** Each one (Slack, tasks, later calendar, …) is a
  module with a root entity. The sidebar lists the modules and nothing else; it
  is how I enter the app.
- **One view at a time, and every view is a tree.** Everything is an entity:
  a Slack conversation, a message, a PR, a note. The bulk of the window is a
  view rooted at one of them: its overview, then what is under it as a tree I
  navigate, open and fold.
- **Modules are just roots.** Slack, GitHub and Tasks are root entities whose
  types load in special ways and draw in special ways; the sidebar enters
  them, and nothing else about them is special.
- **Links are directional, parent → child.** Focusing an entity shows its
  children. Any entity may link to any other, across modules.
- **Navigation is a stack of views.** `d` pushes a view rooted at the
  selected row; `Shift+A` pops it. Entering a module (sidebar or `1`–`9`)
  starts the stack again at its root.
- **Any item can be worked on the same way**, whatever its type: Enter adds a
  note under it (an owned entity with no type, just text), `e` edits its text,
  Backspace takes it out from under its parent *in this view* (nothing at the
  view's root), `x` moves it, `r` / `Shift+R` link it to or from another row.
  `x`, `r` and `Shift+R` are finished by pressing the same key on the other
  row, in any view; Escape gives up.
  Ctrl+Z / Ctrl+Y undo and redo my own edits (the owned log, five minutes
  back); Ctrl+F filters a view.
- **`text` is an item's canonical value**: what it is called or says wherever
  it is shown (a message's content, a PR's or channel's name). It may come
  from a service; editing it writes an owned `text` that overrides it for me
  only.
- **Desktop app, not a web page.** Electron, run locally.

### Data

- **Everything is events.** An entity is what its value and link events roll
  up to (`core/graph/`, ported from entity-graph): sorted by timestamp, so the
  later event wins.
- **Two SQLite files** (`node:sqlite`, no native modules), read as one:
  - **owned** (`detail-views.owned.sqlite`): what I make. An append-only event
    log the app never deletes from, plus the `settings` table (secrets and
    settings stay local).
  - **cache** (`detail-views.cache.sqlite`): what was loaded from other
    services. One event per value and per link, replaced by the next load; it
    is emptied weekly (`Core.clearCache`) and may be deleted any time, since
    everything in it loads again on demand.
  - The file before these (`detail-views.sqlite`) is read once, read-only, to
    import what I owned (`legacy.ts`). Never write to it.
- **Fetched events carry the time they happened elsewhere**: a Slack message at
  its ts (or its edit's), a PR comment and its link from the PR at its
  `createdAt`. What has no date of its own (channel names, reactions, PR
  titles and state, checks) is written at **timestamp 0**. So an
  owned event written now overrides fetched data: renaming a channel or
  unlinking a PR comment for me only is an ordinary owned event.
- **Loads are marked on the entity.** A load writes `loaded.<part>` (Unix ms)
  into the cache with what it fetched. A part is `self` (the entity's own
  fields) or `children` (what sits under it, only loaded once something walks
  into it). `ModuleView.foreign` says which parts come from elsewhere and how
  long each stays fresh; nothing fresh is loaded again unless I refresh.
- **Cached ids are namespaced by service** (`slack:conv:C123`), so they cannot
  collide with owned ids (uuids) or with each other, and an id's shape alone
  gives its type (`ModuleView.typeOf`): a PR seen only as a link is that item
  and loads itself.
- **Slack keeps no unread counts.** The workspace lists conversations (channels,
  DMs, group DMs) and every thread in them, ordered by each one's most recent
  event (`updatedAt`): a conversation's newest message, a thread's newest
  reply. Never by when it was loaded.
- **Slack loads lists, one batch of history, then watches.** On its own the
  app loads only the workspace's lists (`users.conversations`,
  `conversations.list`, `users.list`), a first batch of 1000 messages across
  every conversation by search, and then `Slack.poll` (one `search.messages`
  call every 15 s, started by `Core.start`, catching up on start from
  `watch.at`). Anything older, for all of Slack, one conversation or one
  thread, loads only when I ask (`o`, `Module.older`). Never poll or load
  conversations one by one on their own.
- **Slack is read-only during development.** `slackWrites` in
  `modules/slack/api.ts` gates an allowlist of read methods; do not turn it on
  or widen the list unless I ask.
- **GitHub goes through `gh`.** Reads are `gh api graphql` queries, never a
  mutation. The only writes are the PR actions I asked for, allowlisted in
  `checkWrite` (`modules/github/github.ts`): approve someone else's PR; on my
  own, approve locally (an owned entity) and turn on auto-merge; close mine and
  delete the branch. Each is started by me and confirmed with Enter. `gh` runs
  from the temp dir so `--delete-branch` never touches a local checkout.
- **Actions are generic.** A module offers `actions(entity)` (in its view) and
  does them in `perform`; the UI shows each as a button and a key (`tools.ts`),
  opens a prompt, and only Enter confirms. A PR is an entity keyed by its URL
  (`prEntityId` in `core/types.ts`), so a link to one anywhere is that item.
- Migrations are append-only, per file, in `src/core/db.ts`. Never edit a
  shipped one.

### Rendering

- **Data is only ever rendered from the frontend cache** (`EntityCache`,
  `core/graph/cache.ts`). Reading an entity from it answers at once with
  whatever it has and is what fetches the rest; nothing on screen waits on a
  request or asks the core what to show.
- **Anything from elsewhere is loaded into the cache store, never straight to
  the screen.** When an entity arrives in the frontend cache and a part of it
  is foreign and not fresh, the cache asks the core to load it
  (`Core.actions.load`) asynchronously; the core writes it to the cache store,
  says which ids changed, and the frontend cache re-reads them, so the view
  updates on its own. Until then the view shows what the cache has.
- **Views walk the graph, bounded** (`core/graph/walk.ts`): a focus view walks
  at most `focusLimit` children, so nothing renders unbounded data.
- **Writes say what they wrote.** A core action that writes returns the owned
  events (`Outcome`), which go into the frontend cache at once; the change
  notification then confirms them.

### UI

- **Minimal.** No descriptive headers, no labels that restate what is obvious,
  no information shown twice. If a row already says it, nothing else does.
- **Keyboard first.** Every key press goes through one listener
  (`renderer/src/dispatch.ts`) that runs a tool from the single registry
  (`renderer/src/tools.ts`). Never add a `keydown` listener anywhere else.
- **Navigation keys are a fixed rule** in every view: `w` up, `s` down, `d`
  pushes the selected row's view, `Shift+A` pops it; ArrowRight / ArrowLeft
  open and fold a row. Other bindings may be added beside them, never in their
  place.
- **A click selects, it never pushes.** Clicking a row moves the cursor to it;
  only `d` pushes. The exceptions are deliberate shortcuts: a person or mention
  pushes their conversation, and a thumbnail opens the image.
- **Peeks.** Anything that refers to something else is peekable: hovering it
  opens a floating window onto a URL (a locked-down `<webview>`) or an entity
  (its normal view, without a cursor). Use `Link` for URLs and `usePeek` for anything else
  (`views/primitives.tsx`). Moving or resizing a peek pins it (with an `×`);
  pinned peeks persist. `Open` sends a URL to the browser or pushes an entity.
- **Every item type has its views**, registered in `views/kinds.tsx`:
  - **pill**: named inline, heading a view, or in a peek's bar;
  - **overview**: at the root of a view, above its tree (more than a row);
  - **row**: a child in a view's tree, some detail but not all (the tree draws
    the indent, fold mark and selection around it);
  - **detail** (optional): the whole view, in place of overview and tree.
  `itemTypes` in `core/types.ts` lists every type and the registry is typed
  against it, so `npm run type-check` fails if any type lacks a pill, overview
  or row. Adding a type means adding it to `itemTypes` and giving it those.
- **Pills are one component.** The same pill names an item everywhere: in
  text (`ItemPill`: peek on hover, push on click), heading a view and in a
  peek's bar (`HeaderPill`: inert, and without the pill's frame). Badges are
  worked out by the module (`Badge` in `core/types.ts`).
- **Every renderer takes `findText`** (rows, pills, overviews) and draws its
  text through `Highlight` (or markdown's highlight plugin), so a find shows
  where it matched.
- **Every row starts at its left edge**, whatever its type, so a tree's
  indentation reads. Anything secondary (a message's time, a check's
  duration, a count) goes on the right, and may show only on hover.
- No animations. The cursor never becomes a pointer.
- **Anything interactable shows it on hover** (rows, pills, links, sidebar
  entries, thumbnails, buttons), since the cursor never changes. Every button
  is `Button` (`views/primitives.tsx`): a soft gray fill, darker on hover and
  pressed, no border (after entity-graph). A button whose action has a key
  shows it first, in a key cap, read from the tool registry (`hotkeyOf`).
- **Muted colours**: gray first, one quiet indigo accent, desaturated status
  hues, all as tokens on `:root` in `styles.css`. No colour is written
  anywhere else.
- **A slow action says it is under way**: its button shows `busy` (a
  `…` label, pressed in, not pressable) from `Session.working` until it
  settles.
- **What can't be done now is shown disabled with the reason as its tooltip**
  (`Action.disabled`), never just offered and refused. The core refuses it
  too, so a keypress or a headless caller can't get round it.

### Code

- **State, logic and views are separate, and the app must be drivable with no
  UI.**
  - `src/core/` is plain TypeScript with no Electron or React. Its node half
    (stores, loaders, writes) is reached only through `Core.actions`.
  - Its pure half is shared with the renderer and must not import node:
    `graph/` (events, rollup, walk, the entity cache), `present.ts`
    (`viewOf`, `focusOf`, `itemOf`) and each module's `view.ts`. The renderer
    runs it over its cache; `Core.actions.view` runs the same code over the
    stores, so a headless caller sees what the UI sees. `npm test` drives an
    `EntityCache`, and the renderer's own `Session`, against a `Core` in plain
    node.
  - `src/main/` only hosts the core and forwards IPC.
  - `src/renderer/src/state.ts` is latent UI state plus *pure* derivations.
    Latent means the minimum and serialisable: the stack, each view's
    selection *path*, folds, the in-place edit and its draft. Anything
    derivable (the rows, the resolved selection) is a function of it and the
    cache, and **derived values are never written back**: the selection is
    resolved against the rows each time (`resolveSelection`), so a selection
    inside a folded row comes back when it opens.
  - A row is identified by its **path** from the view's root, never by its
    id: the graph isn't a tree, and an entity can show in several places.
  - A view's tree is **walked once per change of shape** (root, folds, cache)
    and remembered (`Session.walk`); the selection and the edit are laid over
    it by `markRows`. Moving the cursor never walks the tree again, and rows
    that didn't change keep their identity so they don't redraw.
  - Reads during render write nothing: asking the cache is a set membership
    and a microtask.
  - `src/renderer/src/session.ts` holds state and the entity cache, and runs
    effects through the `Api` seam (`api.ts`); `environment.ts` is the only
    place touching `localStorage`.
  - `src/renderer/src/views/` are dumb: props in, gestures out.
- **Ordering and shaping is the module view's job** (`ModuleView.order`,
  `ModuleView.present`), pure and shared, never the renderer's.

## Adding things

- **A module**: a `ModuleView` in `src/core/modules/<id>/view.ts` (types by id,
  what is foreign and how fresh, present, order, actions), listed in
  `moduleViews` in `present.ts`; and a `Module` beside it that loads into the
  cache store and performs actions, registered in `core.ts`.
- **An item type**: add it to `itemTypes` in `core/types.ts`, then give it a
  full view, a row and a pill in `views/kinds.tsx`.
- **A key or command**: a tool in `tools.ts`. Nothing else.

## Checking work

```bash
npm test              # core, headless, in plain node
npm run type-check
npm run dev           # the real thing (passes --no-sandbox; needed here)
```
