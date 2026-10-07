# Architecture

## Layers

```
src/core/            plain TypeScript, no Electron, no React
  graph/             events, rollup, bounded walk, the entity cache   ┐ pure, shared
  present.ts         ModuleView registry, focusOf, itemOf            │ with the
  modules/*/view.ts  each module's pure half                         ┘ renderer
  core.ts, store.ts, db.ts, legacy.ts, modules/*/<module>.ts         node only
src/main/            hosts Core, forwards IPC, nothing else
src/renderer/src/
  session.ts         latent state + the EntityCache + effects through Api
  state.ts           latent UI state and pure derivations
  tools.ts/dispatch  the one key listener and the tool registry
  views/             dumb components
```

- The pure half of `core` is imported by the renderer (see `tsconfig.web.json`'s
  `include`, which is also the list of what must stay free of node). Anything
  added there must not import `node:*`, Electron or React.
- The node half is reached only through `Core.actions` over IPC
  (`renderer/src/api.ts` is the seam). `Core.focus` runs the same pure
  `focusOf` over the stores, so a headless caller sees what the UI sees; `npm
  test` drives an `EntityCache` against a `Core` in plain node.

## From a service to the screen

1. A view needs an entity. `Session.derive` calls `viewOf(id, cache.source(),
   { folds })` (remembered per view until its root, the folds or the cache
   change), which reads through the `EntityCache`. Reading is asking: anything missing is
   scanned (`Core.actions.scan`) in a microtask batch.
2. When an asked-for entity arrives, its module's `ModuleView.foreign` says
   which parts of it come from a service and how long they stay fresh. A part
   never loaded or gone stale is loaded (`Core.actions.load`).
3. The core's `Module.load` fetches and writes events into the **cache store**,
   and marks the part `loaded.<part>` on the entity.
4. Every store write reports the ids it changed. The core throttles these
   (150 ms) and sends them to the renderer, which invalidates those ids.
5. Invalidated entries keep their events (`stale`) and are re-scanned when read
   again, so the view updates without ever emptying.

Watches (the Slack poll) write into the cache store on a timer and go through
steps 3–5 the same way. Writes I make (tasks, approvals) go to the **owned
store**; the core action returns the events (`Outcome`) and the session applies
them to the cache at once, before the change notification confirms them.

**The rule:** data is only ever rendered from the frontend cache. Nothing is
fetched straight to the screen, and no view asks the core what to show.

## Runtime state that is not latent

`Session` also holds things that are neither latent state nor cached data:
`working` (calls under way per entity, for button loading states) and `now` (a
one-second clock for "how long ago" text). Both live in the snapshot and are
never persisted. Views do not run their own timers.

## The renderer, after entity-graph

- **Latent state** (`state.ts`): the stack of view roots, each view's
  selection path, folds, the in-place edit (persisted with its draft), a
  pending move/link (`picking`, not persisted), peeks.
- **Derived** (pure, never written back): the view (`viewOf`), the rows with
  the selection and edit laid over them (`markRows`), the selection in effect
  (`resolveSelection`).
- **Tools** (`tools.ts`): every key is one, scoped `input` → `list` → `app`;
  the first enabled tool bound to a key wins, which is how Escape means
  "stop typing", "give up the move" or "close the peek" by what is going on.
- **Views** (`views/View.tsx`): one tree view for everything. It draws the
  root's `Overview`, then each row's indent and fold mark around its type's
  `Row`, or the type's `Detail` instead of both.
