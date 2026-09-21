# conswap — how this is put together

A local tool for swapping context: one topic per piece of work, a blocker for
everything it is waiting on, and a key that goes to the next thing that needs me.
Electron + React + TypeScript + SQLite, split into three packages so a phone
client can be added without moving any logic.

Read [`README.md`](./README.md) first for what it does. This is how to work on it.

## Layers, and the line between them

The layering matters more than anything else here: **state, logic and rendering
are separate, and the app must be drivable with no DOM at all.**

- **`packages/backend`** owns every rule. It never knows a UI exists. Everything
  it can do is an entry in `src/actions.ts`, reached through one `POST /actions`.
- **`packages/common/src/session.ts`** is the app without a screen: latent state,
  the cache, and every effect. It can be driven from node.
- **`packages/common/src/environment.ts`** is the seam for the world outside:
  where a per-device preference is kept, and what the machine wants to look like.
  Nothing else in `common` may touch `localStorage` or `matchMedia`.
- **`packages/common/src/state.ts`** holds the latent state and the *pure*
  derivations of it. Latent means the minimum — focus, the trail, the expanded
  set, the cursor, drafts. Anything derivable is a function here, and a
  derivation is never written back into latent state.
- **`packages/common/src/tools.ts`** is the single registry of everything a
  person can invoke. Hotkeys, the command palette and the help sheet all read it,
  so they cannot drift apart.
- **`packages/common/src/dispatch.ts`** is the only key listener. It walks the
  focus chain — overlay, then composer, then feed, then app — and takes the first
  enabled tool whose binding matches. While something is being typed into, only
  that scope and bindings held with a modifier get a look in.
- **`packages/common/src/hooks.ts`** is the one hook. It turns the session into
  plain data and callbacks.
- **`packages/common/src/views/`** are dumb. They take props, they draw, they
  forward gestures. No component below `App` decides anything.

**Never** scatter `keydown` listeners, put logic in a view, or reach for the
network outside `client.ts`.

## Adding things

- **A new action**: a row in `backend/src/actions.ts`, then a method on `Session`,
  then a tool in `tools.ts` if a person should be able to invoke it.
- **A new blocker**: a `BlockerDefinition` in `backend/src/blockers/definitions.ts`
  and a line in the `definitions` array. `wakesOn` clears it the moment matching
  activity arrives; `check` is polled. Teach `suggestions.ts` when to offer it.
- **A new integration**: a class implementing `Integration` under
  `backend/src/integrations/`, registered in `index.ts`. Events enter through
  `addEvent`, never by writing to the tables.

## Design

Clean and quiet, in the Linear and Apple direction.

- **Nothing animates.** Every transition is disabled in `styles.css` on purpose: a
  key press should land before the eye moves.
- **Prefer tone to borders.** Panels sit on the canvas by being a shade lighter.
  Where a divider is genuinely needed it is a hairline in `--line`, never darker.
- **Status is a dot and a word**, never a filled or outlined badge.
- **The cursor never becomes a pointer.**
- **One radius scale**: `md` for rows and controls, `lg` for cards and the
  composer, `xl` for the overlay.
- **Colour carries meaning or is not used.** One indigo accent, one colour per
  source for the rule beside an event, desaturated status hues.
- Tokens are in `packages/common/src/styles.css`, written once: every token
  carries both values through `light-dark()`, and `color-scheme` picks the side.
  `system` leaves the root alone; the toggle pins it with `data-theme`.

## Data

One SQLite file. `topics` holds everything, including notes and messages;
`links` says what sits under what; `blockers` says what a topic is waiting for;
`runs` tracks Claude; `mutations` keeps every write the frontend ever asked for,
forever, so a failed optimistic update can be replayed rather than lost; `kv` is
integration bookkeeping.

Migrations are append-only in `backend/src/database.ts`. Never edit one that has
shipped.

## Checking work

```bash
npm run type-check     # both projects
npm run dev            # the real thing, one window
```

There is no test suite yet. The backend is a plain module over a database that
can be opened at `:memory:`, so tests belong there first.
