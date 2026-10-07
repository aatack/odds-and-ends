# The frontend cache

`EntityCache` (`src/core/graph/cache.ts`), ported from entity-graph. Pure, so
it also runs in node tests against a `Core`.

## Reading

- `source().get(ids)` answers synchronously with whatever is cached (an empty
  entity if nothing) and asks for anything unread. Asks within a tick become one
  `scan`. Reads during React render are safe: writes happen in microtasks.
- `scan` returns complete events for the ids plus one layer of their outbound
  links (up to 64): views walk downwards, so this saves a round trip.
- A scan's answer replaces an entity's events, except for entities written to
  since the scan went out (`writtenAt` vs `issued`): those go back to `stale`.

## Invalidating

- `invalidate(ids)` (or `null` for everything) marks entries `stale`. They keep
  their events, so nothing on screen empties; whatever reads them next
  re-scans them. Nothing is fetched that nothing reads.
- `apply(events)` puts written events in straight away (a core action's
  `Outcome`).

## Loading from services

- Only entities something has **read** (`asked`) are considered, not ones that
  arrived by overscan.
- `ModuleView.foreign(id, type)` gives `Freshness`: per part, how many ms it
  stays fresh.
  - `self`: the entity's own fields.
  - `children`: what sits under it; considered only after something walks into
    it (`expand`, called by `focusOf`).
- A part loads when its `loaded.<part>` value is missing/0, or older than its
  freshness. `Infinity` means "load once". Writing `loaded.<part> = 0` forces
  a load next time it is looked at.
- `attempted` stops a part being started again within `min(fresh, 60 s)`: the
  load's reply can beat the change notification that carries the new flag.
- After a load settles the cache invalidates that id itself, so the flag is
  re-read even if the notification is late.
- `revisit()` (every 20 s from the session) re-checks **everything read this
  session**, not just what is on screen, against one freshness per part.
- `refresh(id)` forces every part (the `r` key).
- The core double-checks freshness and joins in-flight loads, so two callers
  never fetch twice.

## In the session

- `Session.derive` recomputes the focus and peek foci when the cache or the
  focused id changes, not on cursor moves.
- Presented items are kept identical across derivations when their JSON is
  unchanged (`stable`), and a focus whose children are all identical is reused,
  so rows (memoised) redraw only when they change.
- `item(id)` (for pills) is memoised per cache snapshot.
