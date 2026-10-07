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

## Large views (after entity-graph)

- **The walk has a budget.** A view walks at most `pageSize` (200) rows at
  first. Scrolling near its end, or rows that don't fill the screen (a narrow
  find), call `Session.loadMore`, which **doubles** it: the limit is on the
  walk, not on the rows a find keeps, so growing by a page would re-walk once
  per page before a find turned up anything. A budget belongs to its query
  (root, folds, find): a different query starts at a page, and coming back to
  one restores what it had. Chat reads bottom up, so its walk keeps the newest
  messages and grows upwards.
- **Only rows near the viewport are mounted** (`TreeList` in
  `views/View.tsx`). Each row measures itself; unmeasured rows are guessed;
  offsets are worked out from the keys, which stay the same array while the
  tree's shape does (`ShownView.keys`), so a cursor move lays out nothing.
  - The selected row is kept on screen, 30% in from the edge, and followed as
    rows around it are measured until the list is scrolled by hand.
  - The row being typed into is pinned (mounted wherever it is), so the caret
    survives scrolling away.
  - When rows above change (chat growing upwards, guesses becoming heights),
    the first visible row stays where it was.
- Together with the walk memo and `markRows` keeping untouched rows, a cursor
  move in a view of thousands of rows re-renders two rows and walks nothing.
