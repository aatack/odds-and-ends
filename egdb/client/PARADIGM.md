# How the client reads a tree

This document tells how `egdb-client` shows a large tree quickly, and why.
The text uses ASD-STE100 Simplified Technical English.

## 1. The idea in short

- The browser keeps a **cache** of all the entities that it has loaded.
- The browser **calculates the rows itself**, from the cache. It does not ask the server for rows.
- When the calculation needs an entity that is not in the cache, the browser shows a **loading row** and continues. It asks the server for a **batch** of entities near that point.
- Writes go **into the cache first**, then to the server. The rows change immediately.
- The rows are counted from an **anchor**: a row near the screen. The browser calculates only the rows near the anchor. It never calculates the full tree.

Thus each change on the screen is a calculation in memory. Only data that is not in the cache causes a request, and one request gets the data for many rows.

## 2. The cache

- The cache is one Jotai atom (`Egdb.cacheAtom`). It holds:
  - the values of each loaded entity,
  - the loaded link groups of each entity (for example "outbound `child` links"),
  - the writes that the server did not confirm yet.
- A read sees the confirmed data with the unconfirmed writes on top (`Egdb.viewAtom`).
- An entity can be partly loaded:
  - Values not loaded: the row shows as `loading`.
  - Values loaded, links not loaded: the row shows, and `childrenLoading` is true. Its children show when the links arrive.
- The cache does not forget entities. A refresh (`Egdb.refresh`) reads all of them again.

## 3. The walk is calculated in the browser

`computeWindow` (in `walk.ts`) uses the same rules as the server's `walk`:

1. The walk is depth-first. Children are in link order.
2. The entity's `type` selects the links to follow (the `follow` map).
3. `descend` and `collapsed` stop the walk below a node. `include` removes a node from the rows, but the walk continues below it.
4. The walk never goes through an entity that is already on the path (no cycles).
5. The walk does not show an entity that has no type (an archived entity).

The browser can step **forward** (to the next row) and **backward** (to the previous row). The backward step is the forward step in reverse: the last descendant of the previous sibling, or else the parent.

Results:

- When you fold a row, the rows change in the same frame. The data is already in the cache, so there is no request.
- When you change the filter, the rows change in the same frame.
- When you write, the rows change in the same frame (section 5).

## 4. Preload in batches

When the calculation finds that it needs data that is not in the cache:

1. It marks the row as loading. It does not stop. It continues as if the row has no children. With an `include` filter, a row whose values are not loaded is not shown: the filter cannot test it, and most such rows go again when they load.
2. It records the path of the first missing row, forward and backward from the anchor (`WalkWindow.missing`).
3. The hook sends one `walk` request to the server from that path. The request has the same `follow`, `descend` and `collapsed` values as the calculation. It asks for about 200 steps (`preloadSteps`), in the same direction, and for the walk's cache (`return_cache`). With an `include` filter it asks for 10 times as many steps, because the browser steps over many nodes for each row that it shows.
4. The server walk loads the nodes that the browser needs next, and about two layers of links below them, in batches.
5. The browser puts the server's cache into its cache. The rows are calculated again and the loading rows fill in.

One request thus fills about the next 200 rows. The calculation also looks 50 rows past each end of the window (`lookahead`), so the preload starts before the user sees a loading row.

The same request is not sent two times. After a write, a preload can be sent again, because the data can be different.

## 5. Writes

1. `write(ops)` adds the ops to the cache as a pending write. All views show it immediately.
2. The browser sends the ops to the server.
3. If the server refuses the write, the pending write is removed. The views go back to the old state. The promise rejects.
4. If the server accepts the write, the browser reads the changed entities again. Then it replaces the pending write with the server's data in one update. Thus the screen does not flicker.
5. A load that started before a write ends is not allowed to replace the data of that write. Each write increments a counter. Each entity keeps the counter of its last write. A load does not change an entity that has a later counter than the load.

**Undo and redo.** The client keeps a list of the batches that it wrote, with the sequence IDs that the server gave them.

1. `undo()` waits for the writes that are not confirmed. Then it asks the server to undo the author's newest batch. Undo is not optimistic: the browser does not know the values that the batch replaced.
2. The server says which sequence IDs it removed. The client finds its batch with those IDs, reads its entities again, and keeps its ops on a redo list.
3. `redo()` sends the newest ops on the redo list again, as a new write. This write is optimistic, like all writes.
4. A new write (not a redo) clears the redo list.
5. If the undone batch is not one that this client wrote (the same author on a different client), the client cannot redo it. It reads all cached entities again.

A write to an entity that the cache does not know makes a new local entity. Its link groups are empty, not "not loaded". This is correct for a new entity. For an old entity that the browser did not load, the server's data replaces it when the write is confirmed.

## 6. The anchor

### 6.1 The problem

A virtualized table shows rows by index: row 0, row 1, and so on. To find row 5000, a simple solution steps 5000 rows from the top of the tree. That is too slow. A browser can calculate some hundred rows in each frame, not many thousands.

The problem also occurs in these conditions:

- The user scrolls far down a long table.
- The user changes the filter while the table is far down. Many rows above the screen are now different.
- The user expands a very large subtree above the screen.

### 6.2 The solution: count rows from an anchor

- The anchor is a **path** (for example `root / projects / launch`), plus the **index** that the table gives to that path.
- The rows are calculated from the anchor:
  - Forward steps give the rows at and below the anchor.
  - Backward steps give the rows above the anchor.
- The browser calculates only the rows near the screen. The cost does not change with the distance from the top of the tree.

When `useWalk` gets a `range`, it moves the anchor with the table:

1. The table gives the range of rows on the screen (`range`).
2. The hook calculates a **window** of rows around the range: the range, plus `overscan` rows on each side, extended to whole **chunks** of 50 rows (`chunk`). While the user scrolls inside the window, the window does not change. The hook does not calculate the rows again. It only gives other rows from the same window.
3. When the screen is more than 100 rows (`reanchorAfter`) from the anchor, the anchor moves to the first row on the screen. The anchor's index becomes the index of that row.
4. Thus the browser calculates rows only when the screen goes into a new chunk, or when the data or the query changes. The rows that it calculates are at most `reanchorAfter` + the window from the anchor.

A change between the anchor and the screen moves the rows on the screen. Example: rows above the screen, but below the anchor, are expanded by another write. A smaller `reanchorAfter` keeps the anchor nearer to the screen and makes this less frequent. With `reanchorAfter: 0`, the anchor is always the first row on the screen, and a change above the screen never moves the rows on it. The cost is a new anchor (and a calculation) when the first row on the screen changes.

Without a `range`, the anchor is the root at row 0, and `useWalk` gives the first `limit` rows: a plain walk.

### 6.3 Changes near the anchor

- **Filter change.** The anchor path stays. If the anchor row is not in the new rows, row 0 is the next row after it that is in the rows. The table thus shows the same part of the tree, filtered. It does not jump to the top.
- **Fold or removal.** If the anchor is below a folded row, or a link on its path is removed, the anchor moves up its path to the first node that is still in the tree.
- **Data that is not loaded.** If the anchor's path is not loaded yet, the rows start at the deepest loaded node. A preload loads the path. Then the anchor goes back to the full path.

### 6.4 The row numbers become wrong, and the hook corrects them

When rows above the anchor change, the anchor's index is not the true number of rows above it. This is acceptable while the user is below. The table only needs the rows near the screen. The hook corrects the numbers when the top of the tree comes near:

- **Rows above row 0.** The backward steps find rows before index 0. Example: a large subtree above was expanded.
- **A gap above the first row.** The backward steps reach the root, but the root's index is more than 0. Example: rows above were folded or filtered out.

In both conditions, the hook changes every index by the same number (`shift`), so that the root is row 0. For rows above row 0, the hook first counts all the rows above the anchor (`countBefore`), so one shift is sufficient. (A shift for each window would cause one more shift for each window above it, one React update each.) It calls `onShift(shift)`. The table must then scroll by `shift` rows in the same frame. The rows thus stay at the same place on the screen.

The table must turn off the browser's own scroll anchoring (`overflow-anchor: none`). The hook does this work. If the browser also does it, the two corrections add together.

### 6.5 The length of the table

- The hook does not know how many rows are below the screen. Until the walk reaches the end, it gives `count` = the last calculated row + `buffer` (200 rows), rounded up to a whole number of `buffer`s. Thus the scroll bar changes only after each 200 rows.
- Until the end is known, `count` does not decrease. A new filter thus does not make the browser move the scroll position.
- The walk reaches the end only when no data is missing. Data that is still loading can add rows.
- When the walk reaches the end, `count` is the true end.

## 7. Use with a virtualizer

This example uses TanStack Virtual (`@tanstack/react-virtual`). Each virtualizer that gives a visible range and a scroll offset works the same way.

```tsx
const ROW = 26
const [range, setRange] = useState({ start: 0, end: 40 })
const virtualizer = useRef<Virtualizer<HTMLDivElement, Element> | null>(null)

const walk = useWalk({
  root: ['root'],
  follow: { list: [['child', 'outbound']] },
  collapsed,
  include,
  range,
  // Rows were found or lost above the anchor: scroll with them.
  onShift: (shift) => {
    const v = virtualizer.current
    if (v) v.scrollToOffset((v.scrollOffset ?? 0) + shift * ROW)
  },
})

virtualizer.current = useVirtualizer({
  count: walk.count,
  getScrollElement: () => scroller.current,
  estimateSize: () => ROW,
  onChange: (v) => v.range && setRange({ start: v.range.startIndex, end: v.range.endIndex + 1 }),
})

// Render virtualizer.getVirtualItems(), with walk.rowAt(item.index) for each item.
// A row that is not calculated yet is undefined: draw a placeholder.
```

`demo/src/App.tsx` is a full example: fold, filter, edit, add, archive, undo and redo.

## 8. Find

`useFind` is simpler, because a find result is a flat list:

- The server gives the IDs, one page at a time, in the requested order. The hook loads pages until it has `offset + limit` rows.
- The values come from the cache. An edit thus shows immediately.
- After a local write, the hook corrects the list. A written entity that does not match now goes out. A written entity that matches now comes in, at its place in the order. If its place is after the last loaded page, a later page gives it.

## 9. Limits

- The browser can step over at most 100 000 nodes in each direction for one window (`maxSteps`). A very narrow `include` filter on a very large tree can stop there. The window then shows fewer rows.
- The browser evaluates filters with the server's rules, but JSON has no integer type. `1` and `1.0` are the same number in the browser and different on the server.
- The cache keeps every loaded entity until the page closes.
