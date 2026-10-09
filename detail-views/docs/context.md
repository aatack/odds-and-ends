# Context

How related items are collected (`c`), shown, and searched.

## The convention

- An item's `context` value (owned) is the id of a **context**: an item of
  type `context` whose children are the items related to each other (notes, a
  PR, a Claude session, a Slack thread…). Each member is linked under it and
  names it in `context`.
- A context has no text until I give it one (`e`); its pill says "Context".
  It is linked under nothing else, so it is reached through its members.

## `c` (`Core.actions.context`, over `Core.shareContext`)

Like `r`: press `c` on one row, then on another (in any view); Escape gives up.
The first row is `first`, the second `second`. All writes are owned, by `me`,
so Ctrl+Z takes them back.

| `first` | `second` | Result |
|-|-|-|
| none | none | A new context; both name it and are linked under it |
| none | has one | `first` joins `second`'s |
| has one | none | `second` joins `first`'s |
| has one | has one | `first` joins `second`'s (its `context` changes) and stays under its old one too |

## Shown

- `present` adds `contextPeers` (the context's other children) to any item
  with a `context`.
- Rows draw them as pills straight after the item's text (wrapping with it,
  not pushed to the right); overviews (so peeks too) to the right of the
  overview, level with its first line: hover peeks, click goes there. Six at most, then `+N`. Pills don't, so
  they never nest.

## Searching (`findInContext` in `present.ts`)

Given a path (the stack, root first) and a type: from the end of the path up,
each item with a `context` has that context's children (only those) looked
through for one of the type (or, with no type, for any item but the one on
the path). The first found wins. Null when the path runs
out, or comes back to an item already looked at (a loop).

The MCP server offers it as `find_context`, and `c` as `merge_context`
(written as `claude`), so a Claude session can find and collect related items
too (`docs/mcp.md`).

## The side panel (Ctrl+Enter)

Ctrl+Enter looks up the path to the selected row (the stack below the view,
then the view's rows to the selection) with no type, and opens what it finds
in a panel on the right: mostly the Claude session attached to something.
With nothing found, a toast says so.

- The panel is a window, as a peek is (`docs/architecture.md`): pressing in
  it gives it the keys, with its own cursor. A chat's prompt box works in
  it: its own draft (by the item), its own Enter and Escape
  (`data-field="window"`). Opening it gives its box the keyboard; Escape
  there hands the keys back to the main view.
- Ctrl+Enter in its prompt box closes it again.
- `k` on a row puts a reference to it (`[](entity:<id>)`) in the next prompt of the session in the panel, opening the nearest session in context there first if the panel shows none, and focuses its box (`docs/claude.md`).
- Its left edge drags to resize it (`sideWidth`, persisted); it never goes
  below 320px, nor leaves the rest of the window less than 400px.
- When it opens, the main view's selected row is brought to the middle of
  the screen, again each time the layout moves, until it has settled for a
  quarter of a second (`centre`, a request like `findFocus`, not state).
- It stays across navigation, and across reloads (`side` is persisted).
- Its bar has the item's pill, Open (pushes it) and × (closes it). On a phone
  it covers the view.

Every view is given `onClose`, which closes the window it is in, however that
window closes: the main view pops the stack, a peek closes, the side panel
closes. The ×s of peeks and the side panel call it.
