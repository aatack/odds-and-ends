# Context

How related items are collected (`c`), shown, and searched.

## The convention

- An item's `context` value (owned) is the id of a **context**: an item of
  type `context` whose children are the items related to each other (notes, a
  PR, a Claude session, a Slack thread…). Each member is linked under it and
  names it in `context`.
- A context has no text until I give it one (`e`); its pill says "Context".
  It is linked under nothing else, so it is reached through its members.

## `c` (`Core.actions.context`)

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
- Rows and overviews (so peeks too) draw them as pills to the right of the
  item: hover peeks, click goes there. Six at most, then `+N`. Pills don't, so
  they never nest.

## Searching (`findInContext` in `present.ts`)

Given a path (the stack, root first) and a type: from the end of the path up,
each item with a `context` has that context's children (only those) looked
through for one of the type. The first found wins. Null when the path runs
out, or comes back to an item already looked at (a loop).

`k` uses it: when no session is on the selection's path (or named by
`claudeSessionId`), a session sharing a context with something on the path is
prompted.
