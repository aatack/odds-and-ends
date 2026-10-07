# Presentation

`src/core/present.ts` and each `modules/<id>/view.ts`. Pure and shared: the
renderer runs them over its cache, `Core.focus` over the stores.

## ModuleView

The pure half of a module. The node half (`Module`) loads, performs and
submits; it never decides what is shown.

| Member | Does |
|-|-|
| `typeOf(id)` | Type from the id's shape, before anything loads |
| `owns(type)` | Which types belong to the module |
| `foreign(id, type)` | Which parts come from a service, and their freshness |
| `newestFirst(type)` | Walk children last-first (chat), so the bound keeps the newest |
| `present(entity, lens)` | Add worked-out fields (names, badges, `from`, `where`) |
| `order(entity, children, lens)` | Order a focus view's children |
| `compose`, `actions` | The composer kind; the actions offered |
| `older(entity)` | Whether "load further back" applies (button and `o`) |

`Lens` gives `read(id)` (an unpresented item) and `children(id)`; reading
through it asks the cache, so presenting something loads what it mentions.

## Items

`toItem` turns a rolled-up entity into `{ id, type, data, createdAt,
updatedAt }`: `type` comes from the `type` value or `typeOf`, and `loaded.*`
keys are dropped from `data`. An entity with values but no type is a **note**
(type `note`): text that means nothing more. One with no values and no type
from its id is not an item yet (null).

`text` is every item's canonical value: each module's `present` sets
`data.text` to what the item is called or says (a message's content, a PR's or
channel's name), preferring an owned `text` where there is one, so `e` edits
the same thing every view shows.

## viewOf: a view as a tree

`viewOf(rootId, source, { folds, limit })` walks a view depth first into
`ViewRow`s, the root first:

1. Read the root; null while nothing is known (loading, or "not found").
2. For each row: its children are walked only while it is **open**: the root
   always, any other by `Folds` (my opens and folds, by entity id) or else its
   type's default (`openByDefault`: notes and tasks open, anything from a
   service shut). Walking into a row calls `source.expand`, so opening a row
   is what loads its `children` part.
3. A row's children are presented and ordered by **its own** module's
   `order`, then walked in that order. An entity already on the path is not
   walked again (the graph can have cycles).
4. The walk stops at `limit` rows (3000). For a root that reads bottom up
   (`newestFirst`: chat) the newest children are kept.

A row carries its path (its identity: an entity can show in several places),
depth, presented entity, its parent here and the sibling above it (for rows
that read on from the one above, like chat), whether it has children and
whether it is open.

`focusOf` is `viewOf` cut to one level, for callers with no tree to draw.

## Ordering by recency

Lists ordered by recency sort on `updatedAt`, the rollup's newest event,
computed in the frontend from the cache. They never sort on a stored "latest"
field or on when something was loaded. This only works because of the
timestamp rules in `events.md`.
