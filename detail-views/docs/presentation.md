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
keys are dropped from `data`. An entity with no type is not an item (null).

## focusOf

1. Read the entity; null while nothing is known (loading, or "not found").
2. `expand` it, so its `children` part may load.
3. Walk its children **one level deep, at most `focusLimit` (3000)**,
   newest-first where the view asks for it (`graph/walk.ts`).
4. Present each child, then let the view `order` them.

The bound is on the walk, before `order`. Anything sorted (Slack's workspace)
must fit under it, or the sort only sees part of the list.

Views get `Focus`: entity, children, module, compose, actions, `older`,
loading, error. Nested walks are supported by `walk.ts` but no view renders
deeper than one level yet.

## Ordering by recency

Lists ordered by recency sort on `updatedAt`, the rollup's newest event,
computed in the frontend from the cache. They never sort on a stored "latest"
field or on when something was loaded. This only works because of the
timestamp rules in `events.md`.
