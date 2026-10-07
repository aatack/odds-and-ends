# Events and stores

Ported from `entity-graph` (`src/core/graph/`), reimplemented over
`node:sqlite`.

## The model

- An entity is what its events roll up to (`rollupEntity`). Two kinds:
  - **value**: `entityId`, `key`, `value`.
  - **link**: `sourceId → destinationId`, action add / remove / move.
- Events sort by `timestamp` (stable sort); the later wins. A link event
  belongs to both ends, so it counts towards both entities' `editedAt`.
- `editedAt` (exposed as `updatedAt` on items) is the newest event touching the
  entity. Lists are ordered by it, so **what timestamp an event is written at is
  a design decision**, not bookkeeping.

## Two stores, read as one

| | owned (`detail-views.owned.sqlite`) | cache (`detail-views.cache.sqlite`) |
|-|-|-|
| Holds | What I make: tasks, local approvals, renames, unlinks | What services said |
| Mode | `log`: append only, never deleted by the app | `snapshot`: one row per (entity, key) and per (source, destination), upserted |
| Also | `settings` table (secrets, tokens) | `blobs` (images), `meta` (created time) |
| Cleared | Never | Weekly (`Core.clearCache`), or by deleting the file |

- `Core.read` returns cache events first, then owned, so on an equal timestamp
  mine win.
- A snapshot write that changes nothing reports nothing (the upsert's `WHERE`
  compares value and timestamp), so reloading unchanged data costs no
  re-render.
- `replaceLinksFrom` drops cached links from a source that a full list no
  longer contains. `{ source, within }` narrows it to destinations with a
  prefix, for a source fed by more than one list (Slack's workspace: the
  conversation list replaces only `slack:conv:` links, not thread links).
- Everything in the cache store must be reloadable from its id alone, because it
  can vanish at any time. Cursors and `loaded.*` flags live there for the same
  reason: clearing the cache resets them together with the data they describe.

## Timestamps

- Fetched data that happened at a known time is written at that time: a Slack
  message at its ts (or its edit's), a PR comment and its link from the PR at
  `createdAt`, a thread's `latestReply` at that reply's ts.
- Fetched data with no date of its own is written at **0**: names, kinds,
  reactions, reply counts, PR state and titles, checks, cursors, `loaded.*`.
  It never reorders anything and never overrides anything of mine.
- Owned events are written at `Date.now()`. Overriding fetched data for myself
  (renaming a channel, hiding a PR comment with an unlink) is therefore just an
  owned event: it is later than the fetched one.
- Consequence: an owned edit also bumps the entity's `updatedAt`.

## Ids

- Cached ids are namespaced by service (`slack:conv:C123`,
  `slack:msg:C123:1712.000100`, `github:pr:<url>`); owned ids are uuids.
- An id's shape gives its type (`ModuleView.typeOf`), so an entity seen only as
  a link (a PR URL, a message id) is already an item and can load itself.

## Migrations and the old database

- `db.ts` has separate append-only migration lists per file. Never edit one
  that has shipped. The cache list may append a migration that empties the
  cache when cached data's meaning changes (done once, when conversations
  started loading their newest message).
- `detail-views.sqlite` (the pre-event database) is opened read-only once by
  `legacy.ts` to import owned rows, owned links and settings, then never
  touched. It is kept so the old code can be checked out and run.
