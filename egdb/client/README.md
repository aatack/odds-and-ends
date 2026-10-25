# egdb-client

A TypeScript client for the egdb HTTP API, and React hooks over it.

- `egdb-client` has the typed client (`EgdbClient`), the wire types, filter and op builders, and the browser cache (`Egdb`). It does not need React.
- `egdb-client/react` has the hooks.

[`PARADIGM.md`](PARADIGM.md) explains how the hooks read a large tree quickly: the cache, the walk that the browser calculates, preloads, optimistic writes and the anchor.

## Install

The package builds on install (`prepare`), and installs its own build tools first when they are missing. From another project:

```sh
# a packed copy (recommended: one copy of react and jotai)
(cd path/to/egdb/client && npm install && npm pack)
npm install path/to/egdb/client/egdb-client-0.1.0.tgz

# or a link to the folder; then dedupe react and jotai in the bundler,
# for example Vite's `resolve.dedupe: ['react', 'react-dom', 'jotai']`
npm install path/to/egdb/client
```

`jotai` (2 or 3) is a peer dependency. `react` (18 or later) is one for `egdb-client/react` only.

The server has no CORS headers, so a browser app must be on the same origin as the server or proxy to it (see `demo/vite.config.ts`).

## Use

```tsx
import { Egdb, EgdbClient, f, op } from 'egdb-client'
import { EgdbProvider, useWalk, useWrite } from 'egdb-client/react'

const client = new EgdbClient({ url: '/api', token })
const egdb = new Egdb({ client, branch, author: 'alex' })

root.render(<EgdbProvider egdb={egdb}><App /></EgdbProvider>)

function Outline() {
  const { rows } = useWalk({
    root: ['root'],
    follow: { list: [['child', 'outbound', 'list'], ['child', 'outbound', 'task']] },
    include: f.containsIgnoreCase('text', 'plan'),
  })
  const { write } = useWrite()
  return rows.map((row) => (
    <div key={row.key} style={{ paddingLeft: row.depth * 16 }}
         onClick={() => write(op.set(row.id, 'done', true))}>
      {row.status === 'loading' ? '…' : String(row.values?.text)}
    </div>
  ))
}
```

## Hooks

| Hook | Arguments | Returns |
| --- | --- | --- |
| `useWalk` | `{ root: Path; follow: Follow; include?: Filter; descend?: Filter; collapsed?: Path[]; limit?: number }`, plus for a virtualized table `{ range?: { start; end }; overscan?; chunk?; buffer?; reanchorAfter?; maxShift?; initialAnchor?: { path; index }; onShift?(shift); lookahead? }` | `{ rows: TreeRow[]; first; rowAt(index): TreeRow \| undefined; count; atEnd; loading; anchor; anchorIndex; reanchor(path, index) }` |
| `useFind` | `{ filter: Filter; order?: FindOrder; include_archived?; offset?; limit?; pageSize? }` | `{ rows: FindRowView[]; total; done; loading; error?; refresh() }` |
| `useWrite` | none | `{ write(ops), set(entity, key, value), link(source, destination, type, sort?), unlink(...), archive(entity), undo(), redo() }`, each a `Promise` |
| `useHistory` | none | `{ undo: number; redo: number }`: how many of this client's batches can be undone and redone |
| `useEntity` | `(id, groups?: LinkPair[])` | `EntityRecord & { status }` |
| `useEntityView` | none | `EntityView`: `get(id)` of any cached entity |
| `useEgdb` | none | the `Egdb` of the nearest `EgdbProvider` |

The main types:

```ts
type Path = readonly string[]

// Which links to follow out of an entity, by its type: each rule follows the
// links of one type and direction to children of one type (null: any type).
type Follow = Record<string, readonly [linkType: string, direction: 'outbound' | 'inbound', childType: string | null][]>

interface TreeRow {
  key: string                   // the path as one string
  path: string[]
  id: string
  depth: number                 // 0 for the root
  values: Values | undefined    // undefined while loading
  status: 'ready' | 'loading'
  childrenLoading: boolean      // expanded, but its links are not loaded yet
  hasChildren: boolean | undefined
  collapsed: boolean
}

interface FindRowView {
  id: string
  values: Values | undefined
  status: 'ready' | 'loading'
}
```

## Without React

`Egdb` is plain TypeScript over a Jotai store (`egdb.store`): read `egdb.viewAtom`, subscribe with `egdb.store.sub`, and call `computeWindow(egdb.view(), request)` and `egdb.preloadWalk(...)` yourself. `EgdbClient` alone is a stateless client for every endpoint.

## Develop

```sh
npm install
npm run typecheck
npm test        # the integration tests need the server binary:
                # (cd .. && cargo build --release --features server --bin egdb-server)
```
