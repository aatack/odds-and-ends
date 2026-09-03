# Pensives, and the graph you draw them in

A **pensive** is a store of notes. Not a server, not a file, not a connection —
an interface, in `src/core/pensive/types.ts`:

```ts
interface Pensive {
  readonly id: string
  readonly label: string

  readEvents(entityIds?: string[]): Promise<AppEvent[]>
  writeEvents(events: AppEvent[]): Promise<void>
  popEvents(windowMs: number): Promise<AppEvent[]>

  readResource(id: string): Promise<ResourceRecord | null>
  writeResource(resource: ResourceRecord): Promise<void>

  listTools(): Promise<ToolMeta[]>
  callTool(toolId: string, args: unknown): Promise<unknown>

  refresh?(): Promise<void>
}
```

Five calls are the store: events in and out, bytes in and out. The last two are
its **vocabulary** — `query`, `readEntities`, `createEntity`, `scanEvents`, and
whatever the user has written under `@tools` — which `core/pensive/tools.ts`
builds out of those five. So a pensive gains the whole language of the app by
being one, and everything above it (the outliner, a script, an agent over MCP,
the phone) speaks that language to a file on this laptop and to somebody else's
machine without knowing which it has.

Two things are deliberately *not* in it:

- **No reach outside the store.** No HTTP, no shell. Those are the app's own
  hands ([`integrations.md`](./integrations.md)), held in the main process, so a
  pensive that is published to somebody cannot be a way onto this machine.
- **No transport, no tokens, no server.** How a pensive is stored and how it is
  exposed are separate decisions, made by the user rather than built in.

## The kinds

`refresh()` is implemented by anything that discovers rather than declares —
`BasePensive` re-reads `@tools`, `ConnectPensive` re-reads the remote registry.

```ts
class SqlitePensive extends BasePensive {
  constructor(id: string, label: string, path: string, defaultAuthor?: string)
}

class CombinedPensive extends BasePensive {
  constructor(
    id: string,
    label: string,
    children: Pensive[],
    /** Which child every edit lands in. */
    writeTo: Pensive | null,
    defaultAuthor?: string,
  )
}

class ConnectPensive implements Pensive {
  constructor(id: string, label: string, baseUrl: string, token: string)
}

class AttributedPensive extends BasePensive {
  constructor(inner: Pensive, author: string)
}

class PausedPensive implements Pensive {
  constructor(id: string, label: string)
}
```

- **`SqlitePensive`** is the only one that holds notes: one file, events and
  resources.
- **`CombinedPensive`** reads its children as one store — the union, in child
  order, since a rollup sorts by timestamp anyway — and writes to exactly one of
  them, because there is nowhere for "both" to mean anything. That write source is
  the setting worth changing often: the same outline, with today's notes going
  somewhere else. Undo pops from it alone, which is the only honest answer.
- **`ConnectPensive`** forwards every call over HTTP, tools included, so a tool
  written as a note in the remote store is callable from here with the schema it
  published.
- **`AttributedPensive`** is what a bearer token *means*: every write is recorded
  as the person it was issued to, whatever the client asks for.
- **`PausedPensive`** is what a switched-off node is. Every call fails with a
  sentence naming who is paused, so a combiner one of whose inputs is paused is
  broken exactly that far, and the fix for all of it is to press play.

## The graph

The Sources page is the arrangement, drawn. A node is a pensive or a way of
publishing one; an edge means "read that one". `src/core/client.ts` holds the
shapes both ends agree on:

```ts
type NodeConfig =
  | { kind: 'sqlite'; path: string }
  | { kind: 'combined'; writeTo: string | null }
  | { kind: 'broadcast'; port: number }
  | { kind: 'mcp'; port: number }
  | { kind: 'connect'; url: string; token: string }
  | { kind: 'desktop' }

interface SourceNode {
  id: string
  label: string
  x: number
  y: number
  paused: boolean
  config: NodeConfig
}

interface SourceEdge {
  id: string
  /** The node being read. */
  from: string
  /** The node reading it. */
  to: string
}

interface SourceToken {
  token: string
  nodeId: string
  /** Who writes made with it are attributed to. */
  name: string
  paused: boolean
}
```

| kind | what it is | inputs | output |
| --- | --- | --- | --- |
| `sqlite` | a file on this machine | 0 | yes |
| `combined` | its inputs as one store | many | yes |
| `connect` | a pensive broadcast elsewhere | 0 | yes |
| `broadcast` | its input, over HTTP, to whoever holds a token | 1 | no |
| `mcp` | its input, as an MCP server, for an agent | 1 | no |
| `desktop` | this window; its input is what the outliner shows | 1 | no |

`NODE_KINDS` is that table as data: the page draws its handles from it and the
main process refuses an edge that disagrees with it. A node's inputs are edges
rather than configuration, which is why a combiner has no `children` — only a
`writeTo` naming one of the nodes drawn into it.

The **desktop** node cannot be added or removed and sits at the origin. It is why
there is no source picker anywhere: what the app shows is whatever has been
dragged into it, and changing that edge changes the store under the outliner at
once (`pensive:changed` → `useApp` re-reads → `SourceView` re-lays its seams).

## What holds where

`src/main/pensive/` is the app's side of it:

| | |
| --- | --- |
| `graph.ts` | the drawing, in a SQLite file of the app's own under `userData` (gitignored) |
| `registry.ts` | building a `Pensive` per node, following edges backwards; cached until the graph changes |
| `http.ts` | one small `node:http` server per published node |
| `mcpServer.ts` | what an agent sees: six tools over the same store |
| `servers.ts` | keeping the listeners in step with the drawing |

Three rules hold here rather than in the page, because the page is not the only
caller — a broadcast answers other machines:

- **A loop is refused**, both when the edge is written (`wouldCycle`) and while a
  pensive is built.
- **A paused node yields a `PausedPensive`**, so a broadcast's callers get a 403
  rather than silence.
- **A token is an identity**: `authorOf` resolves it to a name, and the pensive
  handed to that request is wrapped in `AttributedPensive`.

### What a broadcast serves

```
GET  /tools  → ToolMeta[]
POST /call   → { tool, args } → { status: 'success', result } | { status: 'error', message }
POST /mcp    → the MCP endpoint, on an `mcp` node instead of the two above
```

Bearer token required on all of them; 401 without one, 403 when the node is
paused, 503 when there is nothing to serve. **The path in front of the route is
ignored** — one server serves one pensive, so `/tools` and `/anything/tools` are
the same request, which is what keeps a URL the phone client built (it appends a
source id) working.

A port is chosen for a published node when it is added, and kept: a URL worth
copying is a URL that stays put. The server binds every interface, because being
reachable from another machine is the whole point of a broadcast — and the only
way in is a token.

## Coming from the old server

`npm run migrate:sources` reads the configuration the old app left behind —
`config.json` and each server's `config.db` under Electron's `userData` — and
draws the equivalent: one `sqlite` node per old source with its path made
absolute, a `combined` node over all of them writing to whichever source was
open, and that combiner plugged into the desktop node. It refuses to run over a
graph that already has nodes, and deletes nothing.

It needs `better-sqlite3` built for node, which is not how the app wants it:

```sh
npm run rebuild:node
npm run migrate:sources [old-server-databases-dir]
npm run rebuild:electron
```

The argument is where the old *relative* paths resolved against — `server/databases`
in the checkout the app was running from. Those files are worth moving somewhere
that isn't inside a checkout; a `sqlite` node takes an absolute path or one
beginning `~`.
