# entity-graph server

A backend for **sharing composable sources**. A *source* is just a set of tools
exposed at a URL (event-sourcing is only the base source's under-the-hood
storage). You define a source, hand out its URL + token, and another person or
LLM can call its tools immediately — with fine control over what they can do.

## Layout

- `../src/core/source/` — the shared, transport-agnostic source layer (types,
  standard tools, and the `Sqlite` / `Combined` / `Frozen` / `Filter` / `Remote`
  sources). Reused by both this server and the Electron app.
- `src/` — the HTTP server: config DB, source registry, endpoints, debug page,
  MCP.
- `src/integrations/` — the server's own reach into GitHub, Slack and Claude.
  Not a source: one registry, one endpoint. See
  [`docs/integrations.md`](./docs/integrations.md).

## Sources

| type       | config                                             | behaviour |
|------------|----------------------------------------------------|-----------|
| `sqlite`   | `{ path, defaultAuthor? }`                         | base event store; tools `readEvents`, `scanEvents`, `writeValue`, `writeLink` |
| `combined` | `{ children: string[] }`                           | union of children's tools (deduped by id); the raw-event reads flat-map all children, writes go to `children[0]` |
| `frozen`   | `{ child, beforeTs }`                              | passthrough, but the raw-event reads drop events at/after `beforeTs` |
| `filter`   | `{ child, allow?, deny?, maxSafety? }`             | narrows the tool registry by id and/or safety (`readonly` = `maxSafety: 'pure'`) |
| `remote`   | `{ url, token? }`                                  | proxies all tool calls to another source's URL |

## Endpoints

Source-scoped (bearer token for that source):
- `GET  /:sourceId/tools` — list tools with JSON Schema + safety
- `POST /:sourceId/call` — `{ tool, args }` → `{ status: 'success', result }` / `{ status: 'error', message }`

These answer cross-origin requests (`Access-Control-Allow-Origin: *`, preflight
included), so a browser client served from somewhere else — the phone app in
[`../mobile`](../mobile) — can call them. The wildcard is safe here because
authentication is a bearer token the client sends explicitly rather than a cookie:
credentials are not allowed, so a hostile page gains nothing it didn't already have.
The admin and integration endpoints below are deliberately **excluded** — with
`ADMIN_TOKEN` unset they are open, and no page the browser happens to be showing
should be handed source creation on a machine it can merely reach.
- `POST /:sourceId/mcp` — stateless MCP (Streamable HTTP)
- `GET  /:sourceId/debug` — interactive per-source console (HTML; prompts for a token)

Two of a source's tools are how anything reads the tree, and they answer different
questions:

- **`scanEvents`** — raw events for a list of ids, plus a couple of layers of
  whatever they link out to. For a client that keeps its own cache and does its own
  traversal, which is what both apps in this repo do.
- **`query`** — the tree as an outline: a depth-first walk from a path, with a limit
  and the path to resume from when it is hit, plus `find` and `sections` to narrow
  what comes back. What the MCP's own `query` is built on, since an agent wants the
  answer rather than the events. The same traversal (`src/core/query.ts`) either way.

### MCP

`POST /:sourceId/mcp` does **not** expose the source's tool list. A store's API —
raw events, resources, undo, whatever tools a user has defined — is the wrong
surface for a model: it asks it to design its own reads, and the ones it invents
are worse than the five it is given (`src/mcp.ts`):

| tool | goes through | what it does |
|------|--------------|--------------|
| `query` | `query` | a slice of the outline as markdown, one line per note, each line starting with that note's entity id, padded so the indentation still reads. Ends with the path to resume from when the limit cut the walk short. Takes `path`, `limit`, `maxDepth`, `sections`, `find` |
| `get_details` | `readEntities` | whole entities as JSON for a list of ids — every value, the children in order, and the `inboundLinks` that say where else a note is referenced |
| `set_value` | `writeValue` | one value on one entity |
| `add_link` / `remove_link` | `writeLink` | put a note under a parent, or take it out — `parentId` / `childId` rather than source / destination, "source" already meaning something else here |

Deliberately absent: creating an entity as its own step (setting a value on a
fresh uuid is what creates one), reordering children, and everything to do with
events, resources and undo.

No collapse map either, and no `direction`: a model gets a slice described by
where it starts and how deep it goes, not an arbitrary shape, and finds what
points at a note through `get_details`. The markdown is the same
`src/core/markdown.ts` both clients export through, with the id column switched
on.

A tool whose underlying source tool is missing is not listed at all, so a
read-only or narrowed source (`filter`) advertises exactly what it will accept.
The client is also handed *instructions* at initialize — what the store is, how
to page, what `text` / `section` / `open` mean, and to write in the voice of the
notes already there.

Integrations, admin-scoped (bearer `ADMIN_TOKEN`) — see
[`docs/integrations.md`](./docs/integrations.md):
- `GET  /tools` — the integration tools, with JSON Schema for their arguments
- `POST /runTool` — `{ tool, args }`; the only way to invoke one

Admin (bearer `ADMIN_TOKEN`):
- `GET  /admin` — source management console (HTML; prompts for the admin token): list / create / edit / delete sources and issue / revoke tokens
- `GET/POST /admin/sources`, `GET/PUT/DELETE /admin/sources/:id`
- `POST/GET /admin/sources/:id/tokens`, `DELETE /admin/tokens/:token`

## Running

```sh
# from the repo root, install workspaces:
npm install

# IMPORTANT — better-sqlite3 ABI:
# the repo root runs electron-rebuild in its postinstall, which builds
# better-sqlite3 for Electron's ABI. The server runs under plain Node, so
# rebuild it for Node before running the server:
npm run --prefix server sqlite:node     # == npm --prefix .. rebuild better-sqlite3

# start the server
ADMIN_TOKEN=secret PORT=4000 CONFIG_DB=./data/config.db npm run --prefix server start
```

To run the Electron app again afterwards, switch `better-sqlite3` back with
`npx electron-rebuild -f -w better-sqlite3`. (This is the "two separate installs"
caveat; long-term the app and server can be split into fully isolated installs.)

To reach the server from another device — a phone running [`../mobile`](../mobile) — the
better answer is to leave it on loopback and put `tailscale serve` in front, which adds
TLS and keeps it off the LAN entirely; see [`../mobile/README.md`](../mobile/README.md).
The desktop app has that as a switch per source, on its Sources page, so this is only a
shell job for a server it didn't start. Failing that, `HOST=0.0.0.0` binds every
interface; the default `127.0.0.1` only answers the machine it runs on.

Binding beyond loopback **requires `ADMIN_TOKEN`**: the server refuses to start with a
non-loopback `HOST` and no token, since the admin endpoints are open when it is unset and
that combination is an unauthenticated remote control for the store, offered to every
device that can reach it. The two settings are each defensible and catastrophic together,
so this fails loudly at startup rather than warning into a log.

Env vars: `PORT` (4000), `HOST` (127.0.0.1), `CONFIG_DB` (./data/config.db),
`ADMIN_TOKEN` (unset ⇒ admin endpoints open, loopback only). All of these, plus the
integrations' secrets, can also live in `server/.env` (gitignored — copy
`.env.example`); anything already in the environment wins.

## Tests

```sh
npm run --prefix server test        # vitest, over server/test
```

Covers round-trip, readonly/filter, frozen, combined routing, Remote passthrough
(incl. Frozen/Combined composing over a Remote), undo, the traversal and its
filters, the markdown an outline comes out as, and the MCP tools via a real MCP
client.
