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

## Sources

| type       | config                                             | behaviour |
|------------|----------------------------------------------------|-----------|
| `sqlite`   | `{ path, defaultAuthor? }`                         | base event store; tools `readEvents`, `writeValue`, `writeLink` |
| `combined` | `{ children: string[] }`                           | union of children's tools (deduped by id); reads flat-map all children, writes go to `children[0]` |
| `frozen`   | `{ child, beforeTs }`                              | passthrough, but `readEvents` drops events at/after `beforeTs` |
| `filter`   | `{ child, allow?, deny?, maxSafety? }`             | narrows the tool registry by id and/or safety (`readonly` = `maxSafety: 'pure'`) |
| `remote`   | `{ url, token? }`                                  | proxies all tool calls to another source's URL |

## Endpoints

Source-scoped (bearer token for that source):
- `GET  /:sourceId/tools` — list tools with JSON Schema + safety
- `POST /:sourceId/call` — `{ tool, args }` → `{ status: 'success', result }` / `{ status: 'error', message }`
- `POST /:sourceId/mcp` — stateless MCP (Streamable HTTP)
- `GET  /:sourceId/debug` — interactive per-source console (HTML; prompts for a token)

Admin (bearer `ADMIN_TOKEN`):
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

Env vars: `PORT` (4000), `HOST` (127.0.0.1), `CONFIG_DB` (./data/config.db),
`ADMIN_TOKEN` (unset ⇒ admin endpoints open, dev only).

## Tests

```sh
npm run --prefix server test        # vitest: http.test.ts + integration.test.ts
```

Covers round-trip, readonly/filter, frozen, combined routing, Remote passthrough
(incl. Frozen/Combined composing over a Remote), and the MCP endpoint via a real
MCP client.
