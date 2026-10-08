# MCP server

`src/core/mcp.ts` (the tools and the MCP server, over `Core`), `src/main/mcp.ts`
(the HTTP listener). After entity-graph's `src/main/pensive/mcpServer.ts`.

## What it is for

Every Claude session the app starts (`docs/claude.md`) can read the app (its
modules, any entity, an outline walked down from one, what is related to a
path) and put two items in one context. It is given to each `claude -p` as
`--mcp-config` (the name `detail-views`) with `--allowedTools
mcp__detail-views`: everything but `merge_context` reads, and that only links. A session's own
MCP servers are kept (no `--strict-mcp-config`).

## Tools

| Tool | Takes | Gives |
|-|-|-|
| `modules` | nothing | Each module's root id and name, the id in a column |
| `read` | `ids` | JSON per id: `type`, every value (no `secret.` keys), `createdAt`, `updatedAt`, `children`, `parents` |
| `find_context` | `path` (an id, or ids root first), `type` | The id of the nearest item of that type in a context along the path, or "none" (`findInContext`) |
| `merge_context` | `first`, `second` | Puts them in one context, as `c` does (`Core.shareContext`), written as `claude`; gives the context's id. The only tool that writes |
| `walk` | `path` (an id, or a path to carry on from), `maxDepth?`, `limit?` (200) | An outline: one line per entity, its id in a left column padded to one width, its text as a bullet (`[ ]`/`[x]` a task, `#` a heading); then either "that is everything" or the path to pass back for the next page |

- **Walk** is entity-graph's query: depth first in link order, never into an
  entity already on its path, never above `path[0]`; `maxDepth` counts from
  `path[0]`, so paging keeps the same shape. Raw links, not a view: no
  module's ordering or Slack's read filter. Text is presented as on screen (a
  conversation's title, a message with names for mentions).
- **Loading.** `read` and `walk` load what is foreign and not fresh about the
  entities they reach (`Core.load`, so into the cache store, as the UI would);
  `walk` walks again after, up to three passes, as loading children can add
  rows.
- Instructions and descriptions stay under the 2KB a client keeps of each.

## Context

The server's instructions explain the convention (`docs/context.md`): an
item's `context` names a `context` item whose children are the things related
to it. A session finds what is related to an item with `find_context`, and
collects things it makes or finds with `merge_context`. Its writes are
authored `claude`, so my Ctrl+Z doesn't take them back.

## Transport and security

- Streamable HTTP, stateless (a server and transport per request), at
  `http://127.0.0.1:47822/mcp`. Bound to 127.0.0.1 and not published (the
  phone's port is the one Tailscale publishes).
- A token made fresh each run, in memory only, sent as `Authorization:
  Bearer`. Sessions get it in their `--mcp-config` at each prompt, so a
  restart is no problem.
- The host tells the core where it listens (`Core.setMcp`); a core with none
  (tests, a headless caller, a second copy of the app that can't get the port)
  starts sessions without it.
