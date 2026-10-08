import type { IncomingMessage, ServerResponse } from 'node:http'
import { Server } from '@modelcontextprotocol/sdk/server/index.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js'
import type { Core } from './core.ts'
import type { GraphEntity } from './graph/entity.ts'
import { lensOf, moduleInfos, present, toItem } from './present.ts'
import type { LoadPart } from './types.ts'

// What a Claude session sees of the app (after entity-graph's
// `src/main/pensive/mcpServer.ts`): read-only, three tools, each a whole job.
// The instructions and each description are cut at 2KB by a client, so the
// instructions route and a tool's mechanics live on the tool.

/** Entities one page of `walk` visits, unless the caller says otherwise. */
const walkLimit = 200

/** The MCP server's address, as `claude --mcp-config` wants it. */
export interface McpAddress {
  url: string
  token: string
}

/** The name the server goes by: its tools are `mcp__detail-views__<tool>`. */
export const mcpName = 'detail-views'

/** The `--mcp-config` a Claude session is started with: this server, and nothing taken away. */
export function mcpConfig(address: McpAddress): string {
  return JSON.stringify({
    mcpServers: { [mcpName]: { type: 'http', url: address.url, headers: { Authorization: `Bearer ${address.token}` } } },
  })
}

export const instructions = `The user's work, as a graph of entities: Slack conversations and messages,
GitHub PRs, tasks, notes, Claude sessions. Every entity has an id; a link from one to
another means "this sits under that one", and an entity can sit under several, so it is
a graph rather than a tree. An id's shape often gives its type (\`slack:conv:C123\`,
\`slack:msg:C123:1712345678.000100\`, a PR's URL); what the user made has a uuid.

Each module (Slack, GitHub, Tasks, Claude, Recents) has a root entity: \`modules\` lists
them, and they are where to start. \`walk\` reads down from an id as an outline, one line
per entity with its id in a column on the left; \`read\` gives everything about entities
by id. Nothing here writes.`

export interface McpTool {
  name: string
  description: string
  inputSchema: { type: 'object'; properties: Record<string, unknown>; required?: string[] }
  run(core: Core, args: Record<string, unknown>): Promise<string>
}

/** Loads what is foreign and not fresh about these entities, as the UI would on looking at them. */
async function loadAll(core: Core, ids: readonly string[]): Promise<void> {
  const parts: LoadPart[] = ['self', 'children']
  await Promise.all(ids.flatMap((id) => parts.map((part) => core.load({ id, part }))))
}

/** An entity's text as it is shown (its name, or what it says), and its checkbox or heading mark. */
function textOf(core: Core, entity: GraphEntity): { mark: string; text: string } {
  const item = toItem(entity)
  if (!item) return { mark: '', text: '(nothing known)' }
  const shown = present(item, lensOf(core.source())).data
  const text = typeof shown.text === 'string' && shown.text ? shown.text : typeof shown.title === 'string' ? shown.title : ''
  const box = item.type === 'task' ? (shown.done ? '[x] ' : '[ ] ') : typeof shown.open === 'boolean' ? (shown.open ? '[ ] ' : '[x] ') : ''
  const heading = shown.section === true ? '# ' : ''
  return { mark: `${box}${heading}`, text: text || `(${item.type})` }
}

/** One row of a walk: the path to it from where the walk started. */
type Path = string[]

/**
 * A depth-first walk from `start`, in link order, never into an entity
 * already on its path. The start is the first row. Stops after `limit` rows,
 * and says where to carry on: the walk never climbs above `start[0]`, so a
 * path handed back reads on from exactly there.
 */
export function walk(core: Core, start: Path, limit: number, maxDepth: number | null): { paths: Path[]; next: Path | null } {
  const source = core.source()
  const children = (path: Path): string[] => {
    if (maxDepth !== null && path.length - 1 >= maxDepth) return []
    const id = path[path.length - 1]
    return source.get([id])[id].outboundLinks.filter((child) => !path.includes(child))
  }
  const step = (path: Path): Path | null => {
    const below = children(path)
    if (below.length) return [...path, below[0]]
    for (let i = path.length - 1; i > 0; i--) {
      const siblings = children(path.slice(0, i))
      const at = siblings.indexOf(path[i])
      if (at >= 0 && at + 1 < siblings.length) return [...path.slice(0, i), siblings[at + 1]]
    }
    return null
  }
  const paths: Path[] = []
  let path: Path | null = start
  while (path && paths.length < limit) {
    paths.push(path)
    path = step(path)
  }
  return { paths, next: path }
}

/**
 * Rows as an outline with the ids in a column on the left, padded to one
 * width so the indentation lines up. A row's later lines get a blank column,
 * so every line starting with an id is a row.
 */
export function outline(rows: { id: string; depth: number; mark?: string; text: string }[]): string {
  const width = Math.max(0, ...rows.map((row) => row.id.length))
  return rows
    .flatMap((row) => {
      const opener = `${row.depth === 0 ? '' : `${'    '.repeat(row.depth - 1)}- `}${row.mark ?? ''}`
      const [first, ...rest] = row.text.split('\n')
      return [`${row.id.padEnd(width)}  ${opener}${first}`, ...rest.map((line) => `${' '.repeat(width + 2 + opener.length)}${line}`)]
    })
    .join('\n')
}

const pathOf = (value: unknown): Path | null => {
  if (typeof value === 'string' && value) return [value]
  if (Array.isArray(value) && value.length && value.every((id) => typeof id === 'string' && id)) return value as Path
  return null
}

export const mcpTools: McpTool[] = [
  {
    name: 'modules',
    description:
      "The app's modules, each with its root entity's id: the places to start a `walk`. " +
      'One line per module, the id in the left column.',
    inputSchema: { type: 'object', properties: {} },
    run: async () => outline(moduleInfos.map((module) => ({ id: module.root, depth: 0, text: module.name }))),
  },
  {
    name: 'read',
    description:
      'Everything known about each of these entities, as JSON keyed by id: its `type`, ' +
      'all of its values (`text` is what it is called or says), when it was created and ' +
      'last changed (Unix ms), its `children` in order and its `parents` (every entity ' +
      'linking to it, which is how to find where else it appears). Anything from another ' +
      'service that is not loaded, or no longer fresh, is loaded first.',
    inputSchema: {
      type: 'object',
      properties: {
        ids: { type: 'array', items: { type: 'string' }, description: 'The ids to read. Ask for every one you need in one call.' },
      },
      required: ['ids'],
    },
    run: async (core, args) => {
      const ids = Array.isArray(args.ids) ? args.ids.filter((id): id is string => typeof id === 'string') : []
      await loadAll(core, ids)
      const out: Record<string, unknown> = {}
      for (const id of ids) {
        const entity = core.entity(id)
        const values = Object.fromEntries(Object.entries(entity.values).filter(([key]) => !key.startsWith('secret.')))
        out[id] = {
          type: toItem(entity)?.type ?? null,
          values,
          createdAt: entity.createdAt,
          updatedAt: entity.editedAt,
          children: entity.outboundLinks,
          parents: entity.inboundLinks,
        }
      }
      return JSON.stringify(out, null, 2)
    },
  },
  {
    name: 'walk',
    description:
      'Read down from `path` as an outline: a depth-first walk in link order, one line ' +
      "per entity with its id in the left column (padded, so the indentation lines up) and " +
      'its text as a bullet. `[ ]` / `[x]` is an unticked / ticked task, `#` a heading. An ' +
      'entity is never walked into below itself, though it may appear in several places.\n\n' +
      `The walk visits at most \`limit\` entities (${walkLimit} by default). When more ` +
      'remains, the answer ends with the path to carry on from: pass it straight back as ' +
      '`path`, with the other arguments unchanged, for the next page. Anything from another ' +
      'service on the page that is not loaded, or no longer fresh, is loaded first.',
    inputSchema: {
      type: 'object',
      properties: {
        path: {
          anyOf: [{ type: 'string' }, { type: 'array', items: { type: 'string' } }],
          description: 'Where to start: an entity id, or the path a previous call gave back to carry on from.',
        },
        maxDepth: {
          type: 'number',
          description:
            'Levels to read below the first id in `path`; 1 reads it and its children. Omit ' +
            'for no limit. Measured from there, so paging keeps the same shape.',
        },
        limit: { type: 'number', description: `Most entities to visit before stopping. Defaults to ${walkLimit}.` },
      },
      required: ['path'],
    },
    run: async (core, args) => {
      const start = pathOf(args.path)
      if (!start) throw new Error('`path` must be an id or a list of ids')
      const limit = typeof args.limit === 'number' && args.limit > 0 ? Math.floor(args.limit) : walkLimit
      const maxDepth = typeof args.maxDepth === 'number' && args.maxDepth >= 0 ? Math.floor(args.maxDepth) : null
      // Load what the page reaches, then walk again: loading a row's children
      // can show more rows, so a few passes settle it.
      let page = walk(core, start, limit, maxDepth)
      const tried = new Set<string>()
      for (let pass = 0; pass < 3; pass++) {
        const fresh = [...new Set(page.paths.map((path) => path[path.length - 1]))].filter((id) => !tried.has(id))
        if (!fresh.length) break
        for (const id of fresh) tried.add(id)
        await loadAll(core, fresh)
        page = walk(core, start, limit, maxDepth)
      }
      const rows = page.paths.map((path) => {
        const id = path[path.length - 1]
        return { id, depth: path.length - 1, ...textOf(core, core.entity(id)) }
      })
      const tally = page.next
        ? `[${rows.length} shown; there is more. To carry on, call walk again with path: ${JSON.stringify(page.next)}]`
        : `[${rows.length} shown; that is everything under this path.]`
      return `${outline(rows)}\n\n${tally}`
    },
  },
]

function mcpServer(core: Core): Server {
  const server = new Server({ name: mcpName, version: '0.1.0' }, { capabilities: { tools: {} }, instructions })
  server.setRequestHandler(ListToolsRequestSchema, async () => ({
    tools: mcpTools.map((tool) => ({
      name: tool.name,
      description: tool.description,
      inputSchema: tool.inputSchema,
      annotations: { readOnlyHint: true, destructiveHint: false, idempotentHint: true },
    })),
  }))
  server.setRequestHandler(CallToolRequestSchema, async (request) => {
    const tool = mcpTools.find((one) => one.name === request.params.name)
    if (!tool) return { content: [{ type: 'text', text: `No tool named "${request.params.name}"` }], isError: true }
    try {
      return { content: [{ type: 'text', text: await tool.run(core, request.params.arguments ?? {}) }] }
    } catch (error) {
      return { content: [{ type: 'text', text: error instanceof Error ? error.message : String(error) }], isError: true }
    }
  })
  return server
}

/**
 * Answers one MCP request. Stateless: a fresh server and transport per
 * request, closed with the socket, since a streamed response can outlive the
 * handler.
 */
export async function handleMcpRequest(core: Core, request: IncomingMessage, response: ServerResponse, body: unknown): Promise<void> {
  const server = mcpServer(core)
  const transport = new StreamableHTTPServerTransport({ sessionIdGenerator: undefined })
  response.on('close', () => {
    void transport.close()
    void server.close()
  })
  await server.connect(transport)
  await transport.handleRequest(request, response, body)
}
