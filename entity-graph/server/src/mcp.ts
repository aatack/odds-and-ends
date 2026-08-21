import { existsSync, readFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import type { FastifyInstance } from 'fastify'
import { Server } from '@modelcontextprotocol/sdk/server/index.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import {
  CallToolRequestSchema,
  ListResourcesRequestSchema,
  ListToolsRequestSchema,
  ReadResourceRequestSchema,
} from '@modelcontextprotocol/sdk/types.js'
import { TOOL_ID, TYPE_ID } from '../../src/core/builtins'
import { outlineMarkdown } from '../../src/core/markdown'
import type { QueryPage } from '../../src/core/query'
import { rowsOfPage } from '../../src/core/tree'
import { TOOLS_ENTITY_ID as TOOLS_ID } from '../../src/core/source/index'
import type { Source } from '../../src/core/source/index'
import { bearerToken, formatError } from './app'
import type { ConfigDb } from './config'
import type { Registry } from './registry'

// What an agent sees of a source. Deliberately *not* the source's own tool list:
// that is a store's API — raw events, resources, undo, whatever a user has
// defined — and handing all of it over asks a model to design its own reads. This
// is six tools over the same store, each one a whole job: read an outline, read
// an entity, add a note, set a value, link, unlink. Tools it cannot get right are
// worse than tools it does not have.
//
// Three channels, and a client cuts two of them at 2KB apiece: the server's
// instructions, and each tool's description. So the instructions are routing and
// nothing else, a tool's own mechanics live on that tool, and anything that needs
// a page — this repository's docs — is a *resource*, which is fetched and so is
// never cut. Writing past a cut is writing nothing.

/** Entities one page of `query` walks over, unless the caller says otherwise. */
const QUERY_LIMIT = 200

/** The entity the outline hangs off, and where a model with no bearings starts. */
const ROOT_ID = '@index'

/** Where the types are collected, so a type written here can be found again. */
const TYPES_ID = '@types'

/**
 * How to use this store, told to the client at initialize. A client truncates
 * this at 2KB and a tool's description at 2KB apiece, so what goes where is a
 * budget rather than a preference: this is the routing — what the store is, and
 * which call answers which question — and the mechanics of a tool live on that
 * tool, where there is another 2KB nobody else is spending.
 *
 * Anything longer than either is a *resource*, because a resource is fetched and
 * so is never cut. Keep this under 2000 characters; `npm test` says when it isn't.
 */
const INSTRUCTIONS = `This is a graph database of notes, read as an outline. Every note is an
*entity* with an id; a link from one entity to another means "this note sits under that
one". A note can sit under more than one parent, so the outline is a graph rather than a
strict tree.

Start at \`${ROOT_ID}\`, the root of all of it. \`query\` walks down from an id and hands back
one line per note with its id in the left column; \`sections: true\` reads it as a table of
contents, which is how to survey something before reading it, and \`find\` keeps only the
notes mentioning a string. Each tool's description says the rest.

Three values make the outline: \`text\`, the note itself, as markdown; \`section: true\`,
which makes it a heading; and \`open\`, which makes it a task — \`true\` unticked, \`false\`
ticked. Any other value is arbitrary JSON under a string key.

**Nesting is notes, not markdown.** A dash at the start of a line is nearly always a note
that should have been created: write the lead as one note and each point under it as a
child, however short.

## Where what you write goes

- **A tool you are making for the user goes under \`${TOOLS_ID}\`, and nowhere else.** A note
  there carrying a \`name\` and an \`execute\` body is a tool of this app: it lists in the
  command palette, can hold a key, and other tools call it by name. \`get_details\` on
  \`${TOOL_ID}\` is the whole specification — read it first. Written anywhere else it is an
  ordinary note and does nothing.
- **A type** — a note describing the notes that name it in their \`type\` — goes under
  \`${TYPES_ID}\`. \`get_details\` on \`${TYPE_ID}\` says what one holds.
- **Anything else** is a note in the outline. Ask where it goes if you cannot tell.

More is written down than fits here: list this server's resources and read the one that
covers what you are doing.

**Style.** Copy the notes around the one you are writing: the same length of line, the
same voice, the same amount of detail. This is somebody's own notebook: change what you
were asked to change and leave the rest.`

/**
 * One tool as the agent sees it, plus the source tool it goes through. A tool
 * whose source tool is missing — a read-only source, a narrowed one — is not
 * listed at all, so a model can tell what it may do by what it has.
 */
interface McpTool {
  name: string
  description: string
  inputSchema: { type: 'object'; properties: Record<string, unknown>; required?: string[] }
  /** Id of the source tool it delegates to. */
  needs: string
  readOnly: boolean
  /** True when it replaces or removes something rather than only adding. */
  destructive?: boolean
  /** False when calling it twice does not leave the store as calling it once did. */
  idempotent?: boolean
  run: (source: Source, args: Record<string, unknown>) => Promise<string>
}

/**
 * Spread onto every write below, so an event an agent wrote is recorded as
 * `<author>:mcp` rather than as the person whose store it is. The suffix keeps
 * whose store it is while saying what did the writing — history read back can
 * tell an agent's edit from a keystroke, which matters most for the edits nobody
 * remembers making.
 */
const VIA = { via: 'mcp' } as const

/** What a page says about itself: how much of it there is, and how to get the rest. */
function tally(page: QueryPage): string {
  const rows = `${page.rows.length} row${page.rows.length === 1 ? '' : 's'} shown`
  const visited = `${page.scanned} ${page.scanned === 1 ? 'entity' : 'entities'} visited`
  return page.continuation
    ? `[${rows}, ${visited}; the walk stopped short. To carry on, call query again with ` +
        `path: ${JSON.stringify(page.continuation)}]`
    : `[${rows}, ${visited}; that is everything under this path.]`
}

const MCP_TOOLS: McpTool[] = [
  {
    name: 'query',
    description:
      'Read a slice of the outline: a depth-first walk down from `path`, one line per ' +
      'note, each line starting with that note\'s entity id. Indentation is the shape of ' +
      'the outline, `#` marks a section, and `[ ]` / `[x]` an unticked / ticked task.\n\n' +
      `The walk visits at most \`limit\` notes (${QUERY_LIMIT} by default). When more ` +
      'remains, the answer ends with the path to resume from — pass it straight back as ' +
      '`path`, with the other arguments unchanged, for the next page.\n\n' +
      '`sections` and `find` narrow what comes back *after* the walk, so `limit` always ' +
      'means "notes visited": a narrow filter over a wide outline answers quickly with ' +
      `few rows and a path to continue from. Start from \`${ROOT_ID}\` if you have no ` +
      'better place to begin.',
    needs: 'query',
    readOnly: true,
    inputSchema: {
      type: 'object',
      properties: {
        path: {
          anyOf: [{ type: 'string' }, { type: 'array', items: { type: 'string' } }],
          description:
            'Where to start: an entity id, or the path a previous call gave back to ' +
            'resume from. A path is what lets a walk carry on mid-outline.',
        },
        limit: {
          type: 'number',
          description: `Most notes to visit before stopping. Defaults to ${QUERY_LIMIT}.`,
        },
        maxDepth: {
          type: 'number',
          description:
            'Levels to descend below the first note in `path`; 1 reads it and its ' +
            'children. Omit for no limit. Measured from there rather than from where a ' +
            'page resumes, so paging with the same `maxDepth` keeps reading the same shape.',
        },
        sections: {
          type: 'boolean',
          description:
            'Keep only sections (plus the note asked for) — the outline as a table of ' +
            'contents. The way to survey something before reading it.',
        },
        find: {
          type: 'string',
          description:
            'Keep only notes whose text contains this, plus the notes above them so the ' +
            'outline still reads. Case-insensitive.',
        },
      },
      required: ['path'],
    },
    run: async (source, args) => {
      const page = (await source.call('query', {
        path: args.path,
        limit: args.limit ?? QUERY_LIMIT,
        maxDepth: args.maxDepth,
        sections: args.sections,
        find: args.find,
      })) as QueryPage
      const outline = outlineMarkdown(rowsOfPage(page.rows), { ids: true })
      return [outline || '(nothing here)', tally(page)].join('\n\n')
    },
  },
  {
    name: 'get_details',
    description:
      'Everything known about each of these entities, as JSON keyed by id: all of their ' +
      'values (`text`, `section`, `open`, and anything else), when and by whom they were ' +
      'created and last edited, their children in order (`outboundLinks`), and their ' +
      '`inboundLinks` — every entity in the store that links to this one, which is how ' +
      'to find where else a note is referenced. An id nothing has been written to comes ' +
      'back empty rather than missing.\n\n' +
      'Two ids answer even in a store nobody has written either to, because the store ' +
      `supplies them: \`${TYPE_ID}\`, whose schema says what a type holds, and ` +
      `\`${TOOL_ID}\`, whose schema says what a tool holds — every value a definition can ` +
      'carry and what each one does. **Asked to write a tool for the user, read ' +
      `\`${TOOL_ID}\` first and create the note under \`${TOOLS_ID}\`;** the \`docs://tools\` ` +
      'resource is the same thing at length.',
    needs: 'readEntities',
    readOnly: true,
    inputSchema: {
      type: 'object',
      properties: {
        entityIds: {
          type: 'array',
          items: { type: 'string' },
          description: 'The ids to roll up. Ask for every one you need in a single call.',
        },
      },
      required: ['entityIds'],
    },
    run: async (source, args) => {
      const entities = await source.call('readEntities', { entityIds: args.entityIds })
      return JSON.stringify(entities, null, 2)
    },
  },
  {
    name: 'create',
    description:
      'Add a note under a parent, and hand back the id it was given. The id is a uuid ' +
      'minted here, so there is never one to invent: create the note, then use the id ' +
      'that comes back to hang children off it or to set anything else on it.\n\n' +
      'One call writes the text, the flags and the link, so the note is in the outline ' +
      'the moment it exists — at the end of its parent\'s children.\n\n' +
      '**Nesting is notes, not markdown.** A bullet list typed into one note\'s `text` is ' +
      'a single note as far as everything here is concerned: its points cannot be linked ' +
      'to, ticked, or read as their own lines. Write the lead as one note and each point ' +
      'as a child of it, however short they are.\n\n' +
      `A tool you are making for the user is a note like any other, created under ` +
      `\`${TOOLS_ID}\` — see \`get_details\` on \`${TOOL_ID}\` for what to put on it. It has to ` +
      `be under \`${TOOLS_ID}\` to be a tool at all, so this is the call that decides that.`,
    needs: 'createEntity',
    readOnly: false,
    // The one tool here that is not idempotent: called twice it makes two notes.
    // A client that retries on a timeout should know that before it does.
    idempotent: false,
    inputSchema: {
      type: 'object',
      properties: {
        parentId: {
          type: 'string',
          description:
            'The note it goes under. Required: a note nothing links to exists, but is ' +
            'nowhere in the outline and nobody will find it.',
        },
        text: { type: 'string', description: 'The note itself, as markdown.' },
        section: {
          type: 'boolean',
          description:
            '`true` to make it a heading — a named part of its parent rather than a ' +
            'bullet in it. Omit for an ordinary note.',
        },
        open: {
          type: 'boolean',
          description:
            'Makes it a task: `true` unticked, `false` already ticked. Omit for an ' +
            'ordinary bullet.',
        },
      },
      required: ['parentId', 'text'],
    },
    run: async (source, args) => {
      const values: Record<string, unknown> = { text: args.text }
      // Only a real flag is written. `section: false` and no `section` at all read
      // the same in the outline, so storing the false would be noise on the entity;
      // `open: false` genuinely means something else (a ticked task), so it stays.
      if (args.section === true) values.section = true
      if (typeof args.open === 'boolean') values.open = args.open
      const id = await source.call('createEntity', { values, parentId: args.parentId, ...VIA })
      return `Created ${String(id)} under ${String(args.parentId)}.`
    },
  },
  {
    name: 'set_value',
    description:
      'Set one value on one entity, replacing whatever was there. `key` is usually ' +
      '`text` (the note itself, as markdown), `section` (`true` to make it a heading) or ' +
      '`open` (`true` unticked, `false` ticked); any other key is stored as given. ' +
      '`value` is any JSON.\n\n' +
      'This is for editing a note that already exists. To add one, use `create`, which ' +
      'mints the id and links it in the same call — with one exception: writing to an id ' +
      'nothing has been written to yet is how a note gets a *name* instead of a uuid, ' +
      'which is what a type is (`github/pullRequest`). Link it somewhere afterwards.\n\n' +
      'There is no way to take a key off a note: `null` blanks it, which reads as ' +
      'absent everywhere it matters — no heading, no checkbox — but stays on the entity ' +
      'as a value, and is how a note refuses a default its type would otherwise give it.' +
      '\n\nMost of what is not a note\'s text is written through here: a `type` naming ' +
      'the note that describes it, a type\'s own `schema`, and every value that makes a ' +
      `note under \`${TOOLS_ID}\` a tool — its \`name\`, its \`execute\` body, its ` +
      '`arguments`. Each is one call, and each wants the JSON type the schema asks for: ' +
      'an `arguments` list written as a string is the usual way a tool ends up taking ' +
      'none.',
    needs: 'writeValue',
    readOnly: false,
    // It replaces whatever was under the key, so it is not a purely additive write.
    destructive: true,
    inputSchema: {
      type: 'object',
      properties: {
        entityId: { type: 'string', description: 'The entity to write to.' },
        key: { type: 'string', description: 'Which value to set, e.g. `text`.' },
        value: {
          // Every JSON type, spelled out. Leaving the type off says "anything"
          // here and reads as "a string" at the other end: a client that types
          // its arguments from the schema has nothing else to go on, so the
          // `true` an agent meant to write arrives as `"true"` — and since the
          // rollup asks whether a value *is* `true`, the heading is not a
          // heading and the task is not a task. Only `text` survived that,
          // which is why it took a while to notice.
          anyOf: [
            { type: 'string' },
            { type: 'number' },
            { type: 'boolean' },
            { type: 'null' },
            { type: 'object' },
            { type: 'array' },
          ],
          description:
            'The new value: any JSON, and it is stored as the type it arrives as — ' +
            '`section` and `open` want a real boolean, not `"true"`. `null` blanks the ' +
            'key rather than removing it.',
        },
      },
      required: ['entityId', 'key', 'value'],
    },
    run: async (source, args) => {
      await source.call('writeValue', {
        entityId: args.entityId,
        key: args.key,
        value: args.value ?? null,
        ...VIA,
      })
      return `Set \`${String(args.key)}\` on ${String(args.entityId)}.`
    },
  },
  {
    name: 'add_link',
    description:
      'Put one note under another: the child is added to the end of the parent\'s ' +
      'children, and the parent shows up in the child\'s `inboundLinks`. A note may sit ' +
      'under several parents, so this adds a place it appears rather than moving it. ' +
      'Adding a link that is already there changes nothing.',
    needs: 'writeLink',
    readOnly: false,
    inputSchema: {
      type: 'object',
      properties: {
        parentId: { type: 'string', description: 'The note the child goes under.' },
        childId: { type: 'string', description: 'The note being placed under it.' },
      },
      required: ['parentId', 'childId'],
    },
    run: async (source, args) => {
      await source.call('writeLink', {
        sourceId: args.parentId,
        destinationId: args.childId,
        action: 0,
        ...VIA,
      })
      return `Linked ${String(args.childId)} under ${String(args.parentId)}.`
    },
  },
  {
    name: 'remove_link',
    description:
      'Take a note out from under a parent. The note itself is untouched, as is every ' +
      'other place it appears — check `inboundLinks` with `get_details` first if you ' +
      'mean to unlink it from the only parent it has.',
    needs: 'writeLink',
    readOnly: false,
    destructive: true,
    inputSchema: {
      type: 'object',
      properties: {
        parentId: { type: 'string', description: 'The note the child currently sits under.' },
        childId: { type: 'string', description: 'The note being taken out.' },
      },
      required: ['parentId', 'childId'],
    },
    run: async (source, args) => {
      await source.call('writeLink', {
        sourceId: args.parentId,
        destinationId: args.childId,
        action: 1,
        ...VIA,
      })
      return `Unlinked ${String(args.childId)} from ${String(args.parentId)}.`
    },
  },
]

// --- The documents ----------------------------------------------------------

/**
 * The long-form documentation, served whole. A resource is fetched rather than
 * sent at initialize, so it is the one channel here with no ceiling — which makes
 * it where anything needing a page instead of a paragraph belongs.
 *
 * These are the repository's own docs rather than a retelling of them, so there
 * is one copy to keep true. `path` is resolved against this file, the way
 * `env.ts` and `registry.ts` find theirs; a doc that isn't on disk — a server
 * running from somewhere they weren't copied to — is left out of the listing
 * rather than offered and then failing to read.
 */
interface Doc {
  uri: string
  name: string
  description: string
  path: string
}

const DOCS: Doc[] = [
  {
    uri: 'docs://tools',
    name: 'Writing a tool for this app',
    description:
      `Everything about writing a tool of your own into \`${TOOLS_ID}\`: every value a ` +
      'definition can carry, what the body can call, how arguments are declared and ' +
      'what the app does with them, and what has to happen before a tool that has ' +
      `been written is a tool that runs. **Read this before writing under \`${TOOLS_ID}\`.** ` +
      `\`get_details\` on \`${TOOL_ID}\` is the same shape in short.`,
    path: '../../docs/user-tools.md',
  },
  {
    uri: 'docs://types',
    name: 'What a type is and how to write one',
    description:
      'What a type says about the entities naming it in their `type`, the three keys ' +
      'that are read by name, and why nothing is inherited. The long version of ' +
      `\`get_details\` on \`${TYPE_ID}\`.`,
    path: '../../docs/types.md',
  },
  {
    uri: 'docs://changesets',
    name: 'Changesets: work held open across a worktree, branch and PR',
    description:
      'How a piece of work is carried in this store — the entity, the worktree, the ' +
      'branch and the pull request kept as one thing. Worth reading before writing a ' +
      'tool that touches any of them, since the tools that do are themselves notes ' +
      `under \`${TOOLS_ID}\` and this is what they assume.`,
    path: '../../docs/changesets.md',
  },
  {
    uri: 'docs://integrations',
    name: "The server's integrations: GitHub, Slack, Claude Code, git",
    description:
      'What the server can reach outside itself, and the id and arguments of every ' +
      'one of those tools. You cannot call them from here — they answer to the admin ' +
      'token — but a tool you write can, by the camel case of its name, so this is ' +
      'the list of what a body has to work with.',
    path: '../docs/integrations.md',
  },
]

/** Where a doc actually is, or null when this checkout hasn't got it. */
function docPath(doc: Doc): string | null {
  const path = fileURLToPath(new URL(doc.path, import.meta.url))
  return existsSync(path) ? path : null
}

/** The tools this source can actually serve, in the order they are declared. */
function toolsFor(source: Source): McpTool[] {
  const available = new Set(source.tools().map((t) => t.id))
  return MCP_TOOLS.filter((t) => available.has(t.needs))
}

function makeMcpServer(source: Source): Server {
  const server = new Server(
    { name: `entity-graph:${source.id}`, version: '0.1.0' },
    { capabilities: { tools: {}, resources: {} }, instructions: INSTRUCTIONS }
  )

  server.setRequestHandler(ListResourcesRequestSchema, async () => ({
    resources: DOCS.filter(docPath).map((doc) => ({
      uri: doc.uri,
      name: doc.name,
      description: doc.description,
      mimeType: 'text/markdown',
    })),
  }))

  server.setRequestHandler(ReadResourceRequestSchema, async (req) => {
    const doc = DOCS.find((d) => d.uri === req.params.uri)
    const path = doc && docPath(doc)
    if (!doc || !path) throw new Error(`No document at ${String(req.params.uri)}`)
    return {
      contents: [{ uri: doc.uri, mimeType: 'text/markdown', text: readFileSync(path, 'utf8') }],
    }
  })

  server.setRequestHandler(ListToolsRequestSchema, async () => ({
    tools: toolsFor(source).map((t) => ({
      name: t.name,
      description: t.description,
      inputSchema: t.inputSchema as { type: 'object' },
      annotations: {
        readOnlyHint: t.readOnly,
        destructiveHint: t.destructive ?? false,
        // Every write but `create` says what the state should be rather than nudging
        // it, so making the same call twice leaves the store as the first call did.
        idempotentHint: t.idempotent ?? true,
      },
    })),
  }))

  server.setRequestHandler(CallToolRequestSchema, async (req) => {
    const { name, arguments: args } = req.params
    const tool = toolsFor(source).find((t) => t.name === name)
    if (!tool) {
      return {
        content: [{ type: 'text', text: `No tool named "${name}" on this source` }],
        isError: true,
      }
    }
    try {
      return { content: [{ type: 'text', text: await tool.run(source, args ?? {}) }] }
    } catch (e) {
      return { content: [{ type: 'text', text: formatError(e) }], isError: true }
    }
  })

  return server
}

interface McpDeps {
  db: ConfigDb
  registry: Registry
}

/**
 * POST /:sourceId/mcp — a stateless MCP endpoint per source. A fresh MCP server
 * + transport is created per request (no session state); the tools are the fixed
 * set above, each delegating to one of the source's own tools. GET/DELETE are
 * unused.
 */
export function registerMcp(app: FastifyInstance, deps: McpDeps): void {
  app.post<{ Params: { sourceId: string } }>('/:sourceId/mcp', async (req, reply) => {
    const { sourceId } = req.params
    if (!deps.db.getSource(sourceId)) {
      return reply.code(404).send({ error: `source "${sourceId}" not found` })
    }
    const token = bearerToken(req)
    if (!token || !deps.db.verifyToken(sourceId, token)) {
      return reply.code(401).send({ error: 'invalid or missing source token' })
    }

    const source = await deps.registry.get(sourceId)
    const server = makeMcpServer(source)
    const transport = new StreamableHTTPServerTransport({ sessionIdGenerator: undefined })

    reply.hijack()
    reply.raw.on('close', () => {
      transport.close()
      server.close()
    })
    await server.connect(transport)
    await transport.handleRequest(req.raw, reply.raw, req.body)
  })

  for (const method of ['GET', 'DELETE'] as const) {
    app.route({
      method,
      url: '/:sourceId/mcp',
      handler: async (_req, reply) =>
        reply.code(405).send({ error: 'stateless MCP endpoint: use POST' }),
    })
  }
}
