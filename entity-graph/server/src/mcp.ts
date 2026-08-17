import type { FastifyInstance } from 'fastify'
import { Server } from '@modelcontextprotocol/sdk/server/index.js'
import { StreamableHTTPServerTransport } from '@modelcontextprotocol/sdk/server/streamableHttp.js'
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js'
import { outlineMarkdown } from '../../src/core/markdown'
import type { QueryPage } from '../../src/core/query'
import { rowsOfPage } from '../../src/core/tree'
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

/** Entities one page of `query` walks over, unless the caller says otherwise. */
const QUERY_LIMIT = 200

/** The entity the outline hangs off, and where a model with no bearings starts. */
const ROOT_ID = '@index'

/**
 * How to use this store, told to the client at initialize. Long on purpose: it is
 * the difference between an agent that reads the outline the way a person does —
 * shape first, then the part that matters — and one that pages through
 * everything, or writes notes in a voice nobody else in the file uses.
 */
const INSTRUCTIONS = `This is a graph database of notes, read as an outline. Every note is an
*entity* with an id; a link from one entity to another means "this note sits under that
one". A note can sit under more than one parent, so the outline is a graph rather than a
strict tree.

## Reading

- \`query\` walks down from an id and hands back one line per note, with the note's id in
  the left-hand column. Those ids are what every other tool takes, so read a line and you
  can already act on it. A line reads \`<id>  <outline>\`: the indentation is where the note
  sits, \`#\` marks a section, and \`[ ]\` / \`[x]\` an unticked / ticked task.
- Start from \`${ROOT_ID}\` if you do not know where to look — it is the root of the whole
  outline.
- A walk stops after \`limit\` notes (${QUERY_LIMIT} by default). When it does, the answer
  ends with the path to resume from: call \`query\` again with that as \`path\`, keeping the
  other arguments the same, and you get the next page of the same walk. Only page on when
  what you were after wasn't in what you have already read.
- \`sections: true\` keeps only the headings. That is how to see the shape of something
  large before committing to reading it, and how to read prose: get the sections, then
  query the one you want in full.
- \`maxDepth\` bounds how far below the starting note the walk goes; \`maxDepth: 1\` reads a
  note and its immediate children.
- \`find\` keeps only notes whose text contains a string, plus the notes above them so the
  outline still reads. It filters what the walk visited rather than searching the whole
  store, so widen \`limit\` (or start higher up) rather than expecting one call to find
  every mention. With \`sections: true\` the two narrow together — it searches the headings
  rather than the notes under them, which is how to find where something is written about.
- \`get_details\` returns whole entities for a list of ids: every value on them, their
  children in order (\`outboundLinks\`), and — the usual reason to reach for it —
  \`inboundLinks\`, which is everywhere else in the store that references them.

## What a note holds

Values are arbitrary JSON under string keys, but three of them are what the outline is
made of:

- **\`text\`** is the note itself, and the only value most notes have. It is markdown:
  fenced code blocks, inline code, emphasis, links. Maths goes in LaTeX, \`$inline$\` or
  \`$$display$$\`.
- **\`section: true\`** makes a note a heading — a named part of what it sits under rather
  than a bullet in it. These are what \`sections: true\` returns, so a note that titles a
  group of others should be one.
- **\`open\`** makes a note a checkbox: \`true\` is unticked, \`false\` is ticked. Absent means
  an ordinary bullet, so don't add it to notes that aren't tasks. Tick something by
  setting \`open\` to \`false\`; never delete the value to mark it done.

A note may also carry a \`type\`, which is the app's business rather than yours: \`code\` is a
script it can run, \`file\` an attachment whose bytes live outside these tools. Read them,
but leave their values alone unless you were asked to change them.

## Writing

- \`create\` adds a note under a parent and returns its id. The id is minted for you, so
  never invent one. Give it the \`text\`, plus \`section: true\` if it titles a group of
  notes, or \`open: true\` if it is a task; to build a branch, create the parent first and
  create its children under the id you got back.
- **Nesting is notes, not markdown.** A bullet list typed into one note's \`text\` is a
  single note as far as everything here is concerned: its points can't be linked to,
  ticked, replied to, or read as their own lines. Write the heading or the lead as one
  note and each point under it as a child, however short they are. A dash at the start
  of a line is nearly always a note that should have been created.
- \`set_value\` writes one value on one entity — text, section, open, or anything else. It
  is for editing a note that is already there. Values are typed: \`section\` and \`open\`
  want a real boolean, and \`null\` blanks a key rather than taking it off the note.
- Your writes are recorded under \`<their name>:mcp\`, so the notebook's history says
  which lines were yours. Nothing to pass — it is on every write you make.
- \`add_link\` puts a child under a parent, at the end of the parent's children.
  \`remove_link\` takes it out again; a note under several parents keeps the others.
- Writes are events appended to a log, so nothing is overwritten in place — but this is
  someone's own notebook. Change what you were asked to change and leave the rest.

## Style

- Copy the notes around the one you are writing: the same length of line, the same voice,
  the same amount of detail. Match what is there rather than what you would write.
- Keep a bullet to one thought, and keep it short. If it wants two sentences it is
  probably a section with two bullets under it.`

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
      'back empty rather than missing.',
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
      'the moment it exists — at the end of its parent\'s children.',
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
      'mints the id and links it in the same call.\n\n' +
      'There is no way to take a key off a note: `null` blanks it, which reads as ' +
      'absent everywhere it matters — no heading, no checkbox — but stays on the entity ' +
      'as a value, and is how a note refuses a default its type would otherwise give it.',
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

/** The tools this source can actually serve, in the order they are declared. */
function toolsFor(source: Source): McpTool[] {
  const available = new Set(source.tools().map((t) => t.id))
  return MCP_TOOLS.filter((t) => available.has(t.needs))
}

function makeMcpServer(source: Source): Server {
  const server = new Server(
    { name: `entity-graph:${source.id}`, version: '0.1.0' },
    { capabilities: { tools: {} }, instructions: INSTRUCTIONS }
  )

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
