import { createServer } from 'node:http'
import type { IncomingMessage, Server, ServerResponse } from 'node:http'
import type { ActionRequest, ServerStatus, Topic } from '@conswap/common/types'
import { performAction } from './actions.js'
import type { Context } from './context.js'
import { eventTypes, getTopic } from './topics.js'
import { readQueue, readTopicDetail } from './views.js'

function send(response: ServerResponse, status: number, body: unknown): void {
  const payload = JSON.stringify(body)
  response.writeHead(status, {
    'content-type': 'application/json; charset=utf-8',
    'content-length': Buffer.byteLength(payload),
    'access-control-allow-origin': '*',
  })
  response.end(payload)
}

async function readBody(request: IncomingMessage): Promise<unknown> {
  const chunks: Buffer[] = []
  for await (const chunk of request) chunks.push(chunk as Buffer)
  if (chunks.length === 0) return {}
  return JSON.parse(Buffer.concat(chunks).toString())
}

function searchTopics(context: Context, query: string, limit: number): Topic[] {
  const placeholders = [...eventTypes].map(() => '?').join(', ')
  const rows = context.db
    .prepare(
      `SELECT id FROM topics
       WHERE text LIKE ? AND type NOT IN (${placeholders})
       ORDER BY updated_at DESC LIMIT ?`,
    )
    .all(`%${query}%`, ...eventTypes, limit) as { id: string }[]
  return rows.map((row) => getTopic(context.db, row.id)).filter((topic): topic is Topic => topic !== null)
}

function status(context: Context): ServerStatus {
  return {
    revision: context.revision,
    integrations: [...context.integrations.values()].map((integration) => integration.status()),
    runs: (
      context.db
        .prepare('SELECT id, topic_id, kind, status, started_at, finished_at, error FROM runs ORDER BY started_at DESC LIMIT 20')
        .all() as {
        id: string
        topic_id: string
        kind: string
        status: string
        started_at: string
        finished_at: string | null
        error: string | null
      }[]
    ).map((row) => ({
      id: row.id,
      topicId: row.topic_id,
      kind: row.kind,
      status: row.status as 'running' | 'succeeded' | 'failed',
      startedAt: row.started_at,
      finishedAt: row.finished_at,
      error: row.error,
    })),
  }
}

/** Server-sent events: one line per change, so the client never has to poll. */
function stream(context: Context, response: ServerResponse): void {
  response.writeHead(200, {
    'content-type': 'text/event-stream',
    'cache-control': 'no-cache',
    connection: 'keep-alive',
    'access-control-allow-origin': '*',
  })
  response.write(`data: ${JSON.stringify({ revision: context.revision, topics: [], queue: true })}\n\n`)
  const keepAlive = setInterval(() => response.write(': keep-alive\n\n'), 20_000)
  const unsubscribe = context.subscribe((event) => {
    response.write(`data: ${JSON.stringify(event)}\n\n`)
  })
  response.on('close', () => {
    clearInterval(keepAlive)
    unsubscribe()
  })
}

export function startApi(context: Context): Server {
  const server = createServer((request, response) => {
    void handle(context, request, response).catch((error) => {
      send(response, 500, { error: String(error instanceof Error ? error.message : error) })
    })
  })
  server.listen(context.config.port, '127.0.0.1', () => {
    context.log('api', `listening on http://127.0.0.1:${context.config.port}`)
    process.stdout.write(`CONSWAP_READY http://127.0.0.1:${context.config.port}\n`)
  })
  return server
}

async function handle(context: Context, request: IncomingMessage, response: ServerResponse): Promise<void> {
  const url = new URL(request.url ?? '/', 'http://localhost')
  const path = url.pathname

  if (request.method === 'OPTIONS') {
    response.writeHead(204, {
      'access-control-allow-origin': '*',
      'access-control-allow-methods': 'GET, POST, OPTIONS',
      'access-control-allow-headers': 'content-type',
    })
    response.end()
    return
  }

  if (path === '/health') {
    send(response, 200, { ok: true, revision: context.revision })
    return
  }

  if (path === '/status') {
    send(response, 200, status(context))
    return
  }

  if (path === '/stream') {
    stream(context, response)
    return
  }

  if (path === '/queue' && request.method === 'GET') {
    send(response, 200, readQueue(context))
    return
  }

  if (path === '/topics' && request.method === 'GET') {
    const query = url.searchParams.get('query') ?? ''
    send(response, 200, { topics: searchTopics(context, query, Number(url.searchParams.get('limit') ?? 20)) })
    return
  }

  if (path.startsWith('/topics/') && request.method === 'GET') {
    const id = decodeURIComponent(path.slice('/topics/'.length))
    const expanded = new Set((url.searchParams.get('expanded') ?? '').split(',').filter(Boolean))
    const detail = readTopicDetail(context, id, expanded)
    if (!detail) {
      send(response, 404, { error: 'no such topic' })
      return
    }
    send(response, 200, detail)
    return
  }

  if (path === '/actions' && request.method === 'POST') {
    const body = (await readBody(request)) as ActionRequest
    if (!body?.name || !body?.id) {
      send(response, 400, { ok: false, error: 'id and name are required' })
      return
    }
    send(response, 200, await performAction(context, body.id, body.name, body.args ?? {}))
    return
  }

  send(response, 404, { error: 'no such route' })
}
