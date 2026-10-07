import { randomBytes } from 'node:crypto'
import { createReadStream, existsSync, statSync } from 'node:fs'
import { createServer, type IncomingMessage, type ServerResponse } from 'node:http'
import { extname, join, normalize } from 'node:path'
import type { Core } from '../core/core.ts'

/**
 * The core over HTTP, for the phone (`docs/phone.md`): the same actions and
 * change notifications the window gets over IPC, and the built PWA beside them
 * on one origin. Listens on 127.0.0.1 only; Tailscale publishes it to my
 * devices. Every `/api` call needs the token.
 */

export const phonePort = 47821

const types: Record<string, string> = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript',
  '.css': 'text/css',
  '.json': 'application/json',
  '.webmanifest': 'application/manifest+json',
  '.svg': 'image/svg+xml',
  '.png': 'image/png',
  '.woff': 'font/woff',
  '.woff2': 'font/woff2',
}

/** The phone's token, made the first time it is needed and kept in the owned settings. */
export function phoneToken(core: Core): string {
  let token = core.settings.get('phone.token')
  if (!token) {
    token = randomBytes(24).toString('base64url')
    core.settings.set('phone.token', token)
  }
  return token
}

async function body(request: IncomingMessage): Promise<unknown> {
  const chunks: Buffer[] = []
  for await (const chunk of request) chunks.push(chunk as Buffer)
  const text = Buffer.concat(chunks).toString('utf8')
  return text ? JSON.parse(text) : undefined
}

function send(response: ServerResponse, status: number, value: unknown): void {
  response.writeHead(status, { 'content-type': 'application/json' })
  response.end(JSON.stringify(value ?? null))
}

export function startPhoneServer(core: Core, options: { dist: string; port?: number }): () => void {
  const token = phoneToken(core)
  const actions = core.actions as Record<string, (args: unknown) => unknown>

  const server = createServer(async (request, response) => {
    const url = new URL(request.url ?? '/', 'http://localhost')
    try {
      if (url.pathname.startsWith('/api/')) {
        const given = request.headers.authorization?.replace(/^Bearer /, '') ?? url.searchParams.get('token')
        if (given !== token) return send(response, 401, { error: 'not signed in' })
        const route = url.pathname.slice('/api/'.length)

        if (route === 'changes') {
          response.writeHead(200, { 'content-type': 'text/event-stream', 'cache-control': 'no-cache', connection: 'keep-alive' })
          response.write(': ok\n\n')
          const stop = core.onChange((changed) => response.write(`data: ${JSON.stringify(changed)}\n\n`))
          // A comment now and then, so proxies don't close a quiet stream.
          const ping = setInterval(() => response.write(': ping\n\n'), 25_000)
          request.on('close', () => {
            stop()
            clearInterval(ping)
          })
          return
        }

        if (route.startsWith('slack-image/')) {
          const { mime, data } = await core.slack.image(decodeURIComponent(route.slice('slack-image/'.length)))
          response.writeHead(200, { 'content-type': mime, 'cache-control': 'private, max-age=86400' })
          response.end(Buffer.from(data))
          return
        }

        const action = actions[route]
        if (request.method !== 'POST' || !action) return send(response, 404, { error: `no action ${route}` })
        return send(response, 200, await action(await body(request)))
      }

      // The PWA: a file from its build, or its page for anything else (a deep link).
      const path = normalize(join(options.dist, url.pathname === '/' ? 'phone.html' : url.pathname))
      const file = path.startsWith(options.dist) && existsSync(path) && statSync(path).isFile() ? path : join(options.dist, 'phone.html')
      if (!existsSync(file)) {
        response.writeHead(404, { 'content-type': 'text/plain' })
        response.end('The phone app is not built: npm run build:phone')
        return
      }
      response.writeHead(200, { 'content-type': types[extname(file)] ?? 'application/octet-stream' })
      createReadStream(file).pipe(response)
    } catch (error) {
      send(response, 500, { error: error instanceof Error ? error.message : String(error) })
    }
  })
  server.listen(options.port ?? phonePort, '127.0.0.1')
  return () => server.close()
}
