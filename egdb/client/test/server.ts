// Runs a real egdb-server for the integration tests. Build it first:
//   cargo build --release --features server --bin egdb-server
// (or point EGDB_SERVER_BIN at a binary). Without one, those tests skip.

import { spawn, type ChildProcess } from 'node:child_process'
import { existsSync, mkdtempSync, rmSync } from 'node:fs'
import { createServer } from 'node:net'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { EgdbClient } from '../src/client.js'
import { op } from '../src/ops.js'
import type { Op } from '../src/types.js'

// Relative to the package: vitest runs from it.
const bin = process.env.EGDB_SERVER_BIN ?? join(process.cwd(), '../target/release/egdb-server')

export const haveServer = existsSync(bin)

export interface Running {
  client: EgdbClient
  stop(): void
}

async function freePort(): Promise<number> {
  return new Promise((resolve) => {
    const server = createServer()
    server.listen(0, '127.0.0.1', () => {
      const port = (server.address() as { port: number }).port
      server.close(() => resolve(port))
    })
  })
}

export async function startServer(): Promise<Running> {
  const dir = mkdtempSync(join(tmpdir(), 'egdb-test-'))
  const port = await freePort()
  const child: ChildProcess = spawn(bin, [join(dir, 'test.db'), '--addr', `127.0.0.1:${port}`], { stdio: 'ignore' })
  const client = new EgdbClient({ url: `http://127.0.0.1:${port}` })
  for (let i = 0; ; i++) {
    try {
      await client.health()
      break
    } catch (error) {
      if (i > 100) throw error
      await new Promise((r) => setTimeout(r, 50))
    }
  }
  return {
    client,
    stop() {
      child.kill()
      rmSync(dir, { recursive: true, force: true })
    },
  }
}

/**
 * A tree of lists and tasks: `fanout` children per list, `depth` levels below
 * the root, every third child a task (a leaf). IDs are paths of indices.
 */
export function treeOps(depth: number, fanout: number): Op[] {
  const ops: Op[] = [op.set('root', 'type', 'list'), op.set('root', 'text', 'root')]
  const grow = (id: string, level: number) => {
    if (level === depth) return
    for (let i = 0; i < fanout; i++) {
      const kid = `${id}.${i}`
      const leaf = level + 1 === depth || i % 3 === 2
      ops.push(op.set(kid, 'type', leaf ? 'task' : 'list'))
      ops.push(op.set(kid, 'text', `item ${kid}`))
      ops.push(op.link(id, kid, 'child', { explicit: i }))
      if (!leaf) grow(kid, level + 1)
    }
  }
  grow('root', 0)
  return ops
}
