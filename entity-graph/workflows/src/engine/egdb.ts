// The store's home: an egdb server. `EgdbSink` writes the store's changes to it
// in order, one batch at a time; `loadStore` reads a whole store back; and
// `startEgdb` runs an egdb-server of the backend's own on a file.

import { spawn, type ChildProcess } from 'node:child_process'
import { existsSync } from 'node:fs'
import { join, resolve } from 'node:path'
import { EgdbClient, type Filter, type Op } from 'egdb-client'
import {
  customActionFromValues, eventFromValues, runFromValues, Store, traceFromValues,
  type CustomActionRow, type EventRow, type RunRow, type Sink, type TraceRow,
} from './store'

const AUTHOR = 'workflows'
const BRANCH_NAME = 'workflows'
/** The kinds of entity that are not events. Transcripts are left out of a load. */
const OWN_TYPES = ['wf.root', 'wf.trace', 'wf.run', 'wf.setting', 'wf.custom_action', 'wf.transcript']
/** Bytes per write, about: egdb-server takes requests of 2 MB at most, so a big change (an import) goes in several. */
const MAX_BYTES = 1_000_000

/** The branch the store lives on: made the first time. */
export async function storeBranch(client: EgdbClient): Promise<string> {
  const found = (await client.branches()).find((b) => b.name === BRANCH_NAME && !b.archived)
  return found?.id ?? (await client.createBranch(BRANCH_NAME, []))
}

export class EgdbSink implements Sink {
  private queue: Op[] = []
  private writing: Promise<void> | null = null
  /** The last write that failed, while its ops wait to go again. */
  error: string | null = null

  constructor(
    private client: EgdbClient,
    private branch: string,
    private onError: (message: string) => void = (m) => console.error(m),
  ) {}

  write(ops: Op[]): void {
    this.queue.push(...ops)
    this.writing ??= this.drain()
  }

  /**
   * Writes what is queued, in order; a write that fails is tried again, after a
   * while, until it lands. One op too big for egdb on its own can never land, so
   * it is left out, and said so, rather than holding up everything after it.
   */
  private async drain(): Promise<void> {
    // Everything changed in this turn of the event loop goes in one write.
    await new Promise((r) => setImmediate(r))
    let wait = 250
    while (this.queue.length) {
      let bytes = 0
      let n = 0
      for (; n < this.queue.length; n++) {
        const size = Buffer.byteLength(JSON.stringify(this.queue[n]))
        if (n && bytes + size > MAX_BYTES) break
        bytes += size
      }
      const ops = this.queue.slice(0, n)
      try {
        await this.client.write({ branch: this.branch, author: AUTHOR, ops })
        this.queue.splice(0, ops.length)
        this.error = null
        wait = 250
      } catch (e) {
        this.error = `Could not write to egdb: ${(e as Error).message}`
        this.onError(this.error)
        if (n === 1 && /length limit|too large|413/i.test((e as Error).message)) {
          this.onError(`Left out an op too big for egdb (${bytes} bytes): ${JSON.stringify(ops[0]).slice(0, 200)}`)
          this.queue.shift()
          continue
        }
        await new Promise((r) => setTimeout(r, wait))
        wait = Math.min(wait * 2, 10_000)
      }
    }
    this.writing = null
  }

  async flush(): Promise<void> {
    while (this.writing) await this.writing
  }

  async transcript(runId: string): Promise<string | null> {
    const e = `transcript:${runId}`
    const values = (await this.client.values(this.branch, [e]))[e]
    if (!values || typeof values.parts !== 'number') return null
    return Array.from({ length: values.parts }, (_, i) => String(values[`part:${i}`] ?? '')).join('')
  }
}

/** Every entity that matches, page by page. */
async function findAll(client: EgdbClient, branch: string, filter: Filter, each: (id: string, values: Record<string, unknown>) => void): Promise<void> {
  let cursor: string | null = null
  do {
    const page = await client.find(branch, filter, { limit: 2000, cursor, order: 'insertion' })
    for (const row of page.rows) each(row.id, row.values)
    cursor = page.next
  } while (cursor)
}

/** A store read back from egdb, which writes its changes there from now on. */
export async function loadStore(client: EgdbClient, onError?: (message: string) => void): Promise<{ store: Store; sink: EgdbSink }> {
  const branch = await storeBranch(client)
  const events: EventRow[] = []
  const trace: TraceRow[] = []
  const runs: RunRow[] = []
  const settings: [string, unknown][] = []
  const customActions: CustomActionRow[] = []
  await findAll(client, branch, { and: [{ not_null: { key: 'type' } }, { not: { in: { key: 'type', values: OWN_TYPES } } }] }, (id, x) =>
    events.push(eventFromValues(id, String(x.type), x)),
  )
  await findAll(client, branch, { eq: { key: 'type', value: 'wf.trace' } }, (_id, x) => trace.push(traceFromValues(x)))
  await findAll(client, branch, { eq: { key: 'type', value: 'wf.run' } }, (id, x) => runs.push(runFromValues(id.replace(/^run:/, ''), x)))
  await findAll(client, branch, { eq: { key: 'type', value: 'wf.setting' } }, (_id, x) => {
    try {
      settings.push([String(x.key), JSON.parse(String(x.value))])
    } catch {
      // A setting that does not read is left out, as if it were never set.
    }
  })
  await findAll(client, branch, { eq: { key: 'type', value: 'wf.custom_action' } }, (_id, x) => customActions.push(customActionFromValues(x)))
  // The root is an entity too, so that the tree can be walked from it.
  const sink = new EgdbSink(client, branch, onError)
  const store = new Store(sink)
  store.restore({ events, trace, runs, settings, customActions })
  if (!events.length) sink.write([{ set: { entity: '@root', key: 'type', value: 'wf.root' } }])
  return { store, sink }
}

/** Where to find an egdb-server binary: EGDB_SERVER, else the repository's release build, else the PATH. */
export function egdbServerPath(): string {
  if (process.env.EGDB_SERVER) return process.env.EGDB_SERVER
  // From src/engine (tsx) or out/main (the build): the repository's egdb is a few folders up.
  for (const up of ['../../../../egdb', '../../../egdb', '../../egdb']) {
    const built = resolve(__dirname, up, 'target/release/egdb-server')
    if (existsSync(built)) return built
  }
  return 'egdb-server'
}

export interface EgdbProcess {
  url: string
  child: ChildProcess
  stop: () => void
}

/** Runs an egdb-server on `file`, on a free loopback port, behind a token, and answers once it listens. */
export function startEgdb(file: string, token: string, bin = egdbServerPath()): Promise<EgdbProcess> {
  return new Promise((done, fail) => {
    const child = spawn(bin, [file, '--addr', '127.0.0.1:0'], { env: { ...process.env, EGDB_TOKEN: token }, stdio: ['ignore', 'pipe', 'pipe'] })
    let out = ''
    let err = ''
    const timer = setTimeout(() => fail(new Error(`egdb-server did not start: ${err || out}`)), 15_000)
    child.on('error', (e) => {
      clearTimeout(timer)
      fail(new Error(`Could not run ${bin}: ${e.message}. Build it with \`npm run egdb:build\`, or set EGDB_SERVER.`))
    })
    child.on('exit', (code) => {
      clearTimeout(timer)
      fail(new Error(`egdb-server stopped (${code}): ${err.trim() || out.trim()}`))
    })
    child.stderr.setEncoding('utf8').on('data', (c: string) => (err += c))
    child.stdout.setEncoding('utf8').on('data', (c: string) => {
      out += c
      const m = /listening on (http:\/\/[^\s]+)/.exec(out)
      if (!m) return
      clearTimeout(timer)
      done({ url: m[1], child, stop: () => child.kill('SIGINT') })
    })
  })
}

/** An egdb-server on a file in `dir`, and the store in it. */
export async function openStore(dir: string, token: string, onError?: (message: string) => void) {
  const egdb = await startEgdb(join(dir, 'workflows.egdb'), token)
  const client = new EgdbClient({ url: egdb.url, token })
  const { store, sink } = await loadStore(client, onError)
  return { store, sink, client, egdb }
}
