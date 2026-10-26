// The store's home: an egdb server. `EgdbSink` writes the store's changes to it
// in order, one batch at a time; `loadStore` reads a whole store back; and
// `startEgdb` runs an egdb-server of the backend's own on a file.

import { spawn, type ChildProcess } from 'node:child_process'
import { existsSync } from 'node:fs'
import { resolve } from 'node:path'
import { EgdbClient, type Filter, type Op } from 'egdb-client'
import {
  customActionFromValues, eventFromValues, ROOT, Store, type CustomActionRow, type EventRow, type Inspection, type LinkView, type Sink,
} from './store'

const BRANCH_NAME = 'workflows'
/** The kinds of entity that are not events. */
const OWN_TYPES = ['wf.root', 'wf.setting', 'wf.custom_action']
/** The links an item may have, as the loader and the debug view read them. */
const LINK_TYPES = ['contains', 'assigned_to'] as const
/** Bytes per write, about: egdb-server takes requests of 2 MB at most, so a big change goes in several. */
const MAX_BYTES = 1_000_000

/** The branch the store lives on: made the first time. */
export async function storeBranch(client: EgdbClient): Promise<string> {
  const found = (await client.branches()).find((b) => b.name === BRANCH_NAME && !b.archived)
  return found?.id ?? (await client.createBranch(BRANCH_NAME, []))
}

interface Queued {
  ops: Op[]
  author: string
  at: number
}

export class EgdbSink implements Sink {
  private queue: Queued[] = []
  private writing: Promise<void> | null = null
  /** The last write that failed, while its ops wait to go again. */
  error: string | null = null

  constructor(
    private client: EgdbClient,
    private branch: string,
    private onError: (message: string) => void = (m) => console.error(m),
  ) {}

  write(ops: Op[], author: string, at: number): void {
    const last = this.queue.at(-1)
    // Writes by the same author at the same moment go as one.
    if (last && last.author === author && last.at === at) last.ops.push(...ops)
    else this.queue.push({ ops: [...ops], author, at })
    this.writing ??= this.drain()
  }

  /**
   * Writes what is queued, in order, each batch as its author at its time; a
   * write that fails is tried again, after a while, until it lands. One op too
   * big for egdb on its own can never land, so it is left out, and said so,
   * rather than holding up everything after it.
   */
  private async drain(): Promise<void> {
    await new Promise((r) => setImmediate(r))
    let wait = 250
    while (this.queue.length) {
      const head = this.queue[0]
      let bytes = 0
      let n = 0
      for (; n < head.ops.length; n++) {
        const size = Buffer.byteLength(JSON.stringify(head.ops[n]))
        if (n && bytes + size > MAX_BYTES) break
        bytes += size
      }
      const ops = head.ops.slice(0, n)
      try {
        if (ops.length) await this.client.write({ branch: this.branch, author: head.author, timestamp: head.at, ops })
        head.ops.splice(0, ops.length)
        if (!head.ops.length) this.queue.shift()
        this.error = null
        wait = 250
      } catch (e) {
        this.error = `Could not write to egdb: ${(e as Error).message}`
        this.onError(this.error)
        if (n === 1 && /length limit|too large|413/i.test((e as Error).message)) {
          this.onError(`Left out an op too big for egdb (${bytes} bytes): ${JSON.stringify(ops[0]).slice(0, 200)}`)
          head.ops.shift()
          if (!head.ops.length) this.queue.shift()
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

  async inspect(id: string): Promise<Inspection> {
    const values = (await this.client.values(this.branch, [id]))[id] ?? {}
    const events = (await this.client.valueEvents(this.branch, [id]))[id] ?? {}
    const set = Object.fromEntries(Object.entries(events).map(([k, e]) => [k, { at: e.timestamp, by: e.author }]))
    const groups = LINK_TYPES.flatMap((t) => [[t, 'outbound'], [t, 'inbound']] as const)
    const got = (await this.client.linksIncludingArchived(this.branch, [id], groups))[id] ?? []
    const links: LinkView[] = got.flatMap((g) =>
      g.links.map((l) => ({
        type: g.link_type,
        direction: g.direction,
        other: g.direction === 'outbound' ? l.destination : l.source,
        sort: l.sort ?? null,
        at: l.event?.timestamp ?? null,
        by: l.event?.author ?? null,
      })),
    )
    const made = links.find((l) => l.type === 'contains' && l.direction === 'inbound')
    return { id, values, set, links, createdAt: made?.at ?? null, createdBy: made?.by ?? null }
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

/**
 * A store read back from egdb, which writes its changes there from now on.
 * `blocking(type)` says which events are blockers (resolved only once they say
 * so); the rest resolved as they were made. A source written by an earlier
 * build (its events' params in one value) is refused: start a new one.
 */
export async function loadStore(
  client: EgdbClient,
  blocking: (type: string) => boolean,
  onError?: (message: string) => void,
): Promise<{ store: Store; sink: EgdbSink }> {
  const branch = await storeBranch(client)
  const found = new Map<string, Record<string, unknown>>()
  const settings: [string, unknown][] = []
  const customActions: CustomActionRow[] = []
  await findAll(client, branch, { and: [{ not_null: { key: 'type' } }, { not: { in: { key: 'type', values: OWN_TYPES } } }] }, (id, x) => {
    if ('params' in x && 'created_by' in x) throw new Error('This source was made by an earlier build of the app, which kept items differently: start a new source.')
    found.set(id, x)
  })
  await findAll(client, branch, { eq: { key: 'type', value: 'wf.setting' } }, (_id, x) => {
    try {
      settings.push([String(x.key), JSON.parse(String(x.value))])
    } catch {
      // A setting that does not read is left out, as if it were never set.
    }
  })
  await findAll(client, branch, { eq: { key: 'type', value: 'wf.custom_action' } }, (_id, x) => customActions.push(customActionFromValues(x)))
  // Where each item sits, and who made it when: the `contains` links to it. And who has each task: `assigned_to` links.
  const made = new Map<string, { parent: string | null; seq: number; at: number; by: string }>()
  const assigned: { item: string; to: string; at: number; by: string }[] = []
  const ids = [ROOT, ...found.keys()]
  for (let i = 0; i < ids.length; i += 500) {
    const got = await client.links(branch, ids.slice(i, i + 500), [['contains', 'outbound'], ['assigned_to', 'outbound']])
    for (const groups of Object.values(got))
      for (const g of groups)
        for (const l of g.links) {
          const meta = { at: l.event?.timestamp ?? 0, by: l.event?.author ?? 'system' }
          if (g.link_type === 'contains') made.set(l.destination, { parent: l.source === ROOT ? null : l.source, seq: Number(l.sort ?? 0), ...meta })
          else assigned.push({ item: l.source, to: l.destination, ...meta })
        }
  }
  // When each resolved blocker resolved, and by whom: the event that set its `resolved` true.
  const resolved = new Map<string, { at: number; by: string }>()
  const done = [...found].filter(([, x]) => x.resolved === true).map(([id]) => id)
  for (let i = 0; i < done.length; i += 500) {
    const got = await client.valueEvents(branch, done.slice(i, i + 500))
    for (const [id, keys] of Object.entries(got)) if (keys.resolved) resolved.set(id, { at: keys.resolved.timestamp, by: keys.resolved.author })
  }
  const events: EventRow[] = []
  for (const [id, x] of found) {
    const m = made.get(id)
    if (m) events.push(eventFromValues(id, x, m, blocking(String(x.type)), resolved.get(id)))
  }
  const sink = new EgdbSink(client, branch, onError)
  const store = new Store(sink)
  store.restore({ events, settings, customActions, assigned })
  // The root is an entity too, so that the tree can be walked from it.
  if (!events.length) sink.write([{ set: { entity: ROOT, key: 'type', value: 'wf.root' } }], 'system', Date.now())
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

/** An egdb-server on `file`, and the store in it. */
export async function openStore(file: string, token: string, blocking: (type: string) => boolean, onError?: (message: string) => void) {
  const egdb = await startEgdb(file, token)
  const client = new EgdbClient({ url: egdb.url, token })
  try {
    const { store, sink } = await loadStore(client, blocking, onError)
    return { store, sink, client, egdb }
  } catch (e) {
    egdb.stop()
    throw e
  }
}
