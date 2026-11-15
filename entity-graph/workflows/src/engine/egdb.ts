// The store's home: an egdb server. `EgdbSink` writes the store's changes to it
// in order, one batch at a time; `loadStore` reads a whole store back; and
// `startEgdb` runs an egdb-server of the backend's own on a file.

import type { LogEntry, LogQuery } from '../shared/types'
import { spawn, type ChildProcess } from 'node:child_process'
import { existsSync } from 'node:fs'
import { resolve } from 'node:path'
import { EgdbClient, type Filter, type Op } from 'egdb-client'
import {
  itemFromValues, legacyValues, ROOT, Store, type ItemRow, type Inspection, type LinkView, type Sink, type Via,
} from './store'

const BRANCH_NAME = 'workflows'
/** The kinds of entity that are not items. */
// `wf.custom_action`: an older build's custom actions, which are no more (scripts owned by a type do their job); skipped.
const OWN_TYPES = ['wf.root', 'wf.setting', 'wf.custom_action']
/** The links an item may have, as the loader and the debug view read them (`contains` and `assigned_to`: an older build's). */
const LINK_TYPES = ['comments', 'notes', 'pins', 'assignees', 'owns', 'contains', 'assigned_to'] as const
/** The setting that says up to when an older build's links have been read into this build's (`migrate`). */
const MIGRATED = 'migration.links_until'
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

  async log(query: LogQuery): Promise<LogEntry[]> {
    // What was written just now is in it.
    await this.flush()
    const limit = query.limit ?? 200
    // One read of egdb's log: values and links together, the newest first.
    const { events } = await this.client.log(this.branch, { ...query, limit })
    return events.map((c) =>
      c.kind === 'value'
        ? { at: c.event.timestamp, by: c.event.author, kind: 'value' as const, entity: c.entity, key: c.key, value: c.value, other: null, removed: false }
        : { at: c.event.timestamp, by: c.event.author, kind: 'link' as const, entity: c.source, key: c.link_type, value: null, other: c.destination, removed: c.sort === null },
    )
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
    return { id, values, set, links, createdAt: set.type?.at ?? null, createdBy: set.type?.by ?? null }
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

interface Placed {
  parent: string
  via: Via
  seq: number
  at: number
  by: string
}

/**
 * A store read back from egdb, which writes its changes there from now on. `blocking(type)` says which items are
 * blockers (resolved only once they say so); the rest resolved as they were made. A source written by an earlier build
 * (its items' params in one value) is refused: start a new one.
 *
 * An older build's links are read into this build's as it loads, by adding links and never rewriting any (so the older
 * build reads the source as it was; see `migrate`).
 */
export async function loadStore(
  client: EgdbClient,
  blocking: (type: string) => boolean,
  onError?: (message: string) => void,
  /** Whether an item type is known: what is not (a type since removed) is left out. */
  known: (type: string) => boolean = () => true,
  now: () => number = Date.now,
): Promise<{ store: Store; sink: EgdbSink }> {
  const branch = await storeBranch(client)
  const stored = new Map<string, Record<string, unknown>>()
  const settings: [string, unknown][] = []
  await findAll(client, branch, { and: [{ not_null: { key: 'type' } }, { not: { in: { key: 'type', values: OWN_TYPES } } }] }, (id, x) => {
    if ('params' in x && 'created_by' in x) throw new Error('This source was made by an earlier build of the app, which kept items differently: start a new source.')
    stored.set(id, x)
  })
  await findAll(client, branch, { eq: { key: 'type', value: 'wf.setting' } }, (_id, x) => {
    try {
      settings.push([String(x.key), JSON.parse(String(x.value))])
    } catch {
      // A setting that does not read is left out, as if it were never set.
    }
  })
  const sink = new EgdbSink(client, branch, onError)
  const at = now()
  const migratedUntil = Number(settings.find(([k]) => k === MIGRATED)?.[1] ?? 0)

  // Every link from an item (or the root): where each item sits, what is pinned to it, who has it and who owns it; and
  // an older build's links, newer than the last time they were read in.
  const placed = new Map<string, Placed>()
  const pinned: { from: string; id: string; at: number; by: string }[] = []
  const assigned: { item: string; to: string; at: number; by: string }[] = []
  const owners: { item: string; owner: string; at: number; by: string }[] = []
  const old = { contains: new Map<string, (Placed & { sort: number })[]>(), assigned: [] as { item: string; to: string; at: number; by: string }[] }
  let newest = migratedUntil
  const ids = [ROOT, ...stored.keys()]
  for (let i = 0; i < ids.length; i += 500) {
    const got = await client.links(branch, ids.slice(i, i + 500), LINK_TYPES.map((t) => [t, 'outbound'] as [string, 'outbound']))
    for (const groups of Object.values(got))
      for (const g of groups)
        for (const l of g.links) {
          const meta = { at: l.event?.timestamp ?? 0, by: l.event?.author ?? 'system' }
          // Assignments, pins and ownership are sorted by when they were made: that is their time (a write's own time can't go back).
          const when = typeof l.sort === 'number' ? l.sort : meta.at
          if (g.link_type === 'comments' || g.link_type === 'notes') {
            const p = { parent: l.source, via: g.link_type as Via, seq: Number(l.sort ?? 0), ...meta }
            const had = placed.get(l.destination)
            if (!had || had.at > p.at) placed.set(l.destination, p)
          } else if (g.link_type === 'pins') pinned.push({ from: l.source, id: l.destination, at: when, by: meta.by })
          else if (g.link_type === 'assignees') assigned.push({ item: l.source, to: l.destination, at: when, by: meta.by })
          else if (g.link_type === 'owns') owners.push({ item: l.destination, owner: l.source, at: when, by: meta.by })
          else if (meta.at > migratedUntil) {
            // An older build's, not read in yet.
            newest = Math.max(newest, meta.at)
            if (g.link_type === 'contains') {
              const list = old.contains.get(l.destination) ?? []
              list.push({ parent: l.source, via: 'comments', seq: Number(l.sort ?? 0), sort: Number(l.sort ?? 0), ...meta })
              old.contains.set(l.destination, list)
            } else old.assigned.push({ item: l.source, to: l.destination, at: when, by: meta.by })
          }
        }
  }
  const migrating = old.contains.size > 0 || old.assigned.length > 0
  if (migrating) migrate({ stored, placed, pinned, assigned, owners, old, sink, at })

  // Who made each item, and when: who set its `type`. When each closed blocker or topic closed, and by whom: who set
  // its `resolved` true (or a topic's `open` false).
  const made = new Map<string, { at: number; by: string }>()
  const resolved = new Map<string, { at: number; by: string }>()
  const all = [...stored.keys()]
  for (let i = 0; i < all.length; i += 500) {
    const got = await client.valueEvents(branch, all.slice(i, i + 500))
    for (const [id, keys] of Object.entries(got)) {
      if (keys.type) made.set(id, { at: keys.type.timestamp, by: keys.type.author })
      const x = stored.get(id)!
      const closer = x.open === false ? keys.open : x.resolved === true ? keys.resolved : undefined
      if (closer) resolved.set(id, { at: closer.timestamp, by: closer.author })
    }
  }
  const rows: ItemRow[] = []
  for (const [id, raw] of stored) {
    const x = legacyValues(raw)
    if (!known(String(x.type))) continue
    const p = placed.get(id)
    const m = made.get(id) ?? { at: p?.at ?? 0, by: p?.by ?? 'system' }
    rows.push(itemFromValues(id, x, { parent: p && stored.has(p.parent) ? p.parent : null, via: p?.via ?? null, seq: p?.seq ?? 0, at: m.at, by: m.by }, blocking(String(x.type)), resolved.get(id)))
  }
  const store = new Store(sink)
  const kept = new Set(rows.map((r) => r.id))
  store.restore({
    items: rows,
    settings,
    assigned: assigned.filter((a) => kept.has(a.item)),
    pinned: pinned.filter((p) => kept.has(p.from) && kept.has(p.id)),
    owners: owners.filter((o) => kept.has(o.item)),
  })
  // Read in up to the newest of them (egdb's own times, which a clock behind can't trust): the next load reads only what an
  // older build adds after.
  if (migrating) store.setSetting(MIGRATED, newest)
  return { store, sink }
}

/**
 * Reads an older build's links into this build's, adding links and rewriting nothing: an item an older build put in a
 * task (its first `contains` link) is put in its comments, or in its notes if the parent was a note; a later `contains`
 * (an older build's pin) is a pin; an `assigned_to` link is an `assignees` link; and each topic, bot and script the older
 * build made is owned by its owner as that build saw it (whoever made it if a person, else the owner of what it sits in,
 * else the owner of the bot that made it). Each link is written as whoever wrote the original, sorted by its time.
 */
function migrate(m: {
  stored: Map<string, Record<string, unknown>>
  placed: Map<string, Placed>
  pinned: { from: string; id: string; at: number; by: string }[]
  assigned: { item: string; to: string; at: number; by: string }[]
  owners: { item: string; owner: string; at: number; by: string }[]
  old: { contains: Map<string, (Placed & { sort: number })[]>; assigned: { item: string; to: string; at: number; by: string }[] }
  sink: EgdbSink
  at: number
}): void {
  const { stored, placed, pinned, assigned, owners, old, sink, at } = m
  const ops = new Map<string, Op[]>()
  const write = (by: string, op: Op) => ops.set(by, [...(ops.get(by) ?? []), op])
  const typeOf = (id: string) => String(stored.get(id)?.type ?? '')
  const pins = new Set(pinned.map((p) => `${p.from} ${p.id}`))
  const given = new Set(assigned.map((a) => `${a.item} ${a.to}`))
  const ownerMap = new Map(owners.map((o) => [o.item, o.owner]))
  const isPerson = (id: string) => typeOf(id) === 'user'
  const made = new Map<string, { parent: string | null; at: number; by: string }>()
  for (const [id, links] of old.contains) {
    if (!stored.has(id)) continue
    const sorted = [...links].sort((a, b) => a.at - b.at || a.seq - b.seq)
    const pin = (p: Placed) => {
      if (p.parent === ROOT || !stored.has(p.parent) || pins.has(`${p.parent} ${id}`)) return
      pins.add(`${p.parent} ${id}`)
      pinned.push({ from: p.parent, id, at: p.at, by: p.by })
      write(p.by, { link: { source: p.parent, destination: id, link_type: 'pins', sort: { explicit: p.at } } })
    }
    // The first places it, unless this build placed it already: then every one from elsewhere is a pin.
    const where = placed.get(id)
    if (where) {
      for (const p of sorted) if (p.parent !== where.parent) pin(p)
      continue
    }
    const [first, ...later] = sorted
    made.set(id, { parent: first.parent === ROOT ? null : first.parent, at: first.at, by: first.by })
    if (first.parent !== ROOT && stored.has(first.parent)) {
      const via: Via = typeOf(first.parent) === 'note' ? 'notes' : 'comments'
      placed.set(id, { parent: first.parent, via, seq: first.seq, at: first.at, by: first.by })
      write(first.by, { link: { source: first.parent, destination: id, link_type: via, sort: { explicit: first.seq } } })
    }
    for (const p of later) pin(p)
  }
  for (const a of old.assigned) {
    if (!stored.has(a.item) || given.has(`${a.item} ${a.to}`)) continue
    given.add(`${a.item} ${a.to}`)
    assigned.push(a)
    write(a.by, { link: { source: a.item, destination: a.to, link_type: 'assignees', sort: { explicit: a.at } } })
  }
  // Owners, as the older build worked them out.
  const ownerOf = (id: string, depth = 0): string | null => {
    const own = ownerMap.get(id)
    if (own) return own
    const mk = made.get(id)
    if (!mk || depth > 50) return null
    if (isPerson(mk.by)) return mk.by
    if (mk.parent) return ownerOf(mk.parent, depth + 1)
    const bot = /^bot:(.+)$/.exec(mk.by)?.[1]
    return bot ? ownerOf(bot, depth + 1) : null
  }
  for (const id of made.keys()) {
    if (ownerMap.has(id) || !['task', 'note', 'bot', 'script'].includes(typeOf(id))) continue
    const owner = ownerOf(id)
    if (!owner) continue
    owners.push({ item: id, owner, at: made.get(id)!.at, by: 'system' })
    ownerMap.set(id, owner)
    write('system', { link: { source: owner, destination: id, link_type: 'owns', sort: { explicit: made.get(id)!.at } } })
  }
  for (const [by, list] of ops) sink.write(list, by, at)
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
export async function openStore(file: string, token: string, blocking: (type: string) => boolean, onError?: (message: string) => void, known?: (type: string) => boolean) {
  const egdb = await startEgdb(file, token)
  const client = new EgdbClient({ url: egdb.url, token })
  try {
    const { store, sink } = await loadStore(client, blocking, onError, known)
    return { store, sink, client, egdb }
  } catch (e) {
    egdb.stop()
    throw e
  }
}
