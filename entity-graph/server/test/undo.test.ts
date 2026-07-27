import { mkdtempSync, rmSync } from 'fs'
import { tmpdir } from 'os'
import { join } from 'path'
import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import type { AppEvent } from '../../src/core/events'
import { SqliteInterface } from '../../src/core/interface/sqlite'
import { SqliteSource } from '../../src/core/source/index'
import { readonly } from '../../src/core/source/filter'
import { POP_AGE_LIMIT_MS } from '../../src/core/source/permissions'

// popEvents / writeEvents: the pair the client's undo and redo are built on.
//
// Every fixture here is written relative to the clock, because popping is: the
// store never gives up an event older than POP_AGE_LIMIT_MS, so a fixture at a
// round "1000ms past the epoch" is settled history and comes off as nothing at
// all — which is a test that passes or fails for reasons of its own.

let dir: string
const now = Date.now()
const ago = (ms: number): number => now - ms

beforeAll(() => {
  dir = mkdtempSync(join(tmpdir(), 'eg-undo-'))
})
afterAll(() => rmSync(dir, { recursive: true, force: true }))

const value = (timestamp: number, entityId: string, key: string, v: unknown): AppEvent => ({
  type: 'value',
  timestamp,
  author: 'test',
  entityId,
  key,
  value: v,
})

const linkEvent = (timestamp: number, sourceId: string, destinationId: string): AppEvent => ({
  type: 'link',
  timestamp,
  author: 'test',
  sourceId,
  destinationId,
  action: 0,
})

describe('SqliteInterface.popLatestEvents', () => {
  it('takes the latest event and anything within the window, leaving the rest', async () => {
    const iface = new SqliteInterface(join(dir, 'window.db'))
    await iface.writeEvents([
      value(ago(8_000), 'a', 'text', 'first'),
      value(ago(4_000), 'a', 'text', 'second'),
      // One action, two events at (nearly) the same instant.
      value(ago(50), 'b', 'text', 'third'),
      linkEvent(ago(0), 'a', 'b'),
    ])

    const popped = await iface.popLatestEvents(100)
    expect(popped.map((e) => e.timestamp)).toEqual([ago(50), ago(0)])
    expect(popped.map((e) => e.type)).toEqual(['value', 'link'])

    const left = await iface.readAllEvents()
    expect(left.map((e) => e.timestamp).sort((x, y) => x - y)).toEqual([ago(8_000), ago(4_000)])
    iface.close()
  })

  it('groups only within the window, not across it', async () => {
    const iface = new SqliteInterface(join(dir, 'tight.db'))
    await iface.writeEvents([
      value(ago(200), 'a', 'text', 'old'),
      value(ago(0), 'a', 'text', 'new'),
    ])
    // 200ms apart: separate actions under a 100ms window.
    expect((await iface.popLatestEvents(100)).map((e) => e.value)).toEqual(['new'])
    expect((await iface.popLatestEvents(100)).map((e) => e.value)).toEqual(['old'])
    expect(await iface.readAllEvents()).toEqual([])
    iface.close()
  })

  // The behaviour that made every fixture above look fine while testing nothing:
  // undo deletes rather than compensates, so past the horizon an edit is final.
  it('leaves settled history alone, however wide the window', async () => {
    const iface = new SqliteInterface(join(dir, 'horizon.db'))
    await iface.writeEvents([
      value(ago(POP_AGE_LIMIT_MS + 60_000), 'a', 'text', 'settled'),
      value(ago(0), 'a', 'text', 'fresh'),
    ])

    // A window wide enough to cover both still stops at the horizon: the limit is
    // a floor on the cutoff, not a refusal, so the recent half comes off and the
    // rest stays put.
    const popped = await iface.popLatestEvents(POP_AGE_LIMIT_MS * 2)
    expect(popped.map((e) => e.value)).toEqual(['fresh'])

    // With nothing recent left there is nothing to undo, which is what the client
    // reports as "edits settle after five minutes" rather than "nothing to undo".
    expect(await iface.popLatestEvents(100)).toEqual([])
    expect((await iface.readAllEvents()).map((e) => e.value)).toEqual(['settled'])
    iface.close()
  })

  it('returns nothing from an empty store rather than failing', async () => {
    const iface = new SqliteInterface(join(dir, 'empty.db'))
    expect(await iface.popLatestEvents(100)).toEqual([])
    iface.close()
  })

  it('round-trips: what comes off can be written straight back', async () => {
    const iface = new SqliteInterface(join(dir, 'roundtrip.db'))
    const events = [value(ago(10), 'a', 'text', { nested: [1, null, 'x'] }), linkEvent(ago(0), 'a', 'b')]
    await iface.writeEvents([value(ago(5_000), 'a', 'text', 'kept'), ...events])

    const popped = await iface.popLatestEvents(100)
    expect(popped).toHaveLength(2)
    await iface.writeEvents(popped)

    const all = await iface.readAllEvents()
    expect(all).toHaveLength(3)
    // Timestamps and values survive verbatim — redo restores the store as it was,
    // rather than re-applying the edit at the current time.
    expect(all.find((e) => e.type === 'value' && e.timestamp === ago(10))).toMatchObject({
      value: { nested: [1, null, 'x'] },
    })
    iface.close()
  })
})

describe('popEvents / writeEvents tools', () => {
  it('are exposed by a sqlite source and undo a value write', async () => {
    const source = new SqliteSource('s', 'S', join(dir, 'tools.db'), 'test')
    expect(source.tools().map((t) => t.id)).toEqual(
      expect.arrayContaining(['popEvents', 'writeEvents'])
    )

    await source.call('writeValue', { entityId: 'a', key: 'text', value: 'hello' })
    const entities = (await source.call('readEntities', { entityIds: ['a'] })) as Record<
      string,
      { values: Record<string, unknown> }
    >
    expect(entities.a.values.text).toBe('hello')

    const popped = (await source.call('popEvents', {})) as AppEvent[]
    expect(popped).toHaveLength(1)
    const after = (await source.call('readEntities', { entityIds: ['a'] })) as Record<
      string,
      { values: Record<string, unknown> }
    >
    expect(after.a.values.text).toBeUndefined()

    // Redo.
    await source.call('writeEvents', { events: popped })
    const restored = (await source.call('readEntities', { entityIds: ['a'] })) as Record<
      string,
      { values: Record<string, unknown> }
    >
    expect(restored.a.values.text).toBe('hello')
    source.close()
  })

  it('rejects malformed events rather than storing them', async () => {
    const source = new SqliteSource('s2', 'S2', join(dir, 'bad.db'), 'test')
    await expect(
      source.call('writeEvents', { events: [{ type: 'value', entityId: 'a' }] })
    ).rejects.toThrow()
    expect(await source.call('readEvents', {})).toEqual([])
    source.close()
  })

  it('is absent from a read-only source, so a client can tell undo is unavailable', async () => {
    const source = new SqliteSource('s3', 'S3', join(dir, 'ro.db'), 'test')
    const ids = readonly('ro', 'RO', source)
      .tools()
      .map((t) => t.id)
    expect(ids).not.toContain('popEvents')
    expect(ids).not.toContain('writeEvents')
    source.close()
  })
})
