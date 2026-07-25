import { mkdtempSync, rmSync } from 'fs'
import { tmpdir } from 'os'
import { join } from 'path'
import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import type { AppEvent } from '../../src/core/events'
import { SqliteInterface } from '../../src/core/interface/sqlite'
import { SqliteSource } from '../../src/core/source/index'
import { readonly } from '../../src/core/source/filter'

// popEvents / writeEvents: the pair the client's undo and redo are built on.

let dir: string

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
      value(1_000, 'a', 'text', 'first'),
      value(5_000, 'a', 'text', 'second'),
      // One action, two events at (nearly) the same instant.
      value(9_000, 'b', 'text', 'third'),
      linkEvent(9_050, 'a', 'b'),
    ])

    const popped = await iface.popLatestEvents(100)
    expect(popped.map((e) => e.timestamp)).toEqual([9_000, 9_050])
    expect(popped.map((e) => e.type)).toEqual(['value', 'link'])

    const left = await iface.readAllEvents()
    expect(left.map((e) => e.timestamp).sort((x, y) => x - y)).toEqual([1_000, 5_000])
    iface.close()
  })

  it('groups only within the window, not across it', async () => {
    const iface = new SqliteInterface(join(dir, 'tight.db'))
    await iface.writeEvents([
      value(1_000, 'a', 'text', 'old'),
      value(1_200, 'a', 'text', 'new'),
    ])
    // 200ms apart: separate actions under a 100ms window.
    expect((await iface.popLatestEvents(100)).map((e) => e.value)).toEqual(['new'])
    expect((await iface.popLatestEvents(100)).map((e) => e.value)).toEqual(['old'])
    expect(await iface.readAllEvents()).toEqual([])
    iface.close()
  })

  it('returns nothing from an empty store rather than failing', async () => {
    const iface = new SqliteInterface(join(dir, 'empty.db'))
    expect(await iface.popLatestEvents(100)).toEqual([])
    iface.close()
  })

  it('round-trips: what comes off can be written straight back', async () => {
    const iface = new SqliteInterface(join(dir, 'roundtrip.db'))
    const events = [value(2_000, 'a', 'text', { nested: [1, null, 'x'] }), linkEvent(2_010, 'a', 'b')]
    await iface.writeEvents([value(1_000, 'a', 'text', 'kept'), ...events])

    const popped = await iface.popLatestEvents(100)
    await iface.writeEvents(popped)

    const all = await iface.readAllEvents()
    expect(all).toHaveLength(3)
    // Timestamps and values survive verbatim — redo restores the store as it was,
    // rather than re-applying the edit at the current time.
    expect(all.find((e) => e.type === 'value' && e.timestamp === 2_000)).toMatchObject({
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
