// @vitest-environment jsdom
import { act, renderHook, waitFor } from '@testing-library/react'
import type { ReactNode } from 'react'
import { afterAll, beforeAll, describe, expect, it } from 'vitest'
import { f } from '../src/filter.js'
import { op } from '../src/ops.js'
import { EgdbProvider, useFind, useHistory, useWalk, useWrite } from '../src/react/index.js'
import { Egdb } from '../src/store.js'
import type { WalkQuery } from '../src/walk.js'
import { haveServer, startServer, treeOps, type Running } from './server.js'

const query: WalkQuery = { follow: { list: [['child', 'outbound']] } }

describe.skipIf(!haveServer)('hooks', () => {
  let server: Running
  let branch: string
  let order: string[]

  beforeAll(async () => {
    server = await startServer()
    branch = await server.client.createBranch('main')
    await server.client.write({ branch, author: 'test', ops: treeOps(5, 6) })
    const all = await server.client.walk(branch, { start: ['root'], follow: query.follow, options: { steps: 100_000 } })
    order = all.rows.map((r) => r.path.join('/'))
    expect(order.length).toBeGreaterThan(1000)
  })
  afterAll(() => server?.stop())

  const wrapper = (egdb: Egdb) =>
    function Wrapper({ children }: { children?: ReactNode }) {
      return <EgdbProvider egdb={egdb}>{children}</EgdbProvider>
    }
  const fresh = () => new Egdb({ client: server.client, branch, author: 'test' })

  it('useWalk renders at once, fills in, and shows writes at once', async () => {
    const egdb = fresh()
    const { result } = renderHook(
      () => ({ walk: useWalk({ root: ['root'], follow: query.follow, limit: 40 }), writer: useWrite() }),
      { wrapper: wrapper(egdb) },
    )
    expect(result.current.walk.rows[0]).toMatchObject({ id: 'root', status: 'loading' })
    await waitFor(() => expect(result.current.walk.loading).toBe(false))
    expect(result.current.walk.rows.map((r) => r.path.join('/'))).toEqual(order.slice(0, 40))

    let done!: Promise<unknown>
    act(() => {
      done = result.current.writer.write([op.set('root.0', 'text', 'renamed'), op.unlink('root', 'root.1', 'child')])
    })
    // Before the server answers.
    expect(result.current.walk.rows[1]!.values?.text).toBe('renamed')
    expect(result.current.walk.rows.some((r) => r.id === 'root.1')).toBe(false)
    await act(() => done)
    expect(result.current.walk.rows[1]!.values?.text).toBe('renamed')
    await act(() => egdb.write(op.link('root', 'root.1', 'child', { explicit: 1 })))
  })

  it('folding redraws from the cache, with no request', async () => {
    const egdb = fresh()
    let collapsed: string[][] = []
    const { result, rerender } = renderHook(
      () => useWalk({ root: ['root'], follow: query.follow, collapsed, limit: 30 }),
      { wrapper: wrapper(egdb) },
    )
    await waitFor(() => expect(result.current.loading).toBe(false))
    let calls = 0
    const walk = egdb.client.walk.bind(egdb.client)
    egdb.client.walk = (...args) => (calls++, walk(...args))
    collapsed = [['root', 'root.0']]
    rerender()
    expect(result.current.rows.map((r) => r.id).slice(0, 3)).toEqual(['root', 'root.0', 'root.1'])
    expect(result.current.rows[1]!.collapsed).toBe(true)
    // The rows past the old window may need a preload; the fold itself did not.
    expect(result.current.rows.length).toBe(30)
    expect(calls).toBeLessThanOrEqual(1)
  })

  it('useWalk with a range keeps the anchor near the range and rebases at the top', async () => {
    const egdb = fresh()
    let range = { start: 0, end: 30 }
    const shifts: number[] = []
    const { result, rerender } = renderHook(
      () => useWalk({ root: ['root'], follow: query.follow, range, onShift: (s) => shifts.push(s) }),
      { wrapper: wrapper(egdb) },
    )
    await waitFor(() => expect(result.current.loading).toBe(false))
    expect(result.current.rowAt(0)!.id).toBe('root')
    const paths = (from: number, to: number) =>
      Array.from({ length: to - from }, (_, i) => result.current.rowAt(from + i)?.path.join('/'))

    // Scrolling inside a chunk calculates nothing again.
    const rows = result.current.rows
    range = { start: 2, end: 30 }
    rerender()
    expect(result.current.rows).toBe(rows)

    // Scroll down in steps, as a table would.
    for (let start = 20; start <= 600; start += 20) {
      range = { start, end: start + 30 }
      rerender()
      await waitFor(() => expect(result.current.loading).toBe(false))
    }
    await waitFor(() => expect(paths(600, 630)).toEqual(order.slice(600, 630)))
    expect(result.current.anchorIndex).toBeGreaterThan(400)
    expect(result.current.count).toBeGreaterThan(630)

    // Fold a big subtree far above the screen: the rows on screen stay where they are.
    const before = paths(600, 630)
    const anchorIndex = result.current.anchorIndex
    const { result: folded, rerender: refold } = renderHook(
      () =>
        useWalk({
          root: ['root'],
          follow: query.follow,
          collapsed: [['root', 'root.0']],
          range,
          initialAnchor: { path: result.current.anchor, index: anchorIndex },
          onShift: (s) => shifts.push(s),
        }),
      { wrapper: wrapper(egdb) },
    )
    await waitFor(() => expect(folded.current.loading).toBe(false))
    expect(Array.from({ length: 30 }, (_, i) => folded.current.rowAt(600 + i)?.path.join('/'))).toEqual(before)

    // Scroll back to the top: the folded rows are gone, so indices shift up to 0.
    for (let start = 580; start >= 0; start -= 20) {
      range = { start, end: start + 30 }
      refold()
      await waitFor(() => expect(folded.current.loading).toBe(false))
      if (shifts.length > 0) break
    }
    expect(shifts.length).toBeGreaterThan(0)
    expect(shifts[0]).toBeLessThan(0)
  })

  it('redo sends an undone batch again', async () => {
    const egdb = fresh()
    const { result } = renderHook(
      () => ({ walk: useWalk({ root: ['root'], follow: query.follow, limit: 3 }), writer: useWrite(), history: useHistory() }),
      { wrapper: wrapper(egdb) },
    )
    await waitFor(() => expect(result.current.walk.loading).toBe(false))
    const text = () => result.current.walk.rows[1]!.values?.text
    const original = text()
    await act(() => result.current.writer.set('root.0', 'text', 'v1'))
    await act(() => result.current.writer.set('root.0', 'text', 'v2'))
    expect(result.current.history).toEqual({ undo: 2, redo: 0 })
    await act(() => result.current.writer.undo())
    await act(() => result.current.writer.undo())
    expect(text()).toBe(original)
    expect(result.current.history).toEqual({ undo: 0, redo: 2 })
    let redone!: Promise<unknown>
    act(() => {
      redone = result.current.writer.redo()!
    })
    expect(text()).toBe('v1') // optimistic
    await act(() => redone)
    await act(() => result.current.writer.redo()!)
    expect(text()).toBe('v2')
    expect(result.current.history).toEqual({ undo: 2, redo: 0 })
    // A new write clears what can be redone.
    await act(() => result.current.writer.undo())
    await act(() => result.current.writer.set('root.0', 'text', original as string))
    expect(result.current.history).toEqual({ undo: 2, redo: 0 })
  })

  it('useFind pages and takes in local writes', async () => {
    const egdb = fresh()
    const { result } = renderHook(
      () => ({ find: useFind({ filter: f.eq('type', 'list'), limit: 10, pageSize: 5 }), writer: useWrite() }),
      { wrapper: wrapper(egdb) },
    )
    await waitFor(() => expect(result.current.find.rows).toHaveLength(10))
    expect(result.current.find.rows.every((r) => r.values?.type === 'list')).toBe(true)
    const first = result.current.find.rows[0]!.id
    const writes: Promise<unknown>[] = []
    act(() => {
      writes.push(result.current.writer.write([op.set('!first', 'type', 'list')]))
    })
    expect(result.current.find.rows[0]!.id).toBe('!first')
    act(() => {
      writes.push(result.current.writer.set(first, 'type', 'task'))
    })
    expect(result.current.find.rows.some((r) => r.id === first)).toBe(false)
    await act(() => Promise.all(writes))
    expect(result.current.find.rows[0]!.id).toBe('!first')
    expect(result.current.find.rows.some((r) => r.id === first)).toBe(false)
  })
})
