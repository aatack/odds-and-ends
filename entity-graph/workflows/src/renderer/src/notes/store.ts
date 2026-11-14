// The notes view's tree, kept here in the window so that nothing in it waits for the backend. A note's tree is the
// items in it (`contains` links), read with `subtree`; every change is shown at once, as a pending op over what was
// read, and sent in order. An op is dropped once a read begun after it was answered has come back: that read has
// it. A note made here has a temporary id until the backend answers with its own, and every op after it is sent
// with the real one (`resolve`). One store for every root, so going into a note, or back out, shows at once what
// is already known.

import { useEffect, useRef, useSyncExternalStore } from 'react'
import type { ItemInfo } from '../../../shared/types'
import { api } from '../useWorkflows'

type Op =
  | { kind: 'create'; temp: string; parent: string; params: Record<string, unknown> }
  | { kind: 'set'; id: string; key: string; value: unknown }
  | { kind: 'open'; id: string; value: boolean | null }
  | { kind: 'move'; id: string; to: string | null }

type Pending = { op: Op; doneAt: number | null }

/** What the backend has said: every item read, and the items in each note read (in order). */
const infos = new Map<string, ItemInfo>()
const kids = new Map<string, string[]>()
let pending: Pending[] = []
/** Temporary ids, to the ids the backend gave them. */
const real = new Map<string, string>()
/** Rises with every read begun and every op answered: what a read can have seen. */
let tick = 0
let lastRead = 0
let version = 0
let error: string | null = null
const listeners = new Set<() => void>()
/** The roots on screen: read again after each op, and on every change. */
const mounted = new Map<string, number>()

const emit = () => {
  version++
  for (const l of listeners) l()
}

export const resolve = (id: string): string => real.get(id) ?? id

/** Reads a note's tree from the backend, and lays it over what was known. */
export async function load(root: string): Promise<void> {
  const started = ++tick
  const list = await api().subtree(root)
  // An older read, answered after a newer one, says nothing new.
  if (started < lastRead) return
  lastRead = started
  const byParent = new Map<string, string[]>()
  for (const i of list) {
    infos.set(i.id, i)
    if (i.type === 'topic' || i.id === root) byParent.set(i.id, [])
  }
  for (const i of list) if (i.id !== root && i.parentId) byParent.get(i.parentId)?.push(i.id)
  for (const [id, ids] of byParent) kids.set(id, ids)
  pending = pending.filter((p) => p.doneAt === null || p.doneAt > started)
  emit()
}

const reload = () => {
  for (const root of mounted.keys()) void load(root)
}

let chain: Promise<void> = Promise.resolve()

async function send(op: Op): Promise<void> {
  if (op.kind === 'create') {
    // A note made here is only its values: nobody owns it or is given it, open or not.
    const [id] = await api().addItems(resolve(op.parent), [{ type: 'topic', params: op.params, bare: true }], 'notes')
    real.set(op.temp, id)
  } else if (op.kind === 'set') await api().setValue(resolve(op.id), op.key, op.value)
  else if (op.kind === 'open') await api().setOpen(resolve(op.id), op.value)
  else await api().moveItem(resolve(op.id), op.to && resolve(op.to))
}

/** Shows a change at once, and sends it after those before it. */
function enqueue(op: Op): void {
  const p: Pending = { op, doneAt: null }
  pending = [...pending, p]
  error = null
  emit()
  chain = chain.then(async () => {
    try {
      await send(op)
      p.doneAt = ++tick
    } catch (e) {
      pending = pending.filter((q) => q !== p)
      error = (e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, '')
    }
    emit()
    reload()
  })
}

/** Makes a note in another, shown at once: answers its (temporary) id. */
export function createNote(parent: string, params: Record<string, unknown>): string {
  const temp = `new:${crypto.randomUUID()}`
  enqueue({ kind: 'create', temp, parent, params })
  return temp
}

/** Sets one value on an item (null clears it). */
export const setNoteValue = (id: string, key: string, value: unknown) => enqueue({ kind: 'set', id, key, value })

/** Sets a note's status (open, closed, or none: null) and nothing else: its owner and who has it stay as they are. */
export const setNoteOpen = (id: string, value: boolean | null) => enqueue({ kind: 'open', id, value })

/** Moves an item into another (null: into none). */
export const moveNote = (id: string, to: string | null) => enqueue({ kind: 'move', id, to })

/** Says something went wrong, as a failed change does. */
export const notesError = (message: string) => {
  error = message
  emit()
}

export const clearNotesError = () => {
  error = null
  emit()
}

/** The tree as it is to be shown: what was read, with every pending op laid over it. */
export interface NotesTree {
  info: (id: string) => ItemInfo | undefined
  kids: (id: string) => string[]
}

let shown: { version: number; tree: NotesTree } | null = null

function current(): NotesTree {
  if (shown?.version === version) return shown.tree
  const is = new Map(infos)
  const ks = new Map(kids)
  const without = (parent: string | null | undefined, id: string) => {
    if (parent && ks.has(parent)) ks.set(parent, ks.get(parent)!.filter((k) => k !== id))
  }
  for (const { op } of pending) {
    if (op.kind === 'create') {
      const id = resolve(op.temp)
      if (infos.has(id)) continue
      const parent = resolve(op.parent)
      is.set(id, placeholder(id, parent, op.params))
      ks.set(id, ks.get(id) ?? [])
      ks.set(parent, [...(ks.get(parent) ?? []), id])
    } else if (op.kind === 'set' || op.kind === 'open') {
      const [key, value] = op.kind === 'set' ? [op.key, op.value] : ['open', op.value]
      const id = resolve(op.id)
      const i = is.get(id)
      if (!i) continue
      const params = { ...i.params, [key]: value }
      if (value === null || value === undefined) delete params[key]
      is.set(id, { ...i, params, ...(op.kind === 'open' ? { closed: value === false } : {}) })
    } else {
      const id = resolve(op.id)
      const to = op.to && resolve(op.to)
      const i = is.get(id)
      if (!i || i.parentId === to) continue
      without(i.parentId, id)
      if (to) ks.set(to, [...(ks.get(to) ?? []).filter((k) => k !== id), id])
      is.set(id, { ...i, parentId: to })
    }
  }
  const tree: NotesTree = { info: (id) => is.get(resolve(id)), kids: (id) => ks.get(resolve(id)) ?? [] }
  shown = { version, tree }
  return tree
}

/** A note (a topic in another's notes) made here, until the backend has it. */
function placeholder(id: string, parent: string, params: Record<string, unknown>): ItemInfo {
  const now = Date.now()
  return {
    id, parentId: parent, via: 'notes', owner: null, type: 'topic', params, createdBy: '', createdAt: now, resolved: true, closed: params.open === false, resolvedAt: now, resolvedBy: null,
    cancelled: false, data: null, actor: '', summary: String(params.text ?? ''), message: null, nextCheckAt: null, lastError: null,
    runs: 0, pins: [], pinnedIn: [],
  }
}

const subscribe = (l: () => void) => {
  listeners.add(l)
  return () => void listeners.delete(l)
}

/** The tree from `root`, as it is to be shown, kept up to date while it is on screen; and the last failure, if any. */
export function useNotes(root: string, seed: ItemInfo): { tree: NotesTree; error: string | null } {
  // The root as the view already has it, shown until its tree is read.
  const first = useRef(seed)
  useEffect(() => {
    if (!infos.has(root)) {
      infos.set(root, first.current)
      emit()
    }
    mounted.set(root, (mounted.get(root) ?? 0) + 1)
    void load(root)
    const off = api().onChange(() => void load(root))
    return () => {
      off()
      const n = (mounted.get(root) ?? 1) - 1
      if (n) mounted.set(root, n)
      else mounted.delete(root)
    }
  }, [root])
  const tree = useSyncExternalStore(subscribe, current)
  const err = useSyncExternalStore(subscribe, () => error)
  return { tree, error: err }
}
