// The notes view's tree, read from egdb-client's cache (egdb.ts): each note's values and its links (its notes, in
// order; what it sits in; how many comments; who has it), loaded as the tree is opened and kept up to date by the
// change feed. A change is egdb ops, written through the cache: it shows at once, the engine checks and applies it
// (`/egdb/write`), and the cache reads back what it made of it. A note made here has its id chosen here, so there is
// nothing to swap once the backend has it.

import { useEffect, useMemo, useSyncExternalStore } from 'react'
import { op, type Egdb, type EntityView, type LinkPair, type Op, type Value } from 'egdb-client'
import { useEgdb, useEntityView } from 'egdb-client/react'
import type { ItemInfo, ItemKind } from '../../../shared/types'
import { legacyValues } from '../../../shared/legacy'
import { uuid } from '../state/cache'

/** The link groups each item in a tree is read with. */
const GROUPS: LinkPair[] = [
  ['notes', 'outbound'],
  ['comments', 'outbound'],
  ['assignees', 'outbound'],
  ['notes', 'inbound'],
  ['comments', 'inbound'],
  ['owns', 'inbound'],
]
const key = ([t, d]: LinkPair) => `${t}\u0000${d}`
/** The values an item's params leave out: how it stands, not what it was given (as engine/store.ts's STATE_KEYS). */
const STATE_KEYS = new Set(['type', 'resolved', 'resolved_at', 'resolved_by', 'cancelled', 'data', 'state', 'reactions'])

let error: string | null = null
const listeners = new Set<() => void>()
const emit = () => {
  for (const l of listeners) l()
}
/** The egdb of the tree on screen: what the changes below write through. */
let active: Egdb | null = null

/** Ids are chosen here, so they are what the backend has: nothing to resolve. Kept for the view's paths. */
export const resolve = (id: string): string => id

/** Writes ops through the cache (shown at once); a refusal takes them off and says why. */
function write(ops: Op[]): void {
  if (!active || !ops.length) return
  error = null
  emit()
  active.write(ops).catch((e: unknown) => {
    error = (e as Error).message
    emit()
  })
}

/** Makes a note in another, shown at once: answers its id. */
export function createNote(parent: string, params: Record<string, unknown>): string {
  const id = uuid()
  const values = Object.entries(params).filter(([, v]) => v !== undefined && v !== null)
  write([op.set(id, 'type', 'topic'), ...values.map(([k, v]) => op.set(id, k, v as Value)), op.link(parent, id, 'notes')])
  return id
}

/** Sets one value on an item (null clears it). */
export const setNoteValue = (id: string, key: string, value: unknown) => write([op.set(id, key, (value ?? null) as Value)])

/** Sets a note's status (open, closed, or none: null) and nothing else: its owner and who has it stay as they are. */
export const setNoteOpen = (id: string, value: boolean | null) => write([op.set(id, 'open', value)])

/** Moves an item into another's notes (null: into none), out of wherever it sits now. */
export function moveNote(id: string, to: string | null): void {
  const at = active && placeOf(active.view(), id)
  if (at?.parent === to && at?.via === 'notes') return
  write([...(at ? [op.unlink(at.parent, id, at.via)] : []), ...(to ? [op.link(to, id, 'notes')] : [])])
}

/** Says something went wrong, as a failed change does. */
export const notesError = (message: string) => {
  error = message
  emit()
}

export const clearNotesError = () => {
  error = null
  emit()
}

/** The tree as it is to be shown: what is loaded, with the changes not yet answered laid over it. */
export interface NotesTree {
  info: (id: string) => ItemInfo | undefined
  kids: (id: string) => string[]
}

/** What an item sits in, by which link: its notes or its comments. */
function placeOf(view: EntityView, id: string): { parent: string; via: 'notes' | 'comments' } | null {
  const links = view.get(id)?.links ?? {}
  for (const via of ['notes', 'comments'] as const) {
    const l = links[key([via, 'inbound'])]?.[0]
    if (l) return { parent: l.source, via }
  }
  return null
}

const others = (view: EntityView, id: string, pair: LinkPair) =>
  (view.get(id)?.links[key(pair)] ?? []).map((l) => (pair[1] === 'outbound' ? l.destination : l.source))

/** One line saying what an item is: a topic's text; anything else its title or name, as its type's `describe` mostly does. */
function describe(type: string, p: Record<string, unknown>): string {
  if (type === 'topic') return String(p.text ?? '')
  return String(p.title || p.name || p.username || p.url || p.path || p.text || type)
}

/** An item as the view reads it, from its record; undefined while its values aren't loaded. */
function infoOf(view: EntityView, id: string): ItemInfo | undefined {
  const raw = view.get(id)?.values
  if (!raw?.type) return undefined
  const x = legacyValues(raw as Record<string, unknown>)
  const params = Object.fromEntries(Object.entries(x).filter(([k]) => !STATE_KEYS.has(k)))
  const type = String(x.type) as ItemKind
  const place = placeOf(view, id)
  // Made when (and by whom) it was put where it sits.
  const made = place && view.get(id)?.links[key([place.via, 'inbound'])]?.[0]?.event
  const closed = type === 'topic' ? x.open === false : x.resolved === true
  const summary = describe(type, params)
  return {
    id, parentId: place?.parent ?? null, via: place?.via ?? null, owner: others(view, id, ['owns', 'inbound'])[0] ?? null, type, params,
    createdBy: made?.author ?? '', createdAt: made?.timestamp ?? 0, resolved: closed, closed, resolvedAt: null, resolvedBy: null,
    cancelled: false, data: null, actor: made?.author ?? '', summary, message: type === 'topic' ? summary : null,
    nextCheckAt: null, lastError: null, runs: 0, pins: [], pinnedIn: [],
    comments: others(view, id, ['comments', 'outbound']).length, notes: others(view, id, ['notes', 'outbound']).length,
    assignees: others(view, id, ['assignees', 'outbound']),
  }
}

const subscribe = (l: () => void) => {
  listeners.add(l)
  return () => void listeners.delete(l)
}

/**
 * The tree from `root`, as it is to be shown, loading what it lacks: every item reached through `notes` links from
 * the root (below it, only topics' notes), each with its link groups. Until the root is loaded, it is `seed`.
 */
export function useNotes(root: string, seed: ItemInfo): { tree: NotesTree; error: string | null } {
  const egdb = useEgdb()
  active = egdb
  const view = useEntityView()
  const { tree, missing } = useMemo(() => {
    const infos = new Map<string, ItemInfo>()
    const kids = new Map<string, string[]>()
    const missing: string[] = []
    const seen = new Set<string>()
    const visit = (id: string) => {
      if (seen.has(id)) return
      seen.add(id)
      const record = view.get(id)
      if (!record?.values || (!record.local && GROUPS.some((g) => !record.links[key(g)]))) missing.push(id)
      const info = infoOf(view, id)
      if (info) infos.set(id, info)
      const ks = id === root || info?.type === 'topic' ? others(view, id, ['notes', 'outbound']) : []
      kids.set(id, ks)
      ks.forEach(visit)
    }
    visit(root)
    if (!infos.has(root)) infos.set(root, seed)
    const tree: NotesTree = { info: (id) => infos.get(id) ?? infoOf(view, id), kids: (id) => kids.get(id) ?? others(view, id, ['notes', 'outbound']) }
    return { tree, missing }
    // The seed only stands in until the root is loaded.
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [view, root])
  const missingKey = missing.join(',')
  useEffect(() => {
    if (missing.length) void egdb.load(missing, GROUPS)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [egdb, missingKey])
  const err = useSyncExternalStore(subscribe, () => error)
  return { tree, error: err }
}
