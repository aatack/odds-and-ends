import { canCall } from '../source/connection'
import { readResource } from '../source/entity'
import { atom } from './atom'

// The bytes behind a `type: 'file'` row. Runtime only and never persisted — two
// pasted screenshots would exhaust the localStorage quota between them.
//
// A resource is stored under the id of the entity that describes it, so there is
// no reference to keep in step, and the source only exposes the tools when its
// store can hold bytes at all: absence is how the client knows.

export type ResourceState =
  | { status: 'loading' }
  | { status: 'ready'; mimeType: string; name: string | null; dataUrl: string }
  | { status: 'missing' }
  | { status: 'error'; message: string }

export const resourcesAtom = atom<Record<string, ResourceState>>({})

const inFlight = new Set<string>()

/** Ask for an entity's bytes. Idempotent: several rows share one fetch. */
export function loadResource(id: string): void {
  if (inFlight.has(id) || resourcesAtom.get()[id]) return
  if (!canCall('readResource')) {
    // Settled rather than left loading: a source that can't hold bytes never will,
    // and a row that says "loading…" for ever is the worse of the two answers.
    resourcesAtom.set((r) => ({ ...r, [id]: { status: 'missing' } }))
    return
  }
  inFlight.add(id)
  resourcesAtom.set((r) => ({ ...r, [id]: { status: 'loading' } }))
  void readResource(id)
    .then((record) => {
      resourcesAtom.set((r) => ({
        ...r,
        [id]: record
          ? {
              status: 'ready',
              mimeType: record.mimeType,
              name: record.name,
              // A data URL rather than a blob URL: nothing here has a lifetime to
              // manage, and the cache is dropped wholesale on reload anyway.
              dataUrl: `data:${record.mimeType};base64,${record.data}`,
            }
          : { status: 'missing' },
      }))
    })
    .catch((e: unknown) => {
      resourcesAtom.set((r) => ({
        ...r,
        [id]: { status: 'error', message: e instanceof Error ? e.message : String(e) },
      }))
    })
    .finally(() => inFlight.delete(id))
}
