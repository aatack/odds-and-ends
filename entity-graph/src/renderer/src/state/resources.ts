import type { ResourceRecord } from '../../../core/pensive/types'
import { atom } from './atom'

// The resource cache: the bytes behind every `type: 'file'` row on screen.
// Runtime only, like the query cache, and emphatically so — a couple of pasted
// screenshots would exhaust the localStorage quota between them.
//
// Same shape as the query engine: a view asks for an id, the engine fetches it
// once, and pointing the app at another source clears everything and refetches.
// There is no release, though: a resource costs one fetch and is worth keeping
// for as long as the session lasts, like the entity names.

export type ResourceState =
  | { status: 'loading' }
  /** `url` is a data URL, so it needs no revoking and survives being cached. */
  | { status: 'ready'; mimeType: string; name: string | null; url: string }
  | { status: 'missing' }
  | { status: 'error'; message: string }

export const resourcesAtom = atom<Record<string, ResourceState>>({})

export type ResourceFetcher = (id: string) => Promise<ResourceRecord | null>

let fetcher: ResourceFetcher | null = null
/** Every id anything has asked for, so a late-arriving source can catch up. */
const wanted = new Set<string>()

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

const patch = (id: string, state: ResourceState): void =>
  resourcesAtom.set((cache) => ({ ...cache, [id]: state }))

async function load(id: string, f: ResourceFetcher): Promise<void> {
  // Marked before the await, which is what stops a second render fetching again.
  patch(id, { status: 'loading' })
  try {
    const resource = await f(id)
    if (fetcher !== f) return
    patch(
      id,
      resource
        ? {
            status: 'ready',
            mimeType: resource.mimeType,
            name: resource.name,
            url: `data:${resource.mimeType};base64,${resource.data}`,
          }
        : { status: 'missing' },
    )
  } catch (e) {
    if (fetcher === f) patch(id, { status: 'error', message: message(e) })
  }
}

/** Fetch anything asked for that isn't loaded, in flight or already failed. */
function sync(): void {
  const f = fetcher
  if (!f) return
  const cache = resourcesAtom.get()
  for (const id of wanted) if (!cache[id]) void load(id, f)
}

/** Point the cache at a source. What's loaded belonged to the previous one. */
export function setResourceFetcher(next: ResourceFetcher | null): void {
  fetcher = next
  resourcesAtom.set({})
  sync()
}

/**
 * Ask for a resource. Idempotent and cheap enough to call on every render: rows
 * asking for the same file share one fetch, and asking before a source is open
 * is remembered until there is one.
 */
export function loadResource(id: string): void {
  wanted.add(id)
  sync()
}

/** Drop a cached resource, so whatever is on screen fetches it again. */
export const forgetResource = (id: string): void =>
  resourcesAtom.set(({ [id]: _dropped, ...rest }) => rest)
