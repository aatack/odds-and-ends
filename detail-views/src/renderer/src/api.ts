import type { Changed, Scan } from '../../core/graph/events.ts'
import type { LoadRequest, LoadResult, Outcome } from '../../core/types.ts'

/** The seam between the UI and the core. Nothing else reaches the core. */
export interface Api {
  /** Complete events for entities: the only read. */
  scan(ids: string[]): Promise<Scan>
  /** Loads part of an entity from its service into the cache store. */
  load(request: LoadRequest): Promise<LoadResult>
  submit(id: string, text: string): Promise<Outcome>
  perform(id: string, action: string, text: string): Promise<Outcome>
  toggle(id: string): Promise<Outcome>
  markRead(id: string): Promise<Outcome>
  /** Loads further back than anything loads on its own. */
  older(id: string): Promise<Outcome>
  /** Which entities changed in the stores; null for anything. */
  onChange(listener: (changed: Changed) => void): () => void
  /** Opens a link in my browser. */
  openExternal(url: string): void
}

interface Bridge {
  invoke(name: string, args?: unknown): Promise<unknown>
  onChange(listener: (changed: Changed) => void): () => void
  openExternal(url: string): Promise<void>
}

export function electronApi(): Api {
  const bridge = (window as unknown as { core: Bridge }).core
  const call = <T,>(name: string, args?: unknown) => bridge.invoke(name, args) as Promise<T>
  return {
    scan: (ids) => call('scan', { ids }),
    load: (request) => call('load', request),
    submit: (id, text) => call('submit', { id, text }),
    perform: (id, action, text) => call('perform', { id, action, text }),
    toggle: (id) => call('toggle', { id }),
    markRead: (id) => call('markRead', { id }),
    older: (id) => call('older', { id }),
    onChange: (listener) => bridge.onChange(listener),
    openExternal: (url) => void bridge.openExternal(url),
  }
}
