import type { Entity, Focus, ModuleInfo } from '../../core/types.ts'

/** The seam between the UI and the core. Nothing else reaches the core. */
export interface Api {
  modules(): Promise<ModuleInfo[]>
  focus(id: string): Promise<Focus>
  summaries(ids: string[]): Promise<Record<string, Entity | null>>
  refresh(id: string): Promise<void>
  submit(id: string, text: string): Promise<void>
  perform(id: string, action: string, text: string): Promise<void>
  toggle(id: string): Promise<void>
  markRead(id: string): Promise<void>
  onChange(listener: () => void): () => void
  /** Opens a link in my browser. */
  openExternal(url: string): void
}

interface Bridge {
  invoke(name: string, args?: unknown): Promise<unknown>
  onChange(listener: () => void): () => void
  openExternal(url: string): Promise<void>
}

export function electronApi(): Api {
  const bridge = (window as unknown as { core: Bridge }).core
  const call = <T,>(name: string, args?: unknown) => bridge.invoke(name, args) as Promise<T>
  return {
    modules: () => call('modules'),
    focus: (id) => call('focus', { id }),
    summaries: (ids) => call('summaries', { ids }),
    refresh: (id) => call('refresh', { id }),
    submit: (id, text) => call('submit', { id, text }),
    perform: (id, action, text) => call('perform', { id, action, text }),
    toggle: (id) => call('toggle', { id }),
    markRead: (id) => call('markRead', { id }),
    onChange: (listener) => bridge.onChange(listener),
    openExternal: (url) => void bridge.openExternal(url),
  }
}
