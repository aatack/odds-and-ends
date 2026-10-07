import type { AppEvent, Changed, Scan } from '../../core/graph/events.ts'
import type { LoadRequest, LoadResult, NoteValues, Outcome } from '../../core/types.ts'

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
  /** Takes a child out from under its parent, as an owned event. */
  unlink(parent: string, child: string): Promise<Outcome>
  link(parent: string, child: string): Promise<Outcome>
  move(child: string, from: string, to: string): Promise<Outcome>
  /** A note (no type, just text) under `parent`. */
  create(parent: string, text: string, values?: NoteValues): Promise<Outcome>
  setValue(id: string, key: string, value: unknown): Promise<Outcome>
  /** My text for an item, over whatever its service says. */
  setText(id: string, text: string): Promise<Outcome>
  /** Takes my last action off the owned log; returns what came off. */
  undo(): Promise<Outcome>
  /** Writes undone events back, as they were. */
  redo(events: AppEvent[]): Promise<Outcome>
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

/**
 * The core over HTTP (`src/main/phone.ts`): the phone's way in. The same
 * actions as over IPC, a POST each; changes come as Server-Sent Events.
 */
export function httpApi(base: string, token: string): Api {
  const call = async <T,>(name: string, args?: unknown): Promise<T> => {
    const response = await fetch(`${base}/api/${name}`, {
      method: 'POST',
      headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' },
      body: JSON.stringify(args ?? null),
    })
    const answer = (await response.json()) as T & { error?: string }
    if (!response.ok) throw new Error(answer?.error ?? `${name}: ${response.status}`)
    return answer
  }
  return {
    scan: (ids) => call('scan', { ids }),
    load: (request) => call('load', request),
    submit: (id, text) => call('submit', { id, text }),
    perform: (id, action, text) => call('perform', { id, action, text }),
    toggle: (id) => call('toggle', { id }),
    markRead: (id) => call('markRead', { id }),
    older: (id) => call('older', { id }),
    unlink: (parent, child) => call('unlink', { parent, child }),
    link: (parent, child) => call('link', { parent, child }),
    move: (child, from, to) => call('move', { child, from, to }),
    create: (parent, text, values) => call('create', { parent, text, values }),
    setValue: (id, key, value) => call('setValue', { id, key, value }),
    setText: (id, text) => call('setText', { id, text }),
    undo: () => call('undo'),
    redo: (events) => call('redo', { events }),
    onChange: (listener) => {
      // EventSource reconnects by itself; a reconnect may have missed changes, so it re-reads everything.
      const events = new EventSource(`${base}/api/changes?token=${encodeURIComponent(token)}`)
      let opened = false
      events.onopen = () => {
        if (opened) listener(null)
        opened = true
      }
      events.onmessage = (event) => listener(JSON.parse(event.data) as Changed)
      return () => events.close()
    },
    openExternal: (url) => void window.open(url, '_blank', 'noopener'),
  }
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
    unlink: (parent, child) => call('unlink', { parent, child }),
    link: (parent, child) => call('link', { parent, child }),
    move: (child, from, to) => call('move', { child, from, to }),
    create: (parent, text, values) => call('create', { parent, text, values }),
    setValue: (id, key, value) => call('setValue', { id, key, value }),
    setText: (id, text) => call('setText', { id, text }),
    undo: () => call('undo'),
    redo: (events) => call('redo', { events }),
    onChange: (listener) => bridge.onChange(listener),
    openExternal: (url) => void bridge.openExternal(url),
  }
}
