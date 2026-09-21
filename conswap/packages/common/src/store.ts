type Listener = () => void

export interface Entry<T> {
  data: T | null
  error: string | null
  loading: boolean
  /** Bumped on every change, so a snapshot can be told apart from the last one. */
  version: number
}

const empty: Entry<never> = { data: null, error: null, loading: false, version: 0 }

/**
 * A cache keyed by string, with optimistic writes that can be taken back. There is
 * no React in here: it is a plain observable map.
 */
export class Store {
  private entries = new Map<string, Entry<unknown>>()
  private listeners = new Map<string, Set<Listener>>()
  private inFlight = new Map<string, Promise<unknown>>()
  private fetchers = new Map<string, () => Promise<unknown>>()

  get<T>(key: string): Entry<T> {
    return (this.entries.get(key) as Entry<T> | undefined) ?? (empty as unknown as Entry<T>)
  }

  subscribe(key: string, listener: Listener): () => void {
    const set = this.listeners.get(key) ?? new Set<Listener>()
    set.add(listener)
    this.listeners.set(key, set)
    return () => {
      set.delete(listener)
    }
  }

  private announce(key: string): void {
    for (const listener of this.listeners.get(key) ?? []) listener()
  }

  private write<T>(key: string, patch: Partial<Entry<T>>): void {
    const previous = this.get<T>(key)
    this.entries.set(key, { ...previous, ...patch, version: previous.version + 1 })
    this.announce(key)
  }

  async load<T>(key: string, fetcher: () => Promise<T>, force = false): Promise<void> {
    this.fetchers.set(key, fetcher as () => Promise<unknown>)
    if (this.inFlight.has(key)) return
    if (!force && this.get<T>(key).data !== null) return
    this.write<T>(key, { loading: true })
    const promise = fetcher()
      .then((data) => {
        this.write<T>(key, { data, error: null, loading: false })
        return data as unknown
      })
      .catch((error: unknown) => {
        this.write<T>(key, { error: String(error instanceof Error ? error.message : error), loading: false })
        return null
      })
      .finally(() => {
        this.inFlight.delete(key)
      })
    this.inFlight.set(key, promise)
    await promise
  }

  /** Refetches anything we are holding that the given keys cover. */
  invalidate(keys: string[]): void {
    for (const key of keys) {
      const fetcher = this.fetchers.get(key)
      if (fetcher) void this.load(key, fetcher, true)
    }
  }

  keys(): string[] {
    return [...this.entries.keys()]
  }

  /**
   * Applies a change to the cache straight away and hands back the undo, so a
   * failed write puts the screen back exactly as it was.
   */
  patch<T>(key: string, change: (data: T) => T): () => void {
    const previous = this.get<T>(key)
    if (previous.data === null) return () => undefined
    const before = previous.data
    this.write<T>(key, { data: change(before) })
    return () => this.write<T>(key, { data: before })
  }
}
