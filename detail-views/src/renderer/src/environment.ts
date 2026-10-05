/** The only place that touches browser storage. */
export interface Environment {
  load(key: string): unknown
  save(key: string, value: unknown): void
}

export function browserEnvironment(): Environment {
  return {
    load(key) {
      try {
        const raw = localStorage.getItem(key)
        return raw === null ? undefined : JSON.parse(raw)
      } catch {
        return undefined
      }
    },
    save(key, value) {
      try {
        localStorage.setItem(key, JSON.stringify(value))
      } catch {
        // Losing the trail on reload is fine.
      }
    },
  }
}

export function memoryEnvironment(): Environment {
  const values = new Map<string, unknown>()
  return { load: (key) => values.get(key), save: (key, value) => void values.set(key, value) }
}
