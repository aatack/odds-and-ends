/**
 * The world outside the app: where a per-device preference is kept, and what the
 * machine itself would rather look like. Injected so the session never touches a
 * browser API directly and still runs with no DOM at all.
 */
export interface Environment {
  read(key: string): string | null
  write(key: string, value: string): void
  prefersDark(): boolean
}

/** The default. Remembers nothing, which is what a test wants. */
export function headlessEnvironment(): Environment {
  const values = new Map<string, string>()
  return {
    read: (key) => values.get(key) ?? null,
    write: (key, value) => {
      values.set(key, value)
    },
    prefersDark: () => true,
  }
}

/** Preferences in localStorage, appearance from the operating system. */
export const browserEnvironment: Environment = {
  read(key) {
    try {
      return window.localStorage.getItem(`conswap:${key}`)
    } catch {
      return null
    }
  },
  write(key, value) {
    try {
      window.localStorage.setItem(`conswap:${key}`, value)
    } catch {
      // A machine that will not keep preferences still runs fine without them.
    }
  },
  prefersDark() {
    try {
      return window.matchMedia('(prefers-color-scheme: dark)').matches
    } catch {
      return true
    }
  },
}
