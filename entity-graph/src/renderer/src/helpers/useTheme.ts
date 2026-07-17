import { useCallback, useEffect, useState } from 'react'

export type Theme = 'light' | 'dark'

const KEY = 'entity-graph.theme'

const read = (): Theme => (localStorage.getItem(KEY) === 'dark' ? 'dark' : 'light')

const apply = (theme: Theme): void => {
  document.documentElement.classList.toggle('dark', theme === 'dark')
}

/**
 * The colour theme — user-specific throwaway state, so it lives in localStorage
 * and toggles synchronously, applying `.dark` to <html> for the token overrides.
 */
export function useTheme(): { theme: Theme; toggle: () => void } {
  const [theme, setTheme] = useState<Theme>(read)

  useEffect(() => {
    apply(theme)
  }, [theme])

  const toggle = useCallback(() => {
    setTheme((t) => {
      const next = t === 'dark' ? 'light' : 'dark'
      localStorage.setItem(KEY, next)
      return next
    })
  }, [])

  return { theme, toggle }
}
