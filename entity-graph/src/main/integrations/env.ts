import { existsSync } from 'fs'
import { join } from 'path'

/**
 * Secrets for the integrations live in `.env` at the root of the app —
 * gitignored, written by hand, documented in `docs/integrations.md`. The root is
 * passed in rather than resolved against this file: the main process is bundled
 * into `out/`, so there is nothing here to resolve against.
 */
export function loadEnvFile(root: string): void {
  const path = join(root, '.env')
  if (existsSync(path)) process.loadEnvFile(path)
}

/**
 * A secret an integration can't work without. Several names may be offered —
 * the first one set wins — and the error names them all, since "which variable
 * was I supposed to set?" is the only question a missing secret raises.
 */
export function requireEnv(...names: string[]): string {
  for (const name of names) {
    const value = process.env[name]?.trim()
    if (value) return value
  }
  return unset(names)
}

/** A secret that has a fallback. Blank counts as unset. */
export const optionalEnv = (name: string): string | undefined =>
  process.env[name]?.trim() || undefined

/** The message a missing secret raises, so callers with a default can reuse it. */
export function unset(names: string[]): never {
  const which = names.length === 1 ? names[0] : `one of ${names.join(', ')}`
  throw new Error(`Set ${which} in .env at the app root — see docs/integrations.md`)
}
