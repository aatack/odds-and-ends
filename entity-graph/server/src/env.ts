import { existsSync } from 'fs'
import { fileURLToPath } from 'url'

/**
 * Secrets for the integrations live in `server/.env` — gitignored, written by
 * hand, documented in `docs/integrations.md`. Anchored to the package rather
 * than the cwd so it is found however the server was started: from a shell, or
 * spawned as a child of the Electron app.
 */
const ENV_FILE = fileURLToPath(new URL('../.env', import.meta.url))

/**
 * Fold `server/.env` into the environment. Variables already set win — the app
 * passes PORT, ADMIN_TOKEN and CONFIG_DB to each local server it spawns, and a
 * stray line in the file must not override them.
 */
export function loadEnvFile(): void {
  if (existsSync(ENV_FILE)) process.loadEnvFile(ENV_FILE)
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
  throw new Error(`Set ${which} in server/.env — see server/docs/integrations.md`)
}
