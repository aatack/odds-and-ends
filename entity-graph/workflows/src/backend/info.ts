// Where a backend keeps its data, and how to find one that is running: shared
// by the backend and the app, which starts one when none is.

import { existsSync, readFileSync } from 'node:fs'
import { homedir } from 'node:os'
import { join } from 'node:path'

/** Where the app keeps its data: Electron's userData for this app, unless told otherwise. */
export function defaultDataDir(): string {
  if (process.env.WORKFLOWS_DATA) return process.env.WORKFLOWS_DATA
  const config = process.platform === 'darwin' ? join(homedir(), 'Library', 'Application Support') : process.env.XDG_CONFIG_HOME || join(homedir(), '.config')
  return join(config, 'workflows')
}

export interface BackendInfo {
  url: string
  token: string
  pid: number
}

/** The backend.json of a backend that is running, if one is. */
export async function runningBackend(dir: string): Promise<BackendInfo | null> {
  const file = join(dir, 'backend.json')
  if (!existsSync(file)) return null
  try {
    const info = JSON.parse(readFileSync(file, 'utf8')) as BackendInfo
    const res = await fetch(`${info.url}/health`, { signal: AbortSignal.timeout(2000) })
    return res.ok ? info : null
  } catch {
    return null
  }
}

