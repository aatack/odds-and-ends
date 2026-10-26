// Where a source keeps what goes with it, and how to find a backend running on
// one: shared by the backend and the app, which starts one when none is.
//
// A source is one file: the store (egdb's SQLite). Beside it, `<file>.data/`
// holds the rest: the secrets' key, the bots' folders, and while a backend runs
// on it, its backend.json and lock.

import { existsSync, readFileSync } from 'node:fs'
import { homedir } from 'node:os'
import { join, resolve } from 'node:path'

/** A source's file as typed (`~/` is the home folder; anything else is from the current folder), as an absolute path. */
export function sourceFile(typed: string): string {
  const t = typed.trim()
  if (!t) throw new Error('Say which file: a path, such as ~/tasks/tasks.sqlite.')
  return resolve(t === '~' ? homedir() : t.startsWith('~/') ? join(homedir(), t.slice(2)) : t)
}

/** The folder beside a source that holds the rest of what goes with it. */
export const dataDirOf = (file: string): string => `${file}.data`

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

