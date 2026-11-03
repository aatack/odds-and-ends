// Marks bots' folders as trusted by Claude Code (`hasTrustDialogAccepted` in its ~/.claude.json), so that `claude -p`
// in them reads a repo's `.claude/settings.json` allow-list instead of ignoring it with "this workspace has not been
// trusted". Run on the person's press (the Integrations page, by Claude).

import { existsSync, readdirSync, readFileSync, renameSync, statSync, writeFileSync } from 'node:fs'
import { homedir } from 'node:os'
import { dirname, join, resolve } from 'node:path'

/** Claude Code's own config: where it keeps which folders are trusted. */
export const claudeConfigPath = () => join(process.env.CLAUDE_CONFIG_DIR ?? homedir(), '.claude.json')

/** A folder, every git repo in it (two levels down), and the main repo of each that is a worktree. */
export function trustTargets(folder: string, depth = 2): string[] {
  const out = new Set<string>([resolve(folder)])
  const walk = (dir: string, left: number) => {
    const git = join(dir, '.git')
    if (existsSync(git)) {
      out.add(resolve(dir))
      if (statSync(git).isFile()) {
        // A worktree: `gitdir: <main>/.git/worktrees/<name>`.
        const m = /^gitdir:\s*(.+)$/m.exec(readFileSync(git, 'utf8'))
        const at = m && resolve(dir, m[1].trim())
        if (at && /[/\\]\.git[/\\]worktrees[/\\][^/\\]+$/.test(at)) out.add(dirname(dirname(dirname(at))))
      }
    }
    if (left === 0) return
    let names: string[] = []
    try {
      names = readdirSync(dir, { withFileTypes: true }).filter((d) => d.isDirectory() && d.name !== '.git' && d.name !== 'node_modules').map((d) => d.name)
    } catch {
      return
    }
    for (const n of names) walk(join(dir, n), left - 1)
  }
  if (existsSync(folder)) walk(resolve(folder), depth)
  return [...out]
}

/** Sets `hasTrustDialogAccepted` on each folder in Claude Code's config; answers those it was not set on before. */
export function trustFolders(folders: string[], config = claudeConfigPath()): string[] {
  const data = (existsSync(config) ? JSON.parse(readFileSync(config, 'utf8')) : {}) as { projects?: Record<string, Record<string, unknown>> }
  const projects = (data.projects ??= {})
  const added = folders.filter((f) => projects[f]?.hasTrustDialogAccepted !== true)
  if (added.length === 0) return []
  for (const f of added) projects[f] = { ...(projects[f] ?? {}), hasTrustDialogAccepted: true }
  // Written beside it and renamed over it, so Claude Code never reads half a file.
  const tmp = `${config}.yawn-${process.pid}.tmp`
  writeFileSync(tmp, JSON.stringify(data, null, 2))
  renameSync(tmp, config)
  return added
}
