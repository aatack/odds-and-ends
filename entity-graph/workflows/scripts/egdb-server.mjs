// Builds egdb-server (the store's server) and egdb's TypeScript client when
// they are missing or older than egdb's sources: `npm start` runs this first,
// so starting the app is one command, and a change to egdb is picked up. Set
// EGDB_SERVER to use a binary of your own instead.

import { execFileSync } from 'node:child_process'
import { existsSync, readdirSync, statSync } from 'node:fs'
import { homedir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'

const egdb = fileURLToPath(new URL('../../../egdb', import.meta.url))
const force = process.argv.includes('--force')

/** The newest modification time under `paths` (files, or folders walked), or 0. */
const newest = (paths) =>
  Math.max(
    0,
    ...paths.filter(existsSync).map((p) =>
      statSync(p).isDirectory() ? newest(readdirSync(p).map((f) => join(p, f))) : statSync(p).mtimeMs,
    ),
  )
const stale = (built, sources) => force || !existsSync(built) || newest(sources) > statSync(built).mtimeMs

const built = join(egdb, 'target', 'release', 'egdb-server')
if (!process.env.EGDB_SERVER && stale(built, [join(egdb, 'src'), join(egdb, 'Cargo.toml'), join(egdb, 'Cargo.lock')])) {
  // rustup's cargo, where there is one: a system cargo may be too old for egdb's lockfile.
  const rustup = join(homedir(), '.cargo', 'bin', 'cargo')
  const cargo = existsSync(rustup) ? rustup : 'cargo'
  console.log(`Building egdb-server with ${cargo}…`)
  execFileSync(cargo, ['build', '--release', '--features', 'server', '--bin', 'egdb-server'], { cwd: egdb, stdio: 'inherit' })
}

const client = join(egdb, 'client')
if (stale(join(client, 'dist', 'index.js'), [join(client, 'src')])) {
  console.log('Building egdb-client…')
  execFileSync(process.execPath, [join(client, 'scripts', 'build.mjs')], { cwd: client, stdio: 'inherit' })
}
