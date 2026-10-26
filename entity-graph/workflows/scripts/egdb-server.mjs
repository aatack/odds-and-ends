// Builds egdb-server (the store's server) when there is none yet: `npm start`
// runs this first, so starting the app is one command. Set EGDB_SERVER to use a
// binary of your own instead.

import { execFileSync } from 'node:child_process'
import { existsSync } from 'node:fs'
import { homedir } from 'node:os'
import { join } from 'node:path'
import { fileURLToPath } from 'node:url'

const egdb = fileURLToPath(new URL('../../../egdb', import.meta.url))
const built = join(egdb, 'target', 'release', 'egdb-server')
if (!process.argv.includes('--force') && (process.env.EGDB_SERVER || existsSync(built))) process.exit(0)

// rustup's cargo, where there is one: a system cargo may be too old for egdb's lockfile.
const rustup = join(homedir(), '.cargo', 'bin', 'cargo')
const cargo = existsSync(rustup) ? rustup : 'cargo'
console.log(`Building egdb-server with ${cargo} (once)…`)
execFileSync(cargo, ['build', '--release', '--features', 'server', '--bin', 'egdb-server'], { cwd: egdb, stdio: 'inherit' })
