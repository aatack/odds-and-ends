// The `prepare` build. npm runs it when the package is installed from a
// folder or from git, and a folder install does not install this package's
// own dev dependencies first, so install them here when TypeScript is missing.

import { execSync } from 'node:child_process'
import { existsSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'

const root = join(dirname(fileURLToPath(import.meta.url)), '..')
// The outer install's npm_* settings (prefix, save, …) must not reach the inner one.
const env = Object.fromEntries(Object.entries(process.env).filter(([key]) => !key.toLowerCase().startsWith('npm_')))
const run = (command) => execSync(command, { cwd: root, env, stdio: 'inherit' })

if (!existsSync(join(root, 'node_modules', 'typescript'))) run('npm install --ignore-scripts --no-audit --no-fund')
run('npx --no-install tsc -p tsconfig.build.json')
