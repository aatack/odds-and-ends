// `npm start`: build once, then run the app from a snapshot of that build (.run/), not from out/. Builds that happen
// while it runs (bots checking their work write out/) then change nothing under the running app: it picks up new
// code only when it is started again.

import { spawn } from 'node:child_process'
import { cpSync, rmSync } from 'node:fs'
import { createRequire } from 'node:module'
import { fileURLToPath } from 'node:url'

const root = fileURLToPath(new URL('..', import.meta.url))
const run = (cmd, args) =>
  new Promise((resolve, reject) =>
    spawn(cmd, args, { cwd: root, stdio: 'inherit' }).on('exit', (code) => (code === 0 ? resolve() : reject(new Error(`${cmd} exited ${code}`)))),
  )

await run(process.execPath, [createRequire(import.meta.url).resolve('electron-vite/bin/electron-vite.js'), 'build'])
rmSync(`${root}.run`, { recursive: true, force: true })
cpSync(`${root}out`, `${root}.run`, { recursive: true })
const electron = createRequire(import.meta.url)('electron')
spawn(electron, [`${root}.run/main/index.js`, '--no-sandbox', ...process.argv.slice(2)], { cwd: root, stdio: 'inherit' }).on('exit', (code) => process.exit(code ?? 0))
