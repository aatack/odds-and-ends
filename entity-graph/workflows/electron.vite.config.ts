import { execSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'
import { defineConfig, externalizeDepsPlugin } from 'electron-vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

// egdb's client is used from its source, so a pull needs no rebuild of it, and is bundled: it is ESM only.
const egdbClient = fileURLToPath(new URL('../../egdb/client/src/index.ts', import.meta.url))

export default defineConfig({
  main: {
    plugins: [externalizeDepsPlugin({ exclude: ['egdb-client'] })],
    resolve: { alias: [{ find: /^egdb-client$/, replacement: egdbClient }] },
    // The app, and the backend it starts (or that runs on its own: `npm run backend`).
    build: { rollupOptions: { input: { index: 'src/main/index.ts', backend: 'src/backend/index.ts' } } },
  },
  preload: { plugins: [externalizeDepsPlugin()] },
  // __BUILD__: the commit and time it was built from, which the Phone page shows (the phone gets this same build).
  renderer: { plugins: [react(), tailwindcss()], define: { __BUILD__: JSON.stringify(build()) } },
})

function build(): string {
  let commit = 'unknown'
  try {
    commit = execSync('git rev-parse --short HEAD', { encoding: 'utf8' }).trim()
    if (execSync('git status --porcelain --untracked-files=no', { encoding: 'utf8' }).trim()) commit += ' with changes'
  } catch {
    // Not a checkout.
  }
  return `${commit}, built ${new Date().toLocaleString('en-GB', { dateStyle: 'medium', timeStyle: 'short' })}`
}
