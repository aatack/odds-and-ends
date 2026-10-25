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
  renderer: { plugins: [react(), tailwindcss()] },
})
