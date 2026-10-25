import react from '@vitejs/plugin-react'
import { fileURLToPath } from 'node:url'
import { defineConfig } from 'vite'

// The server has no CORS headers, so the dev server proxies /api to it.
const target = process.env.EGDB_URL ?? 'http://127.0.0.1:7878'
const client = (path: string) => fileURLToPath(new URL(`../client/src/${path}`, import.meta.url))

export default defineConfig({
  plugins: [react()],
  resolve: {
    // Use the client's source, not its build, so a `git pull` needs no rebuild.
    alias: [
      { find: /^egdb-client$/, replacement: client('index.ts') },
      { find: /^egdb-client\/react$/, replacement: client('react/index.tsx') },
    ],
    // The client folder has its own dev copies of these.
    dedupe: ['react', 'react-dom', 'jotai'],
  },
  server: {
    proxy: { '/api': { target, changeOrigin: true, rewrite: (path) => path.replace(/^\/api/, '') } },
  },
})
