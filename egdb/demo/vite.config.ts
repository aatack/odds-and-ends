import react from '@vitejs/plugin-react'
import { defineConfig } from 'vite'

// The server has no CORS headers, so the dev server proxies /api to it.
const target = process.env.EGDB_URL ?? 'http://127.0.0.1:7878'

export default defineConfig({
  plugins: [react()],
  // egdb-client is linked from ../client, which has its own dev copies.
  resolve: { dedupe: ['react', 'react-dom', 'jotai'] },
  server: {
    proxy: { '/api': { target, changeOrigin: true, rewrite: (path) => path.replace(/^\/api/, '') } },
  },
})
