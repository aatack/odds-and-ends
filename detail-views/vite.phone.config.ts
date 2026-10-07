import { resolve } from 'node:path'
import react from '@vitejs/plugin-react'
import { defineConfig } from 'vite'

/** The phone's PWA: the renderer, built for a browser, served by the desktop app (docs/mobile.md). */
export default defineConfig({
  root: resolve('src/renderer'),
  base: '/',
  publicDir: resolve('src/renderer/phone-public'),
  plugins: [react()],
  build: {
    outDir: resolve('out/phone'),
    emptyOutDir: true,
    rollupOptions: { input: resolve('src/renderer/phone.html') },
  },
})
