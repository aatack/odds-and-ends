import { defineConfig, externalizeDepsPlugin } from 'electron-vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'

export default defineConfig({
  main: {
    plugins: [externalizeDepsPlugin()],
  },
  preload: {
    plugins: [externalizeDepsPlugin()],
  },
  renderer: {
    plugins: [react(), tailwindcss()],
    // The code-runner worker is a module worker and pulls in a (code-split)
    // QuickJS bundle, so its output must be ES rather than the default IIFE.
    worker: { format: 'es' },
    server: process.env.PORT
      ? { port: Number(process.env.PORT), strictPort: true }
      : undefined,
  },
})
