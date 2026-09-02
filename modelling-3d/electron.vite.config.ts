import { defineConfig, externalizeDepsPlugin } from 'electron-vite'
import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'
import { resolve } from 'path'

export default defineConfig({
  main: { plugins: [externalizeDepsPlugin()] },
  preload: { plugins: [externalizeDepsPlugin()] },
  renderer: {
    resolve: { alias: { '@core': resolve(__dirname, 'src/core') } },
    plugins: [react(), tailwindcss()],
    server: process.env.PORT ? { port: Number(process.env.PORT), strictPort: true } : undefined,
  },
})
