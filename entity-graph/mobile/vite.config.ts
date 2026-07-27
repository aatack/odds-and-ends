import react from '@vitejs/plugin-react'
import tailwindcss from '@tailwindcss/vite'
import { defineConfig } from 'vite'

// The whole point of this app is to be opened on a phone, so both the dev server
// and `preview` bind every interface rather than localhost: the phone reaches the
// laptop by its LAN address. A fixed port keeps the URL (and the home-screen
// shortcut made from it) stable between restarts.
export default defineConfig({
  plugins: [react(), tailwindcss()],
  server: { host: true, port: 5180, strictPort: true },
  preview: { host: true, port: 5181, strictPort: true },
  build: { target: 'es2022' },
})
