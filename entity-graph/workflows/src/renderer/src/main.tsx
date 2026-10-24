import { createRoot } from 'react-dom/client'
import { App } from './App'
import { isPhone } from './useWorkflows'
import '@fontsource-variable/geist/index.css'
import '@fontsource-variable/lora/index.css'
import '@fontsource-variable/fira-code/index.css'
import './index.css'
import './styles.css'

// Always dark: entity-graph's tokens redefine themselves under html.dark.
document.documentElement.classList.add('dark')

// The service worker caches the app shell for offline use. Only on the phone
// build, and only over HTTPS (a secure context) — never in Electron, never on
// plain-HTTP LAN, where registration would fail or serve a stale shell.
if (isPhone && window.isSecureContext && 'serviceWorker' in navigator)
  window.addEventListener('load', () => void navigator.serviceWorker.register('./sw.js', { scope: './' }).catch(() => {}))

createRoot(document.getElementById('root')!).render(<App />)
