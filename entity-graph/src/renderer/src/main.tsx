import React from 'react'
import ReactDOM from 'react-dom/client'
// Self-hosted fonts (bundled, no network): Geist for UI chrome, Lora for
// user-entered text (the `font-serif` utility). Imported before index.css so
// the @font-face rules exist when our token layer references the families.
import '@fontsource-variable/geist/index.css'
import '@fontsource-variable/lora/index.css'
import '@fontsource-variable/lora/wght-italic.css'
// Fira Code for `type: code` entities (the `font-mono` utility).
import '@fontsource-variable/fira-code/index.css'
import './index.css'
import App from './App'
import { setQueryTracing } from './state/derive'

// Temporary: log every frame traversal to the console, with the stack that asked
// for it, while we work out what is still rebuilding the tree during navigation.
// Note the StrictMode below — it double-invokes render in development, so a memo
// that recomputes once shows up twice. Compare counts, not absolutes.
setQueryTracing(true)

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)
