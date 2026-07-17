import React from 'react'
import ReactDOM from 'react-dom/client'
// Self-hosted fonts (bundled, no network): Geist for UI chrome, Lora for
// user-entered text (the `font-serif` utility). Imported before index.css so
// the @font-face rules exist when our token layer references the families.
import '@fontsource-variable/geist/index.css'
import '@fontsource-variable/lora/index.css'
import '@fontsource-variable/lora/wght-italic.css'
import './index.css'
import App from './App'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)
