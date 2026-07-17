import React from 'react'
import ReactDOM from 'react-dom/client'
// Self-hosted fonts (bundled, no network): Geist for UI chrome, Source Serif 4
// with optical sizing for user-written prose. Imported before index.css so the
// @font-face rules exist when our token layer references the families.
import '@fontsource-variable/geist/index.css'
import '@fontsource-variable/source-serif-4/opsz.css'
import '@fontsource-variable/source-serif-4/opsz-italic.css'
import './index.css'
import App from './App'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)
