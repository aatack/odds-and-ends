import React from 'react'
import ReactDOM from 'react-dom/client'
// Self-hosted font (bundled, no network): Lora, used throughout. Imported
// before index.css so the @font-face rules exist when our token layer
// references the family.
import '@fontsource-variable/lora/index.css'
import '@fontsource-variable/lora/wght-italic.css'
import './index.css'
import App from './App'

ReactDOM.createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)
