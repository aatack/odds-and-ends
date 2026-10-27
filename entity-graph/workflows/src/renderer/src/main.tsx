import { createRoot } from 'react-dom/client'
import { Root } from './App'
import '@fontsource-variable/geist/index.css'
import '@fontsource-variable/lora/index.css'
import '@fontsource-variable/source-serif-4/index.css'
import '@fontsource-variable/newsreader/index.css'
import '@fontsource-variable/literata/index.css'
import '@fontsource-variable/crimson-pro/index.css'
import '@fontsource-variable/fira-code/index.css'
import './index.css'
import './styles.css'
import './themes.css'
import { applyAppearance, storedAppearance } from './appearance'
import { registerServiceWorker } from './phone'

// The chosen theme (dark but for Notebook: entity-graph's tokens redefine themselves under html.dark).
applyAppearance(storedAppearance())

registerServiceWorker()

createRoot(document.getElementById('root')!).render(<Root />)
