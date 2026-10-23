import { createRoot } from 'react-dom/client'
import { App } from './App'
import '@fontsource-variable/geist/index.css'
import '@fontsource-variable/lora/index.css'
import '@fontsource-variable/fira-code/index.css'
import './index.css'
import './styles.css'

createRoot(document.getElementById('root')!).render(<App />)
