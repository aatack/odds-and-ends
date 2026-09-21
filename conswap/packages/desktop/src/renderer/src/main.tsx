import { App, Client, HttpTransport, Session } from '@conswap/common'
import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import './index.css'

declare global {
  interface Window {
    conswap?: { serverUrl: string }
  }
}

const serverUrl = window.conswap?.serverUrl ?? 'http://127.0.0.1:4319'
const session = new Session(new Client(new HttpTransport(serverUrl)))

createRoot(document.getElementById('root') as HTMLElement).render(
  <StrictMode>
    <App session={session} />
  </StrictMode>,
)
