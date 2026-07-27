import React from 'react'
import { createRoot } from 'react-dom/client'
import '@fontsource-variable/geist'
import '@fontsource-variable/lora'
import './index.css'
import {
  connectionAtom,
  connectionFromHash,
  refreshCapabilities,
} from './source/connection'
import { startQueryEngine } from './state/query'
import { App } from './views/App'

// Entry point. Three things happen before the app mounts, in this order for a
// reason.

// 1. A connection handed in through the URL fragment wins. This is how a phone is
//    set up without typing a bearer token: the laptop makes a link, the phone opens
//    it once. The fragment is cleared immediately so the token isn't left in the
//    address bar for the next person to look over your shoulder — and so a reload
//    doesn't keep re-applying a connection you have since changed.
const handed = connectionFromHash(window.location.hash)
if (handed) {
  connectionAtom.set(handed)
  history.replaceState(history.state, '', window.location.pathname + window.location.search)
}

// 2. The query engine starts subscribed but idle: with no connection it has nothing
//    to fetch, and it will fetch as soon as one arrives.
startQueryEngine()

// 3. What the source can do is re-read in the background. The last known answer is
//    persisted, so undo and files don't disappear from the UI for the first second
//    after a cold start on a bad connection.
void refreshCapabilities()

createRoot(document.getElementById('root')!).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>,
)

// The service worker caches nothing (see public/sw.js) — it is registered so the app
// can be *installed* rather than bookmarked. Only in a secure context, which over
// plain HTTP on a LAN means it simply doesn't register, and the app runs the same.
if ('serviceWorker' in navigator && window.isSecureContext) {
  window.addEventListener('load', () => {
    void navigator.serviceWorker.register('/sw.js').catch(() => {
      // Nothing to do about it, and nothing lost: no offline support either way.
    })
  })
}
