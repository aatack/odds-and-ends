// The PWA's service worker: it keeps the app's own files (the page, and the hashed scripts, styles and fonts it
// loads), so the app opens without the network and installs as an app. It never touches the API (/rpc, /events):
// what the app shows is always the backend's, live. Change CACHE to drop what was kept.

const CACHE = 'yawn-shell-v1'
const PAGE = './'

self.addEventListener('install', () => self.skipWaiting())

self.addEventListener('activate', (event) => {
  event.waitUntil(
    (async () => {
      for (const name of await caches.keys()) if (name !== CACHE) await caches.delete(name)
      await self.clients.claim()
    })(),
  )
})

self.addEventListener('fetch', (event) => {
  const req = event.request
  const url = new URL(req.url)
  if (req.method !== 'GET' || url.origin !== self.location.origin) return
  if (url.pathname.startsWith('/rpc/') || url.pathname === '/events' || url.pathname === '/health') return

  // The page: from the network when it can be had (so a new build is picked up), else the one kept.
  if (req.mode === 'navigate') {
    event.respondWith(
      (async () => {
        const cache = await caches.open(CACHE)
        try {
          const res = await fetch(req)
          if (res.ok) await cache.put(PAGE, res.clone())
          return res
        } catch {
          return (await cache.match(PAGE)) || Response.error()
        }
      })(),
    )
    return
  }

  // Everything else the page loads: kept as it is first fetched; the built files' names change with their contents.
  event.respondWith(
    (async () => {
      const cache = await caches.open(CACHE)
      const kept = await cache.match(req)
      if (kept) return kept
      const res = await fetch(req)
      if (res.ok && res.type === 'basic') await cache.put(req, res.clone())
      return res
    })(),
  )
})
