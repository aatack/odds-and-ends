// The PWA service worker: it caches the app shell only (index.html and the
// hashed assets it pulls in), so the app opens offline. Data is always live —
// it NEVER touches /api/*. Bump CACHE to ship a new shell; old caches are
// dropped on activate.

const CACHE = 'workflows-shell-v1'
const SHELL = './' // the navigation request's cache key: the SPA's one HTML document

self.addEventListener('install', () => self.skipWaiting())

self.addEventListener('activate', (event) => {
  event.waitUntil(
    (async () => {
      const names = await caches.keys()
      await Promise.all(names.filter((n) => n !== CACHE).map((n) => caches.delete(n)))
      await self.clients.claim()
    })(),
  )
})

self.addEventListener('fetch', (event) => {
  const req = event.request
  if (req.method !== 'GET') return
  const url = new URL(req.url)
  // Leave the API and anything cross-origin alone: the shell cache is same-origin shell only.
  if (url.origin !== self.location.origin || url.pathname.startsWith('/api/')) return

  // Navigations: network-first, falling back to the cached shell when offline.
  if (req.mode === 'navigate') {
    event.respondWith(
      (async () => {
        try {
          const res = await fetch(req)
          const cache = await caches.open(CACHE)
          void cache.put(SHELL, res.clone())
          return res
        } catch {
          const cache = await caches.open(CACHE)
          return (await cache.match(SHELL)) || Response.error()
        }
      })(),
    )
    return
  }

  // Everything else (the hashed JS/CSS/fonts the shell fetches): cache-first,
  // populated on first fetch so the next load works offline.
  event.respondWith(
    (async () => {
      const cache = await caches.open(CACHE)
      const hit = await cache.match(req)
      if (hit) return hit
      const res = await fetch(req)
      if (res.ok && res.type === 'basic') void cache.put(req, res.clone())
      return res
    })(),
  )
})
