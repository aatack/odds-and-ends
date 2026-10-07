// The app shell, kept so the PWA opens at once; data is never cached here:
// it comes from the desktop's core, live (docs/phone.md).
const shell = 'detail-views-shell-v1'

self.addEventListener('install', () => self.skipWaiting())
self.addEventListener('activate', (event) => event.waitUntil(self.clients.claim()))

self.addEventListener('fetch', (event) => {
  const url = new URL(event.request.url)
  if (event.request.method !== 'GET' || url.origin !== location.origin || url.pathname.startsWith('/api/')) return
  // Network first, so a rebuilt app shows at once; the copy only when offline.
  event.respondWith(
    fetch(event.request)
      .then((response) => {
        const copy = response.clone()
        void caches.open(shell).then((cache) => cache.put(event.request, copy))
        return response
      })
      .catch(() => caches.match(event.request).then((cached) => cached ?? Response.error())),
  )
})
