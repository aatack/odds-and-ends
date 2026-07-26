// A service worker that caches nothing.
//
// Chrome only offers to *install* a page that registers one and answers fetches,
// and this app has nothing worth keeping offline: every screen is a query against
// the source server, and a stale outline you can't write to would be worse than
// an honest "no connection". So this exists to make the app installable, and
// hands every request straight to the network.
//
// It also means a deploy is picked up on the next load rather than being pinned
// by a cache — which matters while the app is changing daily.

self.addEventListener('install', () => self.skipWaiting())
self.addEventListener('activate', (event) => event.waitUntil(self.clients.claim()))
// Registered but never calls respondWith: the browser falls back to the network.
self.addEventListener('fetch', () => {})
