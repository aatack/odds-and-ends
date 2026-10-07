import { httpApi } from './api.ts'
import { browserEnvironment } from './environment.ts'
import { setImageSource } from './images.ts'
import { mount } from './root.tsx'
import { Session } from './session.ts'

/**
 * The phone: the same app, over HTTP to the desktop's core (docs/phone.md).
 * The token arrives once in the link's hash, is kept, and leaves the address.
 */
const env = browserEnvironment()
const fromLink = new URLSearchParams(location.hash.slice(1)).get('token')
if (fromLink) {
  env.save('detail-views.phone.token', fromLink)
  history.replaceState(null, '', location.pathname)
}
const token = (env.load('detail-views.phone.token') as string | undefined) ?? null

if ('serviceWorker' in navigator) void navigator.serviceWorker.register('/sw.js')

if (!token) {
  document.getElementById('root')!.textContent = 'Open the link in phone-link.txt (next to the desktop app’s data) on this phone once.'
} else {
  const base = location.origin
  setImageSource((ref) => `${base}/api/slack-image/${encodeURIComponent(ref)}?token=${encodeURIComponent(token)}`)
  mount(new Session(httpApi(base, token), env), { phone: true })
}
