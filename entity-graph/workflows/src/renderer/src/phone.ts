// The app in a browser (the phone, as a PWA) rather than in its window: the transport, the token a link brings,
// the browser's own Back, the service worker, and whether to lay the views out for a phone.

import { useEffect, useState } from 'react'
import type { WorkflowsAPI } from '../../shared/types'
import { httpApi } from './transport'

/** In a browser, with no preload: the phone (or any browser on the phone listener). */
export const inBrowser = !window.workflows

/** Said when the backend refuses the token: Root goes back to signing in. */
export const REFUSED_EVENT = 'workflows:refused'

/** WorkflowsAPI for the browser: HTTP to the page's own origin, the token in local storage, Back from the browser. */
export function browserApi(): WorkflowsAPI {
  const http = httpApi({ storage: localStorage, onRefused: () => window.dispatchEvent(new Event(REFUSED_EVENT)) })
  return { ...http, onBack }
}

// The browser's Back (a phone's back gesture or button) takes the top view off the stack, as Ctrl+W does in the
// app, rather than leaving the app: a page of history is kept in hand, and put back each time it is used.
const backs = new Set<() => void>()
let holding = false
function onBack(cb: () => void): () => void {
  if (!holding) {
    holding = true
    history.pushState({ yawn: true }, '')
    window.addEventListener('popstate', () => {
      history.pushState({ yawn: true }, '')
      for (const fn of backs) fn()
    })
  }
  backs.add(cb)
  return () => backs.delete(cb)
}

/** A link from the Phone page carries the token after `#token=`: kept (and taken out of the address), it is answered. */
export function tokenFromLink(): string | null {
  const m = /[#&]token=([^&]+)/.exec(location.hash)
  if (!m) return null
  history.replaceState(null, '', location.pathname + location.search)
  return decodeURIComponent(m[1])
}

/** The service worker, which keeps the app's files so it opens offline; only in a browser, and only where it may. */
export function registerServiceWorker(): void {
  if (!inBrowser || !window.isSecureContext || !('serviceWorker' in navigator)) return
  window.addEventListener('load', () => void navigator.serviceWorker.register('./sw.js', { scope: './' }).catch(() => {}))
}

const NARROW = '(max-width: 640px)'
const INSTALLED = '(display-mode: standalone)'
const isMobile = () => matchMedia(NARROW).matches || matchMedia(INSTALLED).matches

/** Whether to lay the views out for a phone: a narrow screen, or the app installed as a PWA. Asked once, at the top. */
export function useMobile(): boolean {
  const [mobile, setMobile] = useState(isMobile)
  useEffect(() => {
    const queries = [matchMedia(NARROW), matchMedia(INSTALLED)]
    const update = () => setMobile(isMobile())
    for (const q of queries) q.addEventListener('change', update)
    return () => queries.forEach((q) => q.removeEventListener('change', update))
  }, [])
  return mobile
}
