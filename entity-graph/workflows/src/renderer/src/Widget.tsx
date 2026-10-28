import { useEffect, useMemo, useRef, useState } from 'react'
import type { ItemInfo } from '../../shared/types'
import { api } from './useWorkflows'

/** What a widget's sandbox may load: its own inline scripts and styles, and data: images and fonts. Nothing else. */
const CSP = "default-src 'none'; script-src 'unsafe-inline'; style-src 'unsafe-inline'; img-src data:; font-src data:"

/** The app's look, as the widget is given it: its CSS variables, read from the page as it is drawn. */
const VARS = ['--ink', '--muted', '--panel', '--bg', '--line', '--accent', '--accent-ink', '--danger', '--font-sans', '--font-mono',
  '--color-gray-100', '--color-gray-200', '--color-gray-300', '--color-success-600', '--color-warning-700']

function theme(): string {
  const css = getComputedStyle(document.documentElement)
  const vars = VARS.map((v) => `${v}: ${css.getPropertyValue(v).trim() || 'initial'};`).join(' ')
  return `:root { ${vars} color-scheme: ${css.colorScheme || 'light dark'}; }
html, body { margin: 0; background: transparent; color: var(--ink); font-family: var(--font-sans); font-size: 13px; line-height: 1.5; }
body { padding: 2px; }
button { font: inherit; color: var(--ink); background: var(--color-gray-100); border: none; border-radius: 6px; padding: 4px 10px; }
button:hover { background: var(--color-gray-200); }
button.primary { background: var(--accent); color: var(--accent-ink); }
input, select, textarea { font: inherit; color: var(--ink); background: var(--panel); border: 1px solid var(--line); border-radius: 6px; padding: 4px 8px; }
input[type=checkbox] { accent-color: var(--accent); }
table { border-collapse: collapse; width: 100%; }
th, td { text-align: left; padding: 4px 8px 4px 0; border-top: 1px solid var(--line); }
th { color: var(--muted); font-weight: 500; }
code, pre { font-family: var(--font-mono); font-size: 12px; }
.muted { color: var(--muted); }
.pill { display: inline-flex; padding: 0 7px; border: 1px solid var(--color-gray-300); border-radius: 9px; }
.card { background: var(--color-gray-100); border-radius: 8px; padding: 8px 12px; margin: 4px 0; }
.card h3 { margin: 0 0 4px; font-size: 13px; font-weight: 500; }`
}

/** What runs in the sandbox before the widget's own code: its state, and the only way out (posting to the app). */
const runtime = (state: unknown) => `
(() => {
  let state = ${JSON.stringify(state ?? null).replace(/</g, '\\u003c')}
  const subs = []
  const post = (m) => parent.postMessage({ __widget: true, ...m }, '*')
  const tell = () => subs.forEach((f) => { try { f(state) } catch (e) { console.error(e) } })
  window.getState = () => state
  window.onState = (f) => { subs.push(f); try { f(state) } catch (e) { console.error(e) } }
  window.setState = (next) => { state = typeof next === 'function' ? next(state) : next; tell(); post({ type: 'set', state }) }
  window.esc = (s) => String(s ?? '').replace(/[&<>"']/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c])
  window.pill = (text) => '<span class="pill">' + esc(text) + '</span>'
  window.card = (title, body) => '<div class="card"><h3>' + esc(title) + '</h3>' + (body ?? '') + '</div>'
  addEventListener('message', (e) => {
    if (e.source !== parent || !e.data || !e.data.__widget || e.data.type !== 'state') return
    if (JSON.stringify(e.data.state) === JSON.stringify(state)) return
    state = e.data.state
    tell()
  })
  const size = () => post({ type: 'height', h: document.documentElement.scrollHeight })
  addEventListener('load', size)
  new ResizeObserver(size).observe(document.documentElement)
})()`

/**
 * A widget: its HTML in a sandboxed frame (scripts, and nothing else: no network, no app, no navigation), styled as
 * the app is. The frame is made once for its HTML, so what is typed in it keeps its place; its state goes in and out
 * by messages. What it sets is saved a moment after it stops changing; a change from elsewhere is passed in.
 */
export function Widget({ info: e, tall = false }: { info: ItemInfo; tall?: boolean }) {
  const frame = useRef<HTMLIFrameElement>(null)
  const [height, setHeight] = useState(40)
  const html = String(e.params.html ?? '')
  const state = e.params.widget_state ?? null
  // Made with the state as it is then: only a change of its HTML makes it afresh.
  const doc = useMemo(
    () =>
      `<!doctype html><html><head><meta charset="utf-8"><meta http-equiv="Content-Security-Policy" content="${CSP}">` +
      `<style>${theme()}</style><script>${runtime(state)}</script></head><body>${html}</body></html>`,
    // eslint-disable-next-line react-hooks/exhaustive-deps
    [html],
  )
  // What the widget itself last said its state was, and a save waiting to go.
  const mine = useRef<string>(JSON.stringify(state))
  const pending = useRef<ReturnType<typeof setTimeout> | null>(null)
  useEffect(() => {
    const onMessage = (m: MessageEvent) => {
      if (m.source !== frame.current?.contentWindow || !m.data?.__widget) return
      if (m.data.type === 'height') setHeight(Math.min(Number(m.data.h) || 40, tall ? 4000 : 600))
      if (m.data.type === 'set') {
        mine.current = JSON.stringify(m.data.state ?? null)
        if (pending.current) clearTimeout(pending.current)
        pending.current = setTimeout(() => {
          pending.current = null
          void api().setWidgetState(e.id, m.data.state ?? null)
        }, 400)
      }
    }
    window.addEventListener('message', onMessage)
    return () => window.removeEventListener('message', onMessage)
  }, [e.id, tall])
  // A change from elsewhere (a bot, a script, another window) is passed in; its own echo is not.
  const now = JSON.stringify(state)
  useEffect(() => {
    if (pending.current || now === mine.current) return
    mine.current = now
    frame.current?.contentWindow?.postMessage({ __widget: true, type: 'state', state: JSON.parse(now) }, '*')
  }, [now])
  return <iframe ref={frame} className="widget-frame" title={String(e.params.title ?? 'Widget')} sandbox="allow-scripts" srcDoc={doc} style={{ height }} />
}
