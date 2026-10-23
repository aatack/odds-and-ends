// The Phone page: turns the mobile PWA server on, and shows everything needed
// to reach it from a phone over Tailscale. Desktop-only — it runs CLIs and can
// change the phone's own access, so the HTTP transport never exposes it.
// Modelled on McpPage.tsx.

import { useCallback, useEffect, useState } from 'react'
import type { PhoneView } from '../../shared/types'
import { Copyable } from './Copy'
import { Form } from './forms'
import { api } from './useWorkflows'

export function PhonePage() {
  const [view, setView] = useState<PhoneView | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [port, setPort] = useState('')
  const [showToken, setShowToken] = useState(false)
  const [output, setOutput] = useState<string | null>(null)

  const refresh = useCallback(async () => {
    const v = await api().phone()
    setView(v)
    setPort(String(v.port))
  }, [])
  useEffect(() => void refresh(), [refresh])

  const act = async (fn: () => Promise<unknown>) => {
    setError(null)
    try {
      await fn()
      await refresh()
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
    }
  }

  if (!view) return <div className="muted">Loading…</div>
  // The connect link carries the token in the fragment, so the phone need not type it.
  const connectLink = view.phoneUrl ? `${view.phoneUrl}#token=${encodeURIComponent(view.token)}` : null

  return (
    <div className="page">
      {error && <div className="error">{error}</div>}

      <section className="card">
        <h2>Phone</h2>
        <p className="muted">
          A mobile web app for this workflow, reached from your phone over Tailscale. It acts as you, the same as this window. Off by
          default; it listens on loopback only, and is exposed to your tailnet by the command below.
        </p>
        {view.error && <p className="error-text">{view.error}</p>}
        <div className="row-inline">
          <button data-ui className={view.enabled ? '' : 'primary'} onClick={() => act(() => api().setPhoneEnabled(!view.enabled))}>
            {view.enabled ? 'Turn off' : 'Turn on'}
          </button>
          <span className="muted">{view.enabled ? 'On' : 'Off'}</span>
        </div>
        <div className="kv">
          <span>On laptop</span>
          <Copyable value={view.loopbackUrl} />
          <span>On phone</span>
          {view.phoneUrl ? <Copyable value={view.phoneUrl} /> : <span className="muted">Unknown until Tailscale has a MagicDNS name.</span>}
          <span>Token</span>
          <div className="row-inline">
            <Copyable value={view.token} display={showToken ? view.token : '•'.repeat(24)} />
            <button data-ui onClick={() => setShowToken(!showToken)}>{showToken ? 'Hide' : 'Show'}</button>
            <button data-ui onClick={() => act(() => api().regeneratePhoneToken())}>Regenerate</button>
          </div>
          <span>Port</span>
          <div className="row-inline">
            <div className="port">
              <Form schema={{ type: 'string' }} value={port} setValue={(v) => setPort(typeof v === 'string' ? v : '')} />
            </div>
            {port !== String(view.port) && <button data-ui onClick={() => act(() => api().setPhonePort(Number(port)))}>Save</button>}
          </div>
        </div>
        {connectLink && (
          <>
            <h3>Connect link</h3>
            <p className="muted">Open this on your phone to connect without typing the token. Treat it like the token itself.</p>
            <Copyable value={connectLink} block />
          </>
        )}
      </section>

      <section className="card">
        <h2>Expose it over Tailscale</h2>
        <p className="muted">Run this on your laptop once the server is on; it serves the app at the phone URL above.</p>
        <Copyable value={view.serveCommand} block />
        <div className="row-inline">
          <button data-ui onClick={() => act(async () => setOutput(await api().runTailscaleServe()))}>Run it</button>
        </div>
        {output !== null && <pre className="output">{output || '(no output)'}</pre>}
      </section>

      <section className="card">
        <h2>Setup</h2>
        <h3>On your laptop</h3>
        <ol>
          <li>Install Tailscale and <code>tailscale up</code>.</li>
          <li>
            <code>sudo tailscale set --operator=$USER</code>, so this app can read the tailnet and run serve without sudo.
          </li>
          <li>In the Tailscale admin console, enable MagicDNS and HTTPS certificates.</li>
          <li>Turn the server on above, then Run it (or run the command yourself).</li>
        </ol>
        <h3>On your phone</h3>
        <ol>
          <li>Install Tailscale and sign in to the same account.</li>
          <li>Open the phone URL (or the connect link) and paste the token.</li>
          <li>Add to Home Screen. On Android, reload once before looking for Install.</li>
        </ol>
      </section>
    </div>
  )
}
