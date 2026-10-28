import { useCallback, useEffect, useMemo, useState } from 'react'
import type { PhoneView } from '../../shared/types'
import { Copyable } from './Copy'
import { Form } from './forms'
import { encodeQr } from './helpers/qr'
import { api } from './useWorkflows'

/** Text as a QR code, dark on white whatever the theme, so a camera reads it. */
function QrCode({ text, size = 220 }: { text: string; size?: number }) {
  // A quiet zone of four modules all round.
  const { path, n } = useMemo(() => {
    const q = encodeQr(text)
    return { path: q.flatMap((row, y) => row.flatMap((dark, x) => (dark ? [`M${x + 4} ${y + 4}h1v1h-1z`] : []))).join(''), n: q.length + 8 }
  }, [text])
  return (
    <svg className="qr" width={size} height={size} viewBox={`0 0 ${n} ${n}`} shapeRendering="crispEdges" role="img" aria-label="QR code">
      <rect width={n} height={n} fill="#fff" />
      <path d={path} fill="#000" />
    </svg>
  )
}

/**
 * Reaching the app from a phone: the backend's phone listener, its token (as a QR code that signs the phone in, or to
 * copy), and whether it is served on the tailnet: the app runs `tailscale serve` itself, on and off. Only in the app on
 * this machine: the phone is refused what changes its own access.
 */
export function PhonePage({ mobile }: { mobile: boolean }) {
  const [view, setView] = useState<PhoneView | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [port, setPort] = useState('')
  const [tailnetPort, setTailnetPort] = useState('')
  const [showToken, setShowToken] = useState(false)
  const refresh = useCallback(async () => {
    const v = await api().phone()
    setView(v)
    setPort(String(v.port))
    setTailnetPort(String(v.tailnetPort))
  }, [])
  useEffect(() => void refresh().catch((e) => setError((e as Error).message)), [refresh])

  const act = async (fn: () => Promise<unknown>) => {
    setError(null)
    try {
      await fn()
      await refresh()
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
    }
  }

  if (error && !view) return <div className="error">{error}</div>
  if (!view) return <div className="muted">Loading…</div>
  // The link carries the token after the #, which the browser never sends: opened, it signs the phone in.
  const link = `${view.phoneUrl ?? view.url}#token=${encodeURIComponent(view.token)}`
  return (
    <div className={`page ${mobile ? 'mobile' : ''}`}>
      {error && <div className="error">{error}</div>}

      <section className="card">
        <h2>Phone</h2>
        <p className="muted">
          YAWN as an app on your phone, over Tailscale: the same app, acting as whoever signs in on it. It listens on this machine only;
          <code> tailscale serve</code> takes it to your tailnet, and it answers from there only while that is let in below.
        </p>
        {view.error && <p className="error-text">{view.error}</p>}
        <div className="phone-connect">
          <QrCode text={link} size={mobile ? 180 : 220} />
          <div className="phone-connect-text">
            <p className="muted">
              Scan it with the phone's camera to open YAWN signed in{view.phoneUrl ? '' : ' (once this machine has a name on the tailnet)'}.
              Treat it as you would the token.
            </p>
            <div className="kv">
              <span>Here</span>
              <Copyable value={view.url} />
              <span>Phone</span>
              {view.phoneUrl ? <Copyable value={view.phoneUrl} /> : <span className="muted">Tailscale has no name for this machine.</span>}
              <span>Token</span>
              <div className="row-inline">
                <Copyable value={view.token} display={showToken ? view.token : '•'.repeat(16)} />
                <button data-ui onClick={() => setShowToken(!showToken)}>{showToken ? 'Hide' : 'Show'}</button>
              </div>
            </div>
            <div className="row-inline">
              <button data-ui onClick={() => act(() => api().regeneratePhoneToken())} title="Signs out every phone signed in with the old one">
                New token
              </button>
            </div>
          </div>
        </div>
      </section>

      <section className="card">
        <h2>Tailnet</h2>
        <label className="row-inline phone-toggle">
          <input type="checkbox" checked={view.tailnet} onChange={(e) => void act(() => api().setPhoneTailnet(e.target.checked))} />
          Serve it on the tailnet
        </label>
        <p className="muted">
          {view.serving === null
            ? "Tailscale can't be asked here (is it installed and up?)."
            : view.serving
              ? `Tailscale serves it on port ${view.tailnetPort}${view.tailnet ? '.' : ', but it answers nothing from there while this is off.'}`
              : view.tailnet
                ? 'On, but Tailscale is not serving it: turn it off and on again, or run the command below.'
                : 'Off: only this machine reaches it.'}{' '}
          It needs MagicDNS and HTTPS certificates on in the tailnet, and Tailscale signed in on the phone.
        </p>
        <div className="kv">
          <span>Port here</span>
          <div className="row-inline">
            <div className="port">
              <Form schema={{ type: 'string' }} value={port} setValue={(v) => setPort(typeof v === 'string' ? v : '')} />
            </div>
            {port !== String(view.port) && <button data-ui onClick={() => act(() => api().setPhonePort(Number(port)))}>Save</button>}
          </div>
          <span>On the tailnet</span>
          <div className="row-inline">
            <div className="port">
              <Form schema={{ type: 'string' }} value={tailnetPort} setValue={(v) => setTailnetPort(typeof v === 'string' ? v : '')} />
            </div>
            {tailnetPort !== String(view.tailnetPort) && (
              <button data-ui onClick={() => act(() => api().setPhoneTailnetPort(Number(tailnetPort)))}>Save</button>
            )}
          </div>
        </div>
        <details>
          <summary className="muted">If the app can't run Tailscale</summary>
          <p className="muted">
            Tailscale may want root to serve: run <code>sudo tailscale set --operator=$USER</code> once, or this, in a shell:
          </p>
          <Copyable value={view.serveCommand} block language="bash" />
        </details>
      </section>

      <section className="card">
        <h2>On the phone</h2>
        <ol>
          <li>Scan the code, or open the phone address and paste the token.</li>
          <li>Sign in with your username.</li>
          <li>Add it to the home screen (Share, then Add to Home Screen; or Install, in the browser's menu).</li>
        </ol>
      </section>
    </div>
  )
}
