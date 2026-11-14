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

declare const __BUILD__: string

/** A numbered step: its number turns to a tick once the app can tell it is done. */
function Step({ n, title, done, children }: { n: number; title: string; done?: boolean; children: React.ReactNode }) {
  return (
    <li className={`phone-step ${done ? 'done' : ''}`}>
      <span className="phone-step-n">{done ? '✓' : n}</span>
      <div className="phone-step-body">
        <h3>{title}</h3>
        {children}
      </div>
    </li>
  )
}

/**
 * Reaching the app from a phone, as numbered steps, each holding what it needs (the command, the checkbox, the QR
 * code): the backend's phone listener, its token, and whether it is served on the tailnet (the app runs
 * `tailscale serve` itself, on and off). Only in the app on this machine: the phone is refused what changes its own
 * access.
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
  const link = view.phoneUrl ? `${view.phoneUrl}#token=${encodeURIComponent(view.token)}` : null
  const served = view.tailnet && view.serving === true && !view.error
  return (
    <div className={`page ${mobile ? 'mobile' : ''}`}>
      {error && <div className="error">{error}</div>}

      <section className="card">
        <h2>Phone</h2>
        <p className="muted">
          YAWN as an app on your phone, over Tailscale: the same app, signed in as you. Do these once, in order; each step turns to a ✓ when
          the app can see it is done.
        </p>
        {view.error && <p className="error-text">{view.error}</p>}
        <ol className="phone-steps">
          <Step n={1} title="Tailscale on this machine and the phone" done={!!view.tailnetName}>
            <p className="muted">
              Install Tailscale on both and sign in to the same tailnet. In the{' '}
              <a href="https://login.tailscale.com/admin/dns" target="_blank" rel="noreferrer">
                admin console's DNS page
              </a>
              , turn on <b>MagicDNS</b> and <b>HTTPS Certificates</b>.
            </p>
            <p className="muted">
              {view.tailnetName ? (
                <>
                  This machine is <code>{view.tailnetName}</code> on the tailnet.
                </>
              ) : view.serving === null ? (
                "Tailscale can't be asked here: is it installed, and up?"
              ) : (
                'Tailscale has no name for this machine yet: turn on MagicDNS.'
              )}
            </p>
          </Step>

          <Step n={2} title="Let the app run Tailscale (once, in a terminal)" done={served}>
            <p className="muted">Tailscale wants root to serve unless you are its operator. Run this once; it lasts:</p>
            <Copyable value="sudo tailscale set --operator=$USER" block language="bash" />
          </Step>

          <Step n={3} title="Tick this box" done={served}>
            <label className="row-inline phone-toggle">
              <input type="checkbox" checked={view.tailnet} onChange={(e) => void act(() => api().setPhoneTailnet(e.target.checked))} />
              Serve it on the tailnet
            </label>
            <p className="muted">
              {view.serving === null
                ? "Tailscale can't be asked here, so this can't be checked."
                : view.serving
                  ? view.tailnet
                    ? `Served: Tailscale passes port ${view.tailnetPort} on the tailnet to this app.`
                    : `Tailscale still serves port ${view.tailnetPort}, but the app answers nothing from there while this is off.`
                  : view.tailnet
                    ? 'On, but Tailscale is not serving it: untick and tick it again, or run the command under Ports.'
                    : 'Off: only this machine can reach it.'}
            </p>
            <details>
              <summary className="muted">Ports</summary>
              <div className="kv phone-ports">
                <span>This machine</span>
                <div className="row-inline">
                  <div className="port">
                    <Form schema={{ type: 'string' }} value={port} setValue={(v) => setPort(typeof v === 'string' ? v : '')} />
                  </div>
                  {port !== String(view.port) && <button data-ui onClick={() => act(() => api().setPhonePort(Number(port)))}>Save</button>}
                </div>
                <span>Tailnet (HTTPS)</span>
                <div className="row-inline">
                  <div className="port">
                    <Form schema={{ type: 'string' }} value={tailnetPort} setValue={(v) => setTailnetPort(typeof v === 'string' ? v : '')} />
                  </div>
                  {tailnetPort !== String(view.tailnetPort) && (
                    <button data-ui onClick={() => act(() => api().setPhoneTailnetPort(Number(tailnetPort)))}>Save</button>
                  )}
                </div>
              </div>
              <p className="muted">Changing either re-serves at once. What the box runs, should you want to run it yourself:</p>
              <Copyable value={view.serveCommand} block language="bash" />
            </details>
          </Step>

          <Step n={4} title="Scan this with the phone's camera">
            {link ? (
              <div className="phone-connect">
                <QrCode text={link} size={mobile ? 180 : 220} />
                <div className="phone-connect-text">
                  <p className="muted">
                    It opens YAWN on the phone with the token filled in.{served ? '' : ' It answers only once step 3 is done.'} Treat the code like a
                    password.
                  </p>
                  <p className="muted">Or open this address on the phone and paste the token:</p>
                  <div className="kv">
                    <span>Address</span>
                    <Copyable value={view.phoneUrl!} />
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
                    <span className="muted">signs out every phone using the old one.</span>
                  </div>
                </div>
              </div>
            ) : (
              <p className="muted">The code appears here once step 1 is done: it needs this machine's name on the tailnet.</p>
            )}
          </Step>

          <Step n={5} title="Sign in on the phone">
            <p className="muted">Type your username and press Sign in; the token is already there.</p>
          </Step>

          <Step n={6} title="Add it to the home screen">
            <p className="muted">
              iPhone, in Safari: Share, then <b>Add to Home Screen</b>. Android, in Chrome: ⋮, then <b>Install app</b> (or Add to Home screen).
              Open YAWN from that icon from now on.
            </p>
          </Step>
        </ol>
      </section>

      <section className="card">
        <h2>Keeping it up to date</h2>
        <p className="muted">
          The phone runs the same build as this window: <code>{__BUILD__}</code>. New code reaches both when YAWN is restarted (<code>npm start</code>{' '}
          rebuilds); the phone picks it up the next time it opens the app, or on a pull to refresh.
        </p>
      </section>
    </div>
  )
}
