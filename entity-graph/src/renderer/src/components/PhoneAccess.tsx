import React, { useState } from 'react'
import { QRCodeSVG } from 'qrcode.react'
import { AlertTriangle, RefreshCw01 } from '@untitledui/icons'
import type { TailscaleModel } from '../views/useTailscale'
import { Button } from './ui/Button'
import { CopyButton } from './ui/CopyButton'
import { IconButton } from './ui/IconButton'
import { Input } from './ui/Input'
import { Switch } from './ui/Switch'

/**
 * **Unwired.** Nothing renders these at the moment: `tailscale serve` is coming
 * back as a node on the sources graph rather than as a panel on the page, and
 * this is left standing so that plumbing it back in is a matter of rendering it
 * again. The IPC it calls is still there, and so is `src/main/tailscale.ts`.
 *
 * The controls for reaching this machine from a phone — the same
 * `tailscale serve` mounts that `mobile/README.md` sets up by hand, as two
 * switches.
 *
 * They are two rather than one because they are published independently and
 * mean different things: the app at the root of the tailnet name is the thing a
 * phone installs, and one broadcast under `/api/<id>` is what it then reads and
 * writes. A phone needs both, but the app is served once for the machine while a
 * broadcast is a per-node decision, which is why one lives on the page and the
 * other in the node's own panel.
 *
 * All state comes from {@link TailscaleModel}; nothing here talks to IPC.
 */

const SECTION = 'text-[11px] font-medium uppercase tracking-[0.09em] text-gray-500'

// ---------------------------------------------------------------------------
// The page panel — the tailnet name, and the phone app itself
// ---------------------------------------------------------------------------

export function PhoneAccessPanel({ model }: { model: TailscaleModel }): React.JSX.Element {
  const { view, error, appUrl, app, actions } = model

  return (
    <section className="rounded-lg bg-white shadow-xs">
      <div className="flex items-center justify-between gap-3 px-4 py-3">
        <div className="min-w-0">
          <p className={SECTION}>Phone access</p>
          <p className="truncate font-mono text-xs text-gray-500">
            {view?.domain ?? 'over Tailscale'}
          </p>
        </div>
        <IconButton title="Re-read Tailscale" onClick={() => void actions.refresh()}>
          <RefreshCw01 size={16} />
        </IconButton>
      </div>

      <div className="space-y-3 border-t border-gray-100 px-4 py-3">
        {!view ? (
          <p className="text-xs text-gray-400">Reading Tailscale…</p>
        ) : !view.running ? (
          <p className="text-[13px] text-gray-500">{view.problem}</p>
        ) : (
          <>
            <div className="flex items-start justify-between gap-3">
              <div className="min-w-0">
                <p className="text-[13px] text-gray-900">Serve the phone app</p>
                <p className="truncate font-mono text-xs text-gray-400">{view.app.path}</p>
              </div>
              <Switch
                label="Serve the phone app"
                checked={app.on}
                disabled={app.busy || !!app.blocked}
                onChange={(on) => void actions.serveApp(on)}
              />
            </div>

            {!view.app.built && (
              <Note>
                Nothing is built there yet — run <Code>npm run build</Code> in{' '}
                <Code>mobile/</Code>. The served files are whatever that directory last held, so
                there is nothing to restart after a rebuild.
              </Note>
            )}

            {app.on && appUrl && (
              <UrlRow url={appUrl} title="Copy the app’s address">
                Open this on the phone, reload once, then <em className="not-italic text-gray-600">
                  Add to Home screen
                </em>
                . The service worker registers after the first load, so the install option only
                appears on the second visit.
              </UrlRow>
            )}

            {app.blocked && <Note>{app.blocked}</Note>}
            {view.locked && <Note>Mounts can’t be removed from here: {view.locked}.</Note>}
          </>
        )}

        {error && <p className="whitespace-pre-line text-[13px] text-error-600">{error}</p>}
      </div>
    </section>
  )
}

// ---------------------------------------------------------------------------
// The per-node section, shown inside a broadcast node's panel
// ---------------------------------------------------------------------------

export function NodePhoneAccess({
  model,
  nodeId,
  localUrl,
}: {
  model: TailscaleModel
  nodeId: string
  /** The broadcast's own address on this machine — what the mount proxies to. */
  localUrl: string | null
}): React.JSX.Element {
  const source = model.node(nodeId, localUrl)
  const [author, setAuthor] = useState('phone')
  const [link, setLink] = useState<string | null>(null)
  const [busy, setBusy] = useState(false)
  const [error, setError] = useState<string | null>(null)

  const makeLink = async (): Promise<void> => {
    setBusy(true)
    setError(null)
    try {
      setLink(await model.actions.phoneLink(nodeId, author.trim() || 'phone'))
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setBusy(false)
    }
  }

  return (
    <section className="space-y-3 border-t border-gray-100 pt-3">
      <p className={SECTION}>Phone access</p>

      {!model.view ? (
        <p className="text-xs text-gray-400">Reading Tailscale…</p>
      ) : !model.view.running ? (
        <p className="text-[13px] text-gray-500">{model.view.problem}</p>
      ) : (
        <>
          <div className="flex items-start justify-between gap-3">
            <div className="min-w-0">
              <p className="text-[13px] text-gray-900">Serve this broadcast on Tailscale</p>
              <p className="truncate font-mono text-xs text-gray-400">
                {localUrl ?? 'not listening'}
              </p>
            </div>
            <Switch
              label="Serve this broadcast on Tailscale"
              checked={source.on}
              disabled={source.busy || !!source.blocked || !localUrl}
              onChange={(on) => void model.actions.serveNode(nodeId, on)}
            />
          </div>

          {source.url && (
            <UrlRow url={source.url} title="Copy the broadcast’s address on the tailnet">
              Only this pensive is exposed. Nothing that configures the app is reachable from the
              tailnet, and a broadcast has no reach outside its own store.
            </UrlRow>
          )}

          {source.blocked && <Note>{source.blocked}</Note>}

          {source.on && (
            <>
              {!model.app.on && (
                <Note>
                  The phone app itself isn’t being served, so a connect link would open nothing.
                  Turn it on in <em className="not-italic text-gray-600">Phone access</em> on the
                  sources page.
                </Note>
              )}

              <div className="flex items-end gap-2">
                <label className="min-w-0 flex-1 space-y-1">
                  <span className="block text-[11px] font-medium uppercase tracking-[0.06em] text-gray-500">
                    Author for writes from the phone
                  </span>
                  <Input value={author} onChange={(e) => setAuthor(e.target.value)} placeholder="phone" />
                </label>
                <Button onClick={() => void makeLink()} disabled={busy}>
                  {busy ? 'Working…' : link ? 'Remake link' : 'Make connect link'}
                </Button>
              </div>

              {link && <ConnectLink url={link} />}
            </>
          )}

          {/* A failed switch lands in the shared model, whose only other reader is
              the page panel — which this modal is covering. */}
          {(error ?? model.error) && (
            <p className="whitespace-pre-line text-[13px] text-error-600">{error ?? model.error}</p>
          )}
        </>
      )}
    </section>
  )
}

/**
 * The connect link, as a QR code and as text.
 *
 * The QR is always dark-on-white, whatever the app's theme, because a camera
 * reads it and an inverted code is a coin flip. It is drawn at a fixed size
 * rather than scaled to the modal for the same reason.
 */
function ConnectLink({ url }: { url: string }): React.JSX.Element {
  return (
    <div className="space-y-2 rounded-md bg-gray-50 p-3">
      <div className="flex gap-3">
        <div className="shrink-0 rounded-md bg-[#ffffff] p-2 shadow-xs">
          <QRCodeSVG value={url} size={124} level="M" marginSize={0} bgColor="#ffffff" fgColor="#292929" />
        </div>
        <p className="min-w-0 flex-1 text-xs text-gray-400">
          Point the phone’s camera at this, or send yourself the link below. It carries a token
          labelled <Code>phone</Code>, so revoking that one token cuts the phone off and leaves
          this app connected. The token rides in the URL fragment, which never reaches a server or
          a log on the way in.
        </p>
      </div>
      <div className="flex items-start gap-1">
        <p className="min-w-0 flex-1 break-all font-mono text-[11px] leading-snug text-gray-500">
          {url}
        </p>
        <CopyButton value={url} title="Copy the connect link" />
      </div>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Small shared pieces
// ---------------------------------------------------------------------------

/** A served URL with its explanation, and a button to copy it. */
function UrlRow({
  url,
  title,
  children,
}: {
  url: string
  title: string
  children: React.ReactNode
}): React.JSX.Element {
  return (
    <div className="space-y-1 rounded-md bg-gray-50 px-3 py-2">
      <div className="flex items-center gap-1">
        <p className="min-w-0 flex-1 truncate font-mono text-xs text-gray-700">{url}</p>
        <CopyButton value={url} title={title} />
      </div>
      <p className="text-xs text-gray-400">{children}</p>
    </div>
  )
}

/** Something worth knowing before the switch does what was expected. */
function Note({ children }: { children: React.ReactNode }): React.JSX.Element {
  return (
    <p className="flex gap-1.5 text-xs text-gray-500">
      <AlertTriangle size={14} className="mt-px shrink-0 text-warning-700" />
      <span>{children}</span>
    </p>
  )
}

function Code({ children }: { children: React.ReactNode }): React.JSX.Element {
  return <code className="rounded bg-gray-100 px-1 font-mono text-[11px] text-gray-600">{children}</code>
}
