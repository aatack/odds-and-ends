import { execFile } from 'child_process'
import { existsSync, readdirSync, statSync } from 'fs'
import { join } from 'path'
import type { TailscaleHandler, TailscaleView } from '../core/client'
import { APP_MOUNT } from '../core/client'

/**
 * Driving `tailscale serve` from the app, so the phone route in
 * `mobile/README.md` is a switch rather than a shell session.
 *
 * **Nothing in the app reaches this at the moment.** The IPC handlers are still
 * registered and this module still works; what is gone is the UI that called
 * them, because serving is coming back as a node on the sources graph rather
 * than as a panel. Left whole so that it is a wiring job when it does.
 *
 * What it publishes is exactly what that document describes: the phone app's
 * `dist/` at `/`, and one source proxied at `/api/<sourceId>`, both on this
 * machine's HTTPS `.ts.net` name. Tailscale's own config is the only state —
 * nothing here is persisted, and a mount the user made by hand reads back the
 * same as one the app made, which is what lets a switch reflect the truth
 * rather than a guess.
 *
 * The asymmetry between adding and removing is worth knowing, because it shapes
 * the whole module: adding a mount is one idempotent command, but `tailscale`
 * has no per-path removal — 1.98 accepts no `off` target and prints its help —
 * so removing one means `serve reset` and re-adding everything else. That makes
 * removal the only destructive operation here, and the only one that can be
 * refused ({@link TailscaleView.editable}).
 */

/** Long enough for the daemon to answer; short enough not to hang a click. */
const TIMEOUT = 20_000

/** Where the phone app's build lands, relative to the app root. */
const APP_DIST = ['mobile', 'dist']

interface Ran {
  ok: boolean
  output: string
}

/** The `tailscale` binary isn't installed, which is a different kind of "no". */
class NotInstalled extends Error {}

function run(args: string[]): Promise<Ran> {
  return new Promise((resolve, reject) => {
    execFile('tailscale', args, { timeout: TIMEOUT }, (err, stdout, stderr) => {
      if (err && (err as NodeJS.ErrnoException).code === 'ENOENT') {
        reject(new NotInstalled('Tailscale isn’t installed on this machine.'))
        return
      }
      resolve({ ok: !err, output: (stderr || stdout).trim() })
    })
  })
}

/**
 * Turn a failed command into something worth reading. Nearly every failure here
 * is the same one — the config is root's unless the user has been named
 * operator — and it has a one-line fix, so it is worth spotting by hand rather
 * than passing tailscaled's wording through.
 */
function failed(ran: Ran, doing: string): Error {
  if (/access denied|permission denied|operator|must be root/i.test(ran.output)) {
    return new Error(
      `${doing} needs permission to change this machine’s serve config. ` +
        'Run `sudo tailscale set --operator=$USER` once in a terminal, then try again.',
    )
  }
  return new Error(`${doing} failed: ${ran.output || 'tailscale gave no reason'}`)
}

// ---------------------------------------------------------------------------
// Reading the current state
// ---------------------------------------------------------------------------

/** The shape of `tailscale serve status --json`, to the depth this cares about. */
interface ServeConfig {
  TCP?: Record<string, { HTTPS?: boolean; HTTP?: boolean }> | null
  Web?: Record<string, { Handlers?: Record<string, Record<string, unknown>> | null }> | null
  AllowFunnel?: Record<string, boolean> | null
  Services?: Record<string, unknown> | null
  Foreground?: Record<string, unknown> | null
}

const empty = (o: Record<string, unknown> | null | undefined): boolean =>
  !o || Object.keys(o).length === 0

/** The HTTPS name serve answers on, or why there isn't one. */
async function certDomain(): Promise<{ domain: string | null; problem: string | null }> {
  const ran = await run(['status', '--json'])
  if (!ran.ok) return { domain: null, problem: `Couldn’t ask Tailscale for its status: ${ran.output}` }

  const status = JSON.parse(ran.output) as { BackendState?: string; CertDomains?: string[] | null }
  if (status.BackendState !== 'Running') {
    return {
      domain: null,
      problem:
        status.BackendState === 'NeedsLogin'
          ? 'Tailscale is installed but signed out. Run `tailscale up`.'
          : `Tailscale is ${status.BackendState ?? 'not running'}. Run \`tailscale up\`.`,
    }
  }

  const domain = status.CertDomains?.[0]
  if (!domain) {
    return {
      domain: null,
      problem:
        'This machine has no HTTPS certificate, so there is no secure origin to serve from — ' +
        'and without one the phone app installs as a bookmark rather than an app. Enable ' +
        'MagicDNS and HTTPS Certificates in the Tailscale admin console.',
    }
  }
  return { domain, problem: null }
}

/**
 * Read the serve config into handlers, and decide whether it is one this app
 * could rebuild after a reset. Anything it can't reproduce — Funnel, a service,
 * a foreground serve, a second host, a raw TCP forwarder — locks removal rather
 * than risking a reset that quietly drops it.
 *
 * Exported because it is the pure half of this module: everything else needs a
 * daemon to say anything, and this is the part whose judgement is worth checking
 * against a config you can write down.
 */
export function readServeConfig(
  cfg: ServeConfig,
  domain: string,
): { handlers: TailscaleHandler[]; locked: string | null } {
  const handlers: TailscaleHandler[] = []
  let locked: string | null = null
  const lock = (reason: string): void => {
    locked ??= reason
  }

  if (!empty(cfg.Services)) lock('a Tailscale service is configured on this machine')
  if (!empty(cfg.Foreground)) lock('a `tailscale serve` is running in a terminal')
  if (!empty(cfg.AllowFunnel)) lock('Funnel is on, which this app can’t re-enable for you')

  for (const [port, tcp] of Object.entries(cfg.TCP ?? {})) {
    // Port 443 terminating HTTPS is just what a web handler implies; anything
    // else was set up deliberately and by other means.
    if (port !== '443' || tcp.HTTPS !== true) lock(`a TCP handler is configured on port ${port}`)
  }

  const hosts = Object.entries(cfg.Web ?? {})
  if (hosts.length > 1) lock('more than one host is being served')
  for (const [host, web] of hosts) {
    if (host !== `${domain}:443`) lock(`something is served on ${host} rather than ${domain}`)
    for (const [mount, handler] of Object.entries(web.Handlers ?? {})) {
      if (typeof handler.Path === 'string') handlers.push({ mount, kind: 'path', target: handler.Path })
      else if (typeof handler.Proxy === 'string') handlers.push({ mount, kind: 'proxy', target: handler.Proxy })
      else if (typeof handler.Text === 'string') handlers.push({ mount, kind: 'text', target: handler.Text })
      else lock(`the handler on ${mount} is of a kind this app doesn’t understand`)
    }
  }

  handlers.sort((a, b) => a.mount.localeCompare(b.mount))
  return { handlers, locked }
}

/**
 * Where the phone app's build sits, and whether anything has been built into it.
 * The app root is passed in rather than read from `electron`, which keeps this
 * module plain Node and so runnable outside the app.
 */
export function phoneAppDist(appRoot: string): { path: string; built: boolean } {
  const path = join(appRoot, ...APP_DIST)
  const built =
    existsSync(path) && statSync(path).isDirectory() && readdirSync(path).includes('index.html')
  return { path, built }
}

/** Everything the config page needs to draw the phone-access controls. */
export async function tailscaleView(appRoot: string): Promise<TailscaleView> {
  const dist = phoneAppDist(appRoot)
  const offline = (problem: string): TailscaleView => ({
    running: false,
    problem,
    domain: null,
    handlers: [],
    editable: false,
    locked: null,
    app: dist,
  })

  let domain: string | null
  let problem: string | null
  try {
    ;({ domain, problem } = await certDomain())
  } catch (e) {
    if (e instanceof NotInstalled) {
      return offline(
        'Tailscale isn’t installed. `curl -fsSL https://tailscale.com/install.sh | sudo sh`, ' +
          'then `sudo tailscale up`.',
      )
    }
    throw e
  }
  if (!domain) return offline(problem ?? 'Tailscale isn’t ready.')

  const ran = await run(['serve', 'status', '--json'])
  if (!ran.ok) return offline(`Couldn’t read the serve config: ${ran.output}`)
  const { handlers, locked } = readServeConfig(JSON.parse(ran.output) as ServeConfig, domain)

  return { running: true, problem: null, domain, handlers, editable: !locked, locked, app: dist }
}

// ---------------------------------------------------------------------------
// Changing it
// ---------------------------------------------------------------------------

/** The command that publishes one handler. */
function serveArgs(handler: TailscaleHandler): string[] {
  const args = ['serve', '--bg', '--yes']
  // The root mount is the default, and `--set-path=/` is not how you ask for it.
  if (handler.mount !== APP_MOUNT) args.push(`--set-path=${handler.mount}`)
  args.push(handler.kind === 'text' ? `text:${handler.target}` : handler.target)
  return args
}

/** Publish one handler. Idempotent, and leaves every other mount alone. */
async function add(handler: TailscaleHandler): Promise<void> {
  const ran = await run(serveArgs(handler))
  if (!ran.ok) throw failed(ran, `Serving ${handler.mount}`)
}

/**
 * Unpublish one mount: reset, then put back everything else. The reset is why
 * this checks first and reports partial states loudly — between the reset and
 * the last re-add, the tailnet name is serving less than it was.
 */
async function remove(view: TailscaleView, mount: string): Promise<void> {
  if (!view.editable) {
    throw new Error(
      `Removing a mount means clearing the serve config and rebuilding it, and ${view.locked}. ` +
        'Sort that out with `tailscale serve` directly.',
    )
  }

  const keep = view.handlers.filter((h) => h.mount !== mount)
  // A directory that has since been deleted would fail on the way back in, after
  // the reset has already happened. Better to find that out now.
  const missing = keep.find((h) => h.kind === 'path' && !existsSync(h.target))
  if (missing) {
    throw new Error(
      `${missing.target} is served at ${missing.mount} but no longer exists, so the serve ` +
        'config can’t be rebuilt around it. Remove that mount first.',
    )
  }

  const reset = await run(['serve', 'reset'])
  if (!reset.ok) throw failed(reset, 'Clearing the serve config')

  const restored: string[] = []
  for (const handler of keep) {
    try {
      await add(handler)
      restored.push(handler.mount)
    } catch (e) {
      const back = restored.length ? `Restored: ${restored.join(', ')}.` : 'Nothing was restored.'
      throw new Error(
        `${(e as Error).message}\n\nThe serve config was cleared to remove ${mount} and could ` +
          `not be fully rebuilt. ${back} Check \`tailscale serve status\`.`,
      )
    }
  }
}

/** Publish or unpublish one mount, reading the current state first. */
export async function setServed(
  appRoot: string,
  handler: TailscaleHandler,
  on: boolean,
): Promise<void> {
  const view = await tailscaleView(appRoot)
  if (!view.running) throw new Error(view.problem ?? 'Tailscale isn’t ready.')

  const current = view.handlers.find((h) => h.mount === handler.mount)
  if (on) {
    if (current?.target === handler.target && current.kind === handler.kind) return
    if (current) {
      throw new Error(
        `${handler.mount} already serves ${current.target}. Point it elsewhere with ` +
          '`tailscale serve` if that isn’t wanted.',
      )
    }
    if (handler.kind === 'path' && !existsSync(handler.target)) {
      throw new Error(`${handler.target} doesn’t exist yet — run \`npm run build\` in mobile/.`)
    }
    await add(handler)
  } else {
    if (!current) return
    await remove(view, handler.mount)
  }
}
