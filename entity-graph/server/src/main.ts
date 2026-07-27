import { buildApp } from './app'
import { ConfigDb } from './config'
import { loadEnvFile } from './env'
import { Registry } from './registry'

// Before anything reads the environment: the integrations' secrets live in
// `server/.env`, and the settings below may as well be settable there too.
loadEnvFile()

const PORT = Number(process.env.PORT ?? 4000)
const HOST = process.env.HOST ?? '127.0.0.1'
const CONFIG_DB = process.env.CONFIG_DB ?? './data/config.db'
// Blank counts as unset, matching `buildApp`'s own `!opts.adminToken` test — an
// `ADMIN_TOKEN=` line in `.env` must not read as "protected".
const ADMIN_TOKEN = process.env.ADMIN_TOKEN?.trim() || undefined

/**
 * Whether a bind address reaches no further than this machine.
 *
 * Only the loopback forms count. Everything else — `0.0.0.0`, `::`, a specific LAN
 * address — is at minimum the local network, which is the case the guard below is
 * about.
 */
const isLoopback = (host: string): boolean =>
  /^(127\.\d+\.\d+\.\d+|::1|\[::1\]|localhost)$/i.test(host.trim())

/**
 * Refuse the one combination that hands the admin surface to the network.
 *
 * With `ADMIN_TOKEN` unset the admin endpoints are open — creating and deleting
 * sources, issuing tokens — which is a deliberate convenience while the server is on
 * loopback and the only caller is the app that spawned it. Bound to anything wider
 * that same convenience is an unauthenticated remote control for the store, offered
 * to every device on the wifi. The two settings are individually reasonable and
 * catastrophic together, so the server declines to start rather than logging a
 * warning nobody reads.
 */
function refuseOpenAdminOnNetwork(): void {
  if (isLoopback(HOST) || ADMIN_TOKEN) return
  // eslint-disable-next-line no-console
  console.error(
    `[entity-graph] refusing to start: HOST=${HOST} exposes this server beyond ` +
      `loopback, and with no ADMIN_TOKEN set the admin endpoints (create/delete ` +
      `sources, issue tokens) would answer anyone who can reach it.\n` +
      `Set ADMIN_TOKEN to a secret, or leave HOST at 127.0.0.1 and put a proxy that ` +
      `terminates TLS in front (see mobile/README.md).`,
  )
  process.exit(1)
}

async function main(): Promise<void> {
  refuseOpenAdminOnNetwork()

  const db = new ConfigDb(CONFIG_DB)
  const registry = new Registry(db)
  const app = buildApp({ db, registry, adminToken: ADMIN_TOKEN })

  await app.listen({ port: PORT, host: HOST })
  // eslint-disable-next-line no-console
  console.log(`[entity-graph] server listening on http://${HOST}:${PORT}`)
}

main().catch((e) => {
  // eslint-disable-next-line no-console
  console.error(e)
  process.exit(1)
})
