import { buildApp } from './app'
import { ConfigDb } from './config'
import { Registry } from './registry'

const PORT = Number(process.env.PORT ?? 4000)
const HOST = process.env.HOST ?? '127.0.0.1'
const CONFIG_DB = process.env.CONFIG_DB ?? './data/config.db'
const ADMIN_TOKEN = process.env.ADMIN_TOKEN

async function main(): Promise<void> {
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
