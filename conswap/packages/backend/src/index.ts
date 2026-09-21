import { checkBlockers } from './blockers/scheduler.js'
import { loadConfig } from './config.js'
import { Context } from './context.js'
import { openDatabase } from './database.js'
import { startApi } from './api.js'
import { ClaudeIntegration } from './integrations/claude.js'
import { GithubIntegration } from './integrations/github.js'
import { SlackIntegration } from './integrations/slack.js'

const config = loadConfig()
const database = openDatabase(config.databasePath)
const context = new Context(config, database)

context.register(new SlackIntegration(context))
context.register(new GithubIntegration(context))
context.register(new ClaudeIntegration(context))

const server = startApi(context)
for (const integration of context.integrations.values()) integration.start()

let ticking = false
const tick = setInterval(() => {
  if (ticking) return
  ticking = true
  void checkBlockers(context)
    .catch((error) => context.log('scheduler', 'tick failed', String(error)))
    .finally(() => {
      ticking = false
    })
}, config.tickSeconds * 1000)

context.log('conswap', `database at ${config.databasePath}`)

function shutDown(): void {
  clearInterval(tick)
  for (const integration of context.integrations.values()) integration.stop()
  server.close()
  database.close()
  process.exit(0)
}

process.on('SIGINT', shutDown)
process.on('SIGTERM', shutDown)
