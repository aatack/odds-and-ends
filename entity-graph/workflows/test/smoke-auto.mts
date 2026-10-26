// One real claude -p run in auto mode, in a bot's own folder, with the shell: it
// reports where it ran and reads its item over MCP. Not part of npm test.
import { mkdtempSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { Engine } from '../src/engine/engine.ts'
import { startMcp } from '../src/engine/mcp.ts'
import { BotRunner } from '../src/engine/workers.ts'

const engine = new Engine({ dataDir: mkdtempSync(join(tmpdir(), 'workflows-smoke-')) })
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)
await engine.signIn('human')
const runner = new BotRunner(engine, mcp.runUrl)
const bot = await engine.saveAgent({ name: 'Auto', instructions: '', provider: 'claude', model: '', debounceMinutes: 0 })
const id = (await engine.create('Auto smoke'))!
await engine.send(id, `assign:${bot}`, 'Run pwd in your shell, and call get_item with your item id. Then send me one line: the folder, and the title get_item gave.')
for (let i = 0; i < 300 && (runner.busy().length || engine.get(id)!.assignees.includes(bot)); i++) {
  runner.fill()
  await new Promise((r) => setTimeout(r, 1000))
}
await engine.idle()
for (const e of engine.children(id)) console.log('-', engine.actorName(e.createdBy).padEnd(8), e.type.padEnd(8), (e.message ?? e.summary).slice(0, 160))
console.log(engine.agent(bot)!.lastRun)
mcp.close()
process.exit(0)
