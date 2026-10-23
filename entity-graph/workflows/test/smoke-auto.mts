// One real claude -p run in auto mode, in a worker's folder, with the shell: it
// reports where it ran and reads its item over MCP. Not part of npm test.
import { DatabaseSync } from 'node:sqlite'
import { Engine } from '../src/engine/engine.ts'
import { startMcp } from '../src/engine/mcp.ts'
import { WorkerPool } from '../src/engine/workers.ts'

const engine = new Engine({ db: new DatabaseSync(':memory:') })
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)
const pool = new WorkerPool(engine, mcp.runUrl, 1)
const id = (await engine.create('Auto smoke'))!
await engine.send(id, 'bot', 'Run pwd in your shell, and call get_item with your item id. Then send me one line: the folder, and the title get_item gave.')
for (let i = 0; i < 300 && (engine.get(id)!.blockers.some((b) => b.type === 'bot') || pool.status().some((w) => w.itemId)); i++)
  await new Promise((r) => setTimeout(r, 1000))
await engine.idle()
for (const e of engine.children(id)) console.log('-', e.createdBy.padEnd(6), e.type.padEnd(8), (e.message ?? e.summary).slice(0, 160))
console.log(engine.detail(id)!.runs.map((r) => ({ cost: r.costUsd, turns: r.turns })))
mcp.close()
process.exit(0)
