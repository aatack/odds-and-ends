// One real claude -p run against the MCP server. Not part of npm test: it costs a run.
import { DatabaseSync } from 'node:sqlite'
import { Engine } from '../src/engine/engine.ts'
import { startMcp } from '../src/engine/mcp.ts'
import { WorkerPool } from '../src/engine/workers.ts'

const engine = new Engine(new DatabaseSync(':memory:'))
const mcp = await startMcp(engine)
new WorkerPool(engine, mcp.url, 1)
const id = engine.create('Smoke test', { instruction: 'Set the state to "smoke ok", then close the item.' })!
for (let i = 0; i < 180 && engine.get(id)!.worker === null && engine.get(id)!.blocker === 'agent'; i++) await new Promise((r) => setTimeout(r, 1000))
while (engine.get(id)!.worker !== null) await new Promise((r) => setTimeout(r, 1000))
const d = engine.detail(id)!
console.log(d.item.blocker, '|', d.item.state, '|', d.item.request?.question ?? '')
console.log(d.events.map((e) => `${e.actor} ${e.kind}: ${e.text}`).join('\n'))
console.log(d.runs)
mcp.server.close()
process.exit(0)
