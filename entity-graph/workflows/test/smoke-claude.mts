// One real claude -p run against the MCP server, with the handoff schema as its
// structured output. Not part of npm test: it costs a run.
import { DatabaseSync } from 'node:sqlite'
import { Engine } from '../src/engine/engine.ts'
import { startMcp } from '../src/engine/mcp.ts'
import { WorkerPool } from '../src/engine/workers.ts'

const engine = new Engine({ db: new DatabaseSync(':memory:') })
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)
const pool = new WorkerPool(engine, mcp.runUrl, 1)
const id = (await engine.create('Smoke test', {
  next: [
    {
      type: 'bot',
      params: {
        instructions:
          'Read this item with get_item. Then set the state to "smoke ok" and hand it to the human to choose between red and blue, ' +
          'followed by a script that closes nothing and returns null.',
      },
    },
  ],
}))!
const busy = () => engine.get(id)!.current?.type === 'bot' || pool.status().some((w) => w.itemId)
for (let i = 0; i < 300 && busy(); i++) await new Promise((r) => setTimeout(r, 1000))
await engine.idle()
const d = engine.detail(id)!
console.log('state:', d.item.state)
console.log('current:', JSON.stringify(d.item.current && { type: d.item.current.type, params: d.item.current.params }))
for (const h of d.handoffs) console.log('-', h.status, h.type, JSON.stringify(h.params).slice(0, 120), JSON.stringify(h.resolution)?.slice(0, 300))
console.log(d.runs.map((r) => ({ cost: r.costUsd, turns: r.turns })))
mcp.close()
process.exit(0)
