// Two real claude -p runs on one item, against the MCP server, with the event
// schema as structured output: the second resumes the first's session. Not part
// of npm test: it costs two runs.
import { DatabaseSync } from 'node:sqlite'
import { Engine } from '../src/engine/engine.ts'
import { startMcp } from '../src/engine/mcp.ts'
import { WorkerPool } from '../src/engine/workers.ts'

const engine = new Engine({ db: new DatabaseSync(':memory:') })
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)
const pool = new WorkerPool(engine, mcp.runUrl, 1)
const id = (await engine.create('Smoke test'))!
const busy = () => engine.get(id)!.blockers.some((b) => b.type === 'bot') || pool.status().some((w) => w.itemId)
const wait = async () => {
  for (let i = 0; i < 300 && busy(); i++) await new Promise((r) => setTimeout(r, 1000))
  await engine.idle()
}

await engine.send(id, 'bot', 'Remember the word "pelican". Read this item with get_item, then ask me whether I prefer red or blue.')
await wait()
await engine.send(id, 'bot', 'Blue. What was the word I asked you to remember? Answer, then add an approval to comment "hi" on o/r#1.')
await wait()

const d = engine.detail(id)!
for (const e of d.events) console.log('-', e.resolved ? 'resolved' : 'BLOCKING', e.createdBy.padEnd(6), e.type.padEnd(8), JSON.stringify(e.params).slice(0, 110), e.data?.error ?? '')
console.log(d.runs.map((r) => ({ cost: r.costUsd, turns: r.turns })))
console.log('sessions:', [...new Set(d.runs.map((r) => (engine.db.prepare('SELECT session_id FROM items WHERE id = ?').get(r.itemId) as { session_id: string }).session_id))])
mcp.close()
process.exit(0)
