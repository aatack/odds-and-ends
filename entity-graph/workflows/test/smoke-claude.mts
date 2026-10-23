// Real claude -p runs, against the MCP server, with the event schema as
// structured output: a parent item's bot splits off a child item, whose bot's
// session is forked from the parent's, so it knows what the parent was told.
// Not part of npm test: it costs a few runs.
import { DatabaseSync } from 'node:sqlite'
import { Engine } from '../src/engine/engine.ts'
import { startMcp } from '../src/engine/mcp.ts'
import { WorkerPool } from '../src/engine/workers.ts'

const engine = new Engine({ db: new DatabaseSync(':memory:') })
engine.settings.set('mcp.port', 0)
const mcp = await startMcp(engine)
const pool = new WorkerPool(engine, mcp.runUrl, 2)
const parent = (await engine.create('Smoke test'))!
const busy = () => engine.overview().items.some((i) => i.blockers.some((b) => b.type === 'bot')) || pool.status().some((w) => w.itemId)
const wait = async () => {
  for (let i = 0; i < 400 && busy(); i++) await new Promise((r) => setTimeout(r, 1000))
  await engine.idle()
}

await engine.send(
  parent,
  'bot',
  'Remember the word "pelican", but do not write it anywhere. Then add a child item titled "Recall", whose first events are a ' +
    'message "Say the word you were asked to remember, then close this item." and a bot. Send me no message.',
)
await wait()

const show = (id: string, depth = 0) => {
  const it = engine.get(id)!
  console.log(`${'  '.repeat(depth)}# ${it.title} (${it.closed ? it.how : 'open'}) session ${it.sessionId}`)
  for (const e of engine.children(id)) {
    console.log(`${'  '.repeat(depth)}- ${e.resolved ? 'done' : 'OPEN'} ${e.createdBy.padEnd(10)} ${e.type.padEnd(8)} ${(e.message ?? e.summary).slice(0, 100)}`)
    if (e.type === 'item') show(e.id, depth + 1)
  }
}
show(parent)
console.log(engine.detail(parent)!.runs.map((r) => ({ cost: r.costUsd })))
mcp.close()
process.exit(0)
