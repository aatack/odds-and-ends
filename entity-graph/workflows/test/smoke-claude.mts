// Real claude -p runs, against the MCP server, with the event schema as
// structured output: a bot splits off a child item and assigns it to itself;
// its next run (woken by hand here, rather than in 15 minutes) takes the child up
// in the same session, so it knows what it was told for the parent. Not part of npm test: it costs a few runs.
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
const bot = await engine.saveAgent({ name: 'Smoke', instructions: 'Keep your messages to one line.', provider: 'claude', model: 'sonnet', debounceMinutes: 0 })
const parent = (await engine.create('Smoke test'))!
const busy = () => runner.busy().length > 0 || engine.agents().some((a) => a.wakesAt !== null && a.wakesAt <= Date.now())
const wait = async () => {
  for (let i = 0; i < 600 && busy(); i++) {
    runner.fill()
    await new Promise((r) => setTimeout(r, 1000))
  }
  await engine.idle()
}

await engine.send(
  parent,
  `assign:${bot}`,
  'Remember the word "pelican", but do not write it anywhere. Then add a child item titled "Recall", whose first events are a ' +
    'message "Say the word you were asked to remember, then close this item.", and assign it to yourself. Keep the parent too.',
)
await wait()
engine.wakeAgent(bot)
await wait()

const show = (id: string, depth = 0) => {
  const it = engine.get(id)!
  console.log(`${'  '.repeat(depth)}# ${it.title} (${it.closed ? it.how : 'open'}) assigned ${it.assignees.map((a) => engine.actorName(a)).join(', ')}`)
  for (const e of engine.children(id)) {
    console.log(`${'  '.repeat(depth)}- ${e.resolved ? 'done' : 'OPEN'} ${engine.actorName(e.createdBy).padEnd(10)} ${e.type.padEnd(8)} ${(e.message ?? e.summary).slice(0, 100)}`)
    if (e.type === 'item') show(e.id, depth + 1)
  }
}
show(parent)
console.log(engine.eventDetail(bot)!.runs.map((r) => ({ items: r.itemIds.length, cost: r.costUsd, error: r.result?.slice(0, 200) })))
mcp.close()
process.exit(0)
