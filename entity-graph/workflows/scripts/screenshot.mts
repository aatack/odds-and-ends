// Screenshots of the app, headless, for a bot to look at a UI change it made (Read shows PNGs).
//
// ⚠ Expensive: it builds the app, starts Chromium and reads big images into your context. Use it only to check a
// specific change you made to the UI (a layout, a theme, an empty state), once or twice, not as a routine step.
//
//   npx tsx scripts/screenshot.mts [view ...] [--out <dir>] [--seed <module>] [--no-build] [--desktop-only | --phone-only]
//
// Views: tasks (the default), task (the seeded task's chat), bot, bots, integrations, types, log, search, file,
// item:<id>. Each is shot at a desktop width (1280×800) and a phone's (390×844), to <out>/<view>-<width>.png
// (./screenshots by default); the paths are printed.
//
// It never touches a real source: the engine is in memory, on a temp folder, with its phone listener on a free port
// serving the built renderer (out/renderer); everything stops when it is done. `--seed` names a module whose default
// export, `(engine) => Promise<Record<string, string>>`, adds what you need and answers extra views by name (an
// item's id each); without it, a small fixture is made (a task with a chat, a bot, a child task, an image file).

import { execSync } from 'node:child_process'
import { deflateSync } from 'node:zlib'
import { existsSync, mkdirSync, mkdtempSync, rmSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join, resolve } from 'node:path'
import { pathToFileURL } from 'node:url'
import { chromium } from 'playwright-core'
import { handlers } from '../src/backend/api.ts'
import { Changes, startPhone } from '../src/backend/http.ts'
import { Admin } from '../src/engine/admin.ts'
import { Engine } from '../src/engine/engine.ts'
import type { Mcp } from '../src/engine/mcp.ts'
import type { View } from '../src/renderer/src/views.ts'

const root = resolve(import.meta.dirname, '..')
const argv = process.argv.slice(2)
const flag = (name: string) => argv.includes(`--${name}`)
const value = (name: string) => {
  const i = argv.indexOf(`--${name}`)
  return i >= 0 ? argv[i + 1] : undefined
}
const valued = new Set([value('out'), value('seed')])
const views = argv.filter((a) => !a.startsWith('--') && !valued.has(a))
const out = resolve(value('out') ?? 'screenshots')
const ME = 'aatack'

if (!flag('no-build')) execSync('npx electron-vite build', { cwd: root, stdio: 'ignore' })
const built = join(root, 'out/renderer')
if (!existsSync(join(built, 'index.html'))) throw new Error('No built renderer in out/renderer: run npm run build.')

// ---- the app, in memory ----
const dir = mkdtempSync(join(tmpdir(), 'yawn-shot-'))
const engine = new Engine({ dataDir: dir, exec: async () => '' })
await engine.signIn(ME)
const extra: Record<string, string> = value('seed') ? await (await import(pathToFileURL(resolve(value('seed')!)).href)).default(engine) : await fixture(engine)
const mcp = { operator: () => ({ url: 'http://127.0.0.1:0/mcp', error: null }), restartOperator: async () => {} } as unknown as Mcp
const admin = new Admin(engine, mcp)
const source = join(dir, 'shot.sqlite')
const calls = (me: string) => handlers(engine, admin, me, source) as unknown as Record<string, (...args: unknown[]) => unknown>
const changes = new Changes(engine)
engine.settings.set('phone.port', 0)
const phone = await startPhone({ engine, calls, changes, files: built })
const url = phone.status().url
const token = engine.settings.phoneToken()

/** Which view stack each name opens. */
function stackFor(name: string): View[] {
  const item = (id: string): View[] => [{ kind: 'event', id }]
  if (name === 'tasks') return []
  if (name.startsWith('item:')) return item(name.slice(5))
  if (extra[name]) return item(extra[name])
  if (['bots', 'integrations', 'types', 'log', 'search', 'mcp'].includes(name)) return [{ kind: name } as View]
  throw new Error(`No view ${name}: tasks, ${Object.keys(extra).join(', ')}, bots, integrations, types, log, search, item:<id>.`)
}

const sizes = [
  ...(flag('phone-only') ? [] : [{ tag: 'desktop', width: 1280, height: 800 }]),
  ...(flag('desktop-only') ? [] : [{ tag: 'phone', width: 390, height: 844 }]),
]
mkdirSync(out, { recursive: true })
const browser = await chromium.launch()
const written: string[] = []
try {
  for (const name of views.length ? views : ['tasks']) {
    const stack = stackFor(name)
    for (const s of sizes) {
      const page = await browser.newPage({ viewport: { width: s.width, height: s.height } })
      const errors: string[] = []
      page.on('pageerror', (e) => errors.push(e.message))
      await page.addInitScript(
        // Signed in already, on this source (a new source would empty the view stack).
        ([t, u, v, src]) => {
          localStorage.setItem('workflows.source', JSON.stringify(src))
          localStorage.setItem('workflows.phoneToken', t)
          localStorage.setItem('workflows.user', JSON.stringify(u))
          localStorage.setItem('workflows.views', JSON.stringify(v))
        },
        [token, ME, stack, source] as const,
      )
      await page.goto(url)
      await page.waitForSelector('.view, .page', { timeout: 15_000 }).catch(() => errors.push('Nothing rendered within 15s.'))
      // Not networkidle: the page keeps /events open for as long as it lasts. A moment for what loads after.
      await page.waitForTimeout(800)
      const file = join(out, `${name.replace(/[^\w.-]+/g, '_')}-${s.tag}.png`)
      await page.screenshot({ path: file })
      written.push(file)
      if (errors.length) console.error(`${name} (${s.tag}): ${errors.join('; ')}`)
      await page.close()
    }
  }
} finally {
  await browser.close()
  phone.close()
  changes.close()
  rmSync(dir, { recursive: true, force: true })
}
console.log(written.join('\n'))
process.exit(0)

/** A small workspace: a task with a chat in it (a person, a bot, a child task, an image), and the bot. */
async function fixture(e: Engine): Promise<Record<string, string>> {
  const [bot] = await e.addItems(null, [{ type: 'bot', params: { name: 'Gazlowe', instructions: 'Builds the app.' } }])
  const task = (await e.create('Tidy the settings page'))!
  await e.addItems(task, [{ type: 'note', params: { text: 'The **Save** button sits under the mic on a phone. Can it move?' } }], ME)
  await e.addItems(task, [{ type: 'note', params: { text: 'Moved it beside the input, in `Composer.tsx`. Typecheck passes.' } }], `bot:${bot}`, 'bot')
  const png = join(e.dataDir, 'dot.png')
  writeFileSync(png, gradientPng(320, 200))
  const [file] = await e.addItems(task, [{ type: 'file', params: { path: png, copy: true, title: 'Before' } }], ME)
  await e.addItems(task, [{ type: 'task', params: { title: 'Check it on the phone', items: [{ type: 'note', params: { text: 'Look at it at 390px.' } }] } }], `bot:${bot}`, 'bot')
  await e.create('Snooze until Monday')
  return { task, bot, file }
}

/** A PNG of a gradient, so the file card and view have an image of a size to draw. */
function gradientPng(w: number, h: number): Buffer {
  const crc = (b: Buffer) => {
    let c = ~0
    for (const x of b) {
      c ^= x
      for (let k = 0; k < 8; k++) c = c & 1 ? (c >>> 1) ^ 0xedb88320 : c >>> 1
    }
    return ~c >>> 0
  }
  const chunk = (type: string, data: Buffer) => {
    const t = Buffer.concat([Buffer.from(type), data])
    const len = Buffer.alloc(4)
    len.writeUInt32BE(data.length)
    const sum = Buffer.alloc(4)
    sum.writeUInt32BE(crc(t))
    return Buffer.concat([len, t, sum])
  }
  const head = Buffer.alloc(13)
  head.writeUInt32BE(w, 0)
  head.writeUInt32BE(h, 4)
  head.set([8, 2, 0, 0, 0], 8)
  const rows = Buffer.alloc((w * 3 + 1) * h)
  for (let y = 0; y < h; y++)
    for (let x = 0; x < w; x++) rows.set([(x * 255) / w, (y * 255) / h, 180], y * (w * 3 + 1) + 1 + x * 3)
  return Buffer.concat([Buffer.from([137, 80, 78, 71, 13, 10, 26, 10]), chunk('IHDR', head), chunk('IDAT', deflateSync(rows)), chunk('IEND', Buffer.alloc(0))])
}
