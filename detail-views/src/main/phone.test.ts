import assert from 'node:assert/strict'
import { request } from 'node:http'
import { mkdtempSync, writeFileSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { join } from 'node:path'
import { test } from 'node:test'
import { memoryCore } from '../core/testing.ts'
import { httpApi } from '../renderer/src/api.ts'
import { memoryEnvironment } from '../renderer/src/environment.ts'
import { Session } from '../renderer/src/session.ts'
import { phoneToken, startPhoneServer } from './phone.ts'

/** Server-Sent Events for node, which has no EventSource: just enough for `httpApi`. */
class NodeEventSource {
  onopen: (() => void) | null = null
  onmessage: ((event: { data: string }) => void) | null = null
  private closed = false
  private req: ReturnType<typeof request>
  constructor(url: string) {
    this.req = request(url, (response) => {
      this.onopen?.()
      let buffer = ''
      response.on('data', (chunk: Buffer) => {
        buffer += chunk.toString()
        let at
        while ((at = buffer.indexOf('\n\n')) >= 0) {
          const block = buffer.slice(0, at)
          buffer = buffer.slice(at + 2)
          const data = block.split('\n').find((line) => line.startsWith('data: '))
          if (data && !this.closed) this.onmessage?.({ data: data.slice(6) })
        }
      })
    })
    this.req.end()
  }
  close() {
    this.closed = true
    this.req.destroy()
  }
}

test('the phone reaches the core over HTTP, with a token, and runs the same Session on it', async () => {
  ;(globalThis as { EventSource?: unknown }).EventSource = NodeEventSource
  const core = memoryCore()
  const dist = mkdtempSync(join(tmpdir(), 'phone-'))
  writeFileSync(join(dist, 'phone.html'), '<!doctype html><title>phone</title>')
  const port = 47900 + Math.floor(Math.random() * 90)
  const stop = startPhoneServer(core, { dist, port })
  const base = `http://127.0.0.1:${port}`
  const token = phoneToken(core)
  try {
    // The app is served to anyone; the API only with the token.
    assert.match(await (await fetch(`${base}/`)).text(), /phone/)
    assert.equal((await fetch(`${base}/api/scan`, { method: 'POST', body: '{"ids":[]}' })).status, 401)
    assert.equal((await fetch(`${base}/api/scan`, { method: 'POST', headers: { authorization: 'Bearer nope' }, body: '{"ids":[]}' })).status, 401)

    // The phone's Session, over httpApi: write a note and see it come back.
    const session = new Session(httpApi(base, token), memoryEnvironment())
    const stopSession = await session.start()
    session.navigate('tasks')
    const settle = async () => {
      for (let i = 0; i < 3; i++) {
        await session.cache.idle()
        await new Promise((resolve) => setTimeout(resolve, 200))
      }
    }
    await settle()
    session.startCreate()
    session.setEditDraft('from my phone')
    session.commitEdit()
    await settle()
    assert.ok(session.get().view!.rows.some((row) => row.entity.data.text === 'from my phone'))

    // A change made on the desktop reaches the phone by its change stream.
    core.actions.create({ parent: 'tasks', text: 'from my desk' })
    await settle()
    assert.ok(session.get().view!.rows.some((row) => row.entity.data.text === 'from my desk'))
    stopSession()
  } finally {
    stop()
  }
})
