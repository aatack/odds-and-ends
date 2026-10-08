import assert from 'node:assert/strict'
import { test } from 'node:test'
import { Client } from '@modelcontextprotocol/sdk/client/index.js'
import { StreamableHTTPClientTransport } from '@modelcontextprotocol/sdk/client/streamableHttp.js'
import { link, values } from '../core/graph/events.ts'
import { mcpConfig } from '../core/mcp.ts'
import { memoryCore } from '../core/testing.ts'
import { startMcpServer } from './mcp.ts'

/** Starts the server on a free port and connects a client to it, as a Claude session would. */
async function connected() {
  const core = memoryCore()
  const stop = startMcpServer(core, 0)
  // The core is told where the server is once it listens.
  for (let i = 0; i < 50 && !core['mcpAddress']; i++) await new Promise((resolve) => setTimeout(resolve, 10))
  const address = core['mcpAddress'] as unknown as { url: string; token: string }
  return { core, stop, address }
}

const text = (result: unknown): string => ((result as { content: { text: string }[] }).content[0]?.text ?? '')

test('mcp: a Claude session reads the app over HTTP: modules, read, and a paged walk with ids in a column', async (t) => {
  const { core, stop, address } = await connected()
  // Closed even when an assertion fails, or the open server keeps the run going.
  t.after(stop)
  assert.match(address.url, /^http:\/\/127\.0\.0\.1:\d+\/mcp$/)
  assert.equal(JSON.parse(mcpConfig(address)).mcpServers['detail-views'].headers.Authorization, `Bearer ${address.token}`)
  core.owned.write([
    ...values('a', { text: 'groceries' }, 1, 'me'),
    ...values('ab', { text: 'milk' }, 2, 'me'),
    ...values('abc', { text: 'semi-skimmed\nnot whole', open: true }, 3, 'me'),
    ...values('b', { text: 'chores' }, 4, 'me'),
    link('tasks', 'a', 1, 'me'),
    link('a', 'ab', 2, 'me'),
    link('ab', 'abc', 3, 'me'),
    link('tasks', 'b', 4, 'me'),
    ...values('slack', { 'secret.token': 'xoxp-1', self: 'UME' }, 5, 'module'),
  ])

  const client = new Client({ name: 'test', version: '0' })
  t.after(() => client.close())
  await client.connect(new StreamableHTTPClientTransport(new URL(address.url), { requestInit: { headers: { Authorization: `Bearer ${address.token}` } } }))
  assert.deepEqual((await client.listTools()).tools.map((tool) => tool.name), ['modules', 'read', 'walk'])
  assert.match(client.getInstructions() ?? '', /graph of entities/)

  const modules = text(await client.callTool({ name: 'modules', arguments: {} }))
  assert.match(modules, /^slack {2,}Slack$/m)
  assert.match(modules, /^tasks {2,}Tasks$/m)

  // The ids are padded to one width, so the bullets line up.
  const whole = text(await client.callTool({ name: 'walk', arguments: { path: 'tasks' } }))
  assert.equal(
    whole,
    [
      'tasks  Tasks',
      'a      - groceries',
      'ab         - milk',
      'abc            - [ ] semi-skimmed',
      '                     not whole',
      'b      - chores',
      '',
      '[5 shown; that is everything under this path.]',
    ].join('\n'),
  )

  // A page that stops short says where to carry on, and carrying on keeps the shape.
  const first = text(await client.callTool({ name: 'walk', arguments: { path: 'tasks', limit: 2 } }))
  assert.match(first, /call walk again with path: \["tasks","a","ab"\]/)
  const second = text(await client.callTool({ name: 'walk', arguments: { path: ['tasks', 'a', 'ab'], limit: 2 } }))
  assert.match(second, /^ab {2,}- milk$/m)
  assert.match(second, /path: \["tasks","b"\]/)
  const shallow = text(await client.callTool({ name: 'walk', arguments: { path: 'tasks', maxDepth: 1 } }))
  assert.ok(!shallow.includes('milk'))

  const read = JSON.parse(text(await client.callTool({ name: 'read', arguments: { ids: ['ab', 'slack'] } })))
  assert.deepEqual(read.ab.children, ['abc'])
  assert.deepEqual(read.ab.parents, ['a'])
  assert.equal(read.ab.values.text, 'milk')
  // Secrets never leave the core.
  assert.equal(read.slack.values['secret.token'], undefined)
  assert.equal(read.slack.values.self, 'UME')

  // Without the token, nothing.
  const refused = await fetch(address.url, { method: 'POST', headers: { 'content-type': 'application/json' }, body: '{}' })
  assert.equal(refused.status, 401)
  await client.close()
})
