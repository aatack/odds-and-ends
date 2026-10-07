import assert from 'node:assert/strict'
import { test } from 'node:test'
import { values } from '../../graph/events.ts'
import { memoryCore } from '../../testing.ts'

test('recents: a visit puts an item first; a note is not kept; the list stays in the cache store, capped', () => {
  let time = 1000
  const core = memoryCore({ now: () => (time += 10) })
  core.cache.write([
    ...values('slack:conv:C1', { type: 'slack.conversation', channel: 'C1', kind: 'channel', name: 'one' }, 0, 'slack'),
    ...values('slack:conv:C2', { type: 'slack.conversation', channel: 'C2', kind: 'channel', name: 'two' }, 0, 'slack'),
  ])
  const note = core.actions.create({ parent: 'tasks', text: 'just a note' }).events.find((e) => e.type === 'link')
  const order = () => core.focus('recents').children.map((child) => child.id)
  core.actions.visit({ id: 'slack:conv:C1' })
  core.actions.visit({ id: 'slack:conv:C2' })
  core.actions.visit({ id: note?.type === 'link' ? note.destinationId : '' })
  core.actions.visit({ id: 'tasks' })
  assert.deepEqual(order(), ['tasks', 'slack:conv:C2', 'slack:conv:C1'])
  // Again: to the front.
  core.actions.visit({ id: 'slack:conv:C1' })
  assert.deepEqual(order(), ['slack:conv:C1', 'tasks', 'slack:conv:C2'])
  // Temporary: in the cache store only, gone with it.
  assert.equal(core.owned.read(['recents']).length, 0)
  // Capped: the oldest go.
  for (let i = 0; i < 205; i++) core.actions.visit({ id: `github:pr:https://github.com/o/r/pull/${i}` })
  assert.equal(order().length, 200)
  assert.equal(order()[0], 'github:pr:https://github.com/o/r/pull/204')
  assert.ok(!order().includes('slack:conv:C1'))
})
