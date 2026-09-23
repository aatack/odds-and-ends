// Whether a task is open now, once what it waits on is taken into account.
//
//   npm test

import assert from 'node:assert/strict'
import { checkboxOf, isWaiting, openNow, parseDuration, PENDING, withSnooze, type WaitProbes } from '../src/core/open'

const NOW = Date.parse('2026-09-22T12:00:00Z')
const LATER = '2026-09-23T12:00:00Z'
const EARLIER = '2026-09-21T12:00:00Z'

const probes = (over: Partial<WaitProbes> = {}): WaitProbes => ({
  now: NOW,
  running: () => false,
  code: () => undefined,
  ...over,
})

const tests: [string, () => void][] = []
const test = (name: string, run: () => void): void => void tests.push([name, run])

test('waits out a snooze, and opens once it has passed', () => {
  assert.equal(openNow({ snooze: LATER }, probes()), false)
  assert.equal(openNow({ snooze: EARLIER }, probes()), true)
  assert.equal(openNow({ snooze: Date.parse(LATER) }, probes()), false)
})

test('waits on a tool call only while it runs', () => {
  const open = { toolCall: 'call-1' }
  assert.equal(openNow(open, probes({ running: (id) => id === 'call-1' })), false)
  assert.equal(openNow(open, probes()), true)
})

test('opens as soon as any one condition does', () => {
  const running = probes({ running: () => true })
  // Still running, but the snooze has run out: offered rather than stuck.
  assert.equal(openNow({ toolCall: 'c', snooze: EARLIER }, running), true)
  // Finished, though the snooze has not run out.
  assert.equal(openNow([{ toolCall: 'c' }, { snooze: LATER }], probes()), true)
  assert.equal(openNow({ toolCall: 'c', snooze: LATER }, running), false)
})

test('opens when code says so, and asks for it only when nothing cheaper has', () => {
  const asked: string[] = []
  const code = (result: unknown) =>
    probes({
      code: (body) => {
        asked.push(body)
        return { result }
      },
    })
  assert.equal(openNow({ code: 'yes' }, code(true)), true)
  assert.equal(openNow({ code: 'no' }, code(false)), false)
  assert.equal(openNow({ code: 'no' }, code(undefined)), false)
  asked.length = 0
  assert.equal(openNow({ code: 'skipped', snooze: EARLIER }, code(false)), true)
  assert.deepEqual(asked, [])
})

test('says it cannot tell yet while code has to run', () => {
  assert.equal(openNow({ code: 'x' }, probes()), PENDING)
  assert.equal(isWaiting({ code: 'x' }, probes()), undefined)
})

test('leaves an open that waits on nothing as written', () => {
  assert.equal(openNow(false, probes()), false)
  assert.equal(openNow(undefined, probes()), undefined)
  assert.equal(openNow(true, probes()), true)
  assert.equal(openNow({}, probes()), true)
})

test('draws a waiting task as an unticked one', () => {
  assert.equal(checkboxOf({ snooze: LATER }), true)
  assert.equal(checkboxOf([{ toolCall: 'c' }]), true)
  assert.equal(checkboxOf(false), false)
  assert.equal(checkboxOf('yes'), undefined)
})

test('reads a duration in hours, days or weeks', () => {
  assert.equal(parseDuration('3h'), 3 * 3_600_000)
  assert.equal(parseDuration(' 2D '), 2 * 86_400_000)
  assert.equal(parseDuration('1w'), 604_800_000)
  assert.equal(parseDuration('3'), null)
  assert.equal(parseDuration('3m'), null)
  assert.equal(parseDuration(''), null)
})

test('sets a snooze, keeping what else the task waits on', () => {
  assert.deepEqual(withSnooze(undefined, LATER), { snooze: LATER })
  assert.deepEqual(withSnooze(true, LATER), { snooze: LATER })
  assert.deepEqual(withSnooze(false, LATER), { snooze: LATER })
  assert.deepEqual(withSnooze({ toolCall: 'c', snooze: EARLIER }, LATER), { toolCall: 'c', snooze: LATER })
  assert.deepEqual(withSnooze([{ toolCall: 'c' }, { snooze: EARLIER }], LATER), [
    { toolCall: 'c' },
    { snooze: LATER },
  ])
})

let failed = 0
for (const [name, run] of tests) {
  try {
    run()
    console.log(`  ok  ${name}`)
  } catch (e) {
    failed++
    console.error(`fail  ${name}`)
    console.error(e instanceof Error ? `      ${e.message}` : e)
  }
}
console.log(failed ? `\n${failed} of ${tests.length} failed` : `\n${tests.length} passed`)
process.exit(failed ? 1 : 0)
