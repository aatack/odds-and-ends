// Whether a task is open now, once what it waits on is taken into account.
//
//   npm test

import assert from 'node:assert/strict'
import { checkboxOf, isPaused, isWaiting, nextWake, openNow, parseDuration, PENDING, withSnooze, type WaitProbes } from '../src/core/open'

const NOW = Date.parse('2026-09-22T12:00:00Z')
const LATER = '2026-09-23T12:00:00Z'
const EARLIER = '2026-09-21T12:00:00Z'

const probes = (over: Partial<WaitProbes> = {}): WaitProbes => ({
  now: NOW,
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

test('is paused while a snooze keeps it shut, and not for code alone', () => {
  assert.equal(isPaused({ snooze: LATER }, NOW), true)
  assert.equal(isPaused({ snooze: EARLIER }, NOW), false)
  assert.equal(isPaused({ code: 'true' }, NOW), false)
  assert.equal(isPaused({ snooze: LATER, code: 'true' }, NOW), true)
  assert.equal(isPaused(true, NOW), false)
})

test('wakes at the soonest snooze still to run out', () => {
  assert.equal(nextWake([{ snooze: LATER }, { snooze: EARLIER }, { code: 'c' }], NOW), Date.parse(LATER))
  assert.equal(nextWake({ snooze: EARLIER }, NOW), undefined)
})

test('opens as soon as any one condition does', () => {
  const yes = probes({ code: () => ({ result: true }) })
  // The code has said so, though the snooze has not run out.
  assert.equal(openNow([{ code: 'c' }, { snooze: LATER }], yes), true)
  assert.equal(openNow({ code: 'c', snooze: LATER }, probes({ code: () => ({ result: false }) })), false)
})

test('treats a condition it does not know as nothing to wait on', () => {
  assert.equal(openNow({ toolCall: 'c' }, probes()), true)
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
  assert.equal(checkboxOf([{ code: 'c' }]), true)
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
  assert.deepEqual(withSnooze({ code: 'c', snooze: EARLIER }, LATER), { code: 'c', snooze: LATER })
  assert.deepEqual(withSnooze([{ code: 'c' }, { snooze: EARLIER }], LATER), [
    { code: 'c' },
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
