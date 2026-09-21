import type { Blocker } from '@conswap/common/types'
import type { Context } from '../context.js'
import { openIfUnblocked, satisfyBlocker } from '../lifecycle.js'
import { now, toBlocker } from '../topics.js'
import { definitionFor } from './definitions.js'

type BlockerRow = Parameters<typeof toBlocker>[0]

function dueBlockers(context: Context): Blocker[] {
  return (
    context.db
      .prepare(
        `SELECT * FROM blockers
         WHERE satisfied_at IS NULL AND cancelled_at IS NULL AND due_at IS NOT NULL AND due_at <= ?
         ORDER BY due_at LIMIT 20`,
      )
      .all(now()) as BlockerRow[]
  ).map(toBlocker)
}

function reschedule(context: Context, blocker: Blocker, seconds: number, error?: string): void {
  const dueAt = new Date(Date.now() + Math.max(seconds, 5) * 1000).toISOString()
  context.db.prepare('UPDATE blockers SET due_at = ?, last_error = ? WHERE id = ?').run(dueAt, error ?? null, blocker.id)
}

/**
 * One pass over everything that is ready to be looked at. Checks run one after
 * another so that a slow `gh` call cannot pile up behind itself.
 */
export async function checkBlockers(context: Context): Promise<void> {
  for (const blocker of dueBlockers(context)) {
    const definition = definitionFor(blocker)
    if (!definition?.check) {
      context.db.prepare('UPDATE blockers SET due_at = NULL WHERE id = ?').run(blocker.id)
      continue
    }
    // Hold the slot while the check runs, so the next tick does not start it again.
    reschedule(context, blocker, definition.interval ?? 60)
    try {
      const outcome = await definition.check(context, blocker)
      if (outcome.satisfied) {
        satisfyBlocker(context, blocker, outcome.note ?? `${blocker.label} — done`)
        const opened = openIfUnblocked(context, blocker.topicId)
        context.changed([blocker.topicId], opened)
      } else {
        reschedule(context, blocker, outcome.retryIn ?? definition.interval ?? 60, outcome.error)
        if (outcome.error) context.changed([blocker.topicId], false)
      }
    } catch (error) {
      reschedule(context, blocker, definition.interval ?? 60, String(error))
      context.log('blockers', `check failed for ${blocker.type}`, String(error))
    }
  }
}
