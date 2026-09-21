import type { ActionResult, TopicId } from '@conswap/common/types'
import type { Context } from './context.js'
import type { ClaudeIntegration } from './integrations/claude.js'
import type { SlackIntegration } from './integrations/slack.js'
import {
  addBlocker,
  addEvent,
  cancelBlocker,
  closeTopic,
  openIfUnblocked,
  promoteTopic,
  resolveTopic,
} from './lifecycle.js'
import { createTopic, linkTopics, requireTopic, topicBlockers, unlinkTopics, updateTopic } from './topics.js'
import { nextOpenTopic } from './views.js'
import { now } from './topics.js'

type Args = Record<string, unknown>

function stringArg(args: Args, key: string, fallback?: string): string {
  const value = args[key]
  if (typeof value === 'string') return value
  if (fallback !== undefined) return fallback
  throw new Error(`${key} is required`)
}

function optionalString(args: Args, key: string): string | undefined {
  const value = args[key]
  return typeof value === 'string' ? value : undefined
}

function record(args: Args, key: string): Record<string, unknown> {
  const value = args[key]
  return value !== null && typeof value === 'object' ? (value as Record<string, unknown>) : {}
}

export interface Action {
  name: string
  run(context: Context, args: Args): Promise<ActionResult> | ActionResult
}

function integration<T>(context: Context, name: string): T {
  const found = context.integrations.get(name)
  if (!found) throw new Error(`${name} is not running`)
  return found as T
}

const actions: Action[] = [
  {
    name: 'topic.create',
    run(context, args) {
      const topic = createTopic(context.db, {
        text: stringArg(args, 'text'),
        type: optionalString(args, 'type') ?? 'topic',
        metadata: record(args, 'metadata'),
        open: true,
      })
      const parentId = optionalString(args, 'parentId')
      if (parentId) linkTopics(context.db, parentId, topic.id, 'reference')
      context.changed([topic.id, ...(parentId ? [parentId] : [])])
      return { ok: true, focus: topic.id }
    },
  },
  {
    name: 'topic.note',
    run(context, args) {
      const topicId = stringArg(args, 'topicId')
      addEvent(context, topicId, { type: 'note', text: stringArg(args, 'text') }, { silent: true })
      return { ok: true }
    },
  },
  {
    name: 'topic.update',
    run(context, args) {
      const topicId = stringArg(args, 'topicId')
      updateTopic(context.db, topicId, {
        ...(args.text === undefined ? {} : { text: stringArg(args, 'text') }),
        ...(args.metadata === undefined ? {} : { metadata: record(args, 'metadata') }),
      })
      context.changed([topicId])
      return { ok: true }
    },
  },
  {
    name: 'topic.link',
    run(context, args) {
      const parentId = stringArg(args, 'parentId')
      const childId = stringArg(args, 'childId')
      const type = optionalString(args, 'type') === 'reference' ? 'reference' : 'watch'
      linkTopics(context.db, parentId, childId, type)
      context.changed([parentId, childId])
      return { ok: true }
    },
  },
  {
    name: 'topic.unlink',
    run(context, args) {
      const parentId = stringArg(args, 'parentId')
      const childId = stringArg(args, 'childId')
      unlinkTopics(context.db, parentId, childId)
      context.changed([parentId, childId])
      return { ok: true }
    },
  },
  {
    name: 'topic.resolve',
    run(context, args) {
      const topicId = stringArg(args, 'topicId')
      const resolved = args.resolved === undefined ? true : args.resolved === true
      resolveTopic(context, topicId, resolved)
      return { ok: true }
    },
  },
  {
    name: 'topic.close',
    run(context, args) {
      const topicId = stringArg(args, 'topicId')
      const blocker = record(args, 'blocker')
      closeTopic(
        context,
        topicId,
        Object.keys(blocker).length === 0
          ? null
          : { topicId, type: stringArg(blocker, 'type'), config: record(blocker, 'config') },
      )
      return { ok: true }
    },
  },
  {
    name: 'topic.open',
    run(context, args) {
      const topicId = stringArg(args, 'topicId')
      for (const blocker of topicBlockers(context.db, topicId)) cancelBlocker(context, blocker.id)
      updateTopic(context.db, topicId, { open: true, resolved: false })
      context.changed([topicId])
      return { ok: true, focus: topicId }
    },
  },
  {
    name: 'topic.promote',
    run(context, args) {
      const parentId = stringArg(args, 'parentId')
      const childId = stringArg(args, 'childId')
      promoteTopic(context, parentId, childId)
      return { ok: true, focus: childId }
    },
  },
  {
    name: 'blocker.add',
    run(context, args) {
      const topicId = stringArg(args, 'topicId')
      addBlocker(context, { topicId, type: stringArg(args, 'type'), config: record(args, 'config') })
      updateTopic(context.db, topicId, { open: false })
      context.changed([topicId])
      return { ok: true }
    },
  },
  {
    name: 'blocker.cancel',
    run(context, args) {
      const blockerId = stringArg(args, 'blockerId')
      cancelBlocker(context, blockerId)
      context.changed([])
      return { ok: true }
    },
  },
  {
    name: 'slack.send',
    async run(context, args) {
      const topicId = stringArg(args, 'topicId')
      await integration<SlackIntegration>(context, 'slack').send(topicId, stringArg(args, 'text'))
      context.changed([topicId])
      return { ok: true }
    },
  },
  {
    name: 'claude.prompt',
    async run(context, args) {
      const topicId = stringArg(args, 'topicId')
      const { runId } = await integration<ClaudeIntegration>(context, 'claude').prompt(topicId, stringArg(args, 'text'))
      if (args.wait !== false) {
        addBlocker(context, { topicId, type: 'claude', config: { runId } })
        updateTopic(context.db, topicId, { open: false })
      }
      context.changed([topicId])
      return { ok: true }
    },
  },
  {
    name: 'queue.next',
    run(context, args) {
      const next = nextOpenTopic(context, optionalString(args, 'after') ?? null)
      return next ? { ok: true, focus: next } : { ok: true }
    },
  },
  {
    name: 'topic.unblock',
    run(context, args) {
      const topicId = stringArg(args, 'topicId')
      requireTopic(context.db, topicId)
      openIfUnblocked(context, topicId)
      context.changed([topicId])
      return { ok: true }
    },
  },
]

const byName = new Map(actions.map((action) => [action.name, action]))

export function actionNames(): string[] {
  return [...byName.keys()]
}

/**
 * Every mutation lands here and is written down before it runs, so a failed
 * optimistic update on the client can always be replayed rather than lost.
 */
export async function performAction(
  context: Context,
  id: string,
  name: string,
  args: Args,
): Promise<ActionResult> {
  const existing = context.db.prepare('SELECT status, result FROM mutations WHERE id = ?').get(id) as
    | { status: string; result: string | null }
    | undefined
  if (existing?.status === 'applied' && existing.result) {
    return JSON.parse(existing.result) as ActionResult
  }

  context.db
    .prepare(
      `INSERT INTO mutations (id, name, args, created_at, status) VALUES (?, ?, ?, ?, 'pending')
       ON CONFLICT (id) DO UPDATE SET status = 'pending'`,
    )
    .run(id, name, JSON.stringify(args), now())

  const action = byName.get(name)
  if (!action) {
    const failure: ActionResult = { ok: false, error: `there is no action called ${name}` }
    context.db.prepare("UPDATE mutations SET status = 'failed', error = ? WHERE id = ?").run(failure.error, id)
    return failure
  }

  try {
    const result = await action.run(context, args)
    context.db
      .prepare("UPDATE mutations SET status = 'applied', result = ? WHERE id = ?")
      .run(JSON.stringify(result), id)
    return result
  } catch (error) {
    const message = String(error instanceof Error ? error.message : error)
    context.db.prepare("UPDATE mutations SET status = 'failed', error = ? WHERE id = ?").run(message, id)
    context.log('actions', `${name} failed`, message)
    return { ok: false, error: message }
  }
}
