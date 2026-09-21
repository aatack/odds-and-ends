import { randomUUID } from 'node:crypto'
import type { Blocker, LinkType, Topic, TopicId } from '@conswap/common/types'
import { definitionFor } from './blockers/definitions.js'
import type { ActivitySignal } from './blockers/types.js'
import type { Context } from './context.js'
import {
  createTopic,
  getTopic,
  isEventType,
  linkTopics,
  now,
  parentIds,
  requireTopic,
  toBlocker,
  topicBlockers,
  touchTopic,
  updateTopic,
} from './topics.js'
import type { CreateTopic } from './topics.js'

/** How far up the watch graph one piece of activity is allowed to travel. */
const maximumDepth = 24

/**
 * Walks up `watch` links from a topic, handing each ancestor the chain of ids
 * that leads back down to where the activity happened.
 */
function walkUp(context: Context, startId: TopicId, visit: (id: TopicId, path: TopicId[]) => void): void {
  const seen = new Set<TopicId>()
  const frontier: { id: TopicId; path: TopicId[] }[] = [{ id: startId, path: [startId] }]
  while (frontier.length > 0) {
    const step = frontier.pop() as { id: TopicId; path: TopicId[] }
    if (seen.has(step.id)) continue
    seen.add(step.id)
    visit(step.id, step.path)
    if (step.path.length >= maximumDepth) continue
    for (const parent of parentIds(context.db, step.id, 'watch')) {
      frontier.push({ id: parent, path: [parent, ...step.path] })
    }
  }
}

function liveBlockers(context: Context, topicId: TopicId): Blocker[] {
  return topicBlockers(context.db, topicId, false)
}

export function satisfyBlocker(context: Context, blocker: Blocker, note?: string): void {
  context.db.prepare('UPDATE blockers SET satisfied_at = ?, last_error = NULL WHERE id = ?').run(now(), blocker.id)
  if (note) {
    addEvent(context, blocker.topicId, { type: 'system', text: note }, { silent: true })
  }
}

export function cancelBlocker(context: Context, blockerId: string): void {
  const row = context.db.prepare('SELECT * FROM blockers WHERE id = ?').get(blockerId) as
    | Parameters<typeof toBlocker>[0]
    | undefined
  if (!row) return
  context.db.prepare('UPDATE blockers SET cancelled_at = ? WHERE id = ?').run(now(), blockerId)
  openIfUnblocked(context, toBlocker(row).topicId)
}

/** A topic with nothing left to wait for is a topic that needs attention. */
export function openIfUnblocked(context: Context, topicId: TopicId): boolean {
  const topic = getTopic(context.db, topicId)
  if (!topic) return false
  if (isEventType(topic.type)) return false
  if (liveBlockers(context, topicId).length > 0) return false
  if (topic.open) return false
  updateTopic(context.db, topicId, { open: true })
  return true
}

export interface AddBlocker {
  topicId: TopicId
  type: string
  config?: Record<string, unknown>
  label?: string
}

export function addBlocker(context: Context, input: AddBlocker): Blocker {
  const definition = definitionFor({ type: input.type })
  if (!definition) throw new Error(`there is no blocker of type ${input.type}`)
  const config = input.config ?? {}
  const blocker: Blocker = {
    id: randomUUID(),
    topicId: input.topicId,
    type: input.type,
    label: input.label ?? definition.describe(context, config),
    config,
    createdAt: now(),
    dueAt: definition.check ? now() : null,
    satisfiedAt: null,
    cancelledAt: null,
    lastError: null,
  }
  context.db
    .prepare(
      `INSERT INTO blockers (id, topic_id, type, label, config, state, created_at, due_at)
       VALUES (@id, @topic_id, @type, @label, @config, '{}', @created_at, @due_at)`,
    )
    .run({
      id: blocker.id,
      topic_id: blocker.topicId,
      type: blocker.type,
      label: blocker.label,
      config: JSON.stringify(blocker.config),
      created_at: blocker.createdAt,
      due_at: blocker.dueAt,
    })
  return blocker
}

export interface AddEventOptions {
  link?: LinkType
  /** Adds the topic without waking any ancestors: used for the app's own notes. */
  silent?: boolean
}

/**
 * The one way anything enters a topic. Creates the child, links it, and lets the
 * activity travel up the watch graph.
 */
export function addEvent(
  context: Context,
  parentId: TopicId,
  child: CreateTopic,
  options: AddEventOptions = {},
): Topic {
  const parent = requireTopic(context.db, parentId)
  const topic = child.id && getTopic(context.db, child.id) ? requireTopic(context.db, child.id) : createTopic(context.db, child)
  linkTopics(context.db, parent.id, topic.id, options.link ?? 'watch', topic.createdAt)
  if (options.silent) {
    touchTopic(context.db, parent.id)
    context.changed([parent.id, topic.id])
  } else {
    recordActivity(context, topic, parent.id)
  }
  return topic
}

/**
 * Tells every watching ancestor that something happened, satisfying the blockers
 * that were waiting for it and reopening whatever is now free.
 */
export function recordActivity(context: Context, topic: Topic, fromId: TopicId): void {
  const at = now()
  const touched = new Set<TopicId>([topic.id])

  walkUp(context, fromId, (ancestorId, path) => {
    touched.add(ancestorId)
    touchTopic(context.db, ancestorId, at)
    const signal: ActivitySignal = { topic, path: [...path, topic.id], at }
    for (const blocker of liveBlockers(context, ancestorId)) {
      const definition = definitionFor(blocker)
      if (!definition?.wakesOn) continue
      if (definition.wakesOn(context, blocker, signal)) {
        satisfyBlocker(context, blocker, `${blocker.label} — done`)
      }
    }
    openIfUnblocked(context, ancestorId)
  })

  context.changed([...touched])
}

/** Closes a topic behind a blocker. This is the normal way to put something down. */
export function closeTopic(context: Context, topicId: TopicId, blocker: AddBlocker | null): Blocker | null {
  const added = blocker ? addBlocker(context, { ...blocker, topicId }) : null
  updateTopic(context.db, topicId, { open: false })
  context.changed([topicId])
  return added
}

export function resolveTopic(context: Context, topicId: TopicId, resolved: boolean): Topic {
  const topic = updateTopic(context.db, topicId, { resolved, ...(resolved ? { open: false } : {}) })
  context.changed([topicId])
  return topic
}

/**
 * Takes a child of the focused topic and makes it a piece of work in its own
 * right, putting the parent down until that work is signed off.
 */
export function promoteTopic(context: Context, parentId: TopicId, childId: TopicId): Blocker {
  updateTopic(context.db, childId, { open: true, resolved: false })
  const blocker = addBlocker(context, { topicId: parentId, type: 'subtopics', config: { ids: [childId] } })
  updateTopic(context.db, parentId, { open: false })
  context.changed([parentId, childId])
  return blocker
}
