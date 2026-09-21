import { randomUUID } from 'node:crypto'
import type { Blocker, LinkType, Topic, TopicId, TopicNode } from '@conswap/common/types'
import type { Db } from './database.js'

/**
 * Types that are an entry in somebody's feed rather than something to come back
 * to. They are created closed, and never appear in the queue on their own.
 */
export const eventTypes = new Set([
  'note',
  'slack_message',
  'claude_prompt',
  'claude_response',
  'github_event',
  'system',
])

export function isEventType(type: string): boolean {
  return eventTypes.has(type)
}

export function now(): string {
  return new Date().toISOString()
}

interface TopicRow {
  id: string
  type: string
  text: string
  metadata: string
  created_at: string
  updated_at: string
  open: number
  resolved: number
}

function toTopic(row: TopicRow): Topic {
  let metadata: Record<string, unknown> = {}
  try {
    metadata = JSON.parse(row.metadata) as Record<string, unknown>
  } catch {
    metadata = {}
  }
  return {
    id: row.id,
    type: row.type,
    text: row.text,
    metadata,
    createdAt: row.created_at,
    updatedAt: row.updated_at,
    open: row.open === 1,
    resolved: row.resolved === 1,
  }
}

export function getTopic(db: Db, id: TopicId): Topic | null {
  const row = db.prepare('SELECT * FROM topics WHERE id = ?').get(id) as TopicRow | undefined
  return row ? toTopic(row) : null
}

export function requireTopic(db: Db, id: TopicId): Topic {
  const topic = getTopic(db, id)
  if (!topic) throw new Error(`no topic ${id}`)
  return topic
}

export interface CreateTopic {
  id?: TopicId
  type?: string
  text?: string
  metadata?: Record<string, unknown>
  open?: boolean
  resolved?: boolean
  createdAt?: string
}

export function createTopic(db: Db, input: CreateTopic): Topic {
  const type = input.type ?? 'topic'
  const timestamp = input.createdAt ?? now()
  const topic: Topic = {
    id: input.id ?? randomUUID(),
    type,
    text: input.text ?? '',
    metadata: input.metadata ?? {},
    createdAt: timestamp,
    updatedAt: timestamp,
    open: input.open ?? !isEventType(type),
    resolved: input.resolved ?? false,
  }
  db.prepare(
    `INSERT INTO topics (id, type, text, metadata, created_at, updated_at, open, resolved)
     VALUES (@id, @type, @text, @metadata, @created_at, @updated_at, @open, @resolved)`,
  ).run({
    id: topic.id,
    type: topic.type,
    text: topic.text,
    metadata: JSON.stringify(topic.metadata),
    created_at: topic.createdAt,
    updated_at: topic.updatedAt,
    open: topic.open ? 1 : 0,
    resolved: topic.resolved ? 1 : 0,
  })
  return topic
}

/** Creates the topic only if the id is free; otherwise returns what is there. */
export function upsertTopic(db: Db, input: CreateTopic & { id: TopicId }): { topic: Topic; created: boolean } {
  const existing = getTopic(db, input.id)
  if (existing) return { topic: existing, created: false }
  return { topic: createTopic(db, input), created: true }
}

export function updateTopic(
  db: Db,
  id: TopicId,
  patch: { text?: string; type?: string; metadata?: Record<string, unknown>; open?: boolean; resolved?: boolean },
): Topic {
  const topic = requireTopic(db, id)
  const next: Topic = {
    ...topic,
    ...(patch.text === undefined ? {} : { text: patch.text }),
    ...(patch.type === undefined ? {} : { type: patch.type }),
    ...(patch.metadata === undefined ? {} : { metadata: { ...topic.metadata, ...patch.metadata } }),
    ...(patch.open === undefined ? {} : { open: patch.open }),
    ...(patch.resolved === undefined ? {} : { resolved: patch.resolved }),
    updatedAt: now(),
  }
  db.prepare(
    `UPDATE topics SET type = @type, text = @text, metadata = @metadata,
       updated_at = @updated_at, open = @open, resolved = @resolved WHERE id = @id`,
  ).run({
    id: next.id,
    type: next.type,
    text: next.text,
    metadata: JSON.stringify(next.metadata),
    updated_at: next.updatedAt,
    open: next.open ? 1 : 0,
    resolved: next.resolved ? 1 : 0,
  })
  return next
}

export function touchTopic(db: Db, id: TopicId, at: string = now()): void {
  db.prepare('UPDATE topics SET updated_at = ? WHERE id = ?').run(at, id)
}

export function linkTopics(
  db: Db,
  parentId: TopicId,
  childId: TopicId,
  type: LinkType = 'watch',
  createdAt: string = now(),
): boolean {
  if (parentId === childId) return false
  const parent = getTopic(db, parentId)
  const child = getTopic(db, childId)
  if (!parent || !child) throw new Error('cannot link a topic that does not exist')
  if (isAncestor(db, childId, parentId)) throw new Error('that link would make a cycle')
  const result = db
    .prepare(
      `INSERT INTO links (parent_id, parent_type, child_id, child_type, type, created_at)
       VALUES (?, ?, ?, ?, ?, ?) ON CONFLICT DO NOTHING`,
    )
    .run(parentId, parent.type, childId, child.type, type, createdAt)
  return result.changes > 0
}

export function unlinkTopics(db: Db, parentId: TopicId, childId: TopicId): void {
  db.prepare('DELETE FROM links WHERE parent_id = ? AND child_id = ?').run(parentId, childId)
}

/** True when `candidate` can be reached by walking down from `rootId`. */
export function isAncestor(db: Db, rootId: TopicId, candidate: TopicId): boolean {
  const seen = new Set<TopicId>()
  const frontier = [rootId]
  while (frontier.length > 0) {
    const id = frontier.pop() as TopicId
    if (id === candidate) return true
    if (seen.has(id)) continue
    seen.add(id)
    for (const child of childIds(db, id)) frontier.push(child)
  }
  return false
}

export function childIds(db: Db, id: TopicId): TopicId[] {
  return (db.prepare('SELECT child_id FROM links WHERE parent_id = ? ORDER BY created_at').all(id) as {
    child_id: string
  }[]).map((row) => row.child_id)
}

export function parentIds(db: Db, id: TopicId, type?: LinkType): TopicId[] {
  const rows = type
    ? (db.prepare('SELECT parent_id FROM links WHERE child_id = ? AND type = ?').all(id, type) as {
        parent_id: string
      }[])
    : (db.prepare('SELECT parent_id FROM links WHERE child_id = ?').all(id) as { parent_id: string }[])
  return rows.map((row) => row.parent_id)
}

export function parents(db: Db, id: TopicId): Topic[] {
  return parentIds(db, id)
    .map((parentId) => getTopic(db, parentId))
    .filter((topic): topic is Topic => topic !== null)
}

export function childCount(db: Db, id: TopicId): number {
  const row = db.prepare('SELECT COUNT(*) AS count FROM links WHERE parent_id = ?').get(id) as { count: number }
  return row.count
}

/**
 * The feed for a topic. Children come back in the order they were linked; a child
 * that has children of its own is only expanded when it is small enough to read
 * inline, or when the caller has asked for it.
 */
export function readChildren(db: Db, id: TopicId, depth: number, expanded: Set<TopicId>): TopicNode[] {
  const rows = db
    .prepare('SELECT child_id, type FROM links WHERE parent_id = ? ORDER BY created_at, child_id')
    .all(id) as { child_id: string; type: LinkType }[]
  const nodes: TopicNode[] = []
  for (const row of rows) {
    const topic = getTopic(db, row.child_id)
    if (!topic) continue
    const count = childCount(db, topic.id)
    const open = depth > 0 && (count === 0 || expanded.has(topic.id))
    nodes.push({
      topic,
      link: row.type,
      childCount: count,
      children: open && count > 0 ? readChildren(db, topic.id, depth - 1, expanded) : null,
    })
  }
  return nodes
}

interface BlockerRow {
  id: string
  topic_id: string
  type: string
  label: string
  config: string
  state: string
  created_at: string
  due_at: string | null
  satisfied_at: string | null
  cancelled_at: string | null
  last_error: string | null
}

export function toBlocker(row: BlockerRow): Blocker {
  let config: Record<string, unknown> = {}
  try {
    config = JSON.parse(row.config) as Record<string, unknown>
  } catch {
    config = {}
  }
  return {
    id: row.id,
    topicId: row.topic_id,
    type: row.type,
    label: row.label,
    config,
    createdAt: row.created_at,
    dueAt: row.due_at,
    satisfiedAt: row.satisfied_at,
    cancelledAt: row.cancelled_at,
    lastError: row.last_error,
  }
}

export function topicBlockers(db: Db, id: TopicId, includeFinished = false): Blocker[] {
  const sql = includeFinished
    ? 'SELECT * FROM blockers WHERE topic_id = ? ORDER BY created_at DESC LIMIT 20'
    : 'SELECT * FROM blockers WHERE topic_id = ? AND satisfied_at IS NULL AND cancelled_at IS NULL ORDER BY created_at'
  return (db.prepare(sql).all(id) as BlockerRow[]).map(toBlocker)
}

export function blockerState(db: Db, id: string): Record<string, unknown> {
  const row = db.prepare('SELECT state FROM blockers WHERE id = ?').get(id) as { state: string } | undefined
  if (!row) return {}
  try {
    return JSON.parse(row.state) as Record<string, unknown>
  } catch {
    return {}
  }
}
