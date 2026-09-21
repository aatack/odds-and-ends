import type { ActionRequest, BlockerSuggestion, TopicDetail, TopicId, TopicNode } from './types'

export type ComposerMode = 'note' | 'slack' | 'claude' | 'subtopic' | 'topic' | 'rename'

export interface Composer {
  mode: ComposerMode
  text: string
}

export type OverlayKind = 'blockers' | 'commands' | 'search' | 'link' | 'help'

export interface Overlay {
  kind: OverlayKind
  query: string
  index: number
  /** A blocker that needs one more value from me before it can be added. */
  pending: BlockerSuggestion | null
}

export interface Toast {
  id: string
  tone: 'info' | 'error'
  text: string
  /** The exact request that failed, kept so it can be sent again. */
  retry: ActionRequest | null
}

/**
 * Everything worth writing down. Anything that can be worked out from the topic
 * itself is a function further down this file, never a field up here.
 */
export interface AppState {
  focus: TopicId | null
  /** Where `[` goes back to. */
  trail: TopicId[]
  expanded: TopicId[]
  /** Which feed row the keyboard is on, as a path of ids. */
  cursor: TopicId[] | null
  composer: Composer | null
  overlay: Overlay | null
  toasts: Toast[]
  sidebar: boolean
  /** Whether the topic's metadata is showing under the title. */
  details: boolean
  /** The feed shows the tail of a long topic until this is turned on. */
  showEarlier: boolean
}

export const initialState: AppState = {
  focus: null,
  trail: [],
  expanded: [],
  cursor: null,
  composer: null,
  overlay: null,
  toasts: [],
  sidebar: true,
  details: false,
  showEarlier: false,
}

/** How many events are drawn before the feed asks whether I really want the rest. */
export const feedWindow = 200

export interface FeedRow {
  /** The path of ids down to this row: the same topic can appear twice. */
  path: TopicId[]
  key: string
  node: TopicNode
  depth: number
  expandable: boolean
  expanded: boolean
}

/** Flattens the tree of children into the rows the feed draws. */
export function feedRows(detail: TopicDetail | null, expanded: TopicId[]): FeedRow[] {
  if (!detail) return []
  const open = new Set(expanded)
  const rows: FeedRow[] = []
  const walk = (nodes: TopicNode[], path: TopicId[], depth: number): void => {
    for (const node of nodes) {
      const here = [...path, node.topic.id]
      const expandable = node.childCount > 0
      const isOpen = expandable && (node.children !== null || open.has(node.topic.id))
      rows.push({
        path: here,
        key: here.join('/'),
        node,
        depth,
        expandable,
        expanded: isOpen,
      })
      if (isOpen && node.children) walk(node.children, here, depth + 1)
    }
  }
  walk(detail.children, [detail.topic.id], 0)
  return rows
}

export function visibleRows(rows: FeedRow[], showEarlier: boolean): { rows: FeedRow[]; hidden: number } {
  if (showEarlier || rows.length <= feedWindow) return { rows, hidden: 0 }
  return { rows: rows.slice(rows.length - feedWindow), hidden: rows.length - feedWindow }
}

export function rowAt(rows: FeedRow[], cursor: TopicId[] | null): FeedRow | null {
  if (!cursor) return null
  const key = cursor.join('/')
  return rows.find((row) => row.key === key) ?? null
}

export function cursorIndex(rows: FeedRow[], cursor: TopicId[] | null): number {
  if (!cursor) return -1
  const key = cursor.join('/')
  return rows.findIndex((row) => row.key === key)
}

export function moveCursor(rows: FeedRow[], cursor: TopicId[] | null, delta: number): TopicId[] | null {
  if (rows.length === 0) return null
  const index = cursorIndex(rows, cursor)
  if (index === -1) return (delta > 0 ? rows[0] : rows[rows.length - 1])?.path ?? null
  const next = Math.min(Math.max(index + delta, 0), rows.length - 1)
  return rows[next]?.path ?? null
}
