import type { Session } from './session'
import type { Overlay } from './state'
import { toolLabel, tools } from './tools'
import type { BlockerSuggestion } from './types'

export interface OverlayItem {
  id: string
  title: string
  hint?: string
  key?: string
  run(session: Session): void | Promise<void>
}

function matches(text: string, query: string): boolean {
  if (query.length === 0) return true
  const needle = query.toLowerCase()
  const haystack = text.toLowerCase()
  let index = 0
  for (const character of needle) {
    index = haystack.indexOf(character, index)
    if (index === -1) return false
    index += 1
  }
  return true
}

function minutes(count: number): string {
  return new Date(Date.now() + count * 60_000).toISOString()
}

/** The blockers that are always on offer, however little we know about the topic. */
function fallbackSuggestions(): BlockerSuggestion[] {
  return [
    { type: 'snooze', label: 'ten minutes pass', config: { until: minutes(10) } },
    { type: 'snooze', label: 'an hour passes', config: { until: minutes(60) } },
    { type: 'activity', label: 'anything new happens here', config: {} },
  ]
}

export const overlayTitles: Record<Overlay['kind'], string> = {
  blockers: 'Put this down until…',
  commands: 'What would you like to do?',
  search: 'Find a topic',
  link: 'Associate this topic with…',
  help: 'Keys',
}

export const overlayPlaceholders: Record<Overlay['kind'], string> = {
  blockers: 'a reply, CI, an hour…',
  commands: 'resolve, snooze, ask Claude…',
  search: 'title of a topic',
  link: 'title of a topic',
  help: '',
}

/**
 * What the overlay is offering, worked out from the same state the view draws. The
 * keyboard and the mouse both go through this list, so they can never disagree.
 */
export function overlayItems(session: Session): OverlayItem[] {
  const overlay = session.getState().overlay
  if (!overlay) return []
  const query = overlay.query

  if (overlay.kind === 'commands') {
    return tools
      .filter((tool) => tool.scope !== 'overlay' && tool.enabled(session) && matches(tool.title, query))
      .map((tool) => ({
        id: tool.id,
        title: tool.title,
        hint: tool.hint,
        key: toolLabel(tool),
        run: (target: Session) => tool.run(target),
      }))
  }

  if (overlay.kind === 'blockers') {
    const pending = overlay.pending
    if (pending) {
      const field = pending.prompt?.field ?? 'value'
      return [
        {
          id: 'pending',
          title: query.length > 0 ? query : (pending.prompt?.placeholder ?? ''),
          hint: `press enter to wait until ${pending.label}`,
          run: (target: Session) => target.addBlocker(pending.type, { ...pending.config, [field]: query }),
        },
      ]
    }
    const detail = session.detail().data
    const suggestions = detail && detail.suggestions.length > 0 ? detail.suggestions : fallbackSuggestions()
    return suggestions
      .filter((suggestion) => matches(suggestion.label, query))
      .map((suggestion, index) => ({
        id: `${suggestion.type}-${index}`,
        title: suggestion.label,
        hint: suggestion.prompt ? `needs a ${suggestion.prompt.label}` : suggestion.type.replace(/_/g, ' '),
        run: (target: Session) => {
          if (suggestion.prompt) {
            target.updateOverlay((current) => ({ ...current, pending: suggestion, query: '', index: 0 }))
            return
          }
          return target.addBlocker(suggestion.type, suggestion.config)
        },
      }))
  }

  if (overlay.kind === 'search' || overlay.kind === 'link') {
    const results = session.searchResults(query).map((topic) => ({
      id: topic.id,
      title: topic.text || '(untitled)',
      hint: topic.type.replace(/_/g, ' '),
      run: (target: Session) =>
        overlay.kind === 'link' ? target.linkTopic(topic.id) : target.focusTopic(topic.id),
    }))
    if (overlay.kind === 'search' && query.trim().length > 0) {
      results.push({
        id: 'create',
        title: `Start a new topic: ${query}`,
        hint: 'new',
        run: (target: Session) => target.createTopic(query),
      })
    }
    return results
  }

  return []
}

export function clampIndex(items: OverlayItem[], index: number): number {
  if (items.length === 0) return 0
  return Math.min(Math.max(index, 0), items.length - 1)
}
