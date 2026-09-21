import { useCallback, useEffect, useMemo, useSyncExternalStore } from 'react'
import { dispatchKey, runTool } from './dispatch'
import { clampIndex, overlayItems, overlayPlaceholders, overlayTitles } from './overlay'
import type { OverlayItem } from './overlay'
import type { Session } from './session'
import type { AppState, ComposerMode, FeedRow } from './state'
import { feedRows, visibleRows } from './state'
import type { Entry } from './store'
import type { QueueView, ServerStatus, TopicDetail, TopicId } from './types'

function useAppState(session: Session): AppState {
  return useSyncExternalStore(
    useCallback((listener) => session.subscribe(listener), [session]),
    useCallback(() => session.getState(), [session]),
  )
}

/** One shared empty entry, so an unfocused snapshot never looks like a new one. */
const nothing: Entry<never> = { data: null, error: null, loading: false, version: 0 }

function useEntry<T>(session: Session, key: string | null): Entry<T> {
  const subscribe = useCallback(
    (listener: () => void) => (key ? session.store.subscribe(key, listener) : () => undefined),
    [session, key],
  )
  const snapshot = useCallback(
    () => (key ? session.store.get<T>(key) : (nothing as unknown as Entry<T>)),
    [session, key],
  )
  return useSyncExternalStore(subscribe, snapshot)
}

export interface OverlayModel {
  kind: AppState['overlay'] extends null ? never : NonNullable<AppState['overlay']>['kind']
  title: string
  placeholder: string
  query: string
  index: number
  items: OverlayItem[]
}

export interface ConswapModel {
  state: AppState
  queue: QueueView | null
  detail: TopicDetail | null
  status: ServerStatus | null
  rows: FeedRow[]
  hidden: number
  cursorKey: string | null
  overlay: OverlayModel | null
  loading: boolean
  error: string | null
  onFocusTopic(id: TopicId): void
  onSelectRow(path: TopicId[]): void
  onToggleRow(id: TopicId): void
  onPromote(id: TopicId): void
  onComposerText(text: string): void
  onComposerMode(mode: ComposerMode): void
  onSubmit(): void
  onCancelComposer(): void
  onOverlayQuery(query: string): void
  onOverlayIndex(index: number): void
  onOverlayChoose(item: OverlayItem): void
  onCloseOverlay(): void
  onDismissToast(id: string): void
  onRetryToast(id: string): void
  onCancelBlocker(id: string): void
  onSetMetadata(key: string, value: string): void
  onShowEarlier(): void
  onRunTool(id: string): void
}

/**
 * The one hook. It reads the session, works out everything the screen needs, and
 * hands back plain data and callbacks: no component below this does any thinking.
 */
export function useConswap(session: Session): ConswapModel {
  const state = useAppState(session)
  const queue = useEntry<QueueView>(session, 'queue')
  const detail = useEntry<TopicDetail>(session, state.focus ? `topic:${state.focus}` : null)
  const status = useEntry<ServerStatus>(session, 'status')

  useEffect(() => session.connect(), [session])

  useEffect(() => {
    const listener = (event: KeyboardEvent): void => {
      if (event.isComposing) return
      const handled = dispatchKey(session, {
        key: event.key,
        ctrlKey: event.ctrlKey,
        metaKey: event.metaKey,
        shiftKey: event.shiftKey,
        altKey: event.altKey,
      })
      if (handled) {
        event.preventDefault()
        event.stopPropagation()
      }
    }
    window.addEventListener('keydown', listener, true)
    return () => window.removeEventListener('keydown', listener, true)
  }, [session])

  // The search overlay needs results before it can show any.
  useEffect(() => {
    if (state.overlay?.kind === 'search' || state.overlay?.kind === 'link') session.loadSearch(state.overlay.query)
  }, [session, state.overlay?.kind, state.overlay?.query])

  const allRows = useMemo(() => feedRows(detail.data, state.expanded), [detail.data, detail.version, state.expanded])
  const windowed = useMemo(() => visibleRows(allRows, state.showEarlier), [allRows, state.showEarlier])

  const overlay = useMemo<OverlayModel | null>(() => {
    if (!state.overlay) return null
    const items = overlayItems(session)
    return {
      kind: state.overlay.kind,
      title: state.overlay.pending?.prompt?.label
        ? `${overlayTitles[state.overlay.kind]} ${state.overlay.pending.label}`
        : overlayTitles[state.overlay.kind],
      placeholder: state.overlay.pending?.prompt?.placeholder ?? overlayPlaceholders[state.overlay.kind],
      query: state.overlay.query,
      index: clampIndex(items, state.overlay.index),
      items,
    }
  }, [session, state.overlay, detail.data, queue.data, state.focus])

  return {
    state,
    queue: queue.data,
    detail: detail.data,
    status: status.data,
    rows: windowed.rows,
    hidden: windowed.hidden,
    cursorKey: state.cursor ? state.cursor.join('/') : null,
    overlay,
    loading: detail.loading || queue.loading,
    error: detail.error ?? queue.error,
    onFocusTopic: useCallback((id) => session.focusTopic(id), [session]),
    onSelectRow: useCallback((path) => session.update((current) => ({ ...current, cursor: path })), [session]),
    onToggleRow: useCallback((id) => session.toggleExpanded(id), [session]),
    onPromote: useCallback((id) => void session.promote(id), [session]),
    onComposerText: useCallback((text) => session.setComposerText(text), [session]),
    onComposerMode: useCallback((mode) => session.setComposerMode(mode), [session]),
    onSubmit: useCallback(() => runTool(session, 'composer.submit'), [session]),
    onCancelComposer: useCallback(() => session.closeComposer(), [session]),
    onOverlayQuery: useCallback(
      (query) => session.updateOverlay((current) => ({ ...current, query, index: 0 })),
      [session],
    ),
    onOverlayIndex: useCallback((index) => session.updateOverlay((current) => ({ ...current, index })), [session]),
    onOverlayChoose: useCallback((item: OverlayItem) => void item.run(session), [session]),
    onCloseOverlay: useCallback(() => session.closeOverlay(), [session]),
    onDismissToast: useCallback((id) => session.dismissToast(id), [session]),
    onRetryToast: useCallback((id) => void session.retryToast(id), [session]),
    onCancelBlocker: useCallback((id) => void session.cancelBlocker(id), [session]),
    onSetMetadata: useCallback((key, value) => void session.setMetadata(key, value), [session]),
    onShowEarlier: useCallback(() => session.update((current) => ({ ...current, showEarlier: true })), [session]),
    onRunTool: useCallback((id) => runTool(session, id), [session]),
  }
}
