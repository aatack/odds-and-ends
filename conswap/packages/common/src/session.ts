import type { Client } from './client'
import type { Environment } from './environment'
import { headlessEnvironment } from './environment'
import type { Entry } from './store'
import { Store } from './store'
import type { AppState, ComposerMode, FeedRow, OverlayKind, Theme, Toast } from './state'
import { feedRows, initialState, moveCursor, rowAt } from './state'
import type {
  ActionRequest,
  ActionResult,
  Blocker,
  Topic,
  QueueView,
  ServerStatus,
  TopicDetail,
  TopicId,
  TopicNode,
} from './types'

let counter = 0
function nextId(prefix: string): string {
  counter += 1
  return `${prefix}-${Date.now().toString(36)}-${counter}`
}

const nothing: Entry<never> = { data: null, error: null, loading: false, version: 0 }

const queueKey = 'queue'
const statusKey = 'status'
const topicKey = (id: TopicId): string => `topic:${id}`

/** Every optimistic write in the app changes one topic's detail, and nothing else. */
interface Optimistic {
  key: string
  change: (detail: TopicDetail) => TopicDetail
}

/**
 * The app, without a screen. It owns the latent state, the cache and every effect,
 * and tells whoever is listening when something moved. React sits on top of this
 * and does nothing else.
 */
export class Session {
  readonly store = new Store()
  private state: AppState = initialState
  private listeners = new Set<() => void>()
  private disconnect: (() => void) | null = null

  constructor(
    readonly client: Client,
    readonly environment: Environment = headlessEnvironment(),
  ) {
    const remembered = environment.read('theme')
    if (remembered === 'light' || remembered === 'dark' || remembered === 'system') {
      this.state = { ...this.state, theme: remembered }
    }
  }

  /** What the screen should actually be, once 'system' has been resolved. */
  effectiveTheme(): 'light' | 'dark' {
    if (this.state.theme !== 'system') return this.state.theme
    return this.environment.prefersDark() ? 'dark' : 'light'
  }

  setTheme(theme: Theme): void {
    this.environment.write('theme', theme)
    this.update((state) => ({ ...state, theme }))
  }

  /** Flips whatever is on screen now, which is what a toggle should do. */
  toggleTheme(): void {
    this.setTheme(this.effectiveTheme() === 'dark' ? 'light' : 'dark')
  }

  getState(): AppState {
    return this.state
  }

  subscribe(listener: () => void): () => void {
    this.listeners.add(listener)
    return () => {
      this.listeners.delete(listener)
    }
  }

  update(change: (state: AppState) => AppState): void {
    const next = change(this.state)
    if (next === this.state) return
    this.state = next
    for (const listener of this.listeners) listener()
  }

  // ---------------------------------------------------------------- reading

  queue(): Entry<QueueView> {
    return this.store.get<QueueView>(queueKey)
  }

  status(): Entry<ServerStatus> {
    return this.store.get<ServerStatus>(statusKey)
  }

  detail(): Entry<TopicDetail> {
    const focus = this.state.focus
    return focus ? this.store.get<TopicDetail>(topicKey(focus)) : nothing
  }

  rows(): FeedRow[] {
    return feedRows(this.detail().data, this.state.expanded)
  }

  /** Starts the live connection and the first loads. Returns the way to stop. */
  connect(): () => void {
    void this.store.load(queueKey, () => this.client.queue(), true)
    void this.store.load(statusKey, () => this.client.status(), true)
    const stop = this.client.transport.subscribe((event) => {
      const keys = [statusKey, ...(event.queue ? [queueKey] : []), ...event.topics.map(topicKey)]
      this.store.invalidate([...new Set(keys)])
    })
    const poll = setInterval(() => this.store.invalidate([statusKey]), 15_000)
    this.disconnect = () => {
      stop()
      clearInterval(poll)
    }
    return this.disconnect
  }

  private loadFocus(force = false): void {
    const focus = this.state.focus
    if (!focus) return
    void this.store.load(topicKey(focus), () => this.client.topic(focus, this.state.expanded), force)
  }

  // ------------------------------------------------------------- navigation

  focusTopic(id: TopicId | null, remember = true): void {
    this.update((state) => ({
      ...state,
      focus: id,
      trail: remember && state.focus && state.focus !== id ? [...state.trail, state.focus].slice(-50) : state.trail,
      cursor: null,
      composer: null,
      overlay: null,
      showEarlier: false,
    }))
    this.loadFocus(true)
  }

  back(): void {
    const previous = this.state.trail[this.state.trail.length - 1]
    if (!previous) return
    this.update((state) => ({ ...state, trail: state.trail.slice(0, -1) }))
    this.focusTopic(previous, false)
  }

  moveCursor(delta: number): void {
    const rows = this.rows()
    this.update((state) => ({ ...state, cursor: moveCursor(rows, state.cursor, delta) }))
  }

  cursorRow(): FeedRow | null {
    return rowAt(this.rows(), this.state.cursor)
  }

  toggleExpanded(id: TopicId): void {
    this.update((state) => ({
      ...state,
      expanded: state.expanded.includes(id) ? state.expanded.filter((entry) => entry !== id) : [...state.expanded, id],
    }))
    this.loadFocus(true)
  }

  // --------------------------------------------------------------- composer

  openComposer(mode: ComposerMode, text = ''): void {
    this.update((state) => ({ ...state, composer: { mode, text }, overlay: null }))
  }

  setComposerText(text: string): void {
    this.update((state) => (state.composer ? { ...state, composer: { ...state.composer, text } } : state))
  }

  setComposerMode(mode: ComposerMode): void {
    this.update((state) => ({ ...state, composer: { mode, text: state.composer?.text ?? '' } }))
  }

  closeComposer(): void {
    this.update((state) => ({ ...state, composer: null }))
  }

  openOverlay(kind: OverlayKind): void {
    this.update((state) => ({ ...state, overlay: { kind, query: '', index: 0, pending: null }, composer: null }))
  }

  updateOverlay(change: (overlay: NonNullable<AppState['overlay']>) => NonNullable<AppState['overlay']>): void {
    this.update((state) => (state.overlay ? { ...state, overlay: change(state.overlay) } : state))
  }

  closeOverlay(): void {
    this.update((state) => ({ ...state, overlay: null }))
  }

  // ----------------------------------------------------------------- toasts

  private toast(tone: Toast['tone'], text: string, retry: ActionRequest | null = null): void {
    const toast: Toast = { id: nextId('toast'), tone, text, retry }
    this.update((state) => ({ ...state, toasts: [...state.toasts, toast].slice(-4) }))
    if (tone === 'info') {
      setTimeout(() => this.dismissToast(toast.id), 2600)
    }
  }

  dismissToast(id: string): void {
    this.update((state) => ({ ...state, toasts: state.toasts.filter((toast) => toast.id !== id) }))
  }

  async retryToast(id: string): Promise<void> {
    const toast = this.state.toasts.find((entry) => entry.id === id)
    if (!toast?.retry) return
    this.dismissToast(id)
    const result = await this.client.act(toast.retry)
    if (result.ok) this.store.invalidate(this.store.keys())
    else this.toast('error', result.error ?? 'that did not work either', toast.retry)
  }

  // ---------------------------------------------------------------- writing

  /**
   * Applies the change to the cache first and puts it back if the server says no.
   * The request is kept on the error so nothing I typed is ever lost.
   */
  private async send(name: string, args: Record<string, unknown>, optimistic: Optimistic[] = []): Promise<ActionResult> {
    const request: ActionRequest = { id: nextId('mutation'), name, args }
    const undo = optimistic.map((entry) => this.store.patch<TopicDetail>(entry.key, entry.change))
    let result: ActionResult
    try {
      result = await this.client.act(request)
    } catch (error) {
      result = { ok: false, error: String(error instanceof Error ? error.message : error) }
    }
    if (!result.ok) {
      for (const revert of undo.reverse()) revert()
      this.toast('error', result.error ?? 'that did not work', request)
      return result
    }
    this.store.invalidate([queueKey, ...(this.state.focus ? [topicKey(this.state.focus)] : [])])
    return result
  }

  /** The child a write puts into the feed before the server has caught up. */
  private pendingNode(type: string, text: string, metadata: Record<string, unknown> = {}): TopicNode {
    const stamp = new Date().toISOString()
    return {
      topic: {
        id: nextId('pending'),
        type,
        text,
        metadata: { ...metadata, pending: true },
        createdAt: stamp,
        updatedAt: stamp,
        open: false,
        resolved: false,
      },
      link: 'watch',
      childCount: 0,
      children: null,
    }
  }

  private appendToFocus(node: TopicNode): Optimistic[] {
    const focus = this.state.focus
    if (!focus) return []
    return [
      {
        key: topicKey(focus),
        change: (detail: TopicDetail) => ({ ...detail, children: [...detail.children, node] }),
      },
    ]
  }

  async addNote(text: string): Promise<void> {
    const focus = this.state.focus
    if (!focus || text.trim().length === 0) return
    this.closeComposer()
    await this.send('topic.note', { topicId: focus, text }, this.appendToFocus(this.pendingNode('note', text)))
  }

  async sendSlack(text: string): Promise<void> {
    const focus = this.state.focus
    if (!focus || text.trim().length === 0) return
    this.closeComposer()
    const result = await this.send(
      'slack.send',
      { topicId: focus, text },
      this.appendToFocus(this.pendingNode('slack_message', text, { userName: 'me', fromMe: true })),
    )
    if (result.ok) this.toast('info', 'sent to Slack')
  }

  async promptClaude(text: string): Promise<void> {
    const focus = this.state.focus
    if (!focus || text.trim().length === 0) return
    this.closeComposer()
    const result = await this.send(
      'claude.prompt',
      { topicId: focus, text },
      this.appendToFocus(this.pendingNode('claude_prompt', text)),
    )
    if (result.ok) this.toast('info', 'Claude is on it')
  }

  async createSubtopic(text: string): Promise<void> {
    const focus = this.state.focus
    if (!focus || text.trim().length === 0) return
    this.closeComposer()
    const result = await this.send('topic.create', { text })
    if (result.ok && result.focus) {
      await this.send('topic.link', { parentId: focus, childId: result.focus, type: 'watch' })
      this.store.invalidate([topicKey(focus), queueKey])
    }
  }

  async createTopic(text: string): Promise<void> {
    if (text.trim().length === 0) return
    this.closeOverlay()
    this.closeComposer()
    const result = await this.send('topic.create', { text })
    if (result.ok && result.focus) this.focusTopic(result.focus)
  }

  async setResolved(resolved: boolean): Promise<void> {
    const focus = this.state.focus
    if (!focus) return
    await this.send('topic.resolve', { topicId: focus, resolved }, [
      {
        key: topicKey(focus),
        change: (detail: TopicDetail) => ({
          ...detail,
          topic: { ...detail.topic, resolved, open: resolved ? false : detail.topic.open },
        }),
      },
    ])
    if (resolved) await this.nextOpen()
  }

  async addBlocker(type: string, config: Record<string, unknown>): Promise<void> {
    const focus = this.state.focus
    if (!focus) return
    this.closeOverlay()
    const pending: Blocker = {
      id: nextId('blocker'),
      topicId: focus,
      type,
      label: 'waiting…',
      config,
      createdAt: new Date().toISOString(),
      dueAt: null,
      satisfiedAt: null,
      cancelledAt: null,
      lastError: null,
    }
    await this.send('blocker.add', { topicId: focus, type, config }, [
      {
        key: topicKey(focus),
        change: (detail: TopicDetail) => ({
          ...detail,
          blocked: true,
          blockers: [...detail.blockers, pending],
          topic: { ...detail.topic, open: false },
        }),
      },
    ])
    await this.nextOpen()
  }

  async cancelBlocker(blockerId: string): Promise<void> {
    const focus = this.state.focus
    if (!focus) return
    await this.send('blocker.cancel', { blockerId }, [
      {
        key: topicKey(focus),
        change: (detail: TopicDetail) => ({
          ...detail,
          blockers: detail.blockers.filter((blocker) => blocker.id !== blockerId),
        }),
      },
    ])
  }

  /** Makes a child a piece of work of its own, and puts this topic down until it lands. */
  async promote(childId: TopicId): Promise<void> {
    const focus = this.state.focus
    if (!focus) return
    const result = await this.send('topic.promote', { parentId: focus, childId })
    if (result.ok) this.focusTopic(childId)
  }

  async reopen(): Promise<void> {
    const focus = this.state.focus
    if (!focus) return
    await this.send('topic.open', { topicId: focus })
  }

  async rename(text: string): Promise<void> {
    const focus = this.state.focus
    if (!focus) return
    await this.send('topic.update', { topicId: focus, text }, [
      {
        key: topicKey(focus),
        change: (detail: TopicDetail) => ({ ...detail, topic: { ...detail.topic, text } }),
      },
    ])
  }

  /** Edits one field of the topic's metadata; an empty value takes it away. */
  async setMetadata(key: string, value: string): Promise<void> {
    const focus = this.state.focus
    if (!focus || key.trim().length === 0) return
    const metadata = { [key]: value.length === 0 ? null : value }
    await this.send('topic.update', { topicId: focus, metadata }, [
      {
        key: topicKey(focus),
        change: (detail: TopicDetail) => ({
          ...detail,
          topic: { ...detail.topic, metadata: { ...detail.topic.metadata, ...metadata } },
        }),
      },
    ])
  }

  async nextOpen(): Promise<void> {
    const result = await this.send('queue.next', { after: this.state.focus ?? undefined })
    if (result.focus) this.focusTopic(result.focus)
    else this.toast('info', 'nothing is waiting for you')
  }

  // ----------------------------------------------------------------- search

  loadSearch(query: string): void {
    void this.store.load(`search:${query}`, () => this.client.search(query))
  }

  searchResults(query: string): Topic[] {
    return this.store.get<{ topics: Topic[] }>(`search:${query}`).data?.topics ?? []
  }

  async linkTopic(childId: TopicId): Promise<void> {
    const focus = this.state.focus
    if (!focus || focus === childId) return
    this.closeOverlay()
    await this.send('topic.link', { parentId: focus, childId, type: 'watch' })
    this.toast('info', 'linked')
  }

  stop(): void {
    this.disconnect?.()
    this.disconnect = null
  }
}
