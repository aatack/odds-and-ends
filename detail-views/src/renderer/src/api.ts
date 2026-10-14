import type { AppEvent, Changed, Scan } from '../../core/graph/events.ts'
import type { LoadRequest, LoadResult, NoteValues, Outcome } from '../../core/types.ts'
import type { TopicDisplay } from '../../core/modules/topics/view.ts'

/** The seam between the UI and the core. Nothing else reaches the core. */
export interface Api {
  /** Complete events for entities: the only read. */
  scan(ids: string[]): Promise<Scan>
  /** Loads part of an entity from its service into the cache store. */
  load(request: LoadRequest): Promise<LoadResult>
  submit(id: string, text: string): Promise<Outcome>
  perform(id: string, action: string, text: string): Promise<Outcome>
  toggle(id: string): Promise<Outcome>
  markRead(path: string[]): Promise<Outcome>
  /** `c`: puts two items in one context (the first joins the second's, if both have one). */
  context(first: string, second: string): Promise<Outcome>
  /** Pauses Slack's watch, or resumes it. */
  pauseSlack(paused: boolean): Promise<Outcome>
  /** Empties the cache store (never the owned one). */
  clearCache(): Promise<Outcome>
  /** Loads further back than anything loads on its own. */
  older(id: string): Promise<Outcome>
  /** Takes a child out from under its parent, as an owned event. */
  unlink(parent: string, child: string): Promise<Outcome>
  link(parent: string, child: string): Promise<Outcome>
  move(child: string, from: string, to: string): Promise<Outcome>
  /** A note (no type, just text) under `parent`. */
  create(parent: string, text: string, values?: NoteValues): Promise<Outcome>
  setValue(id: string, key: string, value: unknown): Promise<Outcome>
  /** My text for an item, over whatever its service says. */
  setText(id: string, text: string): Promise<Outcome>
  /** Splits an item's text into its first part and notes under it. */
  split(id: string): Promise<Outcome>
  /** A new Claude session under the Claude root, in a temporary directory, `cwd`, or a new worktree of it. */
  claudeCreate(input: { name: string; cwd: string; worktree: boolean }): Promise<Outcome>
  /** A session's directory changed: to `cwd`, or a new worktree of its repo. */
  claudeSetDirectory(session: string, cwd: string, worktree: boolean): Promise<Outcome>
  /** A new topic, named, in the one list of them. */
  /** A new topic, named; with its first path, and in another topic's chat (`parent`), if given. */
  /** A new topic: a task (with a first path, if given) or thoughts; in another's chat, if `parent`. */
  topicCreate(name: string, path?: { cwd: string; worktree: boolean }, parent?: string, display?: TopicDisplay): Promise<Outcome>
  /** A path for a topic: `cwd`, or a new worktree of it; where its next dispatch runs. */
  topicAddPath(topic: string, cwd: string, worktree: boolean): Promise<Outcome>
  /** Sends a topic's messages since its last dispatch to Claude at once. */
  topicDispatch(topic: string): Promise<Outcome>
  /** Text pasted into a topic: a PR, Slack message or branch link as that item, anything else as a message. */
  topicPaste(topic: string, text: string): Promise<Outcome>
  /** A file pasted into a topic, kept, as an item under it (its content base64). */
  topicAttach(topic: string, file: { name: string; mime: string; data: string }): Promise<Outcome>
  /** What would be lost resolving a topic, to confirm first. */
  topicResolveCheck(topic: string): Promise<{ warnings: string[]; error: string | null }>
  /** Resolves a topic: out of the list, its worktree (only one proved to be) removed; refused with work to lose, unless `force`. */
  topicResolve(topic: string, force: boolean): Promise<Outcome>
  /** git, from a topic's buttons: its worktree's commit checked out in its repo; the default branch checked out there; its branch pushed to GitHub. */
  topicCheckOut(topic: string): Promise<Outcome>
  topicCheckOutDefault(topic: string): Promise<Outcome>
  topicPush(topic: string): Promise<Outcome>
  /** Puts a topic down until something comes into each of `on` (Slack conversations or threads), from now. */
  topicWait(topic: string, on: string[]): Promise<Outcome>
  topicUnwait(topic: string, on: string): Promise<Outcome>
  /** Puts a topic down until `until` (Unix ms). */
  topicSnooze(topic: string, until: number): Promise<Outcome>
  /** Passes over a message that came into a topic's wait on `on`. */
  topicDismiss(topic: string, on: string, message: string): Promise<Outcome>
  /**
   * Sends to Slack as me from a topic's box (`on`: a conversation or thread
   * the topic offers). Only the desktop window can, and only straight after
   * a key I pressed: never a script, the phone or Claude.
   */
  topicSlackSend(topic: string, on: string, text: string): Promise<Outcome>
  /** A view of `id` was pushed: Recents moves it to the front. */
  visit(id: string): Promise<void>
  /** Takes my last action off the owned log; returns what came off. */
  undo(): Promise<Outcome>
  /** Writes undone events back, as they were. */
  redo(events: AppEvent[]): Promise<Outcome>
  /** Which entities changed in the stores; null for anything. */
  onChange(listener: (changed: Changed) => void): () => void
  /** Opens a link in my browser. */
  openExternal(url: string): void
  /** Puts text on the clipboard. */
  copy(text: string): Promise<void>
}

interface Bridge {
  invoke(name: string, args?: unknown): Promise<unknown>
  slackSend(args: { topic: string; on: string; text: string }): Promise<Outcome>
  onChange(listener: (changed: Changed) => void): () => void
  openExternal(url: string): Promise<void>
  copy(text: string): Promise<void>
}

/**
 * The core over HTTP (`src/main/phone.ts`): the phone's way in. The same
 * actions as over IPC, a POST each; changes come as Server-Sent Events.
 */
export function httpApi(base: string, token: string): Api {
  const call = async <T,>(name: string, args?: unknown): Promise<T> => {
    const response = await fetch(`${base}/api/${name}`, {
      method: 'POST',
      headers: { authorization: `Bearer ${token}`, 'content-type': 'application/json' },
      body: JSON.stringify(args ?? null),
    })
    const answer = (await response.json()) as T & { error?: string }
    if (!response.ok) throw new Error(answer?.error ?? `${name}: ${response.status}`)
    return answer
  }
  return {
    scan: (ids) => call('scan', { ids }),
    load: (request) => call('load', request),
    submit: (id, text) => call('submit', { id, text }),
    perform: (id, action, text) => call('perform', { id, action, text }),
    toggle: (id) => call('toggle', { id }),
    markRead: (path) => call('markRead', { path }),
    context: (first, second) => call('context', { first, second }),
    pauseSlack: (paused) => call('pauseSlack', { paused }),
    clearCache: () => call('clearCache', {}),
    older: (id) => call('older', { id }),
    unlink: (parent, child) => call('unlink', { parent, child }),
    link: (parent, child) => call('link', { parent, child }),
    move: (child, from, to) => call('move', { child, from, to }),
    create: (parent, text, values) => call('create', { parent, text, values }),
    setValue: (id, key, value) => call('setValue', { id, key, value }),
    setText: (id, text) => call('setText', { id, text }),
    split: (id) => call('split', { id }),
    undo: () => call('undo'),
    redo: (events) => call('redo', { events }),
    claudeCreate: (input) => call('claudeCreate', input),
    claudeSetDirectory: (session, cwd, worktree) => call('claudeSetDirectory', { session, cwd, worktree }),
    topicCreate: (name, path, parent, display) => call('topicCreate', { name, ...path, parent, display }),
    topicAddPath: (topic, cwd, worktree) => call('topicAddPath', { topic, cwd, worktree }),
    topicDispatch: (topic) => call('topicDispatch', { topic }),
    topicPaste: (topic, text) => call('topicPaste', { topic, text }),
    topicAttach: (topic, file) => call('topicAttach', { topic, file }),
    topicResolveCheck: (topic) => call('topicResolveCheck', { topic }),
    topicResolve: (topic, force) => call('topicResolve', { topic, force }),
    topicCheckOut: (topic) => call('topicCheckOut', { topic }),
    topicCheckOutDefault: (topic) => call('topicCheckOutDefault', { topic }),
    topicPush: (topic) => call('topicPush', { topic }),
    topicWait: (topic, on) => call('topicWait', { topic, on }),
    topicUnwait: (topic, on) => call('topicUnwait', { topic, on }),
    topicSnooze: (topic, until) => call('topicSnooze', { topic, until }),
    topicDismiss: (topic, on, message) => call('topicDismiss', { topic, on, message }),
    // Slack is written to only from the desktop window.
    topicSlackSend: async () => ({ events: [], error: 'Slack can only be written to from the desktop app' }),
    visit: (id) => call('visit', { id }),
    onChange: (listener) => {
      // EventSource reconnects by itself; a reconnect may have missed changes, so it re-reads everything.
      const events = new EventSource(`${base}/api/changes?token=${encodeURIComponent(token)}`)
      let opened = false
      events.onopen = () => {
        if (opened) listener(null)
        opened = true
      }
      events.onmessage = (event) => listener(JSON.parse(event.data) as Changed)
      return () => events.close()
    },
    openExternal: (url) => void window.open(url, '_blank', 'noopener'),
    // The browser's own clipboard, which it gives a page served over HTTPS (Tailscale's).
    copy: async (text) => navigator.clipboard.writeText(text),
  }
}

export function electronApi(): Api {
  const bridge = (window as unknown as { core: Bridge }).core
  const call = <T,>(name: string, args?: unknown) => bridge.invoke(name, args) as Promise<T>
  return {
    scan: (ids) => call('scan', { ids }),
    load: (request) => call('load', request),
    submit: (id, text) => call('submit', { id, text }),
    perform: (id, action, text) => call('perform', { id, action, text }),
    toggle: (id) => call('toggle', { id }),
    markRead: (path) => call('markRead', { path }),
    context: (first, second) => call('context', { first, second }),
    pauseSlack: (paused) => call('pauseSlack', { paused }),
    clearCache: () => call('clearCache', {}),
    older: (id) => call('older', { id }),
    unlink: (parent, child) => call('unlink', { parent, child }),
    link: (parent, child) => call('link', { parent, child }),
    move: (child, from, to) => call('move', { child, from, to }),
    create: (parent, text, values) => call('create', { parent, text, values }),
    setValue: (id, key, value) => call('setValue', { id, key, value }),
    setText: (id, text) => call('setText', { id, text }),
    split: (id) => call('split', { id }),
    undo: () => call('undo'),
    redo: (events) => call('redo', { events }),
    claudeCreate: (input) => call('claudeCreate', input),
    claudeSetDirectory: (session, cwd, worktree) => call('claudeSetDirectory', { session, cwd, worktree }),
    topicCreate: (name, path, parent, display) => call('topicCreate', { name, ...path, parent, display }),
    topicAddPath: (topic, cwd, worktree) => call('topicAddPath', { topic, cwd, worktree }),
    topicDispatch: (topic) => call('topicDispatch', { topic }),
    topicPaste: (topic, text) => call('topicPaste', { topic, text }),
    topicAttach: (topic, file) => call('topicAttach', { topic, file }),
    topicResolveCheck: (topic) => call('topicResolveCheck', { topic }),
    topicResolve: (topic, force) => call('topicResolve', { topic, force }),
    topicCheckOut: (topic) => call('topicCheckOut', { topic }),
    topicCheckOutDefault: (topic) => call('topicCheckOutDefault', { topic }),
    topicPush: (topic) => call('topicPush', { topic }),
    topicWait: (topic, on) => call('topicWait', { topic, on }),
    topicUnwait: (topic, on) => call('topicUnwait', { topic, on }),
    topicSnooze: (topic, until) => call('topicSnooze', { topic, until }),
    topicDismiss: (topic, on, message) => call('topicDismiss', { topic, on, message }),
    // Only straight after a real key press or click (a script's events give no activation), and on its own IPC channel.
    topicSlackSend: async (topic, on, text) =>
      navigator.userActivation?.isActive ? bridge.slackSend({ topic, on, text }) : { events: [], error: 'Slack is only written to by a key I press' },
    visit: (id) => call('visit', { id }),
    onChange: (listener) => bridge.onChange(listener),
    openExternal: (url) => void bridge.openExternal(url),
    copy: (text) => bridge.copy(text),
  }
}
