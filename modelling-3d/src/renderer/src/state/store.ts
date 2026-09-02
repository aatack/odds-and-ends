/**
 * All of the app's state, and nothing else.
 *
 * No React, no DOM: the store and the actions over it are plain data and pure
 * functions, so the app can in principle be driven headlessly — which is how
 * `npm test` exercises the editing rules without opening a window.
 *
 * Latent only. The evaluation, the preview scene, the navigator's list and what
 * a node's sockets are worth are all *derived* (`derive.ts`) and never written
 * back here.
 */

import type { Models } from '@core/graph'

export interface AppState {
  models: Models
  /** False until the store has been read once; the UI shows nothing before then. */
  loaded: boolean
  /** The model the builder is showing. */
  openModelId: string | null
  /** Node ids selected in that model. Empty means "everything terminal". */
  selection: string[]
  /** The last thing worth saying to the user, shown once and dismissible. */
  notice: Notice | null
}

/** A line of feedback, optionally about a file that was just written. */
export interface Notice {
  text: string
  path?: string
}

export const initialState: AppState = {
  models: {},
  loaded: false,
  openModelId: null,
  selection: [],
  notice: null,
}

export interface Store {
  getState(): AppState
  subscribe(listener: () => void): () => void
  update(change: (state: AppState) => AppState): void
}

export function createStore(state: AppState = initialState): Store {
  let current = state
  const listeners = new Set<() => void>()
  return {
    getState: () => current,
    subscribe(listener) {
      listeners.add(listener)
      return () => void listeners.delete(listener)
    },
    update(change) {
      const next = change(current)
      if (next === current) return
      current = next
      for (const listener of listeners) listener()
    },
  }
}
