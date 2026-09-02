/**
 * The only place React meets the state layer, and the reason it sits outside
 * `state/` — everything in there is checked against the ES libs alone, so a
 * reach for React or the DOM from the state layer fails to compile.
 *
 * The store is read whole: a model is a few dozen nodes, so a selector layer
 * would cost more in indirection than it saves in renders. Everything derived
 * is memoised on the identity of what it derives from, which the actions
 * preserve by replacing only the model they touched.
 */

import { createContext, createElement, useContext, useMemo, useSyncExternalStore, type ReactNode } from 'react'
import type { ModellingAPI, Persistence, WriteOp } from '@core/api'
import { noPersistence } from '@core/api'
import type { Evaluation } from '@core/evaluate'
import type { Actions } from './state/actions'
import { createActions } from './state/actions'
import { evaluationOf, openModel, previewScene } from './state/derive'
import type { AppState, Store } from './state/store'
import { createStore } from './state/store'

declare global {
  interface Window {
    modelling?: ModellingAPI
  }
}

/** The desktop's capabilities, or nothing when the page is opened on its own. */
const bridge = (): ModellingAPI | null => (typeof window === 'undefined' ? null : (window.modelling ?? null))

/** Writes are queued to a microtask, so a burst of edits is one transaction. */
function ipcPersistence(api: ModellingAPI): Persistence {
  let pending: WriteOp[] = []
  return {
    write(ops) {
      if (ops.length === 0) return
      const first = pending.length === 0
      pending.push(...ops)
      if (!first) return
      queueMicrotask(() => {
        const batch = pending
        pending = []
        void api.write(batch)
      })
    },
  }
}

interface AppContext {
  store: Store
  actions: Actions
  api: ModellingAPI | null
}

const Context = createContext<AppContext | null>(null)

export function AppProvider({ children }: { children: ReactNode }): ReactNode {
  const value = useMemo<AppContext>(() => {
    const api = bridge()
    const store = createStore()
    const actions = createActions(store, api ? ipcPersistence(api) : noPersistence)
    if (api) void api.load().then((models) => actions.load(models))
    else actions.load({})
    return { store, actions, api }
  }, [])
  return createElement(Context.Provider, { value }, children)
}

function context(): AppContext {
  const value = useContext(Context)
  if (!value) throw new Error('the app is used outside its provider')
  return value
}

export function useAppState(): AppState {
  const { store } = context()
  return useSyncExternalStore(store.subscribe, store.getState, store.getState)
}

export function useActions(): Actions {
  return context().actions
}

export function useApi(): ModellingAPI | null {
  return context().api
}

/** The open model's evaluation, recomputed only when the graph changes. */
export function useEvaluation(state: AppState): Evaluation {
  const model = openModel(state)
  return useMemo(() => evaluationOf(model, state.models), [model, state.models])
}

export function usePreviewScene(state: AppState, evaluation: Evaluation) {
  return useMemo(
    () => previewScene(state, evaluation),
    [state.models, state.openModelId, state.selection, evaluation],
  )
}
