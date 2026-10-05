import { useCallback, useEffect, useMemo } from 'react'
import { createRoot } from 'react-dom/client'
import { electronApi } from './api.ts'
import { installDispatch } from './dispatch.ts'
import { browserEnvironment } from './environment.ts'
import { useSnapshot } from './hooks.ts'
import { Session } from './session.ts'
import { cursorIndex, focused } from './state.ts'
import { tools } from './tools.ts'
import { App } from './views/App.tsx'
import '@fontsource/lato/400.css'
import '@fontsource/lato/700.css'
import './styles.css'

const session = new Session(electronApi(), browserEnvironment())

function Root() {
  const { state, focus, modules } = useSnapshot(session)
  useEffect(() => {
    const stopDispatch = installDispatch(session, tools)
    let stopSession: (() => void) | undefined
    void session.start().then((stop) => (stopSession = stop))
    return () => {
      stopDispatch()
      stopSession?.()
    }
  }, [])

  const onSelect = useCallback((id: string) => session.select(id), [])
  const onOpen = useCallback((id: string) => session.navigate(id), [])
  const onDraft = useCallback((text: string) => session.setDraft(text), [])
  const onCompose = useCallback((composing: boolean) => session.compose(composing), [])
  const onModule = useCallback((root: string) => session.navigate(root), [])

  const view = useMemo(
    () => ({
      cursor: cursorIndex(state, focus),
      draft: state.drafts[focused(state)] ?? '',
      composing: state.composing,
      onSelect,
      onOpen,
      onDraft,
      onCompose,
    }),
    [state, focus, onSelect, onOpen, onDraft, onCompose],
  )

  return <App modules={modules} module={focus?.module ?? null} focus={focus} view={view} onModule={onModule} />
}

createRoot(document.getElementById('root')!).render(
  <Root />,
)
