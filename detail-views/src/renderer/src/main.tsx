import { useCallback, useEffect, useMemo } from 'react'
import { createRoot } from 'react-dom/client'
import { electronApi } from './api.ts'
import { installDispatch } from './dispatch.ts'
import { browserEnvironment } from './environment.ts'
import { useSnapshot } from './hooks.ts'
import { Session } from './session.ts'
import { cursorIndex, draftKey } from './state.ts'
import type { PeekTarget, Rect } from './state.ts'
import { tools } from './tools.ts'
import { App } from './views/App.tsx'
import '@fontsource/lato/400.css'
import '@fontsource/lato/700.css'
import './styles.css'

const session = new Session(electronApi(), browserEnvironment())

function Root() {
  const { state, focus, modules, peekFoci, item } = useSnapshot(session)
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
  const onImage = useCallback((ref: string | null) => session.view(ref), [])
  const onAction = useCallback((action: string) => session.startAction(action), [])
  const gestures = useMemo(
    () => ({
      onPeekEnter: (target: PeekTarget, anchor: Rect) => session.hoverPeek(target, anchor, null),
      onPeekLeave: () => session.leavePeek(null),
    }),
    [],
  )
  const peekHandlers = useMemo(
    () => ({
      gestures,
      hover: (target: PeekTarget, anchor: Rect, origin: string | null) => session.hoverPeek(target, anchor, origin),
      enter: (key: string) => session.enterPeek(key),
      leave: (window: string | null) => session.leavePeek(window),
      onPlace: (key: string, rect: Rect) => session.placePeek(key, rect),
      onRaise: (key: string) => session.raisePeek(key),
      onClose: (key: string) => session.closePeek(key),
      onOpen: (target: PeekTarget) => session.openPeek(target),
    }),
    [gestures],
  )
  const items = useMemo(() => ({ item, onOpen }), [item, onOpen])
  const peek = useMemo(() => ({ ...peekHandlers, peeks: state.peeks, foci: peekFoci }), [peekHandlers, state.peeks, peekFoci])

  const view = useMemo(
    () => ({
      cursor: cursorIndex(state, focus),
      draft: state.drafts[draftKey(state)] ?? '',
      acting: state.acting,
      onAction,
      composing: state.composing,
      onSelect,
      onOpen,
      onDraft,
      onCompose,
      onImage,
    }),
    [state, focus, onSelect, onOpen, onDraft, onCompose, onImage, onAction],
  )

  return <App items={items} peek={peek} viewing={state.viewing} onImage={onImage} modules={modules} module={focus?.module ?? null} focus={focus} view={view} onModule={onModule} />
}

createRoot(document.getElementById('root')!).render(
  <Root />,
)
