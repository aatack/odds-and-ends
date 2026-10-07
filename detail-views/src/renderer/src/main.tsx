import { useCallback, useEffect, useMemo } from 'react'
import { createRoot } from 'react-dom/client'
import { electronApi } from './api.ts'
import { installDispatch } from './dispatch.ts'
import { browserEnvironment } from './environment.ts'
import { useSnapshot } from './hooks.ts'
import { Session } from './session.ts'
import { draftKey, focused } from './state.ts'
import type { PeekTarget, Rect } from './state.ts'
import { tools } from './tools.ts'
import { App } from './views/App.tsx'
import '@fontsource/lato/400.css'
import '@fontsource/lato/700.css'
import './styles.css'

const session = new Session(electronApi(), browserEnvironment())

function Root() {
  const { state, view: tree, shown, modules, peekViews, item, working, now } = useSnapshot(session)
  useEffect(() => {
    const stopDispatch = installDispatch(session, tools)
    let stopSession: (() => void) | undefined
    void session.start().then((stop) => (stopSession = stop))
    return () => {
      stopDispatch()
      stopSession?.()
    }
  }, [])

  const onSelect = useCallback((path: string[]) => session.select(path), [])
  const onEditDraft = useCallback((text: string) => session.setEditDraft(text), [])
  const onCommitEdit = useCallback(() => session.commitEdit(), [])
  const onCancelEdit = useCallback(() => session.cancelEdit(), [])
  const onOpen = useCallback((id: string) => session.navigate(id), [])
  const onDraft = useCallback((text: string) => session.setDraft(text), [])
  const onCompose = useCallback((composing: boolean) => session.compose(composing), [])
  const onModule = useCallback((root: string) => session.navigate(root), [])
  const onImage = useCallback((ref: string | null) => session.view(ref), [])
  const onAction = useCallback((action: string) => session.startAction(action), [])
  const onOlder = useCallback(() => session.older(), [])
  const onHideChat = useCallback(() => session.hideChatOfMessage(), [])
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
  const peek = useMemo(() => ({ ...peekHandlers, peeks: state.peeks, views: peekViews }), [peekHandlers, state.peeks, peekViews])

  const view = useMemo(
    () =>
      tree && {
        view: tree,
        shown,
        interactive: true,
        edit: state.edit,
        picking: state.picking,
        draft: state.drafts[draftKey(state)] ?? '',
        acting: state.acting,
        composing: state.composing,
        working: working[focused(state)],
        now,
        onAction,
        onSelect,
        onOpen,
        onDraft,
        onCompose,
        onImage,
        onOlder,
        onHideChat,
        onEditDraft,
        onCommitEdit,
        onCancelEdit,
      },
    [tree, shown, state, working, now, onSelect, onOpen, onDraft, onCompose, onImage, onAction, onOlder, onHideChat, onEditDraft, onCommitEdit, onCancelEdit],
  )
  const picking = useMemo(() => {
    const pick = state.picking
    return pick && { pick, subject: item(pick.path[pick.path.length - 1]) }
  }, [state.picking, item])

  return (
    <App
      items={items}
      peek={peek}
      viewing={state.viewing}
      onImage={onImage}
      modules={modules}
      module={tree?.module ?? null}
      view={view}
      picking={picking}
      onModule={onModule}
    />
  )
}

createRoot(document.getElementById('root')!).render(
  <Root />,
)
