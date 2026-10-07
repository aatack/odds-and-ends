import { useCallback, useEffect, useMemo } from 'react'
import { createRoot } from 'react-dom/client'
import { installDispatch } from './dispatch.ts'
import { useSnapshot } from './hooks.ts'
import type { Session } from './session.ts'
import { draftKey, focused } from './state.ts'
import type { PeekTarget, Rect } from './state.ts'
import { tools } from './tools.ts'
import { App } from './views/App.tsx'
import '@fontsource/lato/400.css'
import '@fontsource/lato/700.css'
import './styles.css'

/**
 * The app on screen, for either entry: the window (`main.tsx`) or the phone
 * (`phone.tsx`). Everything below the session is the same code; `phone` only
 * swaps hover for taps and keys for a bar of buttons.
 */
function Root({ session, phone }: { session: Session; phone: boolean }) {
  const { state, view: tree, shown, modules, peekViews, item, working, now, findFocus } = useSnapshot(session)
  useEffect(() => {
    const stopDispatch = installDispatch(session, tools)
    let stopSession: (() => void) | undefined
    void session.start().then((stop) => (stopSession = stop))
    return () => {
      stopDispatch()
      stopSession?.()
    }
  }, [])

  // On a phone a second tap on the selected row pushes its view, since there is no `d`.
  const onSelect = useCallback(
    (path: string[]) => {
      const selected = session.get().shown.selectedPath
      if (phone && selected.join('\0') === path.join('\0')) session.open()
      else session.select(path)
    },
    [session, phone],
  )
  const onEditDraft = useCallback((text: string) => session.setEditDraft(text), [])
  const onCommitEdit = useCallback(() => session.commitEdit(), [])
  const onCancelEdit = useCallback(() => session.cancelEdit(), [])
  const onFind = useCallback((text: string) => session.setFind(text), [])
  const onFindFocused = useCallback(() => session.findFocused(), [])
  const onNearEnd = useCallback(() => session.loadMore(), [])
  const onOpen = useCallback((id: string) => session.navigate(id), [])
  const onDraft = useCallback((text: string) => session.setDraft(text), [])
  const onCompose = useCallback((composing: boolean) => session.compose(composing), [])
  const onModule = useCallback((root: string) => session.enterModule(root), [])
  const onImage = useCallback((ref: string | null) => session.view(ref), [])
  const onAction = useCallback((action: string) => session.startAction(action), [])
  const onOlder = useCallback(() => session.older(), [])
  const onHideChat = useCallback(() => session.hideChatOfMessage(), [])
  // No hover on a phone, so no peeks: a link opens in the browser instead.
  const gestures = useMemo(
    () =>
      phone
        ? { onPeekEnter: () => {}, onPeekLeave: () => {}, onLinkTap: (url: string) => session.openExternal(url) }
        : {
            onPeekEnter: (target: PeekTarget, anchor: Rect) => session.hoverPeek(target, anchor, null),
            onPeekLeave: () => session.leavePeek(null),
          },
    [session, phone],
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
      loadMore: (root: string) => session.loadMore(root),
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
        find: state.finds[focused(state)] ?? null,
        findFocus,
        onFind,
        onFindFocused,
        onNearEnd,
      },
    [tree, shown, onNearEnd, state, working, now, findFocus, onFind, onFindFocused, onSelect, onOpen, onDraft, onCompose, onImage, onAction, onOlder, onHideChat, onEditDraft, onCommitEdit, onCancelEdit],
  )
  const picking = useMemo(() => {
    const pick = state.picking
    return pick && { pick, subject: item(pick.path[pick.path.length - 1]) }
  }, [state.picking, item])

  const crumbs = useMemo(() => state.trail.slice(0, state.at + 1).map((id) => item(id)), [state.trail, state.at, item])
  const onCrumb = useCallback((at: number) => session.goTo(at), [])

  const selectedRow = shown.rows[shown.selectedIndex]
  const phoneBar = useMemo(
    () =>
      phone
        ? {
            canBack: state.at > 0,
            canOpen: selectedRow?.kind === 'entity' && selectedRow.row.depth > 0,
            canOlder: Boolean(tree?.older),
            olderBusy: working[focused(state)]?.includes('older') ?? false,
            onBack: () => session.back(),
            onOpen: () => session.open(),
            onNote: () => session.startCreate(),
            onEdit: () => session.startEdit(),
            onOlder: () => session.older(),
          }
        : null,
    [phone, session, state, selectedRow, tree, working],
  )

  return (
    <App
      phone={phoneBar}
      crumbs={crumbs}
      onCrumb={onCrumb}
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

export function mount(session: Session, options: { phone: boolean }): void {
  createRoot(document.getElementById('root')!).render(<Root session={session} phone={options.phone} />)
}
