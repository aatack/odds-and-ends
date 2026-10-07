import { useCallback, useEffect, useMemo } from 'react'
import { createRoot } from 'react-dom/client'
import { installDispatch } from './dispatch.ts'
import { useSnapshot } from './hooks.ts'
import type { Session } from './session.ts'
import { draftKey, focused } from './state.ts'
import type { PeekTarget, Rect } from './state.ts'
import { actionsNow, labelOf, runTool, tools } from './tools.ts'
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
  const onToggle = useCallback((path: string[]) => session.toggleAt(path), [session])
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
        onToggle,
      },
    [tree, shown, onNearEnd, onToggle, state, working, now, findFocus, onFind, onFindFocused, onSelect, onOpen, onDraft, onCompose, onImage, onAction, onOlder, onHideChat, onEditDraft, onCommitEdit, onCancelEdit],
  )
  const picking = useMemo(() => {
    const pick = state.picking
    return pick && { pick, subject: item(pick.path[pick.path.length - 1]) }
  }, [state.picking, item])

  const crumbs = useMemo(() => state.trail.slice(0, state.at + 1).map((id) => item(id)), [state.trail, state.at, item])
  const onCrumb = useCallback((at: number) => session.goTo(at), [])

  // The phone's buttons are the tool registry's actions, as they stand now:
  // the same things the keys do, so the two can't drift.
  const phoneBar = useMemo(() => {
    if (!phone) return null
    const now = actionsNow(session)
    const can = new Set(now.map((action) => action.id))
    const primary = ['view.back', 'view.push', 'note.create', 'edit.start'].map((id) => ({
      id,
      label: labelOf(tools.find((tool) => tool.id === id)!, session) ?? id,
      enabled: can.has(id),
    }))
    return {
      primary,
      more: now.filter((action) => !primary.some((one) => one.id === action.id)),
      busy: working[focused(state)] ?? [],
      menuOpen: state.phoneMenu,
      onRun: (id: string) => {
        session.setPhoneMenu(false)
        runTool(session, id)
      },
      onMenu: (open: boolean) => session.setPhoneMenu(open),
    }
    // Re-read whenever anything shown changes.
  }, [phone, session, state, shown, tree, working])

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
