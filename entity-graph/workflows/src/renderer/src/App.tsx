import { createContext, useCallback, useContext, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import type { Catalog, ItemDetail, ItemEvent, Overview } from '../../shared/types'
import { useAtom, useAtomValue, useSetAtom } from 'jotai'
import { ChevronLeft, Menu, MoreHorizontal } from 'lucide-react'
import { eventActions, type EventAction } from './eventActions'
import { Dropdown, DropdownItem } from './components/ui/Dropdown'
import { TypesPage } from './TypesPage'
import { IconButton } from './components/ui/IconButton'
import { appearanceAtom, applyAppearance, MODES, SERIFS, THEMES } from './appearance'
import { Chat } from './Chat'
import { DebugView } from './DebugView'
import { EventMenu, useEventMenu } from './EventMenu'
import { EventView } from './EventView'
import { ItemPicker } from './ItemPicker'
import { ItemsList } from './ItemsList'
import { TASKS_SECTIONS } from './sections'
import { Composer } from './Composer'
import { ItemRefContext } from './components/ui/Markdown'
import { ItemRef } from './ItemRef'
import { EventIcon } from './icons'
import { IntegrationsPage } from './IntegrationsPage'
import { McpPage } from './McpPage'
import { api, useWorkflows } from './useWorkflows'
import { findAtom, findFocusAtom, forwardAtom, menuAtom, renameAtom, searchFocusAtom, viewedAtom, viewsAtom, type View } from './views'
import { FindLine } from './FindLine'
import { AssigneePill } from './Assignees'
import { BotsPage } from './BotsPage'
import { BotView } from './BotView'
import { Bot, User as UserIcon } from 'lucide-react'
import { everyone, nameOf, sourceAtom, userAtom, type Someone } from './people'
import { Start } from './Start'

/**
 * First who you are and which source to open (`Start`, which opens them at once when both are kept); then the app,
 * as that person, on that source.
 */
export function Root() {
  const [user, setUser] = useAtom(userAtom)
  const [source, setSource] = useAtom(sourceAtom)
  const [open, setOpen] = useState<string | null>(null)
  // The views on the stack are a source's: another source starts afresh.
  const setViews = useSetAtom(viewsAtom)
  const setForwards = useSetAtom(forwardAtom)
  const setViewed = useSetAtom(viewedAtom)
  if (!open || !user || !source)
    return (
      <Start
        kept={{ user: user ?? undefined, source: source ?? undefined }}
        onBegin={(s) => {
          if (s.source !== source) {
            setViews([])
            setForwards([])
            setViewed([])
          }
          setUser(s.user)
          setSource(s.source)
          setOpen(`${s.source}\n${s.user}`)
        }}
      />
    )
  return <App key={open} onSignOut={() => {
    api().setUser(null)
    setUser(null)
    setOpen(null)
  }} onCloseSource={() => {
    void api().closeSource()
    setSource(null)
    setOpen(null)
  }} />
}

export function App({ onSignOut, onCloseSource }: { onSignOut: () => void; onCloseSource: () => void }) {
  const items = useWorkflows()
  // Ctrl+W and Alt+Left (sent by the main process) take the top view off the stack; Alt+Right puts it back.
  const { back, forward, open } = items
  useEffect(() => api().onBack(back), [back])
  useEffect(() => api().onForward(forward), [forward])
  // Ctrl+L puts the items view on top of the stack; if it is already showing, it puts the cursor in its search line.
  // Ctrl+D is Next. Ctrl+F finds in the view: the items view's own search line, or a find line over a chat.
  const focusSearch = useSetAtom(searchFocusAtom)
  const setFind = useSetAtom(findAtom)
  const focusFind = useSetAtom(findFocusAtom)
  const { act } = items
  const canFind = !!items.detail?.item && items.view?.kind === 'event'
  // A find belongs to the view it was made in.
  const viewKey = JSON.stringify(items.view)
  useEffect(() => setFind(null), [viewKey, setFind])
  const showingItems = !items.view || items.view.kind === 'items'
  const { next } = items
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (!e.ctrlKey || e.altKey || e.shiftKey || e.metaKey) return
      const key = e.key.toLowerCase()
      if (key === 'l') {
        e.preventDefault()
        if (showingItems) focusSearch((n) => n + 1)
        else open({ kind: 'items' })
      } else if (key === 'd') {
        e.preventDefault()
        void next()
      } else if (key === 'f') {
        e.preventDefault()
        if (showingItems) focusSearch((n) => n + 1)
        else if (canFind) {
          setFind((f) => f ?? '')
          focusFind((n) => n + 1)
        }
      }
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [open, next, showingItems, focusSearch, canFind, setFind, focusFind, act, items.selected])
  // `[itemId]()` in any text is drawn as the item it refers to; `[botId]()` and `[username]()` as the bot or person.
  const byId = useMemo(() => new Map(items.overview.items.map((i) => [i.id, i])), [items.overview.items])
  const { bots, users } = items.overview
  const menu = useEventMenu()
  const itemRef = useCallback(
    (id: string) => {
      const bot = bots.find((b) => b.id === id)
      const person = users.find((u) => u.id === id)
      if (bot || person)
        return (
          <button data-ui className="link item-ref bot-ref" onClick={() => bot && open({ kind: 'event', id })} {...menu(id)}>
            {bot ? <Bot className="icon" /> : <UserIcon className="icon" />}@{bot?.name ?? person!.name}
          </button>
        )
      return <ItemRef id={id} item={byId.get(id)} onOpen={(to) => open({ kind: 'event', id: to })} />
    },
    [byId, bots, users, open, menu],
  )
  return (
    <ItemsContext.Provider value={items}>
      <ItemRefContext.Provider value={itemRef}>
        <div className="app">
          <main className="main">
            <div className="content">
              <ItemsMain />
            </div>
          </main>
          <AppMenu onSignOut={onSignOut} onCloseSource={onCloseSource} />
        </div>
        <EventMenu actOn={items.actOn} skip={items.skip} custom={items.customActions} />
      </ItemRefContext.Provider>
    </ItemsContext.Provider>
  )
}

/** The app's state, shared by the content pane and the corner buttons. */
const ItemsContext = createContext<ReturnType<typeof useWorkflows> | null>(null)
const useItems = () => useContext(ItemsContext)!

/** The button in the bottom left: who is signed in, on which source (to leave either), the bots, and the way to the other pages. */
function AppMenu({ onSignOut, onCloseSource }: { onSignOut: () => void; onCloseSource: () => void }) {
  const w = useItems()
  const [open, setOpen] = useState(false)
  const busy = w.overview.bots.filter((b) => b.runId).length
  const [look, setLook] = useAtom(appearanceAtom)
  const source = useAtomValue(sourceAtom)
  useEffect(() => applyAppearance(look), [look])
  useEffect(() => {
    if (!open) return
    const close = () => setOpen(false)
    window.addEventListener('mousedown', close)
    return () => window.removeEventListener('mousedown', close)
  }, [open])
  const go = (view: View) => {
    w.open(view)
    setOpen(false)
  }
  return (
    <div className="app-menu" onMouseDown={(e) => e.stopPropagation()}>
      {open && (
        <div className="menu app-menu-list">
          <button onClick={() => go({ kind: 'items' })}>Tasks</button>
          <button onClick={() => go({ kind: 'bots' })}>
            Bots <span className="menu-status">{busy} of {w.overview.bots.length} working</span>
          </button>
          <button onClick={() => go({ kind: 'integrations' })}>Integrations</button>
          <button onClick={() => go({ kind: 'types' })}>Types</button>
          <button onClick={() => go({ kind: 'mcp' })}>MCP</button>
          <button onClick={onSignOut}>
            Sign out <span className="menu-status">{w.overview.me.name}</span>
          </button>
          <button onClick={onCloseSource} title={source ?? undefined}>
            Close source <span className="menu-status">{source}</span>
          </button>
          {/* For now, while a style is chosen. */}
          <div className="menu-note">Style</div>
          <div className="menu-choices">
            {THEMES.map((t) => (
              <button key={t.id} className={look.theme === t.id ? 'chosen' : ''} onClick={() => setLook({ ...look, theme: t.id })}>
                {t.name}
              </button>
            ))}
          </div>
          <div className="menu-note">Mode</div>
          <div className="menu-choices">
            {MODES.map((m) => (
              <button key={m.id} className={look.mode === m.id ? 'chosen' : ''} onClick={() => setLook({ ...look, mode: m.id })}>
                {m.name}
              </button>
            ))}
          </div>
          <div className="menu-note">Serif</div>
          <div className="menu-choices">
            {SERIFS.map((f) => (
              <button key={f.id} className={look.serif === f.id ? 'chosen' : ''} onClick={() => setLook({ ...look, serif: f.id })}>
                {f.name}
              </button>
            ))}
          </div>
        </div>
      )}
      <button data-ui className="corner-button" title="Menu" aria-label="Menu" onClick={() => setOpen(!open)}>
        <Menu className="icon" />
        {busy > 0 && <span className="busy">{busy}</span>}
      </button>
    </div>
  )
}

/**
 * The content pane: the top view on the stack. Its first line is always the
 * same: Back, the event as text, and what can be done to it, on the right.
 * Below is the view itself, which depends on the event's type.
 */
function ItemsMain() {
  const w = useItems()
  const openEvent = (id: string) => w.open({ kind: 'event', id })
  // What the items view opens takes its place, so Back goes to where you were before it.
  const openFromItems = (id: string) => w.open({ kind: 'event', id }, { replace: w.view?.kind === 'items' })
  const d = w.detail
  if (w.view?.kind === 'types')
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title="Types" />
        {w.error && <div className="error">{w.error}</div>}
        <div className="view-body">{w.catalog && <TypesPage catalog={w.catalog} custom={w.customActions} act={w.act} />}</div>
      </div>
    )
  if (w.view?.kind === 'bots')
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title="Bots" />
        {w.error && <div className="error">{w.error}</div>}
        <div className="view-body">
          <BotsPage bots={w.overview.bots} act={w.act} onOpen={openEvent} />
        </div>
      </div>
    )
  if (w.view?.kind === 'integrations' || w.view?.kind === 'mcp')
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title={w.view.kind === 'mcp' ? 'MCP' : 'Integrations'} />
        <div className="view-body">{w.view.kind === 'mcp' ? <McpPage /> : <IntegrationsPage />}</div>
      </div>
    )
  // With nothing on the stack, every item.
  if (!w.view || w.view.kind === 'items')
    return (
      <div className="view">
        <ViewHeader canGoBack={w.views.length > 0} onBack={w.back} title="Tasks" />
        {w.error && <div className="error">{w.error}</div>}
        {w.empty && <p className="muted view-note">Nothing needs you.</p>}
        <div className="view-body">
          <ItemsList
            items={w.overview.items}
            recent={w.recent}
            sections={TASKS_SECTIONS}
            onEscape={w.views.length > 0 ? w.back : undefined}
            onOpen={openFromItems}
            onCreate={(title) => w.act(async () => openFromItems(await api().create(title, null)))}
          />
        </div>
      </div>
    )
  if (!d || !w.catalog)
    return (
      <div className="view">
        <ViewHeader canGoBack={w.views.length > 0} onBack={w.back} />
        <div className="empty">That event is not there any more.</div>
      </div>
    )
  const e = d.event
  const debug = w.view.kind === 'debug'
  const actions = debug ? null : (
    <>
      {e.type === 'task' && d.item && !d.item.item.closed && <AssigneePill item={d.item.item} overview={w.overview} act={w.actOn(e.id)} />}
      <HeaderActions actions={eventActions(e, { act: w.actOn(e.id), skip: w.skip, custom: w.customActions })} />
    </>
  )
  return (
    <div className="view">
      <ViewHeader
        canGoBack={w.views.length > 0}
        onBack={w.back}
        title={`${debug ? 'Debug: ' : ''}${e.summary}`}
        event={e}
        actions={actions}
        onRename={e.type === 'task' && !debug ? (title) => void w.act(() => api().rename(e.id, title)) : undefined}
      />
      {w.error && <div className="error">{w.error}</div>}
      <div className={`view-body ${d.item && !debug ? 'fill' : ''}`}>
        {debug ? (
          <DebugView id={e.id} />
        ) : e.type === 'bot' && d.item ? (
          <div className="bot-page">
            <BotView key={e.id} bot={w.overview.bots.find((b) => b.id === e.id) ?? null} act={w.act} />
            <ItemView key={`${e.id}:chat`} detail={d.item} catalog={w.catalog} act={w.act} onOpen={openEvent} />
          </div>
        ) : d.item ? (
          <ItemView key={e.id} detail={d.item} catalog={w.catalog} act={w.actOn(e.id)} onOpen={openEvent} />
        ) : (
          <EventView detail={d} catalog={w.catalog} act={w.act} onOpen={openEvent} />
        )}
      </div>
    </div>
  )
}

/** An event's actions in its header: the important ones as icon buttons, the rest in a menu beside them. */
function HeaderActions({ actions }: { actions: EventAction[] }) {
  const rest = actions.filter((a) => !a.important)
  return (
    <>
      {actions
        .filter((a) => a.important)
        .map((a) => (
          <IconButton key={a.id} title={a.title ?? a.label} className={a.tone} onClick={a.run}>
            <a.icon className="size-[17px]" />
          </IconButton>
        ))}
      {rest.length > 0 && (
        <Dropdown
          align="right"
          menuClassName="popover"
          trigger={({ toggle }) => (
            <IconButton title="More" onClick={toggle}>
              <MoreHorizontal className="size-[17px]" />
            </IconButton>
          )}
        >
          {(close) =>
            rest.map((a) => (
              <DropdownItem
                key={a.id}
                onClick={() => {
                  close()
                  a.run()
                }}
              >
                <span className="flex items-center gap-2" title={a.title}>
                  <a.icon className={`size-[14px] ${a.tone || 'text-gray-500'}`} />
                  {a.label}
                </span>
              </DropdownItem>
            ))
          }
        </Dropdown>
      )}
    </>
  )
}

/** Back (to the items view, at the bottom of every stack; its place kept on the items view itself), the event as text, and its actions on the right. */
function ViewHeader(props: {
  canGoBack: boolean
  onBack: () => void
  title?: string
  event?: ItemEvent
  actions?: ReactNode
  /** Given, the title can be edited: Rename in its menu, then Enter (or leaving it) saves and Escape doesn't. */
  onRename?: (title: string) => void
}) {
  const [editing, setEditing] = useState<string | null>(null)
  // Clicking the title opens the event's menu (Open, Debug, Rename, its actions), as a right-click does.
  const setMenu = useSetAtom(menuAtom)
  const [rename, setRename] = useAtom(renameAtom)
  const { event, onRename, title } = props
  useEffect(() => {
    if (!rename || rename !== event?.id) return
    setRename(null)
    if (onRename) setEditing(title ?? '')
  }, [rename, event, onRename, title, setRename])
  const save = () => {
    if (editing !== null && editing.trim() && editing.trim() !== props.title) props.onRename?.(editing.trim())
    setEditing(null)
  }
  return (
    <header className="view-header">
      <button
        data-ui
        className="back"
        title="Back"
        aria-label="Back"
        disabled={!props.canGoBack}
        style={{ visibility: props.canGoBack ? 'visible' : 'hidden' }}
        onClick={props.onBack}
      >
        <ChevronLeft className="icon" />
      </button>
      {props.event && props.event.type !== 'task' && <EventIcon event={props.event} />}
      {editing !== null ? (
        <input
          className="view-title view-title-input"
          autoFocus
          value={editing}
          aria-label="Title"
          onChange={(e) => setEditing(e.target.value)}
          onFocus={(e) => e.target.select()}
          onBlur={save}
          onKeyDown={(e) => {
            if (e.key === 'Enter') {
              e.preventDefault()
              save()
            }
            if (e.key === 'Escape') {
              e.preventDefault()
              e.stopPropagation()
              setEditing(null)
            }
          }}
        />
      ) : (
        <h1
          className={`view-title ${event ? 'clickable' : ''}`}
          onClick={event && ((e) => setMenu({ x: e.clientX, y: e.clientY, id: event.id }))}
          onContextMenu={
            event &&
            ((e) => {
              e.preventDefault()
              setMenu({ x: e.clientX, y: e.clientY, id: event.id })
            })
          }
        >
          {props.title}
        </h1>
      )}
      {props.actions && <div className="buttons">{props.actions}</div>}
    </header>
  )
}

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * Who to offer to hand a task to, the likeliest first: whoever else has it, then those the person signed in
 * handed things to last, then the bots, then the other people. Never the person signed in; nobody in a bot's own chat.
 */
function likeliest(detail: ItemDetail, overview: Overview): Someone[] {
  if (overview.bots.some((b) => b.id === detail.item.id)) return []
  const here = detail.item.assignees
  const all = everyone(overview)
  const ids = [...new Set([...here, ...overview.recentAssignees, ...overview.bots.map((b) => b.id), ...overview.users.map((u) => u.id)])]
  return ids.filter((id) => id !== overview.me.id).flatMap((id) => all.find((s) => s.id === id) ?? [])
}

function ItemView(props: { detail: ItemDetail; catalog: Catalog; act: Act; onOpen: (id: string) => void }) {
  const { detail, catalog, act, onOpen } = props
  const { item } = detail
  const w = useItems()
  const scroller = useRef<HTMLDivElement>(null)
  // Every time the human sees an item is in its trace.
  useEffect(() => {
    w.markViewed(item.id)
  }, [item.id, item.withYou])
  // New events arrive at the bottom, as in any chat.
  const count = detail.events.length
  useEffect(() => {
    scroller.current?.scrollTo({ top: scroller.current.scrollHeight })
  }, [count])
  return (
    <div className="item">
      {detail.parent && (
        <div className="meta item-parent">
          in{' '}
          <button data-ui className="link" onClick={() => onOpen(detail.parent!.id)}>
            {detail.parent.title}
          </button>
        </div>
      )}
      <FindLine />
      <div className="chat-scroll" ref={scroller}>
        <Chat detail={detail} overview={w.overview} catalog={catalog} act={act} onOpen={onOpen} />
      </div>
      <div className="composer">
        {item.closed ? (
          <div className="composer-bar muted">Closed.</div>
        ) : (
          <Composer
            key={item.id}
            draftKey={item.id}
            catalog={catalog}
            onResolve={() => void act(() => api().close(item.id))}
            mention={{ items: w.overview.items.filter((i) => i.id !== item.id), recent: w.recent, create: (title) => api().create(title, null) }}
            assignees={likeliest(detail, w.overview)}
            onSend={async (preset, text) => {
              let ok = false
              await act(async () => {
                await api().send(item.id, preset, text)
                ok = true
              })
              return ok
            }}
          />
        )}
      </div>
    </div>
  )
}
