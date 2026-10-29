import { createContext, useCallback, useContext, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import type { Catalog, Item, ItemDetail, ItemInfo, Overview } from '../../shared/types'
import { useAtom, useAtomValue, useSetAtom } from 'jotai'
import { ChevronLeft, Menu, MoreHorizontal } from 'lucide-react'
import { itemActions, type ItemAction } from './itemActions'
import { Dropdown, DropdownItem } from './components/ui/Dropdown'
import { TypesPage } from './TypesPage'
import { IconButton } from './components/ui/IconButton'
import { appearanceAtom, applyAppearance, MODES, SERIFS, THEMES } from './appearance'
import { Chat } from './Chat'
import { DebugView } from './DebugView'
import { ItemMenu } from './ItemMenu'
import { ItemInfoView } from './ItemInfoView'
import { ItemPicker } from './ItemPicker'
import { ItemsList } from './ItemsList'
import { SearchSectionsContext } from './sections'
import { SearchPage } from './SearchPage'
import { DEFAULT_SECTIONS, type SearchSection } from '../../shared/search'
import { Composer } from './Composer'
import { ItemRefContext } from './components/ui/Markdown'
import { ItemRef } from './ItemRef'
import { FOCUS, Pill } from './sizes'
import { ItemInfoIcon } from './icons'
import { IntegrationsPage } from './IntegrationsPage'
import { McpPage } from './McpPage'
import { defaultActions } from './defaultActions'
import { minutes, useNow } from './time'
import { LogPage } from './LogPage'
import { api, useWorkflows } from './useWorkflows'
import { findAtom, findFocusAtom, forwardAtom, menuAtom, renameAtom, searchFocusAtom, viewedAtom, viewsAtom, type View } from './views'
import { FindLine } from './FindLine'
import { AssigneePill } from './Assignees'
import { BotsPage, botStatus } from './BotsPage'
import { BotView } from './BotView'
import { Bot, User as UserIcon } from 'lucide-react'
import { everyone, nameOf, sourceAtom, userAtom, type Someone } from './people'
import { Start } from './Start'
import { inBrowser, REFUSED_EVENT, useMobile } from './phone'
import { PhonePage } from './PhonePage'

/** A new topic's title when nothing was typed: when it was started, e.g. "New topic, Thursday 20:39". */
const newTopicTitle = (at = new Date()) =>
  `New topic, ${at.toLocaleDateString([], { weekday: 'long' })} ${at.toLocaleTimeString([], { hour: '2-digit', minute: '2-digit', hour12: false })}`

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
  // In a browser, a token the backend refuses (a new one made on the Phone page) is asked for again.
  useEffect(() => {
    const refused = () => setOpen(null)
    window.addEventListener(REFUSED_EVENT, refused)
    return () => window.removeEventListener(REFUSED_EVENT, refused)
  }, [])
  // Whether the views are laid out for a phone: decided here, once, and passed down.
  const mobile = useMobile()
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
  return <App key={open} mobile={mobile} onSignOut={() => {
    api().setUser(null)
    setUser(null)
    setOpen(null)
  }} onCloseSource={() => {
    void api().closeSource()
    setSource(null)
    setOpen(null)
  }} />
}

export function App({ mobile, onSignOut, onCloseSource }: { mobile: boolean; onSignOut: () => void; onCloseSource: () => void }) {
  const items = useWorkflows()
  // Ctrl+W, Alt+Left and Ctrl+Shift+Tab (sent by the main process) take the top view off the stack; Alt+Right and Ctrl+Tab put it back.
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
  // `[itemId]()` in any text is drawn as the pill of the item it refers to; `[botId]()` and `[username]()` as the bot's or person's.
  const byId = useMemo(() => new Map(items.overview.items.map((i) => [i.id, i])), [items.overview.items])
  const { bots, users } = items.overview
  const itemRef = useCallback(
    (id: string, label?: string) => {
      const bot = bots.find((b) => b.id === id)
      const person = users.find((u) => u.id === id)
      if (bot || person)
        return (
          <Pill
            id={id}
            icon={bot ? <Bot className="icon" /> : <UserIcon className="icon" />}
            label={`@${bot?.name ?? person!.name}`}
            onOpen={bot ? () => open({ kind: 'event', id }) : undefined}
          />
        )
      return <ItemRef id={id} item={byId.get(id)} label={label} onOpen={(to) => open({ kind: 'event', id: to })} />
    },
    [byId, bots, users, open],
  )
  // The search page's sections, as configured: read again whenever anything changes (a bot may have set them).
  const [sections, setSections] = useState<SearchSection[] | null>(null)
  useEffect(() => void api().searchSections().then(setSections), [items.overview])
  return (
    <ItemsContext.Provider value={items}>
      <SearchSectionsContext.Provider value={sections ?? DEFAULT_SECTIONS}>
      <ItemRefContext.Provider value={itemRef}>
        <div className={`app ${mobile ? 'mobile' : ''}`}>
          <main className="main">
            <div className="content">
              <ItemsMain mobile={mobile} />
            </div>
          </main>
          <AppMenu mobile={mobile} onSignOut={onSignOut} onCloseSource={onCloseSource} />
        </div>
        <ItemMenu actOn={items.actOn} skip={items.skip} custom={items.customActions} tasks={items.overview.items} recent={items.recent} />
      </ItemRefContext.Provider>
      </SearchSectionsContext.Provider>
    </ItemsContext.Provider>
  )
}

/** The app's state, shared by the content pane and the corner buttons. */
const ItemsContext = createContext<ReturnType<typeof useWorkflows> | null>(null)
const useItems = () => useContext(ItemsContext)!

/**
 * The button in the bottom left: who is signed in, on which source (to leave either), the bots, and the way to the
 * other pages. On a phone it is at the top right, clear of the chat input, and offers Next, which has no key there.
 */
function AppMenu({ mobile, onSignOut, onCloseSource }: { mobile: boolean; onSignOut: () => void; onCloseSource: () => void }) {
  const w = useItems()
  const [open, setOpen] = useState(false)
  const busy = w.overview.bots.filter((b) => b.runId).length
  const [look, setLook] = useAtom(appearanceAtom)
  const source = useAtomValue(sourceAtom)
  useEffect(() => applyAppearance(look), [look])
  // Anywhere new (Back, Next, the phone's own Back) closes it.
  useEffect(() => setOpen(false), [w.views])
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
    <div className={`app-menu ${mobile ? 'mobile' : ''}`} onMouseDown={(e) => e.stopPropagation()}>
      {open && (
        <div className="menu app-menu-list">
          {mobile && (
            <button
              onClick={() => {
                setOpen(false)
                void w.next()
              }}
            >
              Next <span className="menu-status">what needs you</span>
            </button>
          )}
          <button onClick={() => go({ kind: 'items' })}>Tasks</button>
          <button onClick={() => go({ kind: 'bots' })}>
            Bots <span className="menu-status">{busy} of {w.overview.bots.length} working</span>
          </button>
          <button onClick={() => go({ kind: 'integrations' })}>Integrations</button>
          <button onClick={() => go({ kind: 'types' })}>Types</button>
          <button onClick={() => go({ kind: 'mcp' })}>MCP</button>
          {/* Only here: the phone may not change how it is reached. */}
          {!inBrowser && <button onClick={() => go({ kind: 'phone' })}>Phone</button>}
          <button onClick={() => go({ kind: 'log' })}>Edit log</button>
          <button onClick={onSignOut}>
            Sign out <span className="menu-status">{w.overview.me.name}</span>
          </button>
          <button onClick={onCloseSource} title={source ?? undefined}>
            {inBrowser ? 'Disconnect' : 'Close source'} <span className="menu-status">{source}</span>
          </button>
          {/* For now, while a style is chosen: one dropdown each. */}
          <div className="menu-selects">
            <label>
              Style
              <select value={look.theme} onChange={(e) => setLook({ ...look, theme: e.target.value as typeof look.theme })}>
                {THEMES.map((t) => (
                  <option key={t.id} value={t.id}>
                    {t.name}
                  </option>
                ))}
              </select>
            </label>
            <label>
              Mode
              <select value={look.mode} onChange={(e) => setLook({ ...look, mode: e.target.value as typeof look.mode })}>
                {MODES.map((m) => (
                  <option key={m.id} value={m.id}>
                    {m.name}
                  </option>
                ))}
              </select>
            </label>
            <label>
              Serif
              <select value={look.serif} onChange={(e) => setLook({ ...look, serif: e.target.value as typeof look.serif })}>
                {SERIFS.map((f) => (
                  <option key={f.id} value={f.id}>
                    {f.name}
                  </option>
                ))}
              </select>
            </label>
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
 * same: Back, the item as text, and what can be done to it, on the right.
 * Below is the view itself, which depends on the item's type.
 */
function ItemsMain({ mobile }: { mobile: boolean }) {
  const w = useItems()
  const openItem = (id: string) => w.open({ kind: 'event', id })
  // What the items view opens takes its place, so Back goes to where you were before it.
  const openFromItems = (id: string) => w.open({ kind: 'event', id }, { replace: w.view?.kind === 'items' })
  const d = w.detail
  if (w.view?.kind === 'types')
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title="Types" />
        {w.error && <div className="error">{w.error}</div>}
        <div className="view-body">{w.catalog && <TypesPage catalog={w.catalog} custom={w.customActions} act={w.act} mobile={mobile} />}</div>
      </div>
    )
  if (w.view?.kind === 'bots')
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title="Bots" />
        {w.error && <div className="error">{w.error}</div>}
        <div className="view-body">
          <BotsPage bots={w.overview.bots} act={w.act} onOpen={openItem} mobile={mobile} />
        </div>
      </div>
    )
  if (w.view?.kind === 'search')
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title="Search page sections" />
        <div className="view-body">
          <SearchPage />
        </div>
      </div>
    )
  if (w.view?.kind === 'log')
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title="Edit log" />
        <div className="view-body">
          <LogPage key={w.view.id ?? ''} overview={w.overview} recent={w.recent} entities={w.view.id ? [w.view.id] : []} mobile={mobile} />
        </div>
      </div>
    )
  if (w.view?.kind === 'integrations' || w.view?.kind === 'mcp' || w.view?.kind === 'phone') {
    const kind = w.view.kind
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title={kind === 'mcp' ? 'MCP' : kind === 'phone' ? 'Phone' : 'Integrations'} />
        <div className="view-body">
          {kind === 'mcp' ? <McpPage mobile={mobile} /> : kind === 'phone' ? <PhonePage mobile={mobile} /> : <IntegrationsPage mobile={mobile} />}
        </div>
      </div>
    )
  }
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
            bots={w.overview.bots.filter((b) => !b.deleted).map((b) => ({ id: b.id, name: b.name, status: botStatus(b) }))}
            botsLabel="Bots"
            onEscape={w.views.length > 0 ? w.back : undefined}
            onOpen={openFromItems}
            onTab={(title) => w.act(async () => openFromItems(await api().create(title || newTopicTitle(), null)))}
            mobile={mobile}
          />
        </div>
      </div>
    )
  if (!d || !w.catalog)
    return (
      <div className="view">
        <ViewHeader canGoBack={w.views.length > 0} onBack={w.back} />
        <div className="empty">That item is not there any more.</div>
      </div>
    )
  const e = d.info
  const debug = w.view.kind === 'debug'
  const actions = debug ? null : (
    <>
      {e.type === 'task' && d.item && !d.item.item.closed && <AssigneePill item={d.item.item} overview={w.overview} act={w.actOn(e.id)} />}
      <HeaderActions actions={itemActions(e, { act: w.actOn(e.id), skip: w.skip, custom: w.customActions })} mobile={mobile} />
    </>
  )
  return (
    <div className="view">
      <ViewHeader
        canGoBack={w.views.length > 0}
        onBack={w.back}
        title={`${debug ? 'Debug: ' : ''}${e.summary}`}
        info={e}
        actions={actions}
        onRename={e.type === 'task' && !debug ? (title) => void w.act(() => api().rename(e.id, title)) : undefined}
      />
      {w.error && <div className="error">{w.error}</div>}
      <div className={`view-body ${d.item && !debug ? 'fill' : ''}`}>
        {debug ? (
          <DebugView id={e.id} />
        ) : FOCUS[e.type] === 'chat' && e.type === 'bot' && d.item ? (
          <div className="bot-page">
            <BotView key={e.id} bot={w.overview.bots.find((b) => b.id === e.id) ?? null} act={w.act} mobile={mobile} />
            <ItemView key={`${e.id}:chat`} detail={d.item} catalog={w.catalog} act={w.act} onOpen={openItem} mobile={mobile} />
          </div>
        ) : FOCUS[e.type] === 'chat' && d.item ? (
          <ItemView key={e.id} detail={d.item} catalog={w.catalog} act={w.actOn(e.id)} onOpen={openItem} mobile={mobile} />
        ) : (
          <ItemInfoView detail={d} catalog={w.catalog} act={w.act} onOpen={openItem} />
        )}
      </div>
    </div>
  )
}

/**
 * An item's actions in its header: the important ones as icon buttons, the rest in a menu beside them. On a phone,
 * all of them in the menu, leaving the title the width.
 */
function HeaderActions({ actions, mobile = false }: { actions: ItemAction[]; mobile?: boolean }) {
  const rest = actions.filter((a) => mobile || !a.important)
  return (
    <>
      {actions
        .filter((a) => !mobile && a.important)
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

/** Back (to the items view, at the bottom of every stack; its place kept on the items view itself), the item as text, and its actions on the right. */
function ViewHeader(props: {
  canGoBack: boolean
  onBack: () => void
  title?: string
  info?: ItemInfo
  actions?: ReactNode
  /** Given, the title can be edited: Rename in its menu, then Enter (or leaving it) saves and Escape doesn't. */
  onRename?: (title: string) => void
}) {
  const [editing, setEditing] = useState<string | null>(null)
  // Clicking the title opens the item's menu (Open, Debug, Rename, its actions), as a right-click does.
  const setMenu = useSetAtom(menuAtom)
  const [rename, setRename] = useAtom(renameAtom)
  const { info, onRename, title } = props
  useEffect(() => {
    if (!rename || rename !== info?.id) return
    setRename(null)
    if (onRename) setEditing(title ?? '')
  }, [rename, info, onRename, title, setRename])
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
      {props.info && props.info.type !== 'task' && <ItemInfoIcon info={props.info} />}
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
          className={`view-title ${info ? 'clickable' : ''}`}
          onClick={info && ((e) => setMenu({ x: e.clientX, y: e.clientY, id: info.id }))}
          onContextMenu={
            info &&
            ((e) => {
              e.preventDefault()
              setMenu({ x: e.clientX, y: e.clientY, id: info.id })
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
 * Who to offer to hand a task to, the likeliest first: the bot that last wrote anything in it, whoever else has it,
 * then those the person signed in handed things to last, then the bots, then the other people. Never the person
 * signed in; nobody in a bot's own chat.
 */
function likeliest(detail: ItemDetail, overview: Overview): Someone[] {
  if (overview.bots.some((b) => b.id === detail.item.id)) return []
  const here = detail.item.assignees
  const all = everyone(overview)
  const lastBot = detail.children.map((e) => e.createdBy).reverse().find((by) => by.startsWith('bot:'))?.slice(4)
  const ids = [
    ...new Set([...(lastBot ? [lastBot] : []), ...here, ...overview.recentAssignees, ...overview.bots.map((b) => b.id), ...overview.users.map((u) => u.id)]),
  ]
  return ids.filter((id) => id !== overview.me.id).flatMap((id) => all.find((s) => s.id === id) ?? [])
}

/** How whoever has a task stands, in a line, unless the person looking has it: a bot (working, or when it wakes), or a script (running, or when it runs). */
function ownerStatus(item: Item, o: Overview, now: number): string | null {
  const ids = [...new Set([...(item.bot ? [item.bot] : []), ...(o.bots.some((b) => b.id === item.id) ? [item.id] : []), ...item.assignees])]
  for (const id of ids) {
    if (id === o.me.id) continue
    const bot = o.bots.find((b) => b.id === id)
    if (bot) {
      if (bot.runId && bot.lastRun) return `@${bot.name} is working · ${minutes(now - bot.lastRun.startedAt)}`
      if (bot.pausedUntil) return `@${bot.name} is waiting out a usage limit, till ${new Date(bot.pausedUntil).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })}`
      if (bot.wakesAt !== null) return bot.wakesAt <= now ? `@${bot.name} is waking` : `@${bot.name} wakes in ${minutes(bot.wakesAt - now)}`
      continue
    }
    const script = o.scripts.find((s) => s.id === id)
    if (script) {
      if (script.running) return `${script.name} is running`
      if (script.wakesAt !== null) return script.wakesAt <= now ? `${script.name} runs now` : `${script.name} runs in ${minutes(script.wakesAt - now)}`
      return `${script.name} waits to be run`
    }
  }
  return null
}

/** `ownerStatus`, ticking by the second on its own, so the chat above it isn't drawn again each second. */
function OwnerStatus({ item, overview }: { item: Item; overview: Overview }) {
  const status = ownerStatus(item, overview, useNow())
  return status ? <div className="chat-working">{status}</div> : null
}

/** A task's chat and its input; `mobile` lays both out for a phone. */
function ItemView(props: { detail: ItemDetail; catalog: Catalog; act: Act; onOpen: (id: string) => void; mobile?: boolean }) {
  const { detail, catalog, act, onOpen, mobile = false } = props
  const { item } = detail
  const w = useItems()
  const scroller = useRef<HTMLDivElement>(null)
  // Every time the human sees an item is in its trace.
  useEffect(() => {
    w.markViewed(item.id)
  }, [item.id, item.withYou])
  // New items arrive at the bottom, as in any chat.
  const count = detail.children.length
  useEffect(() => {
    scroller.current?.scrollTo({ top: scroller.current.scrollHeight })
  }, [count])
  // While the chat fills less than half the view, its title sits just above it too, to see at a glance.
  const [short, setShort] = useState(false)
  useEffect(() => {
    const el = scroller.current
    if (!el) return
    const chat = el.querySelector<HTMLElement>('.chat')
    if (!chat) return
    const seen = new ResizeObserver(() => setShort(chat.offsetHeight < el.clientHeight / 2))
    seen.observe(el)
    seen.observe(chat)
    return () => seen.disconnect()
  }, [item.id])
  // Whoever has it, unless it is you, says how it stands where their message will appear: a bot working or due to
  // wake, a script running or due to run. In a bot's own chat, the bot itself.
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
        {short && <h2 className="chat-title">{item.title}</h2>}
        <Chat detail={detail} overview={w.overview} catalog={catalog} act={act} onOpen={onOpen} mobile={mobile} />
        <OwnerStatus item={item} overview={w.overview} />
      </div>
      <div className="composer">
        {item.closed ? (
          <div className="composer-bar muted">
            Closed.
            <button data-ui onClick={() => void act(() => api().reopen(item.id))}>
              Reopen
            </button>
          </div>
        ) : (
          <Composer
            key={item.id}
            draftKey={item.id}
            catalog={catalog}
            onResolve={() => void act(() => api().close(item.id))}
            mention={{ items: w.overview.items.filter((i) => i.id !== item.id), recent: w.recent }}
            assignees={likeliest(detail, w.overview)}
            defaults={defaultActions(detail, w.overview).map((d) => ({ ...d, run: () => act(d.run) }))}
            mobile={mobile}
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
