import { createContext, useCallback, useContext, useEffect, useMemo, useRef, useState, type ReactNode } from 'react'
import type { Catalog, Item, ItemDetail, ItemInfo, ItemInfoDetail, Overview } from '../../shared/types'
import { useAtom, useAtomValue, useSetAtom } from 'jotai'
import { ChevronDown, ChevronLeft, ChevronUp, History, House, Inbox, Menu, MoreHorizontal, Plug, Server, Shapes, SkipForward, Smartphone, type LucideIcon } from 'lucide-react'
import { itemActions, type ItemAction } from './itemActions'
import { Dropdown, DropdownItem, DropdownSeparator } from './components/ui/Dropdown'
import { TypesPage } from './TypesPage'
import { IconButton } from './components/ui/IconButton'
import { appearanceAtom, applyAppearance, MODES, SERIFS, THEMES } from './appearance'
import { Chat } from './Chat'
import { DebugView } from './DebugView'
import { Pins } from './Pins'
import { ItemMenu } from './ItemMenu'
import { ItemInfoView } from './ItemInfoView'
import { ItemPicker } from './ItemPicker'
import { ItemsList } from './ItemsList'
import { Composer } from './Composer'
import { ItemRefContext, Markdown } from './components/ui/Markdown'
import { ItemRef } from './ItemRef'
import { Pill } from './sizes'
import { DISPLAYS, displayOf, type Display } from '../../shared/views'
import { FileFocus, uploadFiles } from './FileView'
import { NotesView } from './notes/NotesView'
import { ItemInfoIcon, TYPE_NAME } from './icons'
import { IntegrationsPage } from './IntegrationsPage'
import { McpPage } from './McpPage'
import { defaultActions } from './defaultActions'
import { minutes, useNow } from './time'
import { LogPage } from './LogPage'
import { OverviewContext, StatusLine } from './status'
import { PARADIGM } from '../../shared/paradigm'
import { ScriptCard } from './ScriptCard'
import { api, useWorkflows } from './useWorkflows'
import { findAtom, findFocusAtom, forwardAtom, menuAtom, renameAtom, searchFocusAtom, viewedAtom, viewsAtom, type View } from './views'
import { FindLine } from './FindLine'
import { AssigneePill, OwnerPill, PickPill } from './Assignees'
import { BotsPage, botStatus } from './BotsPage'
import { BotView } from './BotView'
import { Bot, SquareCode, User as UserIcon } from 'lucide-react'
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
  const canFind = (!!items.detail?.item || items.detail?.info.type === 'topic') && items.view?.kind === 'event'
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
            label={bot?.name ?? person!.name}
            onOpen={bot ? () => open({ kind: 'event', id }) : undefined}
          />
        )
      return <ItemRef id={id} item={byId.get(id)} label={label} onOpen={(to) => open({ kind: 'event', id: to })} />
    },
    [byId, bots, users, open],
  )
  return (
    <ItemsContext.Provider value={items}>
      <OverviewContext.Provider value={items.overview}>
      <ItemRefContext.Provider value={itemRef}>
        {/* On a desktop: the pages on the left, the view in the middle, the item's people, actions and pins on the right. */}
        <div className={`app ${mobile ? 'mobile' : ''}`}>
          {!mobile && <SideNav onSignOut={onSignOut} onCloseSource={onCloseSource} />}
          <ItemsMain mobile={mobile} />
          {mobile && <AppMenu mobile={mobile} onSignOut={onSignOut} onCloseSource={onCloseSource} />}
        </div>
        <ItemMenu actOn={items.actOn} custom={items.customActions} overview={items.overview} recent={items.recent} />
      </ItemRefContext.Provider>
      </OverviewContext.Provider>
    </ItemsContext.Provider>
  )
}

/** The app's state, shared by the content pane and the corner buttons. */
const ItemsContext = createContext<ReturnType<typeof useWorkflows> | null>(null)
const useItems = () => useContext(ItemsContext)!

/** The pages, in the left pane, the one on screen marked. */
const PAGES: { kind: 'home' | 'items' | 'bots' | 'integrations' | 'types' | 'mcp' | 'phone' | 'log'; label: string; icon: LucideIcon; title?: string }[] = [
  { kind: 'home', label: 'Home', icon: House, title: 'How this works' },
  { kind: 'items', label: 'Assignments', icon: Inbox, title: 'Assignments (Ctrl+L): what is assigned to you, what you own, what is pinned to you' },
  { kind: 'bots', label: 'Bots', icon: Bot },
  { kind: 'integrations', label: 'Integrations', icon: Plug },
  { kind: 'types', label: 'Types', icon: Shapes },
  { kind: 'mcp', label: 'MCP', icon: Server },
  { kind: 'phone', label: 'Phone', icon: Smartphone },
  { kind: 'log', label: 'Edit log', icon: History },
]

/**
 * The left pane, on a desktop: the pages to go to, and below them a menu with who is signed in and on which source
 * (to leave either), and how the app looks.
 */
function SideNav({ onSignOut, onCloseSource }: { onSignOut: () => void; onCloseSource: () => void }) {
  const w = useItems()
  const busy = w.overview.bots.filter((b) => b.runId).length
  const [look, setLook] = useAtom(appearanceAtom)
  const source = useAtomValue(sourceAtom)
  useEffect(() => applyAppearance(look), [look])
  const at = w.view?.kind ?? 'items'
  return (
    <nav className="side-nav">
      <div className="side-nav-pages">
        {/* Only here, not on the phone: the phone may not change how it is reached. */}
        {PAGES.filter((p) => p.kind !== 'phone' || !inBrowser).map((p) => (
          <button key={p.kind} data-ui className={`side-nav-page ${at === p.kind ? 'chosen' : ''}`} title={p.title} onClick={() => w.open({ kind: p.kind })}>
            <p.icon className="icon" />
            {p.label}
            {p.kind === 'bots' && busy > 0 && <span className="menu-status">{busy} working</span>}
          </button>
        ))}
      </div>
      <Dropdown
        menuClassName="popover"
        trigger={({ toggle }) => (
          <button data-ui className="side-nav-page side-nav-me" title={source ?? undefined} onClick={toggle}>
            <UserIcon className="icon" />
            <span className="pill-label">{w.overview.me.name}</span>
            <ChevronUp className="icon side-nav-chevron" />
          </button>
        )}
      >
        {(close) => (
          <>
            <DropdownItem
              onClick={() => {
                close()
                onSignOut()
              }}
            >
              Sign out <span className="menu-status">{w.overview.me.name}</span>
            </DropdownItem>
            <DropdownItem
              onClick={() => {
                close()
                onCloseSource()
              }}
            >
              {inBrowser ? 'Disconnect' : 'Close source'} <span className="menu-status">{source}</span>
            </DropdownItem>
            <DropdownSeparator />
            <Choices label="Style" options={THEMES} value={look.theme} set={(theme) => setLook({ ...look, theme })} />
            <Choices label="Mode" options={MODES} value={look.mode} set={(mode) => setLook({ ...look, mode })} />
            <Choices label="Serif" options={SERIFS} value={look.serif} set={(serif) => setLook({ ...look, serif })} />
          </>
        )}
      </Dropdown>
    </nav>
  )
}

/** One of the looks, in the left pane's menu: its name, then a chip per choice. */
function Choices<T extends string>({ label, options, value, set }: { label: string; options: readonly { id: T; name: string }[]; value: T; set: (v: T) => void }) {
  return (
    <div className="side-choices">
      <span className="muted">{label}</span>
      <div className="menu-choices">
        {options.map((o) => (
          <button key={o.id} data-ui className={o.id === value ? 'chosen' : ''} onClick={() => set(o.id)}>
            {o.name}
          </button>
        ))}
      </div>
    </div>
  )
}

/** On a phone, the button at the top right, clear of the chat input: the pages, who is signed in, on which source, and Next, which has no key there. */
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
          <button onClick={() => go({ kind: 'home' })}>Home</button>
          <button onClick={() => go({ kind: 'items' })}>Assignments</button>
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
 * The middle of the window, and the right pane beside it: the top view on the stack. Every view's first line is the
 * same (`ViewHeader`): Back, the item as its pill, and Next; pressed anywhere else, an item's offers its views. Below
 * is the view itself. The right pane has the item's people and actions, then its pins (on a phone, there is none: the
 * header has the people and actions, and the debug view the pins).
 */
function ItemsMain({ mobile }: { mobile: boolean }) {
  const w = useItems()
  const openItem = (id: string) => w.open({ kind: 'event', id })
  // What the items view opens takes its place, so Back goes to where you were before it.
  const openFromItems = (id: string) => w.open({ kind: 'event', id }, { replace: w.view?.kind === 'items' })
  const d = w.detail
  const frame = (view: ReactNode, side?: ReactNode) => (
    <>
      <main className="main">
        <div className="content">{view}</div>
      </main>
      {!mobile && <aside className="side-pane">{side}</aside>}
    </>
  )
  const page = (title: string, body: ReactNode) =>
    frame(
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title={title} mobile={mobile} />
        {w.error && <div className="error">{w.error}</div>}
        <div className="view-body">{body}</div>
      </div>,
    )
  if (w.view?.kind === 'types') return page('Types', w.catalog && <TypesPage catalog={w.catalog} custom={w.customActions} act={w.act} mobile={mobile} />)
  if (w.view?.kind === 'bots') return page('Bots', <BotsPage bots={w.overview.bots} act={w.act} onOpen={openItem} mobile={mobile} />)
  if (w.view?.kind === 'log')
    return page('Edit log', <LogPage key={w.view.id ?? ''} overview={w.overview} recent={w.recent} entities={w.view.id ? [w.view.id] : []} mobile={mobile} />)
  if (w.view?.kind === 'integrations') return page('Integrations', <IntegrationsPage mobile={mobile} />)
  if (w.view?.kind === 'mcp') return page('MCP', <McpPage mobile={mobile} />)
  if (w.view?.kind === 'phone') return page('Phone', <PhonePage mobile={mobile} />)
  // Home: how the app works, the same text the bots are given (shared/paradigm.ts), as markdown.
  if (w.view?.kind === 'home') return page('Home', <Markdown className="home-doc" text={PARADIGM} />)
  // Home (nothing on the stack, or the items view) is the person signed in, shown as any item is: `selected` is them.
  const home = !w.view || w.view.kind === 'items'
  if (!d || !w.catalog)
    return frame(
      <div className="view">
        <ViewHeader canGoBack={w.views.length > 0} onBack={w.back} mobile={mobile} />
        {/* Home waits a moment for who is signed in. */}
        {!home && <div className="empty">That item is not there any more.</div>}
      </div>,
    )
  const e = d.info
  const debug = w.view?.kind === 'debug'
  const shown = displayOf(e)
  const act = w.actOn(e.id)
  // A view picked from the debug view takes its place.
  const pick = (display: Display) => {
    if (display !== shown) void w.act(() => api().setValue(e.id, 'display', display))
    if (debug) w.open({ kind: 'event', id: e.id }, { replace: true })
  }
  const pins = <Pins id={e.id} pins={d.pins} pinnedIn={d.pinnedIn} items={w.overview.items} recent={w.recent} act={w.act} onOpen={openItem} />
  return frame(
    <div className="view">
      <ViewHeader
        mobile={mobile}
        canGoBack={w.views.length > 0}
        onBack={w.back}
        title={`${debug ? 'Debug: ' : ''}${e.summary.split('\n')[0]}`}
        info={e}
        actions={
          mobile && !debug ? (
            <>
              {d.item && <AssigneePill item={d.item.item} overview={w.overview} act={act} />}
              <HeaderActions actions={itemActions(e, { act, custom: w.customActions })} mobile />
            </>
          ) : undefined
        }
        menu={viewMenu(debug ? 'debug' : shown, pick, () => w.open({ kind: 'debug', id: e.id }))}
        onRename={e.type === 'topic' && !debug ? (title) => void w.act(() => api().rename(e.id, title)) : undefined}
      />
      {w.error && <div className="error">{w.error}</div>}
      {home && w.empty && <p className="muted view-note">Nothing needs you.</p>}
      <div className={`view-body ${!debug && (shown === 'comments' || shown === 'notes' || (shown === 'focus' && (e.type === 'file' || e.type === 'bot'))) ? 'fill' : ''}`}>
        {debug ? (
          <DebugView id={e.id}>{mobile && pins}</DebugView>
        ) : shown === 'assignments' ? (
          // What is assigned to it, what it owns, what is pinned to it: a person's or a bot's home. Yours, from Home,
          // makes topics with Tab, and what it opens takes its place.
          <ItemsList
            items={w.overview.items}
            recent={{ ...w.recent, who: [e.id] }}
            onOpen={home ? openFromItems : openItem}
            onEscape={home && w.views.length > 0 ? w.back : undefined}
            onTab={home ? (title) => w.act(async () => openFromItems(await api().create(title || newTopicTitle(), null))) : undefined}
            act={w.act}
            mobile={mobile}
          />
        ) : shown === 'notes' ? (
          <NotesView key={e.id} info={e} tasks={w.overview.items} recent={w.recent} bots={everyone(w.overview)} onOpen={openItem} onBack={w.back} mobile={mobile} />
        ) : shown === 'comments' && d.item ? (
          <ItemView key={e.id} detail={d.item} catalog={w.catalog} act={act} onOpen={openItem} mobile={mobile} />
        ) : e.type === 'file' ? (
          <FileFocus key={e.id} info={e} mobile={mobile} />
        ) : e.type === 'bot' ? (
          <BotView key={e.id} bot={w.overview.bots.find((b) => b.id === e.id) ?? null} act={w.act} mobile={mobile} />
        ) : (
          <ItemInfoView detail={d} catalog={w.catalog} act={w.act} onOpen={openItem} />
        )}
      </div>
    </div>,
    <ItemSide detail={d} act={act} pins={pins} />,
  )
}

/** The top of the right pane for an item: who has it, who owns it, and what can be done to it. Its pins follow. */
function ItemSide({ detail: d, act, pins }: { detail: ItemInfoDetail; act: Act; pins: ReactNode }) {
  const w = useItems()
  const e = d.info
  const actions = itemActions(e, { act, custom: w.customActions })
  return (
    <>
      <section className="side-section">
        {d.item && (
          <div className="side-row">
            <span className="side-label">Assignees</span>
            <AssigneePill item={d.item.item} overview={w.overview} act={act} />
          </div>
        )}
        <div className="side-row">
          <span className="side-label">Owner</span>
          {/* A script may be owned by an item type: it is then offered on every item of that type. */}
          <OwnerPill id={e.id} owner={e.owner} overview={w.overview} act={act} types={e.type === 'script' ? (Object.keys(TYPE_NAME) as string[]) : []} />
        </div>
        {actions.length > 0 && (
          <div className="side-actions">
            {actions.map((a) => (
              <button key={a.id} data-ui className={`side-action ${a.tone}`} title={a.title} onClick={a.run}>
                <a.icon className="icon" />
                {a.label}
              </button>
            ))}
          </div>
        )}
        {/* The scripts this item's type owns, each a card that runs it on this item. */}
        {d.typeScripts.length > 0 && (
          <div className="side-scripts">
            {d.typeScripts.map((s) => (
              <ScriptCard key={s.id} info={s} on={e.id} act={act} whole />
            ))}
          </div>
        )}
      </section>
      {pins}
    </>
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

/**
 * Every view's first line: Back (to the items view, at the bottom of every stack; its place kept on the items view
 * itself), the item as its pill, and Next. Given a `menu`, pressing anywhere in it but a button opens that menu (an
 * item's views). Right-click the title for the item's menu.
 */
function ViewHeader(props: {
  canGoBack: boolean
  onBack: () => void
  title?: string
  info?: ItemInfo
  /** On a phone, what the right pane has: drawn on the right of the line. */
  actions?: ReactNode
  menu?: (close: () => void, rename?: () => void) => ReactNode
  /** Given, the title can be edited: Rename in its menu, then Enter (or leaving it) saves and Escape doesn't. */
  onRename?: (title: string) => void
  mobile?: boolean
}) {
  const w = useItems()
  const [editing, setEditing] = useState<string | null>(null)
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
  const header = (toggle?: () => void) => (
    <header
      className={`view-header ${toggle ? 'pickable' : ''}`}
      onClick={toggle && ((ev) => !(ev.target as HTMLElement).closest('button, input, a, .menu') && toggle())}
    >
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
      {/* The item as its pill, in every view's header: its icon (a topic's status), then its title. */}
      {info && <ItemInfoIcon info={info} />}
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
          className="view-title"
          onContextMenu={
            info &&
            ((e) => {
              e.preventDefault()
              setMenu({ x: e.clientX, y: e.clientY, id: info.id })
            })
          }
        >
          <span className="view-title-text">{props.title}</span>
          {toggle && <ChevronDown className="icon view-chevron" />}
        </h1>
      )}
      {props.actions && <div className="buttons">{props.actions}</div>}
      {/* On a phone, Next is in the menu: there is no Ctrl+D. */}
      {!props.mobile && (
        <IconButton title="Next (Ctrl+D): the next thing assigned to you that you haven't looked at in the last half hour" onClick={() => void w.next()}>
          <SkipForward className="size-[17px]" />
        </IconButton>
      )}
    </header>
  )
  const { menu } = props
  if (!menu) return header()
  return (
    <Dropdown menuClassName="popover" trigger={({ toggle }) => header(toggle)}>
      {(close) => menu(close, onRename && (() => setEditing(title ?? '')))}
    </Dropdown>
  )
}

type Act = (fn: () => Promise<unknown>) => Promise<void>

/**
 * Who to offer to hand a task to, the likeliest first: the bot that last wrote anything in it, whoever else has it,
 * then those the person signed in handed things to last, then the bots, then the named scripts, then the other people. Never the person
 * signed in; nobody in a bot's own chat.
 */
function likeliest(detail: ItemDetail, overview: Overview): Someone[] {
  if (overview.bots.some((b) => b.id === detail.item.id)) return []
  const here = detail.item.assignees
  const all = everyone(overview)
  const lastBot = detail.children.map((e) => e.createdBy).reverse().find((by) => by.startsWith('bot:'))?.slice(4)
  const ids = [
    ...new Set([...(lastBot ? [lastBot] : []), ...here, ...overview.recentAssignees,
      ...overview.bots.map((b) => b.id),
      ...overview.scripts.filter((s) => s.named).map((s) => s.id),
      ...overview.users.map((u) => u.id),
    ]),
  ]
  return ids.filter((id) => id !== overview.me.id).flatMap((id) => all.find((s) => s.id === id) ?? [])
}

/**
 * Who else has the item, over its chat's input: each as a card (their picture and name, which open them, and how they
 * stand on the right: a bot working and for how long, or starting soon). Not the person looking: it is theirs anyway.
 */
function Assignees({ item, overview, onOpen }: { item: Item; overview: Overview; onOpen: (id: string) => void }) {
  const others = item.assignees.filter((a) => a !== overview.me.id)
  if (!others.length) return null
  return (
    <div className="assignee-cards">
      {others.map((id) => {
        const bot = overview.bots.some((b) => b.id === id)
        const script = overview.scripts.some((s) => s.id === id)
        return (
          <button key={id} data-ui className="assignee-card" onClick={() => onOpen(id)}>
            {bot ? <Bot className="icon" /> : script ? <SquareCode className="icon" /> : <UserIcon className="icon" />}
            <span className="assignee-name">{nameOf(overview, id)}</span>
            <StatusLine id={id} />
          </button>
        )
      })}
    </div>
  )
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
  // A file dropped on the chat, or pasted into it, is kept as a file item here.
  const [dropping, setDropping] = useState(false)
  const take = (files: File[]) => files.length > 0 && !item.closed && void act(() => uploadFiles(item.id, files))
  return (
    <div
      className={`item ${dropping ? 'dropping' : ''}`}
      onDragOver={(ev) => {
        if (![...ev.dataTransfer.types].includes('Files')) return
        ev.preventDefault()
        setDropping(true)
      }}
      onDragLeave={(ev) => !ev.currentTarget.contains(ev.relatedTarget as Node) && setDropping(false)}
      onDrop={(ev) => {
        setDropping(false)
        if (!ev.dataTransfer.files.length) return
        ev.preventDefault()
        take([...ev.dataTransfer.files])
      }}
      onPaste={(ev) => {
        const files = [...ev.clipboardData.files]
        if (!files.length) return
        ev.preventDefault()
        take(files)
      }}
    >
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
      </div>
      <Assignees item={item} overview={w.overview} onOpen={onOpen} />
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

/**
 * An item's views, the menu its header opens: its focus, comments, notes or assignments (the same for every type,
 * kept as its `display`), then Debug, and Rename for a topic.
 */
function viewMenu(shown: Display | 'debug', pick: (d: Display) => void, onDebug: () => void) {
  return (close: () => void, rename?: () => void) => (
    <>
      {DISPLAYS.map((d) => (
        <DropdownItem
          key={d.id}
          active={d.id === shown}
          onClick={() => {
            close()
            pick(d.id as Display)
          }}
        >
          {d.label}
        </DropdownItem>
      ))}
      <DropdownSeparator />
      <DropdownItem
        active={shown === 'debug'}
        onClick={() => {
          close()
          if (shown !== 'debug') onDebug()
        }}
      >
        Debug
      </DropdownItem>
      {rename && (
        <DropdownItem
          onClick={() => {
            close()
            rename()
          }}
        >
          Rename
        </DropdownItem>
      )}
    </>
  )
}
