import { createContext, useContext, useEffect, useRef, useState, type ReactNode } from 'react'
import type { Catalog, ItemDetail, ItemEvent } from '../../shared/types'
import { Blockers } from './Blockers'
import { useAtom, useSetAtom } from 'jotai'
import { ArrowRight, ChevronLeft, Menu, MoreHorizontal } from 'lucide-react'
import { attachingAtom, eventActions, type EventAction } from './eventActions'
import { Dropdown, DropdownItem } from './components/ui/Dropdown'
import { TypesPage } from './TypesPage'
import { DirectivesPage } from './DirectivesPage'
import { IconButton } from './components/ui/IconButton'
import { appearanceAtom, applyAppearance, SERIFS, THEMES } from './appearance'
import { Chat } from './Chat'
import { DebugView } from './DebugView'
import { EventMenu } from './EventMenu'
import { EventView } from './EventView'
import { ItemPicker } from './ItemPicker'
import { ItemsList } from './ItemsList'
import { Composer } from './Composer'
import { EventIcon } from './icons'
import { IntegrationsPage } from './IntegrationsPage'
import { McpPage } from './McpPage'
import { api, useWorkflows } from './useWorkflows'
import { findAtom, findFocusAtom, searchFocusAtom, type View } from './views'
import { FindLine } from './FindLine'

export function App() {
  const items = useWorkflows()
  // Ctrl+W and Alt+Left (sent by the main process) take the top view off the stack; Alt+Right puts it back.
  const { back, forward, open } = items
  useEffect(() => api().onBack(back), [back])
  useEffect(() => api().onForward(forward), [forward])
  // Ctrl+L puts the items view on top of the stack; if it is already showing, it puts the cursor in its search line.
  // Ctrl+D is the Next button. Ctrl+F finds in the view: the items view's own search line, or a find line over a chat.
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
      } else if (key === 's') {
        // Ctrl+S: a summary of where the task on screen stands, as an event in its chat.
        e.preventDefault()
        if (!canFind || !items.selected) return
        const id = items.selected
        void act(() => api().summarise(id))
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
  return (
    <ItemsContext.Provider value={items}>
      <div className="app">
        <main className="main">
          <div className="content">
            <ItemsMain />
          </div>
        </main>
        <AppMenu />
        <button data-ui className="next-button" title="Next: what needs you" aria-label="Next" onClick={() => void items.next()}>
          <ArrowRight className="icon" />
        </button>
      </div>
      <EventMenu actOn={items.actOn} skip={items.skip} custom={items.customActions} />
      <AttachPicker />
    </ItemsContext.Provider>
  )
}

/** Attaching an event to a task, from its header or its right-click menu: pick the task, or make one. */
function AttachPicker() {
  const w = useItems()
  const [attaching, setAttaching] = useAtom(attachingAtom)
  if (!attaching) return null
  const { id, summary } = attaching
  return (
    <ItemPicker
      title={`Attach “${summary}” to a task`}
      // Any open item but this one can take it.
      items={w.overview.items.filter((i) => !i.closed && i.id !== id)}
      verb="Attach"
      onPick={(target) => {
        setAttaching(null)
        void w.act(() => api().attach(id, target))
      }}
      onCreate={(title) => {
        setAttaching(null)
        void w.act(async () => api().attach(id, await api().create(title, null)))
      }}
      onClose={() => setAttaching(null)}
    />
  )
}

/** The app's state, shared by the content pane and the corner buttons. */
const ItemsContext = createContext<ReturnType<typeof useWorkflows> | null>(null)
const useItems = () => useContext(ItemsContext)!

/** The button in the bottom left: the workers, and the way to Directives, Integrations and MCP. */
function AppMenu() {
  const w = useItems()
  const [open, setOpen] = useState(false)
  const busy = w.overview.workers.filter((x) => x.itemId).length
  const [look, setLook] = useAtom(appearanceAtom)
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
          <div className="menu-note">
            Bots: {busy} of {w.overview.workers.length} busy
          </div>
          <button onClick={() => go({ kind: 'items' })}>Tasks</button>
          <button onClick={() => go({ kind: 'directives' })}>Directives</button>
          <button onClick={() => go({ kind: 'integrations' })}>Integrations</button>
          <button onClick={() => go({ kind: 'types' })}>Types</button>
          <button onClick={() => go({ kind: 'mcp' })}>MCP</button>
          <button
            onClick={() => {
              setOpen(false)
              void w.act(() => api().pollSources())
            }}
          >
            Poll sources
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
  const setAttaching = useSetAtom(attachingAtom)
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
  if (w.view?.kind === 'directives')
    return (
      <div className="view">
        <ViewHeader canGoBack onBack={w.back} title="Directives" />
        {w.error && <div className="error">{w.error}</div>}
        <div className="view-body">
          <DirectivesPage act={w.act} onOpen={openEvent} />
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
        {w.empty && <p className="muted view-note">Nothing needs you right now.</p>}
        <div className="view-body">
          <ItemsList
            items={w.overview.items}
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
    <HeaderActions
      actions={eventActions({ ...e, priority: d.item?.item.priority }, { act: w.actOn(e.id), skip: w.skip, attach: setAttaching, custom: w.customActions })}
    />
  )
  return (
    <div className="view">
      <ViewHeader
        canGoBack={w.views.length > 0}
        onBack={w.back}
        title={`${debug ? 'Debug: ' : ''}${e.summary}`}
        event={e}
        actions={actions}
      />
      {w.error && <div className="error">{w.error}</div>}
      <div className={`view-body ${d.item && !debug ? 'fill' : ''}`}>
        {debug ? (
          <DebugView detail={d} />
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
function ViewHeader(props: { canGoBack: boolean; onBack: () => void; title?: string; event?: ItemEvent; actions?: ReactNode }) {
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
      {props.event && props.event.type !== 'item' && <EventIcon event={props.event} />}
      <h1 className="view-title">{props.title}</h1>
      {props.actions && <div className="buttons">{props.actions}</div>}
    </header>
  )
}

type Act = (fn: () => Promise<unknown>) => Promise<void>

function ItemView(props: { detail: ItemDetail; catalog: Catalog; act: Act; onOpen: (id: string) => void }) {
  const { detail, catalog, act, onOpen } = props
  const { item } = detail
  const scroller = useRef<HTMLDivElement>(null)
  // Every time the human sees an item is in its trace.
  useEffect(() => {
    void api().viewed(item.id)
  }, [item.id, item.withHuman])
  // New messages arrive at the bottom, as in any chat.
  const resolved = detail.events.filter((e) => e.resolved).length
  useEffect(() => {
    scroller.current?.scrollTo({ top: scroller.current.scrollHeight })
  }, [resolved])
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
        <Chat detail={detail} />
      </div>
      <div className="composer">
        <Blockers detail={detail} catalog={catalog} act={act} onOpen={onOpen} />
        {item.closed ? (
          <div className="composer-bar muted">Closed.</div>
        ) : (
          <Composer
            key={item.id}
            draftKey={item.id}
            catalog={catalog}
            onResolve={() => void act(() => api().close(item.id))}
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
