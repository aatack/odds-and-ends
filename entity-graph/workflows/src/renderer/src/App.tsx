import { createContext, useContext, useEffect, useRef, useState, type ReactNode } from 'react'
import type { Catalog, ItemDetail, ItemEvent } from '../../shared/types'
import { Blockers } from './Blockers'
import { useSetAtom } from 'jotai'
import { ArrowRight, ChevronLeft, Menu } from 'lucide-react'
import { Chat } from './Chat'
import { DebugView } from './DebugView'
import { EventMenu } from './EventMenu'
import { EventView } from './EventView'
import { ItemsList } from './ItemsList'
import { Composer } from './Composer'
import { EventIcon } from './icons'
import { IntegrationsPage } from './IntegrationsPage'
import { McpPage } from './McpPage'
import { api, useWorkflows } from './useWorkflows'
import { searchFocusAtom, type View } from './views'

export function App() {
  const items = useWorkflows()
  // Ctrl+W and Alt+Left (sent by the main process) take the top view off the stack; Alt+Right puts it back.
  const { back, forward, open } = items
  useEffect(() => api().onBack(back), [back])
  useEffect(() => api().onForward(forward), [forward])
  // Ctrl+L puts the items view on top of the stack; if it is already showing, it puts the cursor in its search line.
  const focusSearch = useSetAtom(searchFocusAtom)
  const showingItems = !items.view || items.view.kind === 'items'
  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      if (!e.ctrlKey || e.altKey || e.shiftKey || e.metaKey || e.key.toLowerCase() !== 'l') return
      e.preventDefault()
      if (showingItems) focusSearch((n) => n + 1)
      else open({ kind: 'items' })
    }
    window.addEventListener('keydown', onKey)
    return () => window.removeEventListener('keydown', onKey)
  }, [open, showingItems, focusSearch])
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
      <EventMenu />
    </ItemsContext.Provider>
  )
}

/** The app's state, shared by the content pane and the corner buttons. */
const ItemsContext = createContext<ReturnType<typeof useWorkflows> | null>(null)
const useItems = () => useContext(ItemsContext)!

/** The button in the bottom left: the workers, and the way to Integrations and MCP. */
function AppMenu() {
  const w = useItems()
  const [open, setOpen] = useState(false)
  const busy = w.overview.workers.filter((x) => x.itemId).length
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
          <button onClick={() => go({ kind: 'integrations' })}>Integrations</button>
          <button onClick={() => go({ kind: 'mcp' })}>MCP</button>
          <button
            onClick={() => {
              setOpen(false)
              void w.act(() => api().pollSources())
            }}
          >
            Poll sources
          </button>
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
  const actions = debug
    ? null
    : e.type === 'item'
      ? !e.resolved && (
          <>
            <button data-ui onClick={() => w.act(() => api().close(e.id))}>
              Close
            </button>
            <button
              data-ui
              title="You did it yourself, outside this tool."
              onClick={() => w.act(() => api().handledOutside(e.id))}
            >
              Done outside
            </button>
            <button data-ui onClick={() => w.skip(e.id)}>
              Skip
            </button>
          </>
        )
      : !e.resolved && (
          <button data-ui onClick={() => w.act(() => api().cancel(e.id))}>
            Cancel
          </button>
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
          <ItemView key={e.id} detail={d.item} catalog={w.catalog} act={w.act} onOpen={openEvent} />
        ) : (
          <EventView detail={d} catalog={w.catalog} act={w.act} onOpen={openEvent} />
        )}
      </div>
    </div>
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
            catalog={catalog}
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
