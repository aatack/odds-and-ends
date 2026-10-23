import { createContext, useContext, useEffect, useRef, useState, type ReactNode } from 'react'
import type { Catalog, Item, ItemDetail, ItemEvent } from '../../shared/types'
import { Blockers } from './Blockers'
import { ChevronLeft } from 'lucide-react'
import { Chat } from './Chat'
import { DebugView } from './DebugView'
import { EventMenu, useEventMenu } from './EventMenu'
import { EventView } from './EventView'
import { Composer } from './Composer'
import { EventIcon, ItemIcon } from './icons'
import { IntegrationsPage } from './IntegrationsPage'
import { McpPage } from './McpPage'
import { api, useWorkflows } from './useWorkflows'

type Page = 'items' | 'integrations' | 'mcp'
const PAGES: { page: Page; label: string }[] = [
  { page: 'items', label: 'Items' },
  { page: 'integrations', label: 'Integrations' },
  { page: 'mcp', label: 'MCP' },
]

/** The sidebar's groups: the human's turn, a bot's, waiting on something else, done. */
const GROUPS: { label: string; has: (i: Item) => boolean }[] = [
  { label: 'Needs you', has: (i) => i.withHuman },
  { label: 'With bots', has: (i) => !i.withHuman && !i.closed && i.blockers.some((b) => b.type === 'bot') },
  { label: 'Waiting', has: (i) => !i.withHuman && !i.closed && !i.blockers.some((b) => b.type === 'bot') },
  { label: 'Closed today', has: (i) => i.closed },
]

export function App() {
  const [page, setPage] = useState<Page>('items')
  const items = useWorkflows()
  return (
    <ItemsContext.Provider value={items}>
      <div className="app">
        <aside className="sidebar">
          <nav className="nav">
            {PAGES.map((p) => (
              <button data-ui key={p.page} className={p.page === page ? 'active' : ''} onClick={() => setPage(p.page)}>
                {p.label}
              </button>
            ))}
          </nav>
          {page === 'items' && <ItemsSidebar />}
        </aside>
        <main className="main">
          {page === 'items' && <ItemsMain />}
          {page === 'integrations' && <IntegrationsPage />}
          {page === 'mcp' && <McpPage />}
        </main>
      </div>
      <EventMenu />
    </ItemsContext.Provider>
  )
}

/** The items page's state, shared by its sidebar and its main panel. */
const ItemsContext = createContext<ReturnType<typeof useWorkflows> | null>(null)
const useItems = () => useContext(ItemsContext)!

function ItemsSidebar() {
  const w = useItems()
  const busy = w.overview.workers.filter((x) => x.itemId).length
  return (
    <>
      <button data-ui className="primary next" onClick={() => void w.next()}>
        Next
      </button>
      <NewItem
        onCreate={(title, message) =>
          w.act(async () => {
            const id = await api().create(title, null)
            if (message) await api().send(id, 'bot', message)
            w.select(id)
          })
        }
      />
      {GROUPS.map((g) => (
        <ItemList
          key={g.label}
          label={g.label}
          items={w.overview.items.filter(g.has)}
          selected={w.selected}
          onSelect={w.select}
        />
      ))}
      <div className="footer">
        <span>
          Workers: {busy}/{w.overview.workers.length} busy
        </span>
        <button data-ui onClick={() => void w.act(() => api().pollSources())}>
          Poll sources
        </button>
      </div>
    </>
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
  const d = w.detail
  if (!d || !w.catalog || !w.view)
    return (
      <div className="view">
        <ViewHeader canGoBack={w.views.length > 1} onBack={w.back} />
        {w.error && <div className="error">{w.error}</div>}
        <div className="empty">{w.empty ? 'Nothing needs you right now.' : 'Press Next for the next thing that needs you.'}</div>
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
        canGoBack={w.views.length > 1}
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

/** Back (its place kept even with nowhere to go back to), the event as text, and its actions on the right. */
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

function NewItem({ onCreate }: { onCreate: (title: string, instructions: string) => void }) {
  const [title, setTitle] = useState('')
  const [instructions, setInstructions] = useState('')
  return (
    <form
      className="new-item compact"
      onSubmit={(e) => {
        e.preventDefault()
        if (!title.trim()) return
        onCreate(title.trim(), instructions.trim())
        setTitle('')
        setInstructions('')
      }}
    >
      <input placeholder="New item" value={title} onChange={(e) => setTitle(e.target.value)} />
      {title && (
        <>
          <textarea
            placeholder="A message for a bot (optional; without one it is yours)"
            value={instructions}
            onChange={(e) => setInstructions(e.target.value)}
            rows={2}
          />
          <button data-ui type="submit">
            Add
          </button>
        </>
      )}
    </form>
  )
}

function ItemList(props: { label: string; items: Item[]; selected: string | null; onSelect: (id: string) => void }) {
  const menu = useEventMenu()
  if (!props.items.length) return null
  return (
    <section className="list">
      <h3>
        {props.label} <span className="count">{props.items.length}</span>
      </h3>
      {props.items.map((i) => (
        <div
          key={i.id}
          className={`row ${i.id === props.selected ? 'selected' : ''}`}
          onClick={() => props.onSelect(i.id)}
          {...menu(i.id)}
        >
          <ItemIcon item={i} />
          <span className="row-title">{i.title}</span>
          {i.worker !== null && <span className="dot" title="A bot is running" />}
        </div>
      ))}
    </section>
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
