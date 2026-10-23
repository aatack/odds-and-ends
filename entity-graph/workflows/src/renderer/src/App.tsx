import { createContext, useContext, useEffect, useRef, useState } from 'react'
import type { Catalog, Item, ItemDetail } from '../../shared/types'
import { Blockers } from './Blockers'
import { Chat } from './Chat'
import { Composer } from './Composer'
import { ItemIcon } from './icons'
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
        <button data-ui onClick={() => void w.act(() => api().pollSources())}>Poll sources</button>
      </div>
    </>
  )
}

function ItemsMain() {
  const w = useItems()
  return (
    <>
      {w.error && <div className="error">{w.error}</div>}
      {w.detail && w.catalog ? (
        <ItemView
          key={w.detail.item.id}
          detail={w.detail}
          catalog={w.catalog}
          act={w.act}
          onSkip={() => w.skip(w.detail!.item.id)}
          onOpen={w.select}
        />
      ) : (
        <div className="empty">{w.empty ? 'Nothing needs you right now.' : 'Press Next for the next thing that needs you.'}</div>
      )}
    </>
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
  if (!props.items.length) return null
  return (
    <section className="list">
      <h3>
        {props.label} <span className="count">{props.items.length}</span>
      </h3>
      {props.items.map((i) => (
        <div key={i.id} className={`row ${i.id === props.selected ? 'selected' : ''}`} onClick={() => props.onSelect(i.id)}>
          <ItemIcon item={i} />
          <span className="row-title">{i.title}</span>
          {i.worker !== null && <span className="dot" title="A bot is running" />}
        </div>
      ))}
    </section>
  )
}

type Act = (fn: () => Promise<unknown>) => Promise<void>

function ItemView(props: { detail: ItemDetail; catalog: Catalog; act: Act; onSkip: () => void; onOpen: (id: string) => void }) {
  const { detail, catalog, act, onSkip, onOpen } = props
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
      <header className="item-header">
        <div>
          <h1>{item.title}</h1>
          {detail.parent && (
            <div className="meta">
              in{' '}
              <button data-ui className="link" onClick={() => onOpen(detail.parent!.id)}>
                {detail.parent.title}
              </button>
            </div>
          )}
        </div>
        {!item.closed && (
          <div className="buttons">
            <button data-ui onClick={() => act(() => api().close(item.id))}>
              Close
            </button>
            <button data-ui title="You did it yourself, outside this tool." onClick={() => act(() => api().handledOutside(item.id))}>
              Done outside
            </button>
            <button data-ui onClick={onSkip}>Skip</button>
          </div>
        )}
      </header>
      <div className="chat-scroll" ref={scroller}>
        <Chat detail={detail} onOpen={onOpen} />
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
