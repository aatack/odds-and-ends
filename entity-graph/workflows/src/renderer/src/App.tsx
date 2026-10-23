import { createContext, useContext, useEffect, useRef, useState } from 'react'
import type { Catalog, HandoffKind, HandoffSpec, Item, ItemDetail } from '../../shared/types'
import { Chat, HandoffBuilder } from './Handoffs'
import { Form, withHints } from './forms'
import { IntegrationsPage } from './IntegrationsPage'
import { McpPage } from './McpPage'
import { api, useWorkflows } from './useWorkflows'

type Page = 'items' | 'integrations' | 'mcp'
const PAGES: { page: Page; label: string }[] = [
  { page: 'items', label: 'Items' },
  { page: 'integrations', label: 'Integrations' },
  { page: 'mcp', label: 'MCP' },
]

/** The sidebar's groups, by what the item waits for now. */
const GROUPS: { label: string; has: (i: Item) => boolean }[] = [
  { label: 'Needs you', has: (i) => i.current?.type === 'human' },
  { label: 'With bots', has: (i) => i.current?.type === 'bot' },
  { label: 'Waiting', has: (i) => ['github', 'slack', 'items'].includes(i.current?.type ?? '') },
  { label: 'Closed today', has: (i) => i.closed },
]

const WAITING: Partial<Record<HandoffKind, string>> = {
  github: 'Waiting on GitHub',
  slack: 'Waiting on Slack',
  items: 'Waiting on other items',
}

const shortTime = (ms: number) => new Date(ms).toLocaleTimeString([], { hour: '2-digit', minute: '2-digit' })

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
        onCreate={(title, instructions) =>
          w.act(async () =>
            w.select(await api().create(title, instructions ? [{ type: 'bot', params: { instructions } }] : [])),
          )
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
        <ItemView key={w.detail.item.id} detail={w.detail} catalog={w.catalog} act={w.act} onSkip={() => w.skip(w.detail!.item.id)} />
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
            placeholder="Instructions for a bot (optional; without them it comes to you)"
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
          {i.worker !== null && <span className="dot" title="A bot is running" />}
          {i.title}
        </div>
      ))}
    </section>
  )
}

type Act = (fn: () => Promise<unknown>) => Promise<void>

function ItemView({ detail, catalog, act, onSkip }: { detail: ItemDetail; catalog: Catalog; act: Act; onSkip: () => void }) {
  const { item } = detail
  const current = item.current
  const scroller = useRef<HTMLDivElement>(null)
  // Every time the human sees an item, or sees it move on, is in its trace.
  useEffect(() => {
    void api().viewed(item.id)
  }, [item.id, current?.id])
  // New messages arrive at the bottom, as in any chat.
  useEffect(() => {
    scroller.current?.scrollTo({ top: scroller.current.scrollHeight })
  }, [detail.handoffs.length, current?.id])
  return (
    <div className="item">
      <header>
        <h1>{item.title}</h1>
        <div className="meta">
          <span>{item.source}</span>
          {item.url && (
            <a href={item.url} target="_blank" rel="noreferrer">
              {item.url}
            </a>
          )}
        </div>
      </header>
      <div className="chat-scroll" ref={scroller}>
        <Chat detail={detail} />
      </div>
      <div className="composer">
        {current?.type === 'human' && <Decide key={current.id} detail={detail} catalog={catalog} act={act} onSkip={onSkip} />}
        {current?.type === 'bot' && (
          <div className="composer-bar">
            <span className="muted">{item.worker !== null ? 'A bot is on it.' : 'Waiting for a free bot.'}</span>
            <button data-ui onClick={() => act(() => api().takeOver(item.id))}>
              Take over
            </button>
          </div>
        )}
        {current && WAITING[current.type] && (
          <div className="composer-bar">
            <span className="muted">
              {WAITING[current.type]}
              {current.nextCheckAt && ` · next check ${shortTime(current.nextCheckAt)}`}
              {current.lastError && <span className="error-text"> · {current.lastError}</span>}
            </span>
            <button data-ui onClick={() => act(() => api().takeOver(item.id))}>
              Take over
            </button>
          </div>
        )}
        {!current && <div className="composer-bar muted">Closed.</div>}
      </div>
    </div>
  )
}

/** The human's turn: approve what is asked, hand the item on, or close it. Their question is the last message above. */
function Decide({ detail, catalog, act, onSkip }: { detail: ItemDetail; catalog: Catalog; act: Act; onSkip: () => void }) {
  const { item } = detail
  const current = item.current!
  const approval = current.params.approval as { action: string; inputs: Record<string, unknown> } | undefined
  const action = approval && catalog.actions.find((a) => a.name === approval.action)
  const [inputs, setInputs] = useState<Record<string, unknown>>(approval?.inputs ?? {})
  const [next, setNext] = useState<HandoffSpec[]>([{ type: 'bot', params: {} }])
  useEffect(() => setInputs(approval?.inputs ?? {}), [approval])
  return (
    <>
      {approval && (
        <div className="approval">
          {action ? (
            <Form schema={withHints(action.inputSchema)} value={inputs} setValue={(v) => setInputs(v as Record<string, unknown>)} />
          ) : (
            <p className="error-text">No action {approval.action}.</p>
          )}
          <button data-ui className="primary" title={action?.description} onClick={() => act(() => api().approve(item.id, inputs))}>
            Run {approval.action}
          </button>
        </div>
      )}
      <HandoffBuilder catalog={catalog} value={next} onChange={setNext} />
      <div className="composer-bar">
        <div className="buttons">
          <button data-ui onClick={() => act(() => api().close(item.id))}>
            Close
          </button>
          <button data-ui title="You did it yourself, outside this tool." onClick={() => act(() => api().handledOutside(item.id))}>
            Done outside
          </button>
          <button data-ui onClick={onSkip}>Skip</button>
        </div>
        <div className="buttons">
          {item.queued > 0 && (
            <button data-ui title="Go on with the queued handoffs" onClick={() => act(() => api().handOff(item.id, []))}>
              Continue
            </button>
          )}
          <button data-ui className="primary" onClick={() => act(() => api().handOff(item.id, next))}>
            Send
          </button>
        </div>
      </div>
    </>
  )
}
