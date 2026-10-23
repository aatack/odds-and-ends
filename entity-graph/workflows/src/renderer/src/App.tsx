import { createContext, useContext, useEffect, useState } from 'react'
import type { Catalog, HandoffKind, HandoffSpec, Item, ItemDetail } from '../../shared/types'
import { HandoffBuilder, History, summary } from './Handoffs'
import { Form, MULTILINE, withHints } from './forms'
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

const time = (ms: number) => new Date(ms).toLocaleString()

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
  const [showHistory, setShowHistory] = useState(false)
  const queued = detail.handoffs.filter((h) => h.status === 'queued')
  return (
    <div className="item">
      <header>
        <h1>{item.title}</h1>
        <div className="meta">
          <span className={`badge ${current?.type ?? 'closed'}`}>{current?.type ?? 'closed'}</span>
          <span>{item.source}</span>
          {item.url && (
            <a href={item.url} target="_blank" rel="noreferrer">
              {item.url}
            </a>
          )}
        </div>
      </header>

      <StateEditor key={item.state} state={item.state} onSave={(s) => act(() => api().setState(item.id, s))} />

      {current?.type === 'human' && <Decide key={current.id} detail={detail} catalog={catalog} act={act} onSkip={onSkip} />}
      {current?.type === 'bot' && (
        <section className="panel">
          <p className="muted">{item.worker !== null ? `Worker ${item.worker} is on it.` : 'Queued for the next free worker.'}</p>
          <p className="quote">{String(current.params.instructions)}</p>
          <button data-ui onClick={() => act(() => api().takeOver(item.id))}>Take over</button>
        </section>
      )}
      {current && WAITING[current.type] && (
        <section className="panel">
          <p>{summary(current)}</p>
          <p className="muted">
            {WAITING[current.type]}
            {current.nextCheckAt && ` · next check ${time(current.nextCheckAt)}`}
          </p>
          {current.lastError && <p className="error-text">{current.lastError}</p>}
          <button data-ui onClick={() => act(() => api().takeOver(item.id))}>Take over</button>
        </section>
      )}

      {queued.length > 0 && (
        <section className="queue">
          <h3>Then</h3>
          {queued.map((h) => (
            <div key={h.id} className="muted">
              {summary(h)}
            </div>
          ))}
        </section>
      )}

      <button data-ui className="link" onClick={() => setShowHistory(!showHistory)}>
        {showHistory ? 'Hide history' : 'Show history'}
      </button>
      {showHistory && <History detail={detail} />}
    </div>
  )
}

function StateEditor({ state, onSave }: { state: string; onSave: (s: string) => void }) {
  const [draft, setDraft] = useState(state)
  return (
    <section className="state">
      <h3>State</h3>
      <Form schema={MULTILINE} value={draft} setValue={(v) => setDraft(typeof v === 'string' ? v : '')} />
      {draft !== state && (
        <button data-ui onClick={() => onSave(draft)}>
          Save state
        </button>
      )}
    </section>
  )
}

/** The human's turn: approve what is asked, hand the item on, or close it. */
function Decide({ detail, catalog, act, onSkip }: { detail: ItemDetail; catalog: Catalog; act: Act; onSkip: () => void }) {
  const { item } = detail
  const current = item.current!
  const approval = current.params.approval as { action: string; inputs: Record<string, unknown> } | undefined
  const action = approval && catalog.actions.find((a) => a.name === approval.action)
  const [inputs, setInputs] = useState<Record<string, unknown>>(approval?.inputs ?? {})
  const [next, setNext] = useState<HandoffSpec[]>([{ type: 'bot', params: {} }])
  const [note, setNote] = useState('')
  useEffect(() => setInputs(approval?.inputs ?? {}), [approval])
  const queued = item.queued > 0
  return (
    <section className="panel decide">
      <p className="needed">{String(current.params.needed)}</p>
      {approval && (
        <div className="approval">
          <h3>
            {approval.action}
            {action && <span className="muted"> — {action.description}</span>}
          </h3>
          {action ? (
            <Form schema={withHints(action.inputSchema)} value={inputs} setValue={(v) => setInputs(v as Record<string, unknown>)} />
          ) : (
            <p className="error-text">No action {approval.action}.</p>
          )}
          <button data-ui className="primary" onClick={() => act(() => api().approve(item.id, inputs))}>
            Run
          </button>
        </div>
      )}
      {queued && !approval && (
        <button data-ui className="primary" onClick={() => act(() => api().handOff(item.id, null, [], note))}>
          Continue with the queue
        </button>
      )}
      <h3>{approval ? 'Or hand off' : 'Hand off'}</h3>
      <HandoffBuilder catalog={catalog} value={next} onChange={setNext} />
      <div className="compact">
        <textarea rows={1} placeholder="Note (optional; kept in the history)" value={note} onChange={(e) => setNote(e.target.value)} />
      </div>
      <div className="buttons">
        <button data-ui className={approval || queued ? '' : 'primary'} onClick={() => act(() => api().handOff(item.id, null, next, note))}>
          Hand off{queued ? ' (in front of the queue)' : ''}
        </button>
        <button data-ui onClick={() => act(() => api().close(item.id, null, note))}>Close</button>
        <button data-ui
          title="You did it yourself, outside this tool. The note says what and why, so reviews can find it."
          onClick={() => act(() => api().handledOutside(item.id, note))}
        >
          Handled outside
        </button>
        <button data-ui onClick={onSkip}>Skip</button>
      </div>
    </section>
  )
}
