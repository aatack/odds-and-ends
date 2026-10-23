import { useEffect, useState } from 'react'
import type { Blocker, Item, ItemDetail } from '../../shared/types'
import { api, useWorkflows } from './useWorkflows'

const BLOCKERS: { blocker: Blocker; label: string }[] = [
  { blocker: 'operator', label: 'Needs you' },
  { blocker: 'agent', label: 'With agents' },
  { blocker: 'external', label: 'Waiting' },
  { blocker: 'items', label: 'Waiting on items' },
  { blocker: 'closed', label: 'Closed today' },
]

const BLOCKER_LABEL: Record<Blocker, string> = {
  operator: 'needs you',
  agent: 'with an agent',
  external: 'waiting',
  items: 'waiting on items',
  closed: 'closed',
}

const time = (ms: number) => new Date(ms).toLocaleString()

export function App() {
  const w = useWorkflows()
  const busy = w.overview.workers.filter((x) => x.itemId).length
  return (
    <div className="app">
      <aside className="sidebar">
        <button className="primary next" onClick={() => void w.next()}>
          Next
        </button>
        <NewItem onCreate={(title, instruction) => w.act(async () => w.select(await api().create(title, instruction)))} />
        {BLOCKERS.map(({ blocker, label }) => (
          <ItemList
            key={blocker}
            label={label}
            items={w.overview.items.filter((i) => i.blocker === blocker)}
            selected={w.selected}
            onSelect={w.select}
          />
        ))}
        <div className="footer">
          <span>
            Workers: {busy}/{w.overview.workers.length} busy
          </span>
          <button onClick={() => void w.act(() => api().pollSources())}>Poll sources</button>
        </div>
      </aside>
      <main className="main">
        {w.error && <div className="error">{w.error}</div>}
        {w.detail ? (
          <ItemView key={w.detail.item.id} detail={w.detail} act={w.act} onSkip={() => w.skip(w.detail!.item.id)} />
        ) : (
          <div className="empty">{w.empty ? 'Nothing needs you right now.' : 'Press Next for the next thing that needs you.'}</div>
        )}
      </main>
    </div>
  )
}

function NewItem({ onCreate }: { onCreate: (title: string, instruction: string) => void }) {
  const [title, setTitle] = useState('')
  const [instruction, setInstruction] = useState('')
  return (
    <form
      className="new-item"
      onSubmit={(e) => {
        e.preventDefault()
        if (!title.trim()) return
        onCreate(title.trim(), instruction.trim())
        setTitle('')
        setInstruction('')
      }}
    >
      <input placeholder="New item" value={title} onChange={(e) => setTitle(e.target.value)} />
      {title && (
        <>
          <textarea
            placeholder="Instruction for an agent (optional)"
            value={instruction}
            onChange={(e) => setInstruction(e.target.value)}
            rows={3}
          />
          <button type="submit">Add</button>
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
          {i.worker !== null && <span className="dot" title="An agent is running" />}
          {i.title}
        </div>
      ))}
    </section>
  )
}

type Act = (fn: () => Promise<unknown>) => Promise<void>

function ItemView({ detail, act, onSkip }: { detail: ItemDetail; act: Act; onSkip: () => void }) {
  const { item } = detail
  const [showHistory, setShowHistory] = useState(false)
  return (
    <div className="item">
      <header>
        <h1>{item.title}</h1>
        <div className="meta">
          <span className={`badge ${item.blocker}`}>{BLOCKER_LABEL[item.blocker]}</span>
          <span>{item.source}</span>
          {item.url && (
            <a href={item.url} target="_blank" rel="noreferrer">
              {item.url}
            </a>
          )}
        </div>
      </header>

      <StateEditor key={item.state} state={item.state} onSave={(s) => act(() => api().setState(item.id, s))} />

      {item.blocker === 'operator' && <Decide detail={detail} act={act} onSkip={onSkip} />}
      {item.blocker === 'agent' && (
        <section className="panel">
          <p>{item.worker !== null ? `Worker ${item.worker} is on it.` : 'Queued for the next free worker.'}</p>
          {item.instruction && <p className="quote">{item.instruction}</p>}
          <button onClick={() => act(() => api().takeOver(item.id))}>Take over</button>
        </section>
      )}
      {item.blocker === 'external' && detail.wait && (
        <section className="panel">
          <p>Waiting for {detail.wait.description}.</p>
          <p className="muted">
            Next check {time(detail.wait.nextCheckAt)}
            {detail.wait.deadline && `; gives up ${time(detail.wait.deadline)}`}
          </p>
          {detail.wait.lastError && <p className="error-text">{detail.wait.lastError}</p>}
          <button onClick={() => act(() => api().takeOver(item.id))}>Take over</button>
        </section>
      )}
      {item.blocker === 'items' && (
        <section className="panel">
          <p>Waiting for:</p>
          <ul>
            {detail.dependsOn.map((d) => (
              <li key={d.id}>
                {d.title} — {BLOCKER_LABEL[d.blocker]}
              </li>
            ))}
          </ul>
          <button onClick={() => act(() => api().takeOver(item.id))}>Take over</button>
        </section>
      )}

      <button className="link" onClick={() => setShowHistory(!showHistory)}>
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
      <textarea value={draft} onChange={(e) => setDraft(e.target.value)} rows={3} placeholder="No state recorded yet." />
      {draft !== state && <button onClick={() => onSave(draft)}>Save state</button>}
    </section>
  )
}

function Decide({ detail, act, onSkip }: { detail: ItemDetail; act: Act; onSkip: () => void }) {
  const { item } = detail
  const approval = item.request?.approval
  const [inputs, setInputs] = useState<Record<string, string>>(approval?.inputs ?? {})
  const [instruction, setInstruction] = useState('')
  useEffect(() => setInputs(approval?.inputs ?? {}), [approval])
  return (
    <section className="panel decide">
      {item.request && <p className="question">{item.request.question}</p>}
      {approval && (
        <div className="approval">
          <h3>{approval.label}</h3>
          {approval.fields.map((f) => (
            <label key={f.name}>
              <span>{f.label}</span>
              {f.kind === 'select' ? (
                <select value={inputs[f.name] ?? ''} onChange={(e) => setInputs({ ...inputs, [f.name]: e.target.value })}>
                  <option value="" />
                  {f.options?.map((o) => (
                    <option key={o}>{o}</option>
                  ))}
                </select>
              ) : f.kind === 'multiline' ? (
                <textarea rows={5} value={inputs[f.name] ?? ''} onChange={(e) => setInputs({ ...inputs, [f.name]: e.target.value })} />
              ) : (
                <input value={inputs[f.name] ?? ''} onChange={(e) => setInputs({ ...inputs, [f.name]: e.target.value })} />
              )}
            </label>
          ))}
          <button className="primary" onClick={() => act(() => api().execute(item.id, inputs))}>
            Run
          </button>
        </div>
      )}
      <textarea
        rows={4}
        placeholder={approval ? 'Or tell an agent to do something else…' : 'What should an agent do?'}
        value={instruction}
        onChange={(e) => setInstruction(e.target.value)}
      />
      <div className="buttons">
        <button className={approval ? '' : 'primary'} disabled={!instruction.trim()} onClick={() => act(() => api().dispatch(item.id, instruction.trim()))}>
          Dispatch to agent
        </button>
        <button onClick={() => act(() => api().close(item.id, instruction.trim()))}>Close</button>
        <button
          title="You did it yourself, outside this tool. The note says what and why, so reviews can find it."
          onClick={() => act(() => api().handledOutside(item.id, instruction.trim()))}
        >
          Handled outside
        </button>
        <button onClick={onSkip}>Skip</button>
      </div>
    </section>
  )
}

function History({ detail }: { detail: ItemDetail }) {
  return (
    <section className="history">
      {detail.events.map((e) => (
        <div key={e.id} className="event">
          <span className="muted">{time(e.at)}</span> <b>{e.actor}</b> <span className="muted">{e.kind}</span>
          <div className="event-text">{e.text}</div>
        </div>
      ))}
      {detail.runs.length > 0 && <h3>Agent runs</h3>}
      {detail.runs.map((r) => (
        <div key={r.id} className="event">
          <span className="muted">{time(r.startedAt)}</span> worker {r.worker} · {r.outcome ?? 'running'}
          {r.costUsd !== null && ` · $${r.costUsd.toFixed(2)}`}
          {r.turns !== null && ` · ${r.turns} turns`}
          {r.result && <div className="event-text">{r.result}</div>}
        </div>
      ))}
    </section>
  )
}
