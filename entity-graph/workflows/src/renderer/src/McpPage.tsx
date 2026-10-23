import { useCallback, useEffect, useState } from 'react'
import type { Catalog, McpView, ObjectSchema } from '../../shared/types'
import { Copyable } from './Copy'
import { Form } from './forms'
import { api } from './useWorkflows'

export function McpPage() {
  const [view, setView] = useState<McpView | null>(null)
  const [catalog, setCatalog] = useState<Catalog | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [port, setPort] = useState('')
  const [showToken, setShowToken] = useState(false)
  const refresh = useCallback(async () => {
    const [v, c] = await Promise.all([api().mcp(), api().catalog()])
    setView(v)
    setCatalog(c)
    setPort(String(v.port))
  }, [])
  useEffect(() => void refresh(), [refresh])

  const act = async (fn: () => Promise<unknown>) => {
    setError(null)
    try {
      await fn()
      await refresh()
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
    }
  }

  if (!view || !catalog) return <div className="muted">Loading…</div>
  const cli = `claude mcp add --transport http workflows ${view.url} --header "Authorization: Bearer ${view.token}"`
  const json = JSON.stringify(
    { mcpServers: { workflows: { type: 'http', url: view.url, headers: { Authorization: `Bearer ${view.token}` } } } },
    null,
    2,
  )
  return (
    <div className="page">
      {error && <div className="error">{error}</div>}

      <section className="card">
        <h2>Connect</h2>
        <p className="muted">
          Connect anything else to this app, e.g. a Claude Code session. It acts as you: on this machine only, while the app is open.
        </p>
        {view.error && <p className="error-text">{view.error}</p>}
        <div className="kv">
          <span>URL</span>
          <Copyable value={view.url} />
          <span>Token</span>
          <div className="row-inline">
            <Copyable value={view.token} display={showToken ? view.token : '•'.repeat(24)} />
            <button data-ui onClick={() => setShowToken(!showToken)}>{showToken ? 'Hide' : 'Show'}</button>
            <button data-ui onClick={() => act(() => api().regenerateMcpToken())}>Regenerate</button>
          </div>
          <span>Port</span>
          <div className="row-inline">
            <div className="port">
              <Form schema={{ type: 'string' }} value={port} setValue={(v) => setPort(typeof v === 'string' ? v : '')} />
            </div>
            {port !== String(view.port) && <button data-ui onClick={() => act(() => api().setMcpPort(Number(port)))}>Save</button>}
          </div>
        </div>
        <h3>Claude Code</h3>
        <Copyable value={cli} block />
        <h3>JSON config</h3>
        <Copyable value={json} block />
      </section>

      <section className="card">
        <h2>
          Actions <span className="count">{catalog.actions.length}</span>
        </h2>
        <p className="muted">
          Everything clients connected above and your scripts (as <code>actions.name(args)</code>) can do. Unsafe actions change things
          outside this app: bots, and scripts bots wrote, never get them, and ask you for them as an approval instead. A bot also changes
          only its own item, and adds blockers by returning them.
        </p>
        {catalog.actions.map((a) => (
          <div key={a.name} className="tool">
            <div className="row-inline">
              <code className="tool-name">{a.name}</code>
              {a.unsafe && <span className="badge">unsafe</span>}
            </div>
            <div className="tool-description">{a.description}</div>
            <Params schema={a.inputSchema} />
          </div>
        ))}
      </section>

      <section className="card">
        <h2>
          Events <span className="count">{catalog.events.length}</span>
        </h2>
        <p className="muted">
          What can happen to an item. A blocker stays open until what it waits for happens; the rest resolve as they are added. A bot
          adds blockers in its output, and an after chains one onto another.
        </p>
        {catalog.events.map((h) => (
          <div key={h.type} className="tool">
            <div className="row-inline">
              <code className="tool-name">{h.type}</code>
              {h.blocking && <span className="badge">blocker</span>}
            </div>
            <div className="tool-description">{h.description}</div>
            <Params schema={h.params} />
            <div className="muted">
              Resolves with <code>{h.data}</code>
            </div>
          </div>
        ))}
      </section>
    </div>
  )
}

function Params({ schema }: { schema: ObjectSchema }) {
  const params = Object.entries(schema.properties ?? {})
  if (!params.length) return null
  return (
    <table className="params">
      <tbody>
        {params.map(([name, p]) => (
          <tr key={name}>
            <td>
              <code>{name}</code>
              {schema.required?.includes(name) ? '' : '?'}
            </td>
            <td className="muted">{p.enum ? p.enum.join(' | ') : Array.isArray(p.type) ? p.type.join(' | ') : (p.type ?? 'any')}</td>
            <td>{p.description}</td>
          </tr>
        ))}
      </tbody>
    </table>
  )
}
