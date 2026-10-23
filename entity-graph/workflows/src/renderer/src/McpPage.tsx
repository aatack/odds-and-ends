import { useCallback, useEffect, useState } from 'react'
import type { Catalog, McpView, ObjectSchema } from '../../shared/types'
import { Copyable } from './Copy'
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
      <h1>MCP</h1>
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
            <button onClick={() => setShowToken(!showToken)}>{showToken ? 'Hide' : 'Show'}</button>
            <button onClick={() => act(() => api().regenerateMcpToken())}>Regenerate</button>
          </div>
          <span>Port</span>
          <div className="row-inline">
            <input className="port" value={port} onChange={(e) => setPort(e.target.value)} />
            {port !== String(view.port) && <button onClick={() => act(() => api().setMcpPort(Number(port)))}>Save</button>}
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
          The same list for agents, for clients connected above, and for scripts (as <code>actions.name(args)</code>). What a caller may do
          depends on who it is: an agent, or a script an agent wrote, changes only its own item and runs nothing outward, and hands items on by
          returning handoffs.
        </p>
        {catalog.actions.map((a) => (
          <div key={a.name} className="tool">
            <div className="row-inline">
              <code className="tool-name">{a.name}</code>
              {a.outward && <span className="badge">outward</span>}
            </div>
            <div className="tool-description">{a.description}</div>
            <Params schema={a.inputSchema} />
          </div>
        ))}
      </section>

      <section className="card">
        <h2>
          Handoffs <span className="count">{catalog.handoffs.length}</span>
        </h2>
        <p className="muted">
          What an item can wait for. An agent returns a list of these as its output; the first is what the item waits for, and each resolves
          into the next.
        </p>
        {catalog.handoffs.map((h) => (
          <div key={h.type} className="tool">
            <code className="tool-name">{h.type}</code>
            <div className="tool-description">{h.description}</div>
            <Params schema={h.params} />
            <div className="muted">
              Resolves with <code>{h.resolution}</code>
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
