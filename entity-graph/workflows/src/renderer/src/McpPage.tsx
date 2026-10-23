import { useCallback, useEffect, useState } from 'react'
import type { McpView, ToolInfo } from '../../shared/types'
import { Copyable } from './Copy'
import { api } from './useWorkflows'

export function McpPage() {
  const [view, setView] = useState<McpView | null>(null)
  const [error, setError] = useState<string | null>(null)
  const [port, setPort] = useState('')
  const [showToken, setShowToken] = useState(false)
  const refresh = useCallback(async () => {
    const v = await api().mcp()
    setView(v)
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

  if (!view) return <div className="muted">Loading…</div>
  const header = `Authorization: Bearer ${view.token}`
  const cli = `claude mcp add --transport http workflows ${view.url} --header "${header}"`
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
          Connect anything else to this app, e.g. a Claude Code session. It reaches the tools below for outside clients,
          on this machine only, while the app is open.
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

      <Tools
        title="Tools for outside clients"
        note="What a client connected at the URL above sees. Item tools name their item."
        tools={view.operatorTools}
      />
      <Tools
        title="Tools for agents"
        note="What a worker's claude -p sees while it runs one item. Item tools act on that item, and the run ends with one of the END YOUR RUN tools."
        tools={view.runTools}
      />
    </div>
  )
}

function Tools({ title, note, tools }: { title: string; note: string; tools: ToolInfo[] }) {
  return (
    <section className="card">
      <h2>
        {title} <span className="count">{tools.length}</span>
      </h2>
      <p className="muted">{note}</p>
      {tools.map((t) => {
        const params = Object.entries(t.inputSchema.properties ?? {})
        return (
          <div key={t.name} className="tool">
            <code className="tool-name">{t.name}</code>
            <div className="tool-description">{t.description}</div>
            {params.length > 0 && (
              <table className="params">
                <tbody>
                  {params.map(([name, p]) => (
                    <tr key={name}>
                      <td>
                        <code>{name}</code>
                        {t.inputSchema.required?.includes(name) ? '' : '?'}
                      </td>
                      <td className="muted">{p.enum ? p.enum.join(' | ') : p.type ?? 'any'}</td>
                      <td>{p.description}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            )}
          </div>
        )
      })}
    </section>
  )
}
