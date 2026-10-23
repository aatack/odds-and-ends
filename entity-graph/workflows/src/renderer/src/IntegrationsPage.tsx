import { useCallback, useEffect, useState } from 'react'
import type { Check, IntegrationsView } from '../../shared/types'
import { Form } from './forms'
import { api } from './useWorkflows'

export function IntegrationsPage() {
  const [view, setView] = useState<IntegrationsView | null>(null)
  const [error, setError] = useState<string | null>(null)
  const refresh = useCallback(async () => setView(await api().integrations()), [])
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

  if (!view) return <div className="muted">Checking…</div>
  return (
    <div className="page">
      <h1>Integrations</h1>
      {error && <div className="error">{error}</div>}

      <section className="card">
        <h2>GitHub</h2>
        <Status check={view.github} />
        <p className="muted">
          Through the <code>gh</code> CLI, as whoever it is signed in as. To change that, run <code>gh auth login</code>.
        </p>
      </section>

      <section className="card">
        <h2>Claude</h2>
        <Status check={view.claude} />
        <p className="muted">
          {view.claude.workers} workers, each running <code>claude -p</code> with only this app's MCP tools.
        </p>
      </section>

      <Slack view={view} act={act} />

      <button data-ui onClick={() => void refresh()}>Check again</button>
    </div>
  )
}

function Status({ check }: { check: Check }) {
  return <p className={check.ok ? '' : 'error-text'}>{check.ok ? `✓ ${check.detail}` : `✗ ${check.detail}`}</p>
}

const SLACK_WORKSPACE = {
  type: 'object',
  properties: {
    token: { type: 'string', 'x-editor': 'secret', description: 'The User OAuth Token: it starts with xoxp-.' },
    name: { type: 'string', description: "Optional; the workspace's own name by default." },
  },
  required: ['token'],
}

function Slack({ view, act }: { view: IntegrationsView; act: (fn: () => Promise<unknown>) => Promise<void> }) {
  const [value, setValue] = useState<{ name?: string; token?: string }>({})
  return (
    <section className="card">
      <h2>Slack</h2>
      {view.slack.length === 0 && <p className="muted">No workspaces yet.</p>}
      {view.slack.map((w) => (
        <div key={w.id} className="workspace">
          <div>
            <b>{w.name}</b>
            <span className="muted">
              {' '}
              {w.team} · as {w.user} · <code>{w.tokenHint}</code>
            </span>
          </div>
          <button data-ui onClick={() => act(() => api().removeSlack(w.id))}>
            Remove
          </button>
        </div>
      ))}
      <div className="add">
        <h3>Add a workspace</h3>
        <ol className="muted">
          <li>
            Create an app for the workspace on{' '}
            <a href="https://api.slack.com/apps" target="_blank" rel="noreferrer">
              Slack's apps page
            </a>
            .
          </li>
          <li>
            Under OAuth &amp; Permissions, add the User Token Scopes the workflows need, then install it to the workspace. Reading
            and waiting need <code>channels:history</code>, <code>groups:history</code>, <code>im:history</code> and{' '}
            <code>mpim:history</code>; posting needs <code>chat:write</code>; searching needs <code>search:read</code>.
          </li>
          <li>Paste the User OAuth Token below.</li>
        </ol>
        <Form schema={SLACK_WORKSPACE} value={value} setValue={(v) => setValue(v as typeof value)} />
        <button
          data-ui
          className="primary"
          disabled={!value.token?.trim()}
          onClick={() =>
            act(async () => {
              await api().addSlack(value.name ?? '', value.token ?? '')
              setValue({})
            })
          }
        >
          Check and add
        </button>
      </div>
      <p className="muted">The token is checked with Slack before it is kept, and stored encrypted with your system keyring.</p>
    </section>
  )
}
