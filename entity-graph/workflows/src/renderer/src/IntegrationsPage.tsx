import { useCallback, useEffect, useState } from 'react'
import type { Check, IntegrationsView } from '../../shared/types'
import { Form } from './forms'
import { Markdown } from './components/ui/Markdown'
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
      {error && <div className="error">{error}</div>}

      <section className="card">
        <h2>GitHub</h2>
        <Status check={view.github} />
        <Markdown className="muted" text="Through the `gh` CLI, as whoever it is signed in as. To change that, run `gh auth login`." />
      </section>

      <section className="card">
        <h2>Claude</h2>
        <Status check={view.claude} />
      </section>

      <Slack view={view} act={act} />

      <button data-ui onClick={() => void refresh()}>Refresh</button>
    </div>
  )
}

function Status({ check }: { check: Check }) {
  return <p className={check.ok ? '' : 'error-text'}>{check.ok ? `✓ ${check.detail}` : `✗ ${check.detail}`}</p>
}

const SLACK_STEPS = `1. Create an app for the workspace on [Slack's apps page](https://api.slack.com/apps).
2. Under OAuth & Permissions, add the User Token Scopes the workflows need, then install it to the workspace. Reading and waiting need \`channels:history\`, \`groups:history\`, \`im:history\` and \`mpim:history\`; posting needs \`chat:write\`; searching needs \`search:read\`.
3. Paste the User OAuth Token below.`

const SLACK_WORKSPACE = {
  type: 'object',
  properties: {
    token: { type: 'string', 'x-editor': 'secret', description: 'The User OAuth Token: it starts with xoxp-.' },
  },
  required: ['token'],
}

function Slack({ view, act }: { view: IntegrationsView; act: (fn: () => Promise<unknown>) => Promise<void> }) {
  const [value, setValue] = useState<{ token?: string }>({})
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
        <h3>Add workspace</h3>
        <Markdown className="muted" text={SLACK_STEPS} />
        <Form schema={SLACK_WORKSPACE} value={value} setValue={(v) => setValue(v as typeof value)} />
        <button
          data-ui
          className="primary"
          disabled={!value.token?.trim()}
          onClick={() =>
            act(async () => {
              await api().addSlack(value.token ?? '')
              setValue({})
            })
          }
        >
          Check and add
        </button>
      </div>
    </section>
  )
}
