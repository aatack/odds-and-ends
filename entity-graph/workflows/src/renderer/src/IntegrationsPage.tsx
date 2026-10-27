import { useCallback, useEffect, useState } from 'react'
import type { Check, IntegrationsView, IntegrationView } from '../../shared/types'
import { Form } from './forms'
import { Markdown } from './components/ui/Markdown'
import { api } from './useWorkflows'

/** `mobile`: one column, full width. */
export function IntegrationsPage({ mobile = false }: { mobile?: boolean }) {
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
    <div className={`page ${mobile ? 'mobile' : ''}`}>
      {error && <div className="error">{error}</div>}

      {view.registered.map((i) => (
        <IntegrationCard key={i.id} integration={i} act={act} />
      ))}

      <section className="card">
        <h2>Claude</h2>
        <Status check={view.claude} />
      </section>

      <Slack view={view} act={act} />

      <Deepgram view={view} act={act} />

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
      {view.slack.length > 0 && (
        <p className="muted">
          Pulled in: a Slack message's link pasted into a chat (or in a note) brings the message in; bots can add a channel over a range too.
          The token needs channels:read, groups:read, users:read, search:read and history scopes. {view.slackCache.entries} answers cached
          (names a day, channels an hour, messages five minutes).{' '}
          <button data-ui className="link" onClick={() => act(() => api().clearSlackCache())}>
            Clear the cache
          </button>
        </p>
      )}
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

const DEEPGRAM_KEY = {
  type: 'object',
  properties: { key: { type: 'string', 'x-editor': 'secret', description: 'An API key from the Deepgram console.' } },
  required: ['key'],
}

/** The key the chat input's record button streams to Deepgram with: kept here, or the backend's DEEPGRAM_API_KEY. */
function Deepgram({ view, act }: { view: IntegrationsView; act: (fn: () => Promise<unknown>) => Promise<void> }) {
  const [value, setValue] = useState<{ key?: string }>({})
  const d = view.deepgram
  return (
    <section className="card">
      <h2>Deepgram</h2>
      <p className="muted">
        For recording into the chat input: what you say is transcribed live and sent as notes.{' '}
        {d ? (
          <>
            Using <code>{d.hint}</code>, {d.from === 'env' ? 'from DEEPGRAM_API_KEY' : 'kept here'}.
          </>
        ) : (
          'No key yet: add one, or start the app with DEEPGRAM_API_KEY set.'
        )}
      </p>
      {d?.from === 'settings' && (
        <button data-ui onClick={() => act(() => api().setDeepgramKey(null))}>
          Forget the key
        </button>
      )}
      <Form schema={DEEPGRAM_KEY} value={value} setValue={(v) => setValue(v as typeof value)} />
      <button
        data-ui
        className="primary"
        disabled={!value.key?.trim()}
        onClick={() =>
          act(async () => {
            await api().setDeepgramKey(value.key ?? '')
            setValue({})
          })
        }
      >
        Keep the key
      </button>
    </section>
  )
}

/**
 * An integration in the one shape every integration takes: how it is signed in, then four sections, each folding:
 * setup (each value its own field, how to get it beside it; shown unless secret, then only that it is set, with a
 * way to remove it), the read tools, the write tools (only a person runs these), and the triggers.
 */
function IntegrationCard({ integration: i, act }: { integration: IntegrationView; act: (fn: () => Promise<unknown>) => Promise<void> }) {
  const tools = (list: { name: string; description: string }[]) =>
    list.length ? (
      <ul className="integration-tools">
        {list.map((t) => (
          <li key={t.name}>
            <code>{t.name}</code> <Markdown className="muted" text={t.description} />
          </li>
        ))}
      </ul>
    ) : (
      <p className="muted">None yet.</p>
    )
  return (
    <section className="card integration">
      <h2>{i.name}</h2>
      {i.status && <Status check={i.status} />}
      <Markdown className="muted" text={`${i.description} ${i.auth.instructions}`} />
      <details open>
        <summary>Setup</summary>
        {i.setup.length === 0 && <p className="muted">Nothing to set up.</p>}
        {i.setup.map((f) => (
          <SetupValue key={f.key} integration={i.id} field={f} act={act} />
        ))}
      </details>
      <details>
        <summary>Read · {i.read.length}</summary>
        {tools(i.read)}
      </details>
      <details>
        <summary>Write · {i.write.length} (a person approves each)</summary>
        {tools(i.write)}
      </details>
      <details>
        <summary>Triggers · {i.triggers.length}</summary>
        {i.triggers.length ? tools(i.triggers) : <p className="muted">None yet: a script polling the read tools stands in.</p>}
      </details>
    </section>
  )
}

function SetupValue({ integration, field: f, act }: { integration: string; field: IntegrationView['setup'][number]; act: (fn: () => Promise<unknown>) => Promise<void> }) {
  const [draft, setDraft] = useState('')
  return (
    <div className="setup-value">
      <div className="setup-head">
        <b>{f.label}</b>{' '}
        {f.set ? (f.secret ? <span className="muted">· set</span> : <code>{f.value}</code>) : <span className="muted">· not set</span>}
        {f.set && (
          <button data-ui className="link" onClick={() => act(() => api().setIntegrationValue(integration, f.key, null))}>
            Remove
          </button>
        )}
      </div>
      <Markdown className="muted" text={f.instructions} />
      <div className="setup-input">
        <input type={f.secret ? 'password' : 'text'} value={draft} placeholder={f.set ? 'Change it' : 'Set it'} onChange={(e) => setDraft(e.target.value)} />
        <button
          data-ui
          disabled={!draft.trim()}
          onClick={() =>
            act(async () => {
              await api().setIntegrationValue(integration, f.key, draft)
              setDraft('')
            })
          }
        >
          Save
        </button>
      </div>
    </div>
  )
}
