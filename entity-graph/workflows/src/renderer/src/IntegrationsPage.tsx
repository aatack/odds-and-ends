import { useCallback, useEffect, useState } from 'react'
import type { Check, IntegrationsView } from '../../shared/types'
import { api } from './useWorkflows'

/** What the app relies on outside itself, and whether it is there: for now, Claude Code, which runs the bots. */
export function IntegrationsPage() {
  const [view, setView] = useState<IntegrationsView | null>(null)
  const refresh = useCallback(async () => setView(await api().integrations()), [])
  useEffect(() => void refresh(), [refresh])
  if (!view) return <div className="muted">Checking…</div>
  return (
    <div className="page">
      <section className="card">
        <h2>Claude</h2>
        <Status check={view.claude} />
      </section>
      <button data-ui onClick={() => void refresh()}>Check again</button>
    </div>
  )
}

function Status({ check }: { check: Check }) {
  return <p className={check.ok ? '' : 'error-text'}>{check.ok ? `✓ ${check.detail}` : `✗ ${check.detail}`}</p>
}
