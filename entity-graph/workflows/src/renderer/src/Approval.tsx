import { useState } from 'react'
import type { Catalog, ItemEvent } from '../../shared/types'
import { Form, withHints } from './forms'
import { api } from './useWorkflows'

type Act = (fn: () => Promise<unknown>) => Promise<void>

/** An open approval: the unsafe action's inputs, to change, and the button that runs it. */
export function Approval({ b, catalog, act }: { b: ItemEvent; catalog: Catalog; act: Act }) {
  const name = String(b.params.action)
  const action = catalog.actions.find((a) => a.name === name)
  const [inputs, setInputs] = useState<Record<string, unknown>>((b.params.inputs as Record<string, unknown>) ?? {})
  return (
    <div className="approval">
      {action ? (
        <Form schema={withHints(action.inputSchema)} value={inputs} setValue={(v) => setInputs(v as Record<string, unknown>)} />
      ) : (
        <p className="error-text">No action {name}.</p>
      )}
      <button data-ui className="primary" title={action?.description} onClick={() => act(() => api().approve(b.id, inputs))}>
        Run {name}
      </button>
    </div>
  )
}
