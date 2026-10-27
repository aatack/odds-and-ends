import fuzzysort from 'fuzzysort'
import { ShieldAlert, Wrench } from 'lucide-react'
import { useState } from 'react'
import type { ActionInfo, Catalog } from '../../shared/types'
import { Modal } from './components/ui/Modal'
import { Markdown } from './components/ui/Markdown'
import { Form, withHints } from './forms'
import { api } from './useWorkflows'

/**
 * `/` in the chat input: every tool there is (the one registry of actions: the MCP's, and a pressed script's
 * `tools`), to pick by name; then its arguments, as a form; then Run, as the person. What it answered is a note in
 * the task. The unsafe ones (which act outside the app) are marked: running one here is a person's own press.
 */
export function ToolRunner({ itemId, catalog, onClose }: { itemId: string; catalog: Catalog; onClose: () => void }) {
  const [text, setText] = useState('')
  const [at, setAt] = useState(0)
  const [tool, setTool] = useState<ActionInfo | null>(null)
  const [args, setArgs] = useState<Record<string, unknown>>({})
  const [error, setError] = useState<string | null>(null)
  const [running, setRunning] = useState(false)
  const shown = text.trim() ? fuzzysort.go(text, catalog.actions, { keys: ['name', 'description'], threshold: 0.2 }).map((r) => r.obj) : catalog.actions
  const pick = (a: ActionInfo) => {
    setTool(a)
    setArgs({})
    setError(null)
  }
  const run = async () => {
    if (!tool) return
    setRunning(true)
    setError(null)
    try {
      await api().runTool(itemId, tool.name, args)
      onClose()
    } catch (e) {
      setError((e as Error).message.replace(/^Error invoking remote method '\w+': (Error: )?/, ''))
    } finally {
      setRunning(false)
    }
  }
  return (
    <Modal title={tool ? `/${tool.name}` : 'Run a tool'} size="large" blur onClose={onClose}>
      {!tool ? (
        <div className="tool-runner">
          <input
            autoFocus
            className="tool-search"
            value={text}
            placeholder="Which tool?"
            onChange={(e) => {
              setText(e.target.value)
              setAt(0)
            }}
            onKeyDown={(e) => {
              if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
                e.preventDefault()
                setAt(Math.max(0, Math.min(shown.length - 1, at + (e.key === 'ArrowDown' ? 1 : -1))))
              }
              if (e.key === 'Enter' && shown[at]) {
                e.preventDefault()
                pick(shown[at])
              }
            }}
          />
          <div className="list">
            {shown.map((a, i) => (
              <div key={a.name} className={`row ${i === at ? 'selected' : ''}`} onClick={() => pick(a)} onMouseMove={() => setAt(i)}>
                {a.unsafe ? <ShieldAlert className="icon" aria-label="Acts outside the app" /> : <Wrench className="icon" />}
                <span className="row-title">/{a.name}</span>
                <span className="muted row-status">{a.description.split(/(?<=\.)\s/)[0]}</span>
              </div>
            ))}
          </div>
        </div>
      ) : (
        <div className="tool-runner">
          <Markdown className="muted" text={tool.description} />
          {tool.unsafe && <p className="error-text">This acts outside the app, as you.</p>}
          <Form schema={withHints(tool.inputSchema)} value={args} setValue={(v) => setArgs(v as Record<string, unknown>)} />
          {error && <div className="error">{error}</div>}
          <div className="buttons">
            <button data-ui onClick={() => setTool(null)}>
              Back
            </button>
            <button data-ui className="primary" disabled={running} onClick={() => void run()}>
              {running ? 'Running…' : `Run /${tool.name}`}
            </button>
          </div>
        </div>
      )}
    </Modal>
  )
}
