import React, { useState } from 'react'
import type { Entity, QueryResult, StackFrame } from '../../../core/wrapper'
import { EditorView } from '../views/Editor'
import type { EditorActions } from '../views/useEditor'

type RenderMode = 'debug' | 'editor'

interface Props {
  onResolve: (rootId: string, opts: { maxDepth?: number; limit?: number; continuationStack?: StackFrame[] }) => Promise<{ results: QueryResult[]; continuationStack: StackFrame[] | null }>
  onReadEntities: (ids: string[]) => Promise<Record<string, Entity>>
  editorActions: EditorActions
}

function EntityCard({ result }: { result: QueryResult }) {
  const { entity, depth, parentId } = result
  const [expanded, setExpanded] = useState(false)

  const valueKeys = Object.keys(entity.values)
  const displayName = (entity.values['title'] ?? entity.values['name'] ?? entity.id) as string

  return (
    <div
      className="card border-l-4 border-l-primary-300 p-3 space-y-2"
      style={{ marginLeft: `${depth * 20}px` }}
    >
      <div className="flex items-start justify-between gap-2">
        <div className="min-w-0">
          <span className="font-mono text-text-xs text-gray-400 mr-2">{entity.id}</span>
          <span className="font-medium text-text-sm text-gray-900">{String(displayName)}</span>
        </div>
        <div className="flex items-center gap-2 shrink-0">
          <span className="badge badge-gray">depth {depth}</span>
          {parentId && <span className="text-text-xs text-gray-400 font-mono">↑ {parentId.slice(0, 8)}…</span>}
        </div>
      </div>

      <div className="flex gap-2 text-text-xs text-gray-500">
        <span>{entity.outboundLinks.length} out</span>
        <span>·</span>
        <span>{entity.inboundLinks.length} in</span>
        <span>·</span>
        <span>{valueKeys.length} values</span>
        {valueKeys.length > 0 && (
          <button
            className="text-primary-600 hover:text-primary-800 ml-1"
            onClick={() => setExpanded((x) => !x)}
          >
            {expanded ? 'hide' : 'show'} values
          </button>
        )}
      </div>

      {expanded && (
        <div className="rounded-md bg-gray-50 border border-gray-200 divide-y divide-gray-100 text-text-xs font-mono">
          {valueKeys.map((k) => (
            <div key={k} className="flex gap-2 px-3 py-1.5">
              <span className="text-gray-500 shrink-0">{k}</span>
              <span className="text-gray-900 break-all">{JSON.stringify(entity.values[k])}</span>
            </div>
          ))}
        </div>
      )}
    </div>
  )
}

export function QueryExplorer({ onResolve, editorActions }: Props) {
  const [rootId, setRootId]         = useState('')
  const [maxDepth, setMaxDepth]     = useState('')
  const [limit, setLimit]           = useState('100')
  const [results, setResults]           = useState<QueryResult[] | null>(null)
  const [continuation, setContinuation] = useState<StackFrame[] | null>(null)
  const [loading, setLoading]           = useState(false)
  const [error, setError]               = useState<string | null>(null)
  const [mode, setMode]                 = useState<RenderMode>('debug')
  // Entity id / depth committed via "Resolve" — what the editor renders from.
  const [submitted, setSubmitted]       = useState<{ rootId: string; maxDepth?: number } | null>(null)

  const resolve = () => {
    const rid = rootId.trim()
    if (!rid) return
    setSubmitted({ rootId: rid, maxDepth: maxDepth ? parseInt(maxDepth, 10) : undefined })
    setResults(null)
    setContinuation(null)
    run()
  }

  const run = async (continuationStack?: StackFrame[]) => {
    if (!rootId.trim()) return
    setError(null); setLoading(true)
    try {
      const opts = {
        maxDepth: maxDepth ? parseInt(maxDepth, 10) : undefined,
        limit:    limit    ? parseInt(limit, 10)    : 100,
        continuationStack,
      }
      const page = await onResolve(rootId.trim(), opts)
      setResults((prev) => continuationStack && prev ? [...prev, ...page.results] : page.results)
      setContinuation(page.continuationStack)
    } catch (e) {
      setError(e instanceof Error ? e.message : String(e))
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="space-y-4">
      <div className="card p-4 space-y-3">
        <p className="section-title">Query</p>
        <div>
          <label className="label">Root entity ID</label>
          <input
            className="input font-mono"
            value={rootId}
            onChange={(e) => setRootId(e.target.value)}
            placeholder="entity-uuid"
            onKeyDown={(e) => e.key === 'Enter' && run()}
          />
        </div>
        <div className="flex gap-3">
          <div className="flex-1">
            <label className="label">Max depth</label>
            <input className="input" type="number" min="1" value={maxDepth} onChange={(e) => setMaxDepth(e.target.value)} placeholder="unlimited" />
          </div>
          <div className="flex-1">
            <label className="label">Page limit</label>
            <input className="input" type="number" min="1" value={limit} onChange={(e) => setLimit(e.target.value)} />
          </div>
        </div>
        <div className="flex items-center gap-2">
          <button className="btn-primary" onClick={resolve} disabled={loading || !rootId.trim()}>
            {loading ? 'Loading…' : 'Resolve'}
          </button>
          {(results || submitted) && (
            <button
              className="btn-secondary"
              onClick={() => { setResults(null); setContinuation(null); setSubmitted(null) }}
            >
              Clear
            </button>
          )}
          <div className="ml-auto inline-flex rounded-md border border-gray-300 overflow-hidden shadow-xs">
            {(['debug', 'editor'] as RenderMode[]).map((m) => (
              <button
                key={m}
                onClick={() => setMode(m)}
                className={`px-3 py-1.5 text-text-sm font-medium capitalize transition-colors ${
                  mode === m ? 'bg-primary-600 text-white' : 'bg-white text-gray-600 hover:bg-gray-50'
                }`}
              >
                {m}
              </button>
            ))}
          </div>
        </div>
        {error && <p className="text-text-sm text-error-500">{error}</p>}
      </div>

      {mode === 'editor' && submitted && (
        <EditorView rootId={submitted.rootId} maxDepth={submitted.maxDepth} actions={editorActions} />
      )}

      {mode === 'debug' && results !== null && (
        <div className="space-y-2">
          <div className="flex items-center justify-between px-1">
            <p className="text-text-sm text-gray-600">{results.length} entities</p>
            {continuation && (
              <button className="btn-secondary text-text-xs" onClick={() => run(continuation)} disabled={loading}>
                Load more (page limit reached)
              </button>
            )}
          </div>
          {results.map((r, i) => (
            <EntityCard key={`${r.entity.id}-${i}`} result={r} />
          ))}
        </div>
      )}
    </div>
  )
}
