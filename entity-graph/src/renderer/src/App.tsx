import React, { useEffect, useState, useCallback } from 'react'
import { SourceManager } from './components/SourceManager'
import { EventEditor } from './components/EventEditor'
import { QueryExplorer } from './components/QueryExplorer'
import type { AppEvent } from '../../core/events'

interface SourceConfig {
  id: string
  label: string
  type: 'sqlite' | 'http'
  path?: string
  url?: string
}

type Tab = 'sources' | 'query' | 'events'

const api = window.entityGraph

export default function App() {
  const [tab, setTab]           = useState<Tab>('sources')
  const [sources, setSources]   = useState<SourceConfig[]>([])
  const [user, setUserState]    = useState('anonymous')
  const [editingUser, setEditingUser] = useState(false)
  const [userInput, setUserInput]     = useState('')

  useEffect(() => {
    api.getSources().then(setSources)
    api.getUser().then(setUserState)
  }, [])

  const addSource = useCallback(async (cfg: Omit<SourceConfig, 'id'>) => {
    await api.addSource(cfg)
    setSources(await api.getSources())
  }, [])

  const removeSource = useCallback(async (id: string) => {
    await api.removeSource(id)
    setSources(await api.getSources())
  }, [])

  const writeEvents = useCallback(async (events: AppEvent[]) => {
    await api.writeEvents(events)
  }, [])

  const resolveQuery = useCallback(
    (rootId: string, opts: { maxDepth?: number; limit?: number }) =>
      api.resolveQuery(rootId, opts),
    [],
  )

  const readEntities = useCallback(
    (ids: string[]) => api.readEntities(ids),
    [],
  )

  const saveUser = async () => {
    await api.setUser(userInput.trim() || 'anonymous')
    setUserState(userInput.trim() || 'anonymous')
    setEditingUser(false)
  }

  const TABS: { id: Tab; label: string }[] = [
    { id: 'sources', label: 'Sources' },
    { id: 'query',   label: 'Query' },
    { id: 'events',  label: 'Events' },
  ]

  return (
    <div className="min-h-screen flex flex-col">
      {/* Header */}
      <header className="bg-white border-b border-gray-200 px-6 py-3 flex items-center justify-between shadow-xs">
        <div className="flex items-center gap-3">
          <span className="font-semibold text-gray-900 text-text-md">Entity Graph</span>
          <span className="badge badge-gray">{sources.length} source{sources.length !== 1 ? 's' : ''}</span>
        </div>
        <div className="flex items-center gap-2">
          {editingUser ? (
            <div className="flex items-center gap-2">
              <input
                className="input w-40 py-1.5"
                value={userInput}
                onChange={(e) => setUserInput(e.target.value)}
                onKeyDown={(e) => e.key === 'Enter' && saveUser()}
                autoFocus
              />
              <button className="btn-primary text-text-xs py-1.5" onClick={saveUser}>Save</button>
              <button className="btn-secondary text-text-xs py-1.5" onClick={() => setEditingUser(false)}>Cancel</button>
            </div>
          ) : (
            <button
              className="flex items-center gap-1.5 text-text-sm text-gray-600 hover:text-gray-900"
              onClick={() => { setUserInput(user); setEditingUser(true) }}
            >
              <span className="w-6 h-6 rounded-full bg-primary-100 text-primary-700 flex items-center justify-center text-text-xs font-medium">
                {user[0]?.toUpperCase()}
              </span>
              <span>{user}</span>
            </button>
          )}
        </div>
      </header>

      {/* Tab bar */}
      <nav className="bg-white border-b border-gray-200 px-6 flex gap-0">
        {TABS.map((t) => (
          <button
            key={t.id}
            onClick={() => setTab(t.id)}
            className={`
              px-4 py-3 text-text-sm font-medium border-b-2 -mb-px transition-colors
              ${tab === t.id
                ? 'border-primary-600 text-primary-700'
                : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'}
            `}
          >
            {t.label}
          </button>
        ))}
      </nav>

      {/* Content */}
      <main className="flex-1 p-6 max-w-3xl mx-auto w-full">
        {tab === 'sources' && (
          <SourceManager sources={sources} onAdd={addSource} onRemove={removeSource} />
        )}
        {tab === 'query' && (
          sources.length === 0
            ? <EmptyState message="Add a source first." onGoTo={() => setTab('sources')} />
            : <QueryExplorer onResolve={resolveQuery} onReadEntities={readEntities} />
        )}
        {tab === 'events' && (
          sources.length === 0
            ? <EmptyState message="Add a source first." onGoTo={() => setTab('sources')} />
            : <EventEditor onWrite={writeEvents} />
        )}
      </main>
    </div>
  )
}

function EmptyState({ message, onGoTo }: { message: string; onGoTo: () => void }) {
  return (
    <div className="card p-8 text-center space-y-3">
      <p className="text-text-sm text-gray-500">{message}</p>
      <button className="btn-primary mx-auto" onClick={onGoTo}>Go to Sources</button>
    </div>
  )
}
