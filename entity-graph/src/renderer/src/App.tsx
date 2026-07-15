import React, { useCallback, useEffect, useState } from 'react'
import type { Connection, NewConnection } from '../../core/client'
import { ConnectionManager } from './components/ConnectionManager'
import { AdminPanel } from './components/AdminPanel'
import { SourceView } from './views/SourceView'

const api = window.entityGraph

export default function App(): React.JSX.Element | null {
  const [connections, setConnections] = useState<Connection[]>([])
  const [active, setActive] = useState<Connection | null>(null)
  const [user, setUser] = useState('anonymous')
  const [ready, setReady] = useState(false)

  const [editingUser, setEditingUser] = useState(false)
  const [userInput, setUserInput] = useState('')

  const refresh = useCallback(async () => {
    const [conns, act] = await Promise.all([api.listConnections(), api.getActiveConnection()])
    setConnections(conns)
    setActive(act)
  }, [])

  useEffect(() => {
    Promise.all([refresh(), api.getUser().then(setUser)]).finally(() => setReady(true))
  }, [refresh])

  const open = useCallback(
    async (id: string) => {
      await api.setActiveConnection(id)
      await refresh()
    },
    [refresh],
  )

  const disconnect = useCallback(async () => {
    await api.setActiveConnection(null)
    await refresh()
  }, [refresh])

  const addConnection = useCallback(
    async (cfg: NewConnection) => {
      await api.addConnection(cfg)
      await refresh()
    },
    [refresh],
  )

  const updateConnection = useCallback(
    async (id: string, patch: Partial<NewConnection>) => {
      await api.updateConnection(id, patch)
      await refresh()
    },
    [refresh],
  )

  const removeConnection = useCallback(
    async (id: string) => {
      await api.removeConnection(id)
      await refresh()
    },
    [refresh],
  )

  // Called by the admin panel after issuing a token: create + activate a source
  // connection so the user jumps straight into that source's tree.
  const connectToSource = useCallback(
    async (cfg: NewConnection) => {
      const id = await api.addConnection(cfg)
      await api.setActiveConnection(id)
      await refresh()
    },
    [refresh],
  )

  const saveUser = async () => {
    const name = userInput.trim() || 'anonymous'
    await api.setUser(name)
    setUser(name)
    setEditingUser(false)
  }

  if (!ready) return null

  return (
    <div className="min-h-screen flex flex-col">
      <header className="bg-white border-b border-gray-200 px-6 py-3 flex items-center justify-between shadow-xs">
        <div className="flex items-center gap-3 min-w-0">
          <span className="font-semibold text-gray-900 text-text-md">Entity Graph</span>
          {active && (
            <>
              <span className={`badge ${active.kind === 'admin' ? 'badge-blue' : 'badge-green'}`}>
                {active.kind}
              </span>
              <span className="text-text-sm text-gray-600 truncate">{active.label}</span>
              <button className="btn-secondary text-text-xs py-1" onClick={disconnect}>
                Switch connection
              </button>
            </>
          )}
        </div>
        <div className="flex items-center gap-2 shrink-0">
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

      <main className="flex-1 min-h-0">
        {!active && (
          <div className="p-6 max-w-3xl mx-auto w-full">
            <ConnectionManager
              connections={connections}
              onOpen={open}
              onAdd={addConnection}
              onUpdate={updateConnection}
              onRemove={removeConnection}
            />
          </div>
        )}
        {active?.kind === 'admin' && (
          <div className="p-6 max-w-3xl mx-auto w-full">
            <AdminPanel conn={active} onConnectToSource={connectToSource} />
          </div>
        )}
        {active?.kind === 'source' && <SourceView conn={active} user={user} />}
      </main>
    </div>
  )
}
