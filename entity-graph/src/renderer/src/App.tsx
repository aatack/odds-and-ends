import React, { useCallback, useEffect, useState } from 'react'
import type { ActiveSource } from '../../core/client'
import { Servers } from './components/Servers'
import { SourceView } from './views/SourceView'

const api = window.entityGraph

export default function App(): React.JSX.Element | null {
  const [active, setActive] = useState<ActiveSource | null>(null)
  const [user, setUser] = useState('anonymous')
  const [ready, setReady] = useState(false)

  const [editingUser, setEditingUser] = useState(false)
  const [userInput, setUserInput] = useState('')

  useEffect(() => {
    api.getUser().then(setUser).finally(() => setReady(true))
  }, [])

  const closeSource = useCallback(async () => {
    if (active) await api.closeSource(active.id)
    setActive(null)
  }, [active])

  const saveUser = async (): Promise<void> => {
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
              <span className="badge badge-green">source</span>
              <span className="text-text-sm text-gray-600 truncate">{active.label}</span>
              <button className="btn-secondary text-text-xs py-1" onClick={closeSource}>
                Close source
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
        {active ? (
          <SourceView active={active} user={user} />
        ) : (
          <div className="p-6 max-w-3xl mx-auto w-full">
            <Servers onOpenSource={setActive} />
          </div>
        )}
      </main>
    </div>
  )
}
