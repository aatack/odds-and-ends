import React, { useState } from 'react'
import { Servers } from './components/Servers'
import { SourceView } from './views/SourceView'
import { useApp, type AppActions, type Page } from './views/useApp'

export default function App(): React.JSX.Element | null {
  const { ready, user, page, current, active, openError, actions } = useApp()

  if (!ready) return null

  return (
    <div className="h-screen flex flex-col overflow-hidden">
      <header className="bg-white border-b border-gray-200 px-6 py-3 flex items-center justify-between shadow-xs">
        <div className="flex items-center gap-3 min-w-0">
          <button
            className="font-semibold text-gray-900 text-text-md hover:text-primary-700"
            onClick={() => actions.setPage('editor')}
          >
            Entity Graph
          </button>
          {page === 'editor' && current && (
            <>
              <span className="badge badge-green">source</span>
              <span className="text-text-sm text-gray-600 truncate">{current.label}</span>
            </>
          )}
          {page === 'sources' && <span className="text-text-sm text-gray-400">Sources</span>}
        </div>
        <ProfileMenu user={user} page={page} actions={actions} />
      </header>

      <main className="flex-1 min-h-0">
        {page === 'sources' ? (
          <div className="p-6 max-w-3xl mx-auto w-full">
            <Servers current={current} onSelectSource={actions.selectSource} />
          </div>
        ) : active ? (
          <SourceView active={active} user={user} />
        ) : (
          <EditorPlaceholder
            openError={openError}
            hasCurrent={!!current}
            onOpenSources={() => actions.setPage('sources')}
          />
        )}
      </main>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Empty / error state shown in the editor area when no source is open
// ---------------------------------------------------------------------------

function EditorPlaceholder({
  openError,
  hasCurrent,
  onOpenSources,
}: {
  openError: string | null
  hasCurrent: boolean
  onOpenSources: () => void
}): React.JSX.Element {
  return (
    <div className="p-6 max-w-md mx-auto w-full text-center space-y-3 mt-16">
      {openError ? (
        <>
          <p className="text-text-sm text-error-600">Couldn’t open the current source.</p>
          <p className="text-text-xs text-gray-400 font-mono break-all">{openError}</p>
        </>
      ) : (
        <p className="text-text-sm text-gray-500">
          {hasCurrent ? 'Opening source…' : 'No source selected yet.'}
        </p>
      )}
      <button className="btn-primary text-text-sm" onClick={onOpenSources}>
        Go to sources
      </button>
    </div>
  )
}

// ---------------------------------------------------------------------------
// Profile dropdown — nav between pages + rename
// ---------------------------------------------------------------------------

function ProfileMenu({
  user,
  page,
  actions,
}: {
  user: string
  page: Page
  actions: AppActions
}): React.JSX.Element {
  const [open, setOpen] = useState(false)
  const [editing, setEditing] = useState(false)
  const [name, setName] = useState(user)

  const close = (): void => {
    setOpen(false)
    setEditing(false)
  }

  const go = (p: Page): void => {
    actions.setPage(p)
    close()
  }

  const saveName = async (): Promise<void> => {
    await actions.setUser(name)
    setEditing(false)
  }

  return (
    <div className="relative shrink-0">
      <button
        className="flex items-center gap-1.5 text-text-sm text-gray-600 hover:text-gray-900"
        onClick={() => setOpen((v) => !v)}
      >
        <span className="w-6 h-6 rounded-full bg-primary-100 text-primary-700 flex items-center justify-center text-text-xs font-medium">
          {user[0]?.toUpperCase()}
        </span>
        <span>{user}</span>
        <ChevronIcon />
      </button>

      {open && (
        <>
          {/* Click-away backdrop */}
          <div className="fixed inset-0 z-40" onClick={close} />
          <div className="absolute right-0 mt-2 w-56 bg-white rounded-lg shadow-xl border border-gray-200 z-50 py-1.5">
            <MenuItem label="Editor" active={page === 'editor'} onClick={() => go('editor')} />
            <MenuItem label="Sources" active={page === 'sources'} onClick={() => go('sources')} />

            <div className="border-t border-gray-100 my-1.5" />

            <div className="px-3 py-1.5">
              {editing ? (
                <div className="flex items-center gap-1.5">
                  <input
                    className="input w-full py-1 text-text-xs"
                    value={name}
                    onChange={(e) => setName(e.target.value)}
                    onKeyDown={(e) => e.key === 'Enter' && saveName()}
                    autoFocus
                  />
                  <button className="btn-primary text-text-xs py-1 px-2" onClick={saveName}>
                    Save
                  </button>
                </div>
              ) : (
                <button
                  className="w-full text-left text-text-xs text-gray-500 hover:text-gray-900"
                  onClick={() => {
                    setName(user)
                    setEditing(true)
                  }}
                >
                  Signed in as <span className="font-medium text-gray-700">{user}</span>
                  <span className="text-primary-600"> · rename</span>
                </button>
              )}
            </div>
          </div>
        </>
      )}
    </div>
  )
}

function MenuItem({
  label,
  active,
  onClick,
}: {
  label: string
  active: boolean
  onClick: () => void
}): React.JSX.Element {
  return (
    <button
      className={`w-full text-left px-3 py-1.5 text-text-sm hover:bg-gray-50 ${
        active ? 'text-primary-700 font-medium' : 'text-gray-700'
      }`}
      onClick={onClick}
    >
      {label}
    </button>
  )
}

function ChevronIcon(): React.JSX.Element {
  return (
    <svg className="w-3.5 h-3.5" viewBox="0 0 20 20" fill="none" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" strokeLinejoin="round">
      <path d="M6 8l4 4 4-4" />
    </svg>
  )
}
