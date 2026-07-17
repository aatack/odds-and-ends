import React, { useState } from 'react'
import { ChevronDown, Moon01, Sun } from '@untitledui/icons'
import { Servers } from './components/Servers'
import { SourceView } from './views/SourceView'
import { Badge } from './components/ui/Badge'
import { Button } from './components/ui/Button'
import { Input } from './components/ui/Input'
import { useTheme } from './helpers/useTheme'
import { useApp, type AppActions, type Page } from './views/useApp'

export default function App(): React.JSX.Element | null {
  const { ready, user, page, current, active, openError, actions } = useApp()
  const { theme, toggle } = useTheme()

  if (!ready) return null

  return (
    <div className="flex h-screen flex-col overflow-hidden">
      <header className="relative z-30 flex items-center gap-3 border-b border-gray-100 bg-white/80 px-6 py-3 backdrop-blur">
        <div className="flex min-w-0 items-center gap-3">
          <button
            className="text-[15px] font-semibold tracking-tightish text-gray-900 focus:outline-none"
            onClick={() => actions.setPage('editor')}
          >
            Entity Graph
          </button>
          {page === 'editor' && current && (
            <>
              <Badge color="gray">source</Badge>
              <span className="truncate text-[13px] text-gray-500">{current.label}</span>
            </>
          )}
          {page === 'sources' && <span className="text-[13px] text-gray-400">Sources</span>}
        </div>

        <div className="flex-1" />

        <Button
          variant="tertiary"
          size="sm"
          className="px-1.5"
          onClick={toggle}
          aria-label={theme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode'}
          title={theme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode'}
        >
          {theme === 'dark' ? <Sun size={16} /> : <Moon01 size={16} />}
        </Button>
        <ProfileMenu user={user} page={page} actions={actions} />
      </header>

      <main className="min-h-0 flex-1">
        {page === 'sources' ? (
          <div className="mx-auto w-full max-w-3xl p-6">
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
    <div className="mx-auto mt-24 w-full max-w-md space-y-4 px-6 text-center">
      {openError ? (
        <>
          <p className="text-[13px] text-error-600">Couldn’t open the current source.</p>
          <p className="break-all font-mono text-xs text-gray-400">{openError}</p>
        </>
      ) : (
        <p className="text-[13px] text-gray-400">
          {hasCurrent ? 'Opening source…' : 'No source selected yet.'}
        </p>
      )}
      <div className="flex justify-center">
        <Button variant="secondary" size="sm" onClick={onOpenSources}>
          Go to sources
        </Button>
      </div>
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
        className="flex items-center gap-1.5 text-[13px] text-gray-600 transition-colors hover:text-gray-900 focus:outline-none"
        onClick={() => setOpen((v) => !v)}
      >
        <span className="flex size-6 items-center justify-center rounded-full bg-brand-50 text-[11px] font-medium text-brand-700">
          {user[0]?.toUpperCase()}
        </span>
        <span>{user}</span>
        <ChevronDown size={14} className="text-gray-400" />
      </button>

      {open && (
        <>
          {/* Click-away backdrop */}
          <div className="fixed inset-0 z-40" onClick={close} />
          <div className="absolute right-0 z-50 mt-2 w-56 rounded-lg bg-white py-1.5 shadow-lg animate-fade-in">
            <MenuItem label="Editor" active={page === 'editor'} onClick={() => go('editor')} />
            <MenuItem label="Sources" active={page === 'sources'} onClick={() => go('sources')} />

            <div className="my-1.5 h-px bg-gray-100" />

            <div className="px-3 py-1.5">
              {editing ? (
                <div className="flex items-center gap-1.5">
                  <Input
                    className="h-7"
                    value={name}
                    onChange={(e) => setName(e.target.value)}
                    onKeyDown={(e) => e.key === 'Enter' && saveName()}
                    autoFocus
                  />
                  <Button variant="primary" size="sm" onClick={saveName}>
                    Save
                  </Button>
                </div>
              ) : (
                <button
                  className="w-full text-left text-xs text-gray-500 transition-colors hover:text-gray-900 focus:outline-none"
                  onClick={() => {
                    setName(user)
                    setEditing(true)
                  }}
                >
                  Signed in as <span className="font-medium text-gray-700">{user}</span>
                  <span className="text-brand-600"> · rename</span>
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
      className={`w-full px-3 py-1.5 text-left text-[13px] transition-colors hover:bg-gray-100/70 focus:outline-none ${
        active ? 'font-medium text-brand-700' : 'text-gray-700'
      }`}
      onClick={onClick}
    >
      {label}
    </button>
  )
}
