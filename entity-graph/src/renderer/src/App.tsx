import React, { useEffect, useState } from 'react'
import { ChevronDown, Moon01, Sun } from '@untitledui/icons'
import { Servers } from './components/Servers'
import { SourceView } from './views/SourceView'
import { Activity, cancelledCount } from './components/Activity'
import { CallGuide } from './components/CallGuide'
import { CommandPalette } from './components/CommandPalette'
import { Badge } from './components/ui/Badge'
import { Button } from './components/ui/Button'
import { Dropdown, DropdownItem, DropdownSeparator } from './components/ui/Dropdown'
import { Input } from './components/ui/Input'
import { Toaster, showToast } from './components/ui/Toast'
import { useCalls, useTheme, useUi } from './state/hooks'
import { toggleTheme, updateUi } from './state/ui'
import { onCallSettled, openToolList, togglePalette } from './tools/call'
import { PALETTE_KEY, installKeyRouter } from './tools/dispatch'
import { keyHint } from './tools/keys'
import { useApp, type AppActions } from './views/useApp'

export default function App(): React.JSX.Element | null {
  const { ready, user, current, active, openError, actions } = useApp()
  const ui = useUi()
  const theme = useTheme()
  const calls = useCalls()
  const pendingResumable = cancelledCount(calls)

  // One keydown listener for the whole app. Keys act on global state and resolve
  // through the focus chain, not through whatever has DOM focus.
  useEffect(installKeyRouter, [])

  // Every call that finishes announces itself; errors and confirmations become
  // toasts. Nothing else in the app raises one.
  useEffect(
    () =>
      onCallSettled((call) => {
        if (call.outcome.kind === 'error') {
          showToast({ message: call.outcome.message, variant: 'error' })
        } else if (call.outcome.kind === 'success' && call.outcome.message) {
          showToast({ message: call.outcome.message, variant: 'success' })
        }
      }),
    [],
  )

  // Right-click is a general "run something here" gesture: it opens the tool list
  // at the cursor, seeded with the entity under it. Rows publish their ids as data
  // attributes, so any argument bound to `entityId`/`parentId` fills itself in.
  useEffect(() => {
    const onContextMenu = (e: MouseEvent): void => {
      e.preventDefault()
      const el = e.target instanceof HTMLElement ? e.target.closest('[data-entity-id]') : null
      const extra: Record<string, unknown> = {}
      const entityId = el?.getAttribute('data-entity-id')
      const parentId = el?.getAttribute('data-parent-id')
      if (entityId) extra.entityId = entityId
      if (parentId) extra.parentId = parentId
      openToolList({ anchor: { x: e.clientX, y: e.clientY }, extra })
    }
    window.addEventListener('contextmenu', onContextMenu)
    return () => window.removeEventListener('contextmenu', onContextMenu)
  }, [])

  if (!ready) return null

  return (
    <div className="flex h-screen flex-col overflow-hidden">
      {/* Both are popups on the same layer, so the palette is drawn second: a
          command started while the activity log is open belongs on top of it. */}
      <Activity open={ui.activityOpen} />
      <CommandPalette />
      {/* One corner stack: transient messages above the pending-call guide. */}
      <div className="pointer-events-none fixed bottom-4 right-4 z-50 flex w-72 flex-col gap-2">
        <Toaster />
        <CallGuide />
      </div>

      <header className="relative z-30 flex items-center gap-3 border-b border-gray-100 bg-white/80 px-6 py-3 backdrop-blur">
        <div className="flex min-w-0 items-center gap-3">
          <button
            className="text-[15px] font-semibold tracking-tightish text-gray-900 focus:outline-none"
            onClick={() => updateUi({ page: 'editor' })}
          >
            Entity Graph
          </button>
          {ui.page === 'editor' && current && (
            <>
              <Badge color="gray">source</Badge>
              <span className="truncate text-[13px] text-gray-500">{current.label}</span>
            </>
          )}
          {ui.page === 'sources' && <span className="text-[13px] text-gray-400">Sources</span>}
        </div>

        <div className="flex-1" />

        <Button variant="secondary" size="sm" onClick={togglePalette}>
          Actions
          <kbd className="ml-1 rounded bg-gray-200 px-1 text-[10px] text-gray-500">
            {keyHint([PALETTE_KEY])}
          </kbd>
        </Button>
        <Button
          variant="tertiary"
          size="sm"
          onClick={() => updateUi({ activityOpen: !ui.activityOpen })}
        >
          Activity
          {pendingResumable > 0 && (
            <span className="ml-1 rounded bg-gray-200 px-1 text-[10px] text-gray-500">
              {pendingResumable}
            </span>
          )}
        </Button>
        <Button
          variant="tertiary"
          size="sm"
          className="px-1.5"
          onClick={toggleTheme}
          aria-label={theme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode'}
          title={theme === 'dark' ? 'Switch to light mode' : 'Switch to dark mode'}
        >
          {theme === 'dark' ? <Sun size={16} /> : <Moon01 size={16} />}
        </Button>
        <ProfileMenu user={user} page={ui.page} actions={actions} />
      </header>

      <main className="min-h-0 flex-1">
        {ui.page === 'sources' ? (
          <div className="mx-auto w-full max-w-3xl p-6">
            <Servers current={current} onSelectSource={actions.selectSource} />
          </div>
        ) : active ? (
          <SourceView active={active} user={user} />
        ) : (
          <EditorPlaceholder openError={openError} hasCurrent={!!current} />
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
}: {
  openError: string | null
  hasCurrent: boolean
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
        <Button variant="secondary" size="sm" onClick={() => updateUi({ page: 'sources' })}>
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
  page: 'editor' | 'sources'
  actions: AppActions
}): React.JSX.Element {
  const [editing, setEditing] = useState(false)
  const [name, setName] = useState(user)

  const saveName = async (): Promise<void> => {
    await actions.setUser(name)
    setEditing(false)
  }

  return (
    <div className="shrink-0">
      <Dropdown
        align="right"
        trigger={({ toggle }) => (
          <button
            className="flex items-center gap-1.5 text-[13px] text-gray-600 hover:text-gray-900 focus:outline-none"
            onClick={toggle}
          >
            <span className="flex size-6 items-center justify-center rounded-full bg-brand-50 text-[11px] font-medium text-brand-700">
              {user[0]?.toUpperCase()}
            </span>
            <span>{user}</span>
            <ChevronDown size={14} className="text-gray-400" />
          </button>
        )}
      >
        {(close) => (
          <>
            <DropdownItem
              active={page === 'editor'}
              onClick={() => {
                updateUi({ page: 'editor' })
                close()
              }}
            >
              Editor
            </DropdownItem>
            <DropdownItem
              active={page === 'sources'}
              onClick={() => {
                updateUi({ page: 'sources' })
                close()
              }}
            >
              Sources
            </DropdownItem>

            <DropdownSeparator />

            <div className="px-3 py-1.5">
              {editing ? (
                <div className="flex items-center gap-1.5">
                  <Input
                    className="h-7"
                    value={name}
                    onChange={(e) => setName(e.target.value)}
                    onKeyDown={(e) => {
                      if (e.key === 'Enter') {
                        void saveName()
                        close()
                      }
                    }}
                    autoFocus
                  />
                  <Button
                    variant="primary"
                    size="sm"
                    onClick={() => {
                      void saveName()
                      close()
                    }}
                  >
                    Save
                  </Button>
                </div>
              ) : (
                <button
                  className="w-full text-left text-xs text-gray-500 hover:text-gray-900 focus:outline-none"
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
          </>
        )}
      </Dropdown>
    </div>
  )
}
