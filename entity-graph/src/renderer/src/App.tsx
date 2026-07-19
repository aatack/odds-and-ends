import React, { useCallback, useMemo, useState } from 'react'
import { ChevronDown, Moon01, Sun } from '@untitledui/icons'
import { Servers } from './components/Servers'
import { SourceView } from './views/SourceView'
import { CommandPalette, type Command } from './components/CommandPalette'
import { Badge } from './components/ui/Badge'
import { Button } from './components/ui/Button'
import { Dropdown, DropdownItem, DropdownSeparator } from './components/ui/Dropdown'
import { Input } from './components/ui/Input'
import { useTheme } from './helpers/useTheme'
import { useHotkeys } from './helpers/useHotkeys'
import { APP_ACTIONS, type AppController } from './actions/appActions'
import { hotkeyHint } from './actions/keys'
import { useApp, type AppActions, type Page } from './views/useApp'

export default function App(): React.JSX.Element | null {
  const { ready, user, page, current, active, openError, actions } = useApp()
  const { theme, toggle } = useTheme()
  const [paletteOpen, setPaletteOpen] = useState(false)
  // Editor actions register themselves here while a source is open, so the one
  // command palette lists them alongside the app-level commands.
  const [editorCommands, setEditorCommands] = useState<Command[]>([])
  const registerEditorCommands = useCallback(
    (commands: Command[] | null) => setEditorCommands(commands ?? []),
    [],
  )

  // The one imperative handle every app-level action dispatches through — the
  // top-level hotkeys, the palette, and the header buttons all go via this.
  const controller = useMemo<AppController>(
    () => ({
      togglePalette: () => setPaletteOpen((v) => !v),
      setPage: actions.setPage,
      toggleTheme: toggle,
    }),
    [actions, toggle],
  )
  // Every registered app action with a hotkey is bound here, at the top level.
  useHotkeys(APP_ACTIONS, controller)

  const appCommands = useMemo<Command[]>(
    () =>
      APP_ACTIONS.filter((a) => a.palette !== false).map((a) => ({
        id: `app.${a.id}`,
        label: a.label,
        aliases: a.aliases,
        hint: hotkeyHint(a.keys) ?? a.hint,
        run: () => a.run(controller),
      })),
    [controller],
  )
  // Editor actions first — they're the most relevant while the tree is focused.
  const commands = useMemo(() => [...editorCommands, ...appCommands], [editorCommands, appCommands])
  const paletteHint = hotkeyHint(APP_ACTIONS.find((a) => a.id === 'toggle-palette')?.keys)

  if (!ready) return null

  return (
    <div className="flex h-screen flex-col overflow-hidden">
      <CommandPalette open={paletteOpen} commands={commands} onClose={() => setPaletteOpen(false)} />
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

        <Button variant="secondary" size="sm" onClick={controller.togglePalette}>
          Actions
          {paletteHint && (
            <kbd className="ml-1 rounded bg-gray-200 px-1 text-[10px] text-gray-500">{paletteHint}</kbd>
          )}
        </Button>
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
          <SourceView active={active} user={user} onRegisterCommands={registerEditorCommands} />
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
                actions.setPage('editor')
                close()
              }}
            >
              Editor
            </DropdownItem>
            <DropdownItem
              active={page === 'sources'}
              onClick={() => {
                actions.setPage('sources')
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
