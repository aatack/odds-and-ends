import React from 'react'
import { Button } from '../components/ui/Button'
import { useConnection, useSheet, useTheme } from '../state/hooks'
import { closeSheet, type Sheet } from '../state/ui'
import { ActionSheet } from './ActionSheet'
import { ArgSheet } from './ArgSheet'
import { BottomBar } from './BottomBar'
import { Connect } from './Connect'
import { CrumbSheet } from './CrumbSheet'
import { Header } from './Header'
import { Outline } from './Outline'
import { SettingsSheet } from './SettingsSheet'
import { Toasts } from './Toasts'
import { useBackButton } from './useBackButton'
import { useOutline } from './useOutline'

// The shell: a header, the outline, and the bar under the thumb. One screen, because
// a phone is one screen — the desktop app's tab groups, tabs and side-by-side frames
// all collapse into the navigation stack the header shows.

export function App(): React.JSX.Element {
  useTheme()
  const connection = useConnection()
  // Split so the hooks below only ever run with a source open: a component that
  // called them conditionally would break the hook order the moment one is set up.
  return connection ? <Shell /> : <Connect />
}

function Shell(): React.JSX.Element {
  const outline = useOutline()
  const sheet = useSheet()
  useBackButton()

  return (
    <div className="flex h-full flex-col bg-white">
      <Header />
      {sheet?.kind === 'pick' && <PickBanner prompt={sheet.prompt} />}
      <main
        className="min-h-0 flex-1 overflow-y-auto overscroll-contain"
        // Room for the bar, and then some: appending at the bottom of a list
        // shouldn't leave the row you are typing into jammed against it.
        style={{ paddingBottom: 'calc(7rem + var(--inset-bottom))' }}
      >
        <Outline {...outline} />
      </main>
      <BottomBar />
      <Sheets sheet={sheet} />
      <Toasts />
    </div>
  )
}

/**
 * The banner shown while a tool waits for a row to be tapped — a move's destination,
 * a link's far end. The outline stays live underneath, which is the whole point: the
 * thing being pointed at may be several levels away, and navigating to it must not
 * cancel the call.
 */
function PickBanner({ prompt }: { prompt: string }): React.JSX.Element {
  return (
    <div className="flex shrink-0 items-center gap-2 bg-brand-50 px-3 py-2">
      <span className="min-w-0 flex-1 text-[13.5px] text-brand-700">{prompt}</span>
      <Button tone="quiet" className="min-h-9 px-3 text-[13px]" onClick={closeSheet}>
        Cancel
      </Button>
    </div>
  )
}

/** One sheet at a time, by kind. `pick` has no sheet — it has the banner above. */
function Sheets({ sheet }: { sheet: Sheet | null }): React.JSX.Element | null {
  if (!sheet) return null
  switch (sheet.kind) {
    case 'actions':
      return <ActionSheet />
    case 'crumbs':
      return <CrumbSheet />
    case 'settings':
      return <SettingsSheet />
    case 'args':
      return <ArgSheet toolId={sheet.toolId} args={sheet.args} />
    case 'pick':
      return null
  }
}
