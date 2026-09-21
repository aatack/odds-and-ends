import type { ReactNode } from 'react'
import { useConswap } from '../hooks'
import type { Session } from '../session'
import { Composer } from './Composer'
import { Feed } from './Feed'
import { Overlay } from './Overlay'
import { Queue } from './Queue'
import { Empty, Key } from './primitives'
import { Toasts } from './Toasts'
import { TopicHeader } from './TopicHeader'

/**
 * The whole screen. Every decision above this line has already been made in
 * `useConswap`; this only says where things go.
 */
export function App({ session }: { session: Session }): ReactNode {
  const model = useConswap(session)

  return (
    <div className="flex h-full w-full overflow-hidden bg-canvas text-ink">
      {model.state.sidebar && (
        <Queue
          queue={model.queue}
          status={model.status}
          focus={model.state.focus}
          onFocusTopic={model.onFocusTopic}
        />
      )}

      <main className="flex min-w-0 flex-1 flex-col">
        {model.detail ? (
          <>
            <TopicHeader
              detail={model.detail}
              details={model.state.details}
              onFocusTopic={model.onFocusTopic}
              onCancelBlocker={model.onCancelBlocker}
              onSetMetadata={model.onSetMetadata}
              onRunTool={model.onRunTool}
            />
            <Feed
              detail={model.detail}
              rows={model.rows}
              hidden={model.hidden}
              cursorKey={model.cursorKey}
              onSelectRow={model.onSelectRow}
              onToggleRow={model.onToggleRow}
              onFocusTopic={model.onFocusTopic}
              onPromote={model.onPromote}
              onShowEarlier={model.onShowEarlier}
            />
          </>
        ) : (
          <div className="flex min-h-0 flex-1 flex-col">
            <div className="drag h-10 shrink-0" />
            <Empty title={model.error ? 'The server is not answering' : 'Nothing focused'}>
              {model.error ? (
                model.error
              ) : (
                <>
                  Press <Key>.</Key> for the next thing that needs you, <Key>n</Key> to start something new, or{' '}
                  <Key>?</Key> to see every key.
                </>
              )}
            </Empty>
          </div>
        )}

        <Composer
          composer={model.state.composer}
          detail={model.detail}
          onComposerText={model.onComposerText}
          onComposerMode={model.onComposerMode}
          onSubmit={model.onSubmit}
          onCancel={model.onCancelComposer}
        />
      </main>

      <Overlay
        overlay={model.overlay}
        onQuery={model.onOverlayQuery}
        onIndex={model.onOverlayIndex}
        onChoose={model.onOverlayChoose}
        onClose={model.onCloseOverlay}
      />
      <Toasts toasts={model.state.toasts} onDismiss={model.onDismissToast} onRetry={model.onRetryToast} />
    </div>
  )
}
