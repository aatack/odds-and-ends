/**
 * The three panes, the one key listener, and nothing else.
 *
 * Navigator, builder, preview. Every gesture that isn't direct manipulation of
 * the canvas goes through the command registry, so the buttons here and the
 * hotkeys are the same thing said twice.
 */

import { useCallback, useEffect, useMemo, useRef, useState } from 'react'
import { COMMANDS, dispatchKey, type CommandContext, type ViewHandles } from './commands'
import { Builder } from './components/Builder'
import { Navigator, type NavigatorHandle } from './components/Navigator'
import { Preview, type PreviewHandle } from './components/Preview'
import { Button, cn } from './components/ui'
import { useActions, useApi, useAppState, useEvaluation, usePreviewScene } from './hooks'
import { openModel, previewedNodes } from './state/derive'

export function App() {
  const state = useAppState()
  const actions = useActions()
  const api = useApi()
  const evaluation = useEvaluation(state)
  const scene = usePreviewScene(state, evaluation)
  const preview = useRef<PreviewHandle>(null)
  const navigator = useRef<NavigatorHandle>(null)

  const [navWidth, setNavWidth] = useState(224)
  const [previewWidth, setPreviewWidth] = useState(420)

  const views = useMemo<ViewHandles>(
    () => ({
      frameCamera: () => preview.current?.frame(),
      focusSearch: () => navigator.current?.focusSearch(),
    }),
    [],
  )

  // The listener is bound once; the context it dispatches with is read fresh.
  const context = useRef<CommandContext>({ state, actions, api, views })
  context.current = { state, actions, api, views }

  useEffect(() => {
    const onKeyDown = (event: KeyboardEvent): void => {
      if (dispatchKey(event, context.current)) event.preventDefault()
    }
    window.addEventListener('keydown', onKeyDown)
    return () => window.removeEventListener('keydown', onKeyDown)
  }, [])

  const run = useCallback((id: string) => {
    const command = COMMANDS.find((candidate) => candidate.id === id)
    if (command) void command.run(context.current)
  }, [])

  const model = openModel(state)
  const shown = previewedNodes(state).length
  const errors = evaluation.errors.size

  return (
    <div className="flex h-full flex-col bg-line">
      <header className="flex h-10 shrink-0 items-center gap-3 bg-panel px-3">
        <span className="text-[13px] font-medium">{model?.name ?? '3D modelling'}</span>
        <span className="text-[11px] text-faint">
          {model
            ? `${shown} of ${Object.keys(model.nodes).length} shown · ${scene.triangles.length} triangles`
            : 'no model open'}
          {errors > 0 && ` · ${errors} in error`}
        </span>
        <div className="grow" />
        <Button variant="quiet" size="sm" onClick={() => run('preview.frame')}>
          Frame <Key>F</Key>
        </Button>
        <Button variant="plain" size="sm" onClick={() => run('model.export')}>
          Export glTF <Key>⌘E</Key>
        </Button>
      </header>

      <div className="flex min-h-0 grow gap-px">
        <div style={{ width: navWidth }} className="shrink-0">
          <Navigator ref={navigator} />
        </div>
        <Splitter onMove={(dx) => setNavWidth((w) => clamp(w + dx, 170, 400))} />

        <div className="min-w-0 grow bg-sunken">
          <Builder evaluation={evaluation} />
        </div>
        <Splitter onMove={(dx) => setPreviewWidth((w) => clamp(w - dx, 260, 800))} />

        <div style={{ width: previewWidth }} className="shrink-0 bg-panel">
          <Preview ref={preview} scene={scene} frameOn={state.openModelId} />
        </div>
      </div>

      {state.notice && (
        <div className="flex h-8 shrink-0 items-center gap-2 bg-panel px-3 text-[11px] text-muted">
          <span className="grow truncate">{state.notice.text}</span>
          {state.notice.path && api && (
            <>
              <Button size="sm" variant="quiet" onClick={() => api.openFile(state.notice!.path!)}>
                Open
              </Button>
              <Button size="sm" variant="quiet" onClick={() => api.revealFile(state.notice!.path!)}>
                Show
              </Button>
            </>
          )}
          <Button size="sm" variant="quiet" onClick={() => actions.notify(null)}>
            Dismiss
          </Button>
        </div>
      )}
    </div>
  )
}

const clamp = (value: number, low: number, high: number): number =>
  Math.min(high, Math.max(low, value))

function Key({ children }: { children: string }) {
  return <span className="text-[10px] text-faint">{children}</span>
}

/** A hairline you can drag. */
function Splitter({ onMove }: { onMove: (dx: number) => void }) {
  const [dragging, setDragging] = useState(false)
  return (
    <div
      onPointerDown={(event) => {
        event.currentTarget.setPointerCapture(event.pointerId)
        setDragging(true)
      }}
      onPointerMove={(event) => {
        if (dragging) onMove(event.movementX)
      }}
      onPointerUp={(event) => {
        event.currentTarget.releasePointerCapture(event.pointerId)
        setDragging(false)
      }}
      className={cn('w-px shrink-0 cursor-col-resize', dragging ? 'bg-brand-400' : 'bg-line')}
      style={{ boxShadow: '0 0 0 2px transparent' }}
    />
  )
}
