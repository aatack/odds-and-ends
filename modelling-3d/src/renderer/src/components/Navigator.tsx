/**
 * The left pane: the models, and the vocabulary they are built from.
 *
 * A model is both a thing you open and a transform you can drop into another
 * model, which is why they sit in the same list as the built-ins rather than
 * somewhere separate.
 */

import { forwardRef, useImperativeHandle, useMemo, useRef, useState } from 'react'
import { useActions, useAppState } from '../hooks'
import { modelList, paletteGroups, searchGroups } from '../state/derive'
import { Button, Empty, PanelHeader, TextField, cn } from './ui'

export interface NavigatorHandle {
  /** Put the cursor in the search box, whatever else had it. */
  focusSearch(): void
}

export const Navigator = forwardRef<NavigatorHandle>(function Navigator(_props, ref) {
  const state = useAppState()
  const actions = useActions()
  const [query, setQuery] = useState('')
  const [renaming, setRenaming] = useState<string | null>(null)
  // Deleting a model takes every node standing for it with it, so it asks once.
  const [confirming, setConfirming] = useState<string | null>(null)
  const search = useRef<HTMLInputElement>(null)

  useImperativeHandle(ref, () => ({
    focusSearch: () => {
      search.current?.focus()
      search.current?.select()
    },
  }))

  const models = modelList(state)
  const groups = useMemo(() => searchGroups(paletteGroups(), query), [query])
  // While searching, a folded section still shows what it matched — otherwise
  // the search would quietly miss things.
  const searching = query.trim() !== ''

  return (
    <div className="flex h-full min-h-0 flex-col bg-panel">
      <PanelHeader>
        <span className="grow">Navigator</span>
        <Button size="sm" variant="quiet" onClick={() => setRenaming(actions.createModel())}>
          New model
        </Button>
      </PanelHeader>

      <div className="min-h-0 grow overflow-y-auto pb-4">
        <div className="px-2 pb-2">
          {models.length === 0 && <Empty>Nothing here yet.</Empty>}
          {models.map((model) => {
            const open = model.id === state.openModelId
            return (
              <div
                key={model.id}
                draggable={!open}
                onDragStart={(event) => {
                  event.dataTransfer.setData('application/transform', `model:${model.id}`)
                  event.dataTransfer.effectAllowed = 'copy'
                }}
                onClick={() => {
                  actions.openModel(model.id)
                  setConfirming(null)
                }}
                onDoubleClick={() => setRenaming(model.id)}
                className={cn(
                  'group flex items-center gap-1 rounded-md px-2 py-1',
                  open ? 'bg-brand-50 text-brand-700' : 'hover:bg-sunken',
                )}
              >
                {renaming === model.id ? (
                  <TextField
                    autoFocus
                    defaultValue={model.name}
                    onBlur={(event) => {
                      actions.renameModel(model.id, event.target.value.trim() || model.name)
                      setRenaming(null)
                    }}
                    onKeyDown={(event) => {
                      if (event.key === 'Enter') event.currentTarget.blur()
                      if (event.key === 'Escape') setRenaming(null)
                    }}
                  />
                ) : (
                  <>
                    <span className="grow truncate text-[13px] font-medium">{model.name}</span>
                    <span className="shrink-0 text-[10px] text-faint">
                      {Object.keys(model.nodes).length}
                    </span>
                    <button
                      onClick={(event) => {
                        event.stopPropagation()
                        if (confirming === model.id) actions.deleteModel(model.id)
                        else setConfirming(model.id)
                      }}
                      onBlur={() => setConfirming(null)}
                      title="Delete this model, and every node standing for it"
                      className={cn(
                        'shrink-0 px-1 text-[11px]',
                        confirming === model.id
                          ? 'block text-danger'
                          : 'hidden text-faint group-hover:block hover:text-danger',
                      )}
                    >
                      {confirming === model.id ? 'delete?' : '×'}
                    </button>
                  </>
                )}
              </div>
            )
          })}
        </div>

        <div className="sticky top-0 z-10 bg-panel px-2 pb-2">
          <TextField
            ref={search}
            placeholder="Search transforms"
            value={query}
            onChange={(event) => setQuery(event.target.value)}
            onKeyDown={(event) => {
              if (event.key === 'Escape') {
                setQuery('')
                event.currentTarget.blur()
              }
            }}
          />
        </div>

        {groups.map((group) => {
          const folded = !searching && state.collapsed.includes(group.category)
          return (
            <div key={group.category} className="px-2 pt-1">
              <button
                onClick={() => actions.toggleCategory(group.category)}
                className="flex w-full items-center gap-1 px-1 py-1 text-left text-[10px] font-medium tracking-wide text-faint uppercase hover:text-muted"
              >
                <span className="w-2 shrink-0 text-[8px]">{folded ? '▸' : '▾'}</span>
                <span className="grow truncate">{group.category}</span>
                {folded && <span className="shrink-0 tabular-nums">{group.items.length}</span>}
              </button>
              {!folded &&
                group.items.map((item) => (
                  <div
                    key={item.transform}
                    draggable
                    onDragStart={(event) => {
                      event.dataTransfer.setData('application/transform', item.transform)
                      event.dataTransfer.effectAllowed = 'copy'
                    }}
                    title={item.summary}
                    className="rounded-md px-2 py-1 hover:bg-sunken"
                  >
                    <div className="truncate text-[12px]">{item.label}</div>
                  </div>
                ))}
            </div>
          )
        })}
        {groups.length === 0 && <Empty>Nothing matches “{query}”.</Empty>}
      </div>

      <div className="shrink-0 px-3 py-2 text-[10px] leading-relaxed text-faint">
        Drag a transform onto the canvas, or right-click the canvas to search for
        one. Drop an input handle on empty space to make a constant.
      </div>
    </div>
  )
})
