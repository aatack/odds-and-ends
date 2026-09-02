/**
 * The left pane: the models, and the vocabulary they are built from.
 *
 * A model is both a thing you open and a transform you can drop into another
 * model, which is why they sit in the same list as the built-ins rather than
 * somewhere separate.
 */

import { useMemo, useState } from 'react'
import { useActions, useAppState } from '../hooks'
import { modelList, paletteGroups, searchGroups } from '../state/derive'
import { Button, Empty, PanelHeader, TextField, cn } from './ui'

export function Navigator() {
  const state = useAppState()
  const actions = useActions()
  const [query, setQuery] = useState('')
  const [renaming, setRenaming] = useState<string | null>(null)

  const models = modelList(state)
  const groups = useMemo(() => searchGroups(paletteGroups(), query), [query])

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
                onClick={() => actions.openModel(model.id)}
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
                        actions.deleteModel(model.id)
                      }}
                      title="Delete this model, and anywhere it is used"
                      className="hidden shrink-0 px-1 text-[11px] text-faint group-hover:block hover:text-danger"
                    >
                      ×
                    </button>
                  </>
                )}
              </div>
            )
          })}
        </div>

        <div className="sticky top-0 z-10 bg-panel px-2 pb-2">
          <TextField
            placeholder="Search transforms"
            value={query}
            onChange={(event) => setQuery(event.target.value)}
          />
        </div>

        {groups.map((group) => (
          <div key={group.category} className="px-2 pt-1">
            <div className="px-1 py-1 text-[10px] font-medium tracking-wide text-faint uppercase">
              {group.category}
            </div>
            {group.items.map((item) => (
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
        ))}
        {groups.length === 0 && <Empty>Nothing matches “{query}”.</Empty>}
      </div>

      <div className="shrink-0 px-3 py-2 text-[10px] leading-relaxed text-faint">
        Drag a transform onto the canvas. Drop an input handle on empty space to
        make a constant.
      </div>
    </div>
  )
}
