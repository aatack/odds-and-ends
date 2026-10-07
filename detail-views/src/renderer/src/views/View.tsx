import { memo, useContext, useEffect, useRef } from 'react'
import type { ViewRow } from '../../../core/types.ts'
import type { ShownRow } from '../state.ts'
import { KindsContext, Status } from './primitives.tsx'
import { Composer } from './primitives.tsx'
import type { ViewProps } from './types.ts'

/**
 * Every view: its root's overview and its tree, or its type's `Detail` in
 * their place. The root is the first row and selectable like any other, so
 * Enter, `e` and links work on it too.
 */
export function ViewPane(props: ViewProps) {
  const kinds = useContext(KindsContext)!
  const { view } = props
  if (!view.root) {
    return (
      <div className="pane centred">
        {view.loading ? <span className="muted">Loading…</span> : <Status error={view.error} />}
      </div>
    )
  }
  const root = view.root
  const Detail = kinds[root.type].Detail
  if (Detail) return <Detail {...props} entity={root} />
  const Overview = kinds[root.type].Overview
  const action = view.actions.find((offered) => offered.id === props.acting)
  return (
    <div className="pane">
      <div className="list">
        {props.shown.rows.map((shown) =>
          shown.kind === 'input' ? (
            <EditRow key={shown.key} depth={shown.depth} props={props} />
          ) : shown.row.depth === 0 ? (
            <Selectable key={shown.key} shown={shown} className="overview" onSelect={props.interactive ? props.onSelect : undefined}>
              {shown.editing ? <EditBox props={props} /> : <Overview {...props} entity={root} />}
            </Selectable>
          ) : (
            <TreeRow key={shown.key} shown={shown} props={props} />
          ),
        )}
        {view.loading && view.rows.length <= 1 && <div className="loading">Loading…</div>}
      </div>
      <Status error={view.error} />
      {props.interactive && action && props.composing && (
        <Composer
          kind="action"
          placeholder={action.prompt}
          draft={props.draft}
          composing={props.composing}
          onDraft={props.onDraft}
          onCompose={props.onCompose}
        />
      )}
    </div>
  )
}

/** A row's frame: takes the cursor on a click, and keeps itself on screen while it holds it. */
function Selectable(props: { shown: Extract<ShownRow, { kind: 'entity' }>; className?: string; style?: React.CSSProperties; onSelect?(path: string[]): void; children: React.ReactNode }) {
  const ref = useRef<HTMLDivElement>(null)
  const { selected } = props.shown
  useEffect(() => {
    if (selected) ref.current?.scrollIntoView({ block: 'nearest' })
  }, [selected])
  const { onSelect } = props
  const path = props.shown.row.path
  return (
    <div
      ref={ref}
      className={`row${selected ? ' selected' : ''}${props.className ? ` ${props.className}` : ''}`}
      style={props.style}
      onMouseDown={onSelect ? () => onSelect(path) : undefined}
    >
      {props.children}
    </div>
  )
}

const indent = (depth: number): React.CSSProperties => ({ paddingLeft: 10 + (depth - 1) * 20 })

/** A child row: indented by depth, marked when it has children (open or folded), then its type's `Row`. */
const TreeRow = memo(function TreeRow(props: { shown: Extract<ShownRow, { kind: 'entity' }>; props: ViewProps }) {
  const kinds = useContext(KindsContext)!
  const { row } = props.shown
  const view = props.props
  const Row = kinds[row.entity.type].Row
  return (
    <Selectable
      shown={props.shown}
      className={`tree${rowClass(row)}`}
      style={indent(row.depth)}
      onSelect={view.interactive ? view.onSelect : undefined}
    >
      <span className="fold">{row.hasChildren ? (row.open ? '▾' : '▸') : ''}</span>
      <div className="cell">
        {props.shown.editing ? (
          <EditBox props={view} />
        ) : (
          <Row entity={row.entity} parent={row.parent} above={row.above} onOpen={view.onOpen} onImage={view.onImage} />
        )}
      </div>
    </Selectable>
  )
})

function rowClass(row: ViewRow): string {
  return row.entity.type === 'slack.message' && row.entity.data.quiet ? ' quiet' : ''
}

/** The box a new note is typed into, where it will appear. */
function EditRow(props: { depth: number; props: ViewProps }) {
  return (
    <div className="row tree selected" style={indent(props.depth)}>
      <span className="fold" />
      <div className="cell">
        <EditBox props={props.props} />
      </div>
    </div>
  )
}

/** Typing in place: Enter (or leaving it) writes, Escape gives up. */
function EditBox(props: { props: ViewProps }) {
  const { edit, onEditDraft, onCommitEdit } = props.props
  return (
    <input
      className="edit"
      autoFocus
      spellCheck
      value={edit?.draft ?? ''}
      placeholder={edit?.mode === 'create' ? 'Note' : ''}
      onChange={(event) => onEditDraft(event.target.value)}
      onBlur={() => onCommitEdit()}
      onMouseDown={(event) => event.stopPropagation()}
    />
  )
}
