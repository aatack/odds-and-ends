import React, { createContext, useContext, useEffect, useState } from 'react'
import { Handle, Position, type NodeProps, type Node } from '@xyflow/react'
import { Key01, PauseCircle, PlayCircle, Trash03 } from '@untitledui/icons'
import type { NodeStatus, SourceNode } from '../../../../core/client'
import { nodeKind, publishes } from '../../../../core/client'
import { cn } from '../../helpers/cn'
import { Badge } from '../ui/Badge'
import { CopyButton } from '../ui/CopyButton'
import { Field } from '../ui/Field'
import { IconButton } from '../ui/IconButton'
import { Input } from '../ui/Input'
import { Select } from '../ui/Select'
import type { SourceGraphActions } from '../../views/useSourceGraph'

// One node of the sources graph, drawn as a small card.
//
// What a node shows is what it holds: a SQLite node shows its path, a connection
// its URL, a broadcast the address to copy. Every field writes straight back
// through the actions — there is no save button, because there is nothing to
// batch: each value is its own column, and a node that is half-configured says
// so in its own problem line rather than refusing to exist.
//
// The pause button is in the header of all of them, since it means the same
// thing everywhere: switch this off, and let anything that calls through it be
// told so.

export interface NodeContext {
  actions: SourceGraphActions
  /** Every node, so a combiner can name its own inputs. */
  nodes: SourceNode[]
  /** Which nodes feed each node's input. */
  inputs: Record<string, string[]>
  status: Record<string, NodeStatus>
  /** Open the tokens panel for a published node. */
  openAccess: (nodeId: string) => void
}

const Context = createContext<NodeContext | null>(null)

export const NodeContextProvider = Context.Provider

const useNodeContext = (): NodeContext => {
  const ctx = useContext(Context)
  if (!ctx) throw new Error('a pensive node was drawn outside the sources page')
  return ctx
}

export interface PensiveNodeData extends Record<string, unknown> {
  node: SourceNode
}

export type PensiveFlowNode = Node<PensiveNodeData, 'pensive'>

/**
 * A field whose value is committed rather than written per keystroke: a path is
 * not a path until it is finished being typed, and each write rebuilds every
 * pensive downstream of the node.
 */
function DraftInput({
  value,
  onCommit,
  ...rest
}: {
  value: string
  onCommit: (next: string) => void
  mono?: boolean
} & Omit<React.InputHTMLAttributes<HTMLInputElement>, 'value' | 'onChange'>): React.JSX.Element {
  const [draft, setDraft] = useState(value)
  // Somebody else may have changed it — a node deleted upstream, a rebuild.
  useEffect(() => setDraft(value), [value])
  return (
    <Input
      {...rest}
      value={draft}
      onChange={(e) => setDraft(e.target.value)}
      onBlur={() => draft !== value && onCommit(draft)}
      onKeyDown={(e) => {
        if (e.key === 'Enter') e.currentTarget.blur()
        if (e.key === 'Escape') setDraft(value)
      }}
    />
  )
}

export function PensiveNode({ data, selected }: NodeProps<PensiveFlowNode>): React.JSX.Element {
  const { node } = data
  const { actions, nodes, inputs, status, openAccess } = useNodeContext()
  const info = nodeKind(node.config.kind)
  const state = status[node.id]
  const fixed = !info.addable

  return (
    <div
      className={cn(
        'w-64 rounded-lg bg-white shadow-xs',
        selected && 'ring-2 ring-brand-500/40',
        node.paused && 'bg-gray-50 opacity-60',
      )}
    >
      {info.inputs !== 0 && (
        <Handle type="target" position={Position.Left} className="!size-2 !border-0 !bg-gray-300" />
      )}
      {info.output && (
        <Handle type="source" position={Position.Right} className="!size-2 !border-0 !bg-gray-300" />
      )}

      <div className="flex items-center gap-1.5 px-3 py-2">
        <Badge color="gray">{info.label}</Badge>
        <div className="min-w-0 flex-1">
          {fixed ? (
            <p className="truncate text-[13px] text-gray-900">{node.label}</p>
          ) : (
            <DraftInput
              className="h-7 border-transparent bg-transparent px-1 shadow-none"
              value={node.label}
              onCommit={(label) => void actions.updateNode(node.id, { label })}
              placeholder="Name"
            />
          )}
        </div>
        <IconButton
          title={node.paused ? `Switch "${node.label}" back on` : `Switch "${node.label}" off`}
          onClick={() => void actions.updateNode(node.id, { paused: !node.paused })}
        >
          {node.paused ? <PlayCircle size={16} /> : <PauseCircle size={16} />}
        </IconButton>
        {!fixed && (
          <IconButton
            title={`Delete "${node.label}"`}
            onClick={() => void actions.removeNode(node.id)}
          >
            <Trash03 size={16} />
          </IconButton>
        )}
      </div>

      <div className="space-y-2 border-t border-gray-100 px-3 py-2">
        <Body
          node={node}
          nodes={nodes}
          inputs={inputs[node.id] ?? []}
          status={state}
          actions={actions}
          openAccess={openAccess}
        />
        {node.paused ? (
          <p className="text-xs text-gray-400">Switched off — calls through it are refused.</p>
        ) : (
          state?.problem && <p className="text-xs text-error-600">{state.problem}</p>
        )}
      </div>
    </div>
  )
}

function Body({
  node,
  nodes,
  inputs,
  status,
  actions,
  openAccess,
}: {
  node: SourceNode
  nodes: SourceNode[]
  inputs: string[]
  status: NodeStatus | undefined
  actions: SourceGraphActions
  openAccess: (nodeId: string) => void
}): React.JSX.Element {
  const config = node.config
  const named = (id: string): string => nodes.find((n) => n.id === id)?.label ?? id

  switch (config.kind) {
    case 'sqlite':
      return (
        <Field label="File">
          <DraftInput
            mono
            value={config.path}
            placeholder="~/notes/flow.db"
            onCommit={(path) =>
              void actions.updateNode(node.id, { config: { kind: 'sqlite', path } })
            }
          />
        </Field>
      )

    case 'connect':
      return (
        <>
          <Field label="URL">
            <DraftInput
              mono
              value={config.url}
              placeholder="http://192.168.1.20:7321"
              onCommit={(url) =>
                void actions.updateNode(node.id, { config: { ...config, url } })
              }
            />
          </Field>
          <Field label="Token">
            <DraftInput
              mono
              type="password"
              value={config.token}
              placeholder="bearer token"
              onCommit={(token) =>
                void actions.updateNode(node.id, { config: { ...config, token } })
              }
            />
          </Field>
        </>
      )

    case 'combined':
      return (
        <Field label="Edits go to">
          <Select
            value={config.writeTo ?? ''}
            onChange={(e) =>
              void actions.updateNode(node.id, {
                config: { kind: 'combined', writeTo: e.target.value || null },
              })
            }
          >
            <option value="">nowhere — read only</option>
            {inputs.map((id) => (
              <option key={id} value={id}>
                {named(id)}
              </option>
            ))}
          </Select>
        </Field>
      )

    case 'broadcast':
    case 'mcp':
      return (
        <>
          <div className="flex items-center gap-1">
            <p className="min-w-0 flex-1 truncate font-mono text-xs text-gray-500">
              {status?.url ?? `port ${config.port}`}
            </p>
            {status?.url && <CopyButton value={status.url} title="Copy the address" />}
            <IconButton title="Tokens" onClick={() => openAccess(node.id)}>
              <Key01 size={16} />
            </IconButton>
          </div>
          <p className="text-xs text-gray-400">
            {publishes(config.kind) && config.kind === 'mcp'
              ? 'An agent points at this, with a token, and gets the outline.'
              : 'Whoever holds a token can read and write through this.'}
          </p>
        </>
      )

    case 'desktop':
      return (
        <p className="text-xs text-gray-400">
          {inputs.length
            ? `Showing ${named(inputs[0])}.`
            : 'Nothing plugged in — the outliner has nothing to show.'}
        </p>
      )
  }
}
