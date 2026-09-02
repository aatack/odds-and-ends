/**
 * One node on the canvas: a transform, its labelled sockets, and an editor for
 * every input nothing is wired into.
 *
 * Dumb: everything it draws arrives in `data`, and every gesture goes back out
 * as a callback. The builder is what turns the model into these.
 */

import { Handle, Position, type NodeProps } from '@xyflow/react'
import type { ValueType } from '@core/values'
import { TYPE_LABELS } from '@core/values'
import { ValueEditor } from './editors'
import { cn } from './ui'

export interface SocketView {
  name: string
  label: string
  type: ValueType
  /** Inputs only: something is wired in, so there is nothing to edit. */
  connected?: boolean
  literal?: unknown
  /** Outputs only: what came out, in a few words. */
  summary?: string
}

export interface NodeView extends Record<string, unknown> {
  label: string
  /** A word above the title: the category, or what kind of port this is. */
  kind: string
  accent: boolean
  error?: string
  inputs: SocketView[]
  outputs: SocketView[]
  params: SocketView[]
  onValue: (key: string, value: unknown) => void
  onOpen?: () => void
}

const TYPE_TONE: Record<ValueType, string> = {
  number: '#8b93e6',
  text: '#9a9da6',
  vec2: '#5fa8a0',
  vec3: '#4f8f86',
  colour: '#c08a5a',
  path2: '#6169d9',
  path3: '#4c53c4',
  mesh: '#7a7f8a',
}

const wide = (sockets: SocketView[]): boolean =>
  sockets.some((socket) => !socket.connected && (socket.type === 'path2' || socket.type === 'vec3'))

export function TransformNode({ data, selected }: NodeProps & { data: NodeView }) {
  const width = wide([...data.inputs, ...data.params]) ? 196 : 168

  return (
    <div
      style={{ width }}
      className={cn(
        'rounded-lg bg-panel py-1.5 shadow-xs',
        selected ? 'ring-2 ring-brand-500' : 'ring-1 ring-line',
        data.error && 'ring-1 ring-danger/50',
      )}
    >
      <div className="px-2.5 pb-1">
        <div className="text-[9px] font-medium tracking-wide text-faint uppercase">{data.kind}</div>
        <div className="flex items-baseline justify-between gap-1">
          <span className={cn('truncate text-[13px] font-medium', data.accent && 'text-brand-600')}>
            {data.label}
          </span>
          {data.onOpen && (
            <button
              onClick={data.onOpen}
              className="shrink-0 text-[10px] text-faint hover:text-brand-600"
            >
              open
            </button>
          )}
        </div>
      </div>

      {data.params.map((param) => (
        <div key={param.name} className="px-2.5 py-0.5">
          <ValueEditor
            type={param.type}
            value={param.literal}
            onChange={(value) => data.onValue(param.name, value)}
          />
        </div>
      ))}

      {data.inputs.map((socket) => (
        <div key={socket.name} className="relative px-2.5 py-[3px]">
          <Handle
            type="target"
            position={Position.Left}
            id={socket.name}
            style={{ left: -5, top: 11, background: '#fff', borderColor: TYPE_TONE[socket.type] }}
            title={TYPE_LABELS[socket.type]}
          />
          <div className="flex items-center justify-between gap-2">
            <span className="truncate text-[11px] text-muted">{socket.label}</span>
            {socket.connected && (
              <span className="shrink-0 text-[10px] text-faint">{TYPE_LABELS[socket.type]}</span>
            )}
          </div>
          {!socket.connected && (
            <div className="pt-0.5">
              <ValueEditor
                type={socket.type}
                value={socket.literal}
                onChange={(value) => data.onValue(socket.name, value)}
              />
            </div>
          )}
        </div>
      ))}

      {data.outputs.map((socket) => (
        <div key={socket.name} className="relative px-2.5 py-[3px]">
          <Handle
            type="source"
            position={Position.Right}
            id={socket.name}
            style={{ right: -5, top: 11, background: '#fff', borderColor: TYPE_TONE[socket.type] }}
            title={TYPE_LABELS[socket.type]}
          />
          <div className="flex items-baseline justify-end gap-2">
            {socket.summary && (
              <span className="truncate text-[10px] text-faint">{socket.summary}</span>
            )}
            <span className="shrink-0 text-[11px] text-muted">{socket.label}</span>
          </div>
        </div>
      ))}

      {data.error && (
        <div className="mt-1 px-2.5 pt-1 text-[10px] leading-tight text-danger">{data.error}</div>
      )}
    </div>
  )
}
