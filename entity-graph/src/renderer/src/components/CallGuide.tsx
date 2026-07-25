import React from 'react'
import { Maximize01, X } from '@untitledui/icons'
import { usePendingCall } from '../state/hooks'
import { cancelCall, maximisePending } from '../tools/call'
import { keyHint } from '../tools/keys'
import { findTool } from '../tools/registry'
import { argsOf } from '../tools/types'

/**
 * The corner guide for a call started by a hotkey that still needs something —
 * a link waiting for its far end, say. Deliberately not an editor: it names what
 * is outstanding and offers to expand into the palette if more control is wanted.
 */
export function CallGuide(): React.JSX.Element | null {
  const pending = usePendingCall()
  if (!pending || pending.display.kind !== 'hidden' || !pending.toolId) return null
  const tool = findTool(pending.toolId)
  if (!tool) return null
  const arg = argsOf(tool).find((a) => a.name === pending.activeArg)
  const again = keyHint(tool.keys)

  const waiting = !arg
    ? 'Waiting…'
    : arg.pick
      ? `Select an entity, then press ${again ?? 'the same key'} again`
      : `Waiting for ${arg.label.toLowerCase()}`

  return (
    // Unbordered, on the toast's shadow: the two stack in the same corner and
    // should read as one family.
    <div className="pointer-events-auto rounded-lg bg-brand-50 px-3 py-2 shadow-lg">
      <div className="flex items-start gap-2">
        <div className="min-w-0 flex-1">
          <div className="text-[13px] font-medium text-brand-700">{tool.label}</div>
          <div className="mt-0.5 text-xs text-brand-700/70">{waiting}</div>
        </div>
        <button
          className="text-brand-700/50 hover:text-brand-700 focus:outline-none"
          onClick={maximisePending}
          title="Expand into the command palette"
          aria-label="Expand"
        >
          <Maximize01 size={13} />
        </button>
        <button
          className="text-brand-700/50 hover:text-brand-700 focus:outline-none"
          onClick={cancelCall}
          title="Cancel (Esc)"
          aria-label="Cancel"
        >
          <X size={13} />
        </button>
      </div>
    </div>
  )
}
