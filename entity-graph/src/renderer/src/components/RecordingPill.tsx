import React from 'react'
import { Loading02, Microphone01, PauseCircle, PlayCircle } from '@untitledui/icons'
import { cn } from '../helpers/cn'
import { useAtomValue } from '../state/hooks'
import { IDLE_RECORDING, recordingsAtom } from '../state/recordings'
import { runTool } from '../tools/call'
import { DOT_COLORS } from './ui/Badge'

// A recording, drawn as what it is: a large pill with the one control a meeting
// needs within reach — pause, and resume — and what is being said right now.
//
// The state is the app's and not the entity's (`state/recordings`): whether the
// microphone is open is a fact about this window. So a recording read anywhere
// else, or after a restart, is simply paused until somebody presses play here.

export function RecordingPill({
  entityId,
  path,
}: {
  entityId: string
  path: string[]
}): React.JSX.Element {
  const status = useAtomValue(recordingsAtom)[entityId] ?? IDLE_RECORDING
  const live = status.state === 'live'
  const connecting = status.state === 'connecting'

  const said = status.error
    ? status.error
    : status.hearing ||
      (status.structuringCallId
        ? 'Bringing the notes up to date'
        : live
          ? 'Listening'
          : connecting
            ? 'Opening the microphone'
            : 'Paused')

  return (
    <div
      className="my-1 flex max-w-xl items-center gap-2 rounded-full bg-gray-100 py-1 pr-4 pl-1 font-sans select-none"
      // A press on the pill is a press on the recording, not on the row under it.
      onClick={(e) => e.stopPropagation()}
    >
      <button
        type="button"
        title={live || connecting ? 'Pause' : 'Resume'}
        disabled={connecting}
        className={cn(
          'flex size-9 shrink-0 items-center justify-center rounded-full bg-white shadow-xs',
          'text-gray-700 hover:text-gray-900 focus:outline-none focus-visible:ring-2 focus-visible:ring-brand-300',
        )}
        onClick={() => runTool('recording.toggle', { within: path })}
      >
        {connecting ? (
          <Loading02 size={18} className="text-gray-400" />
        ) : live ? (
          <PauseCircle size={20} />
        ) : (
          <PlayCircle size={20} />
        )}
      </button>
      <span
        className={cn('size-2 shrink-0 rounded-full', live ? DOT_COLORS.error : DOT_COLORS.gray)}
      />
      <Microphone01 size={14} className="shrink-0 text-gray-400" />
      <span
        className={cn(
          'min-w-0 flex-1 truncate text-[13px]',
          status.error ? 'text-error-600' : status.hearing ? 'text-gray-900' : 'text-gray-500',
        )}
        title={said}
      >
        {said}
      </span>
      {status.structuringCallId && status.hearing && (
        <Loading02 size={13} className="shrink-0 text-gray-400" />
      )}
    </div>
  )
}
