import { atom } from './atom'

// What the app is doing with each recording it has touched since it started.
// Runtime only, never persisted: a microphone does not survive a restart, and a
// recording the app is not listening to reads as paused, which is the truth.

export interface RecordingStatus {
  /** `connecting` until the microphone and the socket are both up. */
  state: 'connecting' | 'live' | 'paused'
  /** What is being said right now, for the pill. */
  hearing: string
  /** The id of the structuring call in flight, if there is one. */
  structuringCallId: string | null
  /** When the notes were last brought up to date. */
  structuredAt: number | null
  /** The last thing that went wrong, until something goes right. */
  error: string | null
}

export const IDLE_RECORDING: RecordingStatus = {
  state: 'paused',
  hearing: '',
  structuringCallId: null,
  structuredAt: null,
  error: null,
}

export const recordingsAtom = atom<Readonly<Record<string, RecordingStatus>>>({})

export const recordingStatus = (id: string): RecordingStatus =>
  recordingsAtom.get()[id] ?? IDLE_RECORDING

export function patchRecording(id: string, patch: Partial<RecordingStatus>): void {
  recordingsAtom.set((all) => ({ ...all, [id]: { ...(all[id] ?? IDLE_RECORDING), ...patch } }))
}
