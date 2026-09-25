import { v4 as uuid } from 'uuid'
import { RECORDING_ID, RECORDING_KEY } from '../../../core/builtins'
import { str } from '../../../core/entity'
import { startLiveTranscription, type LiveTranscription } from '../helpers/liveTranscription'
import { createEntity, readEntities, writeValue } from '../source/entity'
import * as A from '../state/actions'
import { patchRecording, recordingsAtom, recordingStatus } from '../state/recordings'
import { focusOf, getLayout } from '../state/store'
import { entityArg } from './entityTools'
import type { CallInfo, ToolSpec } from './types'

// Recordings: a meeting transcribed as it is spoken, and notes kept of it while
// it goes on. See docs/recordings.md.
//
// Two loops, joined by the store. One writes each sentence heard as an open line
// under the transcript. The other, every few seconds, hands the open lines to a
// Claude session that folds them into the notes over MCP, and ticks them off once
// it has. So the transcript's open lines are the queue, and nothing else is: a
// restart, a failed pass or a pause loses nothing that was heard.

/** How often the open lines are handed over. */
const STRUCTURE_EVERY_MS = 20_000

/**
 * Passes per session before a fresh one. A resumed session carries the whole
 * conversation so far as input on every turn, and the notes themselves are the
 * memory worth keeping — so a short conversation and a new one is cheaper than
 * one long one, and loses nothing the notes don't hold.
 */
const TURNS_PER_SESSION = 6

/** Fast enough to keep up, and cheap enough to run every twenty seconds. */
const MODEL = 'sonnet'

interface Halves {
  transcriptId: string
  notesId: string
}

/** What the app holds for a recording it has started or resumed since it opened. */
interface Running {
  halves: Halves
  /** The microphone and the socket, while live. */
  listening: LiveTranscription | null
  /** Lines are written in the order heard, one after the other. */
  writes: Promise<unknown>
  timer: ReturnType<typeof setInterval>
  /** Names this run's sessions, so a restart never resumes an old long one. */
  runId: string
  turns: number
  /** A pass is under way, from reading the lines to ticking them off. */
  passing: boolean
}

const running = new Map<string, Running>()

/** The recording's two halves, from the store. */
async function halvesOf(id: string): Promise<Halves> {
  const entity = (await readEntities([id]))[id]
  const held = entity?.values[RECORDING_KEY] as Partial<Halves> | undefined
  if (entity?.values.type !== RECORDING_ID || !held?.transcriptId || !held?.notesId) {
    throw new Error(`${str(entity?.values.text) ?? id} is not a recording`)
  }
  return { transcriptId: held.transcriptId, notesId: held.notesId }
}

/** What the app holds for a recording, made on first touch. Starts its loop. */
function track(id: string, halves: Halves): Running {
  const held = running.get(id)
  if (held) return held
  const run: Running = {
    halves,
    listening: null,
    writes: Promise.resolve(),
    timer: setInterval(() => void structure(id), STRUCTURE_EVERY_MS),
    runId: uuid(),
    turns: 0,
    passing: false,
  }
  running.set(id, run)
  return run
}

/** Start listening, and start the loop that structures what is heard. */
async function listen(id: string, halves: Halves): Promise<void> {
  const run = track(id, halves)
  if (run.listening) return
  patchRecording(id, { state: 'connecting', error: null })

  try {
    const key = await window.entityGraph.transcriptionKey()
    run.listening = await startLiveTranscription(key, {
      onSentence: (text) => {
        run.writes = run.writes
          .then(() => createEntity({ text, open: true }, halves.transcriptId))
          .catch((e) => patchRecording(id, { error: messageOf(e) }))
      },
      onHearing: (hearing) => patchRecording(id, { hearing }),
      onFailure: (error) => {
        run.listening = null
        patchRecording(id, { state: 'paused', hearing: '', error })
        void structure(id)
      },
    })
    patchRecording(id, { state: 'live' })
  } catch (e) {
    patchRecording(id, { state: 'paused', error: messageOf(e) })
    throw e
  }
}

/** Stop listening. The loop runs on until what was heard is in the notes. */
async function pause(id: string): Promise<void> {
  const run = running.get(id)
  if (!run?.listening) return
  const listening = run.listening
  run.listening = null
  await listening.stop()
  patchRecording(id, { state: 'paused', hearing: '' })
  await run.writes
  void structure(id)
}

/**
 * One pass: the transcript's open lines, folded into the notes. Skipped while a
 * pass is in flight — two sessions writing one set of notes would each undo the
 * other — and when nothing is waiting. The lines are ticked off only once the
 * session has answered, so a failed pass leaves them for the next.
 */
async function structure(id: string): Promise<void> {
  const run = running.get(id)
  if (!run || run.passing) return
  run.passing = true
  try {
    const { transcriptId, notesId } = run.halves
    const transcript = (await readEntities([transcriptId]))[transcriptId]
    const children = await readEntities(transcript?.outboundLinks ?? [])
    const lines = (transcript?.outboundLinks ?? [])
      .map((line) => children[line])
      .filter((line) => line && line.values.open === true && str(line.values.text))
    if (lines.length === 0) {
      // Nothing to do, and nothing will arrive while it isn't listening.
      stopLoop(id)
      return
    }

    const callId = uuid()
    patchRecording(id, { structuringCallId: callId })
    const first = run.turns % TURNS_PER_SESSION === 0
    const session = `${id}:${run.runId}:${Math.floor(run.turns / TURNS_PER_SESSION)}`
    // Imported here rather than at the top: the registry is built out of this
    // file, so a static import of the call machine is a cycle.
    const { callToolByName, contextWithin } = await import('./call')
    await callToolByName(
      'claude.runPrompt',
      [
        {
          $callId: callId,
          prompt: turnPrompt(id, notesId, lines.map((line) => str(line.values.text) ?? '')),
          sessionId: session,
          model: MODEL,
          ...(first ? { systemPrompt: systemPrompt(id, run.halves) } : {}),
        },
      ],
      contextWithin([id]),
    )
    run.turns++
    await Promise.all(lines.map((line) => writeValue(line.id, 'open', false)))
    patchRecording(id, { structuredAt: Date.now(), error: null })
  } catch (e) {
    patchRecording(id, { error: messageOf(e) })
  } finally {
    run.passing = false
    patchRecording(id, { structuringCallId: null })
  }
}

/** Put a recording down once it is paused and has nothing left to pass. */
function stopLoop(id: string): void {
  const run = running.get(id)
  if (!run || run.listening || recordingStatus(id).state !== 'paused') return
  clearInterval(run.timer)
  running.delete(id)
}

const messageOf = (e: unknown): string => (e instanceof Error ? e.message : String(e))

// --- What the session is told ----------------------------------------------

/**
 * The rules, once per session. Kept to ids and rules, since it goes in an
 * argument vector; the lines themselves go in the prompt.
 */
const systemPrompt = (id: string, { transcriptId, notesId }: Halves): string =>
  [
    'You keep the notes of a meeting while it happens. Every few seconds you are',
    'given the latest lines of its transcript, and you fold them into notes in a',
    'graph you can read and write over MCP. People read the notes during the',
    'meeting, as a visual aid, so keep them current and easy to scan.',
    '',
    `The notes are under entity \`${notesId}\`. The transcript is under`,
    `\`${transcriptId}\`, and both are children of the recording \`${id}\`. Do not`,
    'edit the transcript: the app ticks lines off once you have taken them. If a',
    'notes server answers these ids as empty, they are in another one.',
    '',
    'Rules for the notes:',
    '- One note per point, nested as child notes, never as markdown bullets.',
    '- Upstream for context, downstream for detail: a topic, then what was said',
    '  about it under it.',
    '- Record the current state, not the path to it. Revise notes as the',
    '  discussion moves: merge, reword, move and delete. Turn a topic that has',
    '  grown detailed into a section.',
    '- Not every word is worth a note. Skip small talk and repetition.',
    '- Anything somebody said should be done or answered later is a task',
    '  (`open: true`), so it can be come back to.',
    '- The people in the meeting may edit the notes too. Keep what they write,',
    '  and treat it as a sign of what matters to them.',
    '- If the notes get large, read them by section rather than whole.',
    '',
    'Nobody reads what you return. Answer "Done" when the notes are up to date.',
  ].join('\n')

const turnPrompt = (id: string, notesId: string, lines: string[]): string =>
  [
    `New lines from the transcript of recording \`${id}\`, oldest first:`,
    '',
    ...lines.map((line) => `> ${line}`),
    '',
    `Fold them into the notes under \`${notesId}\`, reading those as they are now first.`,
  ].join('\n')

// --- The tools ---------------------------------------------------------------

/**
 * The recording a call is about: the one named, when it is one — a press on a
 * recording's own pill, or the palette on its row — and otherwise the one the
 * app is listening to, so pausing works from anywhere.
 */
async function recordingOf(named: unknown): Promise<string> {
  const id = str(named)?.trim()
  if (id) {
    const entity = (await readEntities([id]))[id]
    if (entity?.values.type === RECORDING_ID) return id
  }
  const live = Object.entries(recordingsAtom.get()).filter(([, s]) => s.state !== 'paused')
  if (live.length === 1) return live[0][0]
  throw new Error(
    live.length ? 'More than one recording is live: run this on the one you mean' : 'Select a recording',
  )
}

/** The local time, as a recording's name says it. */
const stamp = (): string =>
  new Date().toLocaleString(undefined, { dateStyle: 'medium', timeStyle: 'short' })

export const RECORDING_TOOLS: ToolSpec[] = [
  {
    id: 'recording.start',
    label: 'Start a recording',
    aliases: ['record', 'transcribe', 'transcription', 'meeting', 'minutes', 'listen'],
    hint: 'Recording',
    scope: 'frame',
    reach: 'external',
    mutates: true,
    args: [entityArg('parentId', 'Under', 'entityId')],
    run: async ({ parentId }, call: CallInfo) => {
      const parent = str(parentId)?.trim() || str(call.context.values.rootId)
      if (!parent) throw new Error('Select where the recording goes')
      const id = await createEntity({ text: `Recording, ${stamp()}`, type: RECORDING_ID }, parent)
      const transcriptId = await createEntity({ text: 'Transcript' }, id)
      const notesId = await createEntity({ text: 'Notes', section: true }, id)
      const halves = { transcriptId, notesId }
      await writeValue(id, RECORDING_KEY, halves)
      // The transcript is the working, and long: folded, so the notes are what
      // is read while the meeting goes on.
      const tab = call.context.tabId ?? focusOf(getLayout()).tabId
      if (tab) A.setCollapsed(tab, transcriptId, true)
      await listen(id, halves)
      return { data: { entityId: id, ...halves }, message: 'Recording' }
    },
  },
  {
    // The big button on the pill, and the palette's way to it.
    id: 'recording.toggle',
    label: 'Pause or resume the recording',
    aliases: ['pause recording', 'resume recording', 'stop recording', 'record', 'mute'],
    hint: 'Recording',
    scope: 'frame',
    reach: 'external',
    args: [{ ...entityArg('recordingId', 'Recording'), optional: true }],
    run: async ({ recordingId }) => {
      const id = await recordingOf(recordingId)
      if (recordingStatus(id).state === 'paused') {
        await listen(id, await halvesOf(id))
        return { message: 'Recording' }
      }
      await pause(id)
      return { message: 'Paused' }
    },
  },
  {
    // Also run by itself every few seconds. By hand, for when the notes should
    // catch up now rather than at the next tick.
    id: 'recording.structure',
    label: 'Bring the recording’s notes up to date',
    aliases: ['structure', 'summarise recording', 'notes'],
    hint: 'Recording',
    scope: 'frame',
    reach: 'external',
    args: [{ ...entityArg('recordingId', 'Recording'), optional: true }],
    run: async ({ recordingId }) => {
      const id = await recordingOf(recordingId)
      // A recording nobody has touched since the app opened gets a loop too,
      // which ends itself once nothing is left open.
      if (!running.has(id)) track(id, await halvesOf(id))
      await structure(id)
      const { error } = recordingStatus(id)
      if (error) throw new Error(error)
    },
  },
]
