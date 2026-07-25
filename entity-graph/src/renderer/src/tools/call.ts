import { v4 as uuid } from 'uuid'
import { buildCallContext } from '../state/derive'
import { queryAtom, refreshQueries } from '../state/query'
import { callsAtom, getLayout, pendingAtom } from '../state/store'
import { clearUndo } from '../state/undo'
import type {
  ArgValue,
  ArgValues,
  CallContext,
  CallDisplay,
  CallOutcome,
  PendingCall,
  RecordedCall,
} from '../state/types'
import {
  firstEmpty,
  missingRequired,
  nextEmptyAfter,
  nextStopAfter,
  prevStopBefore,
  resolveArgs,
  seedArgs,
} from './args'
import { findTool } from './registry'
import { argsOf, type ToolSpec } from './types'

// The pending-call state machine: starting a call, filling its arguments,
// running it, and recording what happened. Every user-triggered action in the
// app goes through here — a hotkey and a palette entry differ only in how the
// call is displayed while its arguments are collected.

const CENTRED: CallDisplay = { kind: 'palette', anchor: null }
const HIDDEN: CallDisplay = { kind: 'hidden' }

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

// --- Settling ---------------------------------------------------------------

type Listener = (call: RecordedCall) => void
const listeners = new Set<Listener>()

/**
 * Every call that finishes is announced here, whether or not it is kept in the
 * log — this is how errors and confirmations reach the toast layer.
 */
export function onCallSettled(listener: Listener): () => void {
  listeners.add(listener)
  return () => listeners.delete(listener)
}

/**
 * Retention: a cancelled call is worth keeping when the tool takes arguments
 * (there's something to resume), and a finished one only when the tool reached
 * outside the app. Source reads and writes are far too frequent, and their
 * results far too large, to persist.
 */
const worthKeeping = (tool: ToolSpec, outcome: CallOutcome): boolean =>
  outcome.kind === 'cancelled' ? argsOf(tool).length > 0 : tool.reach === 'external'

/**
 * How much of a result is kept, and how many results. The log is persisted, and
 * an integration will happily hand back a hundred pull requests: without a
 * ceiling it eventually fills localStorage, at which point the whole log
 * silently stops surviving a reload. Generous enough to read; bounded.
 */
const RESULT_CHARS = 20_000
const LOG_LENGTH = 200

/** The outcome as the log will keep it: a large result kept as its opening. */
function bounded(outcome: CallOutcome): CallOutcome {
  if (outcome.kind !== 'success' || outcome.data === undefined) return outcome
  let text: string | undefined
  try {
    text = JSON.stringify(outcome.data)
  } catch {
    // Circular, or something else that won't serialise: there is nothing to keep.
    return { ...outcome, data: undefined }
  }
  if (text === undefined || text.length <= RESULT_CHARS) return outcome
  return { ...outcome, data: { truncated: text.length, opening: text.slice(0, RESULT_CHARS) } }
}

function settle(
  call: { callId: string; toolId: string; args: ArgValues; context: CallContext; fromCallId?: string },
  tool: ToolSpec,
  outcome: CallOutcome,
): void {
  const record: RecordedCall = { ...call, settledAt: Date.now(), outcome: bounded(outcome) }
  listeners.forEach((l) => l(record))
  // Keyed by callId, so resuming and re-settling updates one entry in place
  // rather than leaving a stale row behind.
  callsAtom.set((list) => {
    const without = list.filter((r) => r.callId !== record.callId)
    return worthKeeping(tool, outcome) ? [record, ...without].slice(0, LOG_LENGTH) : without
  })
}

// --- Running ----------------------------------------------------------------

async function execute(call: {
  callId: string
  toolId: string
  args: ArgValues
  context: CallContext
  fromCallId?: string
}): Promise<void> {
  const tool = findTool(call.toolId)
  const pending = pendingAtom.get()
  if (pending?.callId === call.callId) pendingAtom.set(null)
  if (!tool) return
  // Backstop: nothing may run with a required argument outstanding. Both entry
  // points check first, but a call that slipped through here once linked an
  // entity to a blank id, which is unpleasant to clean up.
  const missing = missingRequired(tool, call.args)
  if (missing) {
    settle(call, tool, { kind: 'error', message: `${missing.label} is required` })
    return
  }
  try {
    const outcome = (await tool.run(resolveArgs(tool, call.args), {
      callId: call.callId,
      context: call.context,
    })) ?? {}
    // Anything that wrote to the entity store invalidates every open frame. A
    // tool that decided there was nothing to write says so and skips the refetch.
    if (outcome.mutated ?? tool.mutates) {
      refreshQueries()
      // A write that didn't come from the undo stack strands it: those events are
      // no longer the store's most recent, so replaying them would land them
      // after the newer edit.
      if (!tool.preservesUndo) clearUndo()
    }
    settle(call, tool, { kind: 'success', data: outcome.data, message: outcome.message })
  } catch (e) {
    settle(call, tool, { kind: 'error', message: message(e) })
  }
}

// --- Starting ---------------------------------------------------------------

const liveContext = (extra?: Record<string, unknown>): CallContext =>
  buildCallContext(getLayout(), queryAtom.get(), extra)

/**
 * Taking over the pending slot records whatever was in it, so starting a second
 * argument-collecting call doesn't silently lose the first. Note that a call
 * which runs straight through never touches the slot — pressing `w` to move the
 * selection while a link waits for its target leaves the link alone.
 */
function displacePending(nextCallId: string): void {
  const prior = pendingAtom.get()
  if (prior && prior.callId !== nextCallId && prior.toolId) cancelCall()
}

/**
 * Begin a call to a known tool. Arguments are seeded from the context; if
 * nothing is left empty the tool runs at once, with no confirmation.
 */
function begin(
  toolId: string,
  context: CallContext,
  display: CallDisplay,
  seed?: { callId?: string; args?: ArgValues; fromCallId?: string },
): void {
  const tool = findTool(toolId)
  if (!tool) return
  const callId = seed?.callId ?? uuid()
  const args = { ...seedArgs(tool, context), ...(seed?.args ?? {}) }
  const empty = firstEmpty(tool, args)
  if (empty == null) {
    void execute({ callId, toolId, args, context, fromCallId: seed?.fromCallId })
    return
  }
  // The corner guide can only serve an argument that is *pointed at*: there's
  // nowhere to type in it. Anything else opens the palette, even from a hotkey.
  const outstanding = argsOf(tool).find((a) => a.name === empty)
  const shown = display.kind === 'hidden' && !outstanding?.pick ? CENTRED : display
  displacePending(callId)
  pendingAtom.set({
    callId,
    toolId,
    args,
    activeArg: empty,
    display: shown,
    context,
    query: '',
    fromCallId: seed?.fromCallId,
  })
}

/**
 * Run a tool by id, collecting anything it still needs. `display: 'hidden'` is
 * the hotkey path — a toast names what's outstanding rather than taking over the
 * screen.
 */
export function runTool(
  toolId: string,
  opts: { display?: CallDisplay; extra?: Record<string, unknown> } = {},
): void {
  begin(toolId, liveContext(opts.extra), opts.display ?? HIDDEN)
}

/** Open the tool list: a call with no tool chosen yet. */
export function openToolList(opts: { anchor?: { x: number; y: number }; extra?: Record<string, unknown> } = {}): void {
  const callId = uuid()
  displacePending(callId)
  pendingAtom.set({
    callId,
    toolId: null,
    args: {},
    activeArg: null,
    display: { kind: 'palette', anchor: opts.anchor ?? null },
    context: liveContext(opts.extra),
    query: '',
  })
}

/**
 * The ⌘P gesture. With a call already on screen it closes; with one waiting in
 * the corner it maximises that instead of throwing it away.
 */
export function togglePalette(): void {
  const pending = pendingAtom.get()
  if (!pending) {
    openToolList()
    return
  }
  if (pending.display.kind === 'hidden') setDisplay(CENTRED)
  else cancelCall()
}

// --- Editing the pending call ----------------------------------------------

const patch = (fn: (p: PendingCall) => PendingCall): void =>
  pendingAtom.set((p) => (p ? fn(p) : p))

export const setPendingQuery = (query: string): void => patch((p) => ({ ...p, query }))

export const setDisplay = (display: CallDisplay): void => patch((p) => ({ ...p, display }))

/** Maximise the corner toast into the centred palette. */
export const maximisePending = (): void => setDisplay(CENTRED)

/** Send the palette back to the corner, leaving the call in progress. */
export const minimisePending = (): void => setDisplay(HIDDEN)

/** Pick a tool from the list, keeping the call's id and context. */
export function chooseTool(toolId: string): void {
  const pending = pendingAtom.get()
  if (!pending) {
    runTool(toolId, { display: CENTRED })
    return
  }
  begin(toolId, pending.context, pending.display, {
    callId: pending.callId,
    fromCallId: pending.fromCallId,
  })
}

/** Drop back to the tool list — Shift+Tab off the first argument. */
export const clearTool = (): void =>
  patch((p) => ({ ...p, toolId: null, args: {}, activeArg: null, query: '' }))

export const setArg = (name: string, value: ArgValue): void =>
  patch((p) => ({ ...p, args: { ...p.args, [name]: value } }))

export const setActiveArg = (name: string | null): void => patch((p) => ({ ...p, activeArg: name }))

/** Tab: forward to the next argument the context didn't fill. Never runs. */
export function advanceArg(): void {
  const p = pendingAtom.get()
  const tool = p?.toolId ? findTool(p.toolId) : null
  if (!p || !tool) return
  const next = nextStopAfter(tool, p.context, p.activeArg)
  if (next) setActiveArg(next)
}

/** Shift+Tab: back one argument, or out to the tool list from the first. */
export function retreatArg(): void {
  const p = pendingAtom.get()
  const tool = p?.toolId ? findTool(p.toolId) : null
  if (!p || !tool) return
  const prev = prevStopBefore(tool, p.activeArg)
  if (prev) setActiveArg(prev)
  else clearTool()
}

/**
 * Enter: on to the next empty argument, or run once nothing is empty. Returns a
 * message when a required argument is still missing.
 */
export function submitCall(): string | null {
  const p = pendingAtom.get()
  const tool = p?.toolId ? findTool(p.toolId) : null
  if (!p || !tool) return null
  const next = nextEmptyAfter(tool, p.args, p.activeArg)
  if (next) {
    setActiveArg(next)
    return null
  }
  const missing = missingRequired(tool, p.args)
  if (missing) {
    setActiveArg(missing.name)
    return `${missing.label} is required`
  }
  void execute({
    callId: p.callId,
    toolId: p.toolId!,
    args: p.args,
    context: p.context,
    fromCallId: p.fromCallId,
  })
  return null
}

/**
 * Fill the argument being waited on from the *live* selection, then carry on.
 * This is what makes "press x, move to the new parent, press x again" fall out
 * of the general model rather than being a special case.
 */
export function pickForPending(): void {
  const p = pendingAtom.get()
  const tool = p?.toolId ? findTool(p.toolId) : null
  if (!p || !tool || !p.activeArg) return
  const arg = argsOf(tool).find((a) => a.name === p.activeArg)
  if (!arg?.pick) return
  const picked = liveContext().values.entityId
  if (picked === undefined) return
  setArg(arg.name, { kind: 'value', value: picked })
  submitCall()
}

/** Abandon the pending call, recording it if there's something to resume. */
export function cancelCall(): void {
  const p = pendingAtom.get()
  pendingAtom.set(null)
  if (!p?.toolId) return
  const tool = findTool(p.toolId)
  if (!tool) return
  settle(
    { callId: p.callId, toolId: p.toolId, args: p.args, context: p.context, fromCallId: p.fromCallId },
    tool,
    { kind: 'cancelled' },
  )
}

// --- Replaying recorded calls ----------------------------------------------

const recorded = (callId: string): RecordedCall | undefined =>
  callsAtom.get().find((r) => r.callId === callId)

/**
 * Reopen a recorded call's arguments for editing. A cancelled one is popped from
 * the log and keeps its id, so finishing or re-cancelling it updates that entry;
 * a finished one is left alone and its replay gets a fresh id pointing back at
 * it.
 */
export function editRecordedCall(callId: string): void {
  const call = recorded(callId)
  if (!call) return
  const tool = findTool(call.toolId)
  if (!tool) return
  const replay = call.outcome.kind === 'cancelled'
  const nextId = replay ? call.callId : uuid()
  displacePending(nextId)
  if (replay) callsAtom.set((list) => list.filter((r) => r.callId !== callId))
  const args = { ...seedArgs(tool, call.context), ...call.args }
  pendingAtom.set({
    callId: nextId,
    toolId: call.toolId,
    args,
    activeArg: firstEmpty(tool, args) ?? argsOf(tool)[0]?.name ?? null,
    display: CENTRED,
    context: call.context,
    query: '',
    fromCallId: replay ? call.fromCallId : call.callId,
  })
}

/** Whether a recorded call has everything it needs to be run as it stands. */
export function isRunnable(call: RecordedCall): boolean {
  const tool = findTool(call.toolId)
  return !!tool && missingRequired(tool, call.args) == null
}

/**
 * Run a recorded call again immediately, with the arguments it was given. A call
 * that was abandoned before its last argument has nothing to run, so this opens
 * it for editing instead — landing on whatever is missing.
 */
export function rerunRecordedCall(callId: string): void {
  const call = recorded(callId)
  if (!call) return
  if (!isRunnable(call)) {
    editRecordedCall(callId)
    return
  }
  const replay = call.outcome.kind === 'cancelled'
  if (replay) callsAtom.set((list) => list.filter((r) => r.callId !== callId))
  void execute({
    callId: replay ? call.callId : uuid(),
    toolId: call.toolId,
    args: call.args,
    context: call.context,
    fromCallId: replay ? call.fromCallId : call.callId,
  })
}

export const clearCalls = (): void => callsAtom.set([])
