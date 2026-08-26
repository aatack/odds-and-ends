import { v4 as uuid } from 'uuid'
import { refreshEntities } from '../../../core/cache'
import { liveContext } from '../state/query'
import { callsAtom, pendingAtom, runningCallsAtom } from '../state/store'
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
  argsFromCall,
  firstEmpty,
  missingRequired,
  nextEmptyAfter,
  nextStopAfter,
  prevStopBefore,
  resolveArgs,
  seedArgs,
  takeCallId,
} from './args'
import { findTool, findToolByName, nearestToolNames } from './registry'
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
 * Every call *the user made* is announced here when it finishes, whether or not
 * it is kept in the log — this is how errors and confirmations reach the toast
 * layer. A script's are not: see `settle`.
 */
export function onCallSettled(listener: Listener): () => void {
  listeners.add(listener)
  return () => listeners.delete(listener)
}

/**
 * Who asked for a call. `user` is a gesture — a hotkey, the palette, a
 * right-click, a button; `code` is a script's, whether a `type: code` entity the
 * user ran or an `events` key that ran itself.
 */
type CallOrigin = 'user' | 'code'

/** One invocation: what to run, with what, and on whose behalf. */
interface Invocation {
  /** Identifies this invocation — not the tool. */
  callId: string
  toolId: string
  args: ArgValues
  context: CallContext
  /** The recorded call this one was resumed or rerun from. */
  fromCallId?: string
  origin: CallOrigin
  /** True when the caller chose `callId` — see {@link takeCallId}. */
  named?: boolean
}

/**
 * Retention: a cancelled call is worth keeping when the tool takes arguments
 * (there's something to resume), and a finished one only when the tool reached
 * outside the app. Source reads and writes are far too frequent, and their
 * results far too large, to persist.
 *
 * And nothing a script did is kept, however far it reached. The log answers "what
 * have I done?", and a script's calls are not that: one `events` key runs itself
 * every time an entity is read, so a single script left in the tree would fill
 * the log on its own and push the day's actual work off the end of it. What a
 * script did is shown where it was run — the code entity's own output. It doesn't
 * toast either; `settle` says why.
 *
 * Unless the script named the call. Choosing an id is the one thing a caller can
 * only be doing in order to point at the call afterwards — a note that watches a
 * Claude turn is the case — so a named call is kept however it came about. The
 * flood the rule above is guarding against is calls nobody chose an id for.
 */
const worthKeeping = (call: Invocation, tool: ToolSpec, outcome: CallOutcome): boolean => {
  if (call.named) return true
  if (call.origin === 'code') return false
  return outcome.kind === 'cancelled' ? argsOf(tool).length > 0 : tool.reach === 'external'
}

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

/** An invocation as the log keeps it — which is everything but who asked. */
const recordOf = (call: Invocation, outcome: CallOutcome): RecordedCall => ({
  callId: call.callId,
  toolId: call.toolId,
  args: call.args,
  context: call.context,
  ...(call.fromCallId ? { fromCallId: call.fromCallId } : {}),
  settledAt: Date.now(),
  outcome,
})

/**
 * Put a call in the log *before* it has run, so that one taking minutes — a
 * Claude session — can be watched rather than appearing from nowhere once it is
 * over. Recorded under the call's own id, so settling it later updates that row
 * in place rather than leaving a second one behind.
 *
 * Only for calls the log would keep anyway: every keystroke is a call, and a
 * record written and then removed on every press is precisely the cost the note
 * in `settle` is about.
 */
function markRunning(call: Invocation, tool: ToolSpec): void {
  const outcome: CallOutcome = { kind: 'running' }
  if (!worthKeeping(call, tool, outcome)) return
  const record = recordOf(call, outcome)
  callsAtom.set((list) =>
    [record, ...list.filter((r) => r.callId !== record.callId)].slice(0, LOG_LENGTH),
  )
}

function settle(call: Invocation, tool: ToolSpec, outcome: CallOutcome): void {
  const record = recordOf(call, bounded(outcome))
  // Announced only if the user asked for it — a toast is the app answering
  // something you just did. A script's calls are not that, and an `events` key
  // that fails would otherwise raise the same toast every time anything reads the
  // entity it sits on. What a script did, and how it failed, is shown where the
  // script is: the code entity's own output.
  if (call.origin === 'user') listeners.forEach((l) => l(record))
  // Keyed by callId, so resuming and re-settling updates one entry in place
  // rather than leaving a stale row behind.
  //
  // The list is handed back *unchanged* when this call leaves no trace, which is
  // the common case by a mile: every keystroke settles a call, and almost none of
  // them are worth keeping. Filtering unconditionally returned a new array with
  // the same contents every time — and an atom compares by identity, so that
  // notified every subscriber and wrote the whole log to localStorage on every
  // press. `App` reads this log for one badge, so the cost of that was the entire
  // app re-rendering, and a `JSON.stringify` of up to two hundred records with
  // their contexts and results, inside the keystroke that moved the cursor.
  callsAtom.set((list) => {
    const held = list.some((r) => r.callId === record.callId)
    if (!worthKeeping(call, tool, outcome)) {
      return held ? list.filter((r) => r.callId !== record.callId) : list
    }
    const without = held ? list.filter((r) => r.callId !== record.callId) : list
    return [record, ...without].slice(0, LOG_LENGTH)
  })
}

// --- Running ----------------------------------------------------------------

/** Run a call to completion, recording it. The outcome is settled either way. */
async function execute(call: Invocation): Promise<CallOutcome> {
  const tool = findTool(call.toolId)
  const pending = pendingAtom.get()
  if (pending?.callId === call.callId) pendingAtom.set(null)
  // Nothing to settle against: a record needs the tool that made it.
  if (!tool) return { kind: 'error', message: `No tool "${call.toolId}"` }
  // Backstop: nothing may run with a required argument outstanding. Both entry
  // points check first, but a call that slipped through here once linked an
  // entity to a blank id, which is unpleasant to clean up.
  const missing = missingRequired(tool, call.args)
  if (missing) {
    const failed: CallOutcome = { kind: 'error', message: `${missing.label} is required` }
    settle(call, tool, failed)
    return failed
  }
  markRunning(call, tool)
  // Apart from the log: this is what anything that started a call watches to know
  // it is still going, and almost no call is worth logging.
  runningCallsAtom.set((ids) => [...ids, call.callId])
  try {
    const outcome = (await tool.run(resolveArgs(tool, call.args), {
      callId: call.callId,
      context: call.context,
    })) ?? {}
    // Nothing is invalidated here. A write goes through `source/entity`, which
    // hands the cache the events it is making — or, where it can't, the names of
    // the entities it touched — so by the time this runs the cache is already in
    // step. Marking the whole cache unloaded after every write was what made
    // every row on screen flash its loading state on every keystroke, and what
    // had the app re-read the screen for a change to one value.
    if (outcome.mutated ?? tool.mutates) {
      // A write that didn't come from the undo stack strands it: those events are
      // no longer the store's most recent, so replaying them would land them
      // after the newer edit.
      if (!tool.preservesUndo) clearUndo()
    }
    // The exception: a tool that may have changed the store somewhere this side
    // cannot see. There is nothing to work out from, so everything is read again.
    if (tool.writesUnseen) refreshEntities()
    const succeeded: CallOutcome = { kind: 'success', data: outcome.data, message: outcome.message }
    settle(call, tool, succeeded)
    return succeeded
  } catch (e) {
    const failed: CallOutcome = { kind: 'error', message: message(e) }
    settle(call, tool, failed)
    return failed
  } finally {
    runningCallsAtom.set((ids) => ids.filter((id) => id !== call.callId))
  }
}

// --- Starting ---------------------------------------------------------------

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
 * nothing is left empty the tool runs at once, with no confirmation — unless
 * `autorun` is off, which is how Tab steps into a tool without invoking it.
 */
function begin(
  toolId: string,
  context: CallContext,
  display: CallDisplay,
  seed?: { callId?: string; args?: ArgValues; fromCallId?: string; autorun?: boolean },
): string | null {
  const tool = findTool(toolId)
  if (!tool) return null
  const callId = seed?.callId ?? uuid()
  const args = { ...seedArgs(tool, context), ...(seed?.args ?? {}) }
  const empty = firstEmpty(tool, args)
  if (empty == null && (seed?.autorun ?? true)) {
    void execute({ callId, toolId, args, context, fromCallId: seed?.fromCallId, origin: 'user' })
    return callId
  }
  // Nothing outstanding and nothing to show: a tool with no arguments at all has
  // no state to sit in, so it waits for the caller to ask for it again.
  const active = empty ?? argsOf(tool)[0]?.name ?? null
  if (active == null) return null
  // The corner guide can only serve an argument that is *pointed at*: there's
  // nowhere to type in it. Anything else opens the palette, even from a hotkey.
  const outstanding = argsOf(tool).find((a) => a.name === active)
  const shown = display.kind === 'hidden' && !outstanding?.pick ? CENTRED : display
  displacePending(callId)
  pendingAtom.set({
    callId,
    toolId,
    args,
    activeArg: active,
    display: shown,
    context,
    query: '',
    fromCallId: seed?.fromCallId,
  })
  // It is waiting on the user, not running: nothing to watch yet.
  return null
}

/**
 * Run a tool by id, collecting anything it still needs. `display: 'hidden'` is
 * the hotkey path — a toast names what's outstanding rather than taking over the
 * screen.
 *
 * `within` aims the call at a row other than the selected one — a button inside
 * an entity's own text, which acts on that entity however the keyboard is
 * pointed. See {@link contextWithin}, which is the same thing for a script.
 *
 * Hands back the id of the call it started, or null when the tool still wants an
 * argument and the palette has it instead. That is what lets whatever made the
 * gesture — a button in a row's prose — say it is still going.
 */
export function runTool(
  toolId: string,
  opts: { display?: CallDisplay; extra?: Record<string, unknown>; within?: string[] } = {},
): string | null {
  return begin(
    toolId,
    liveContext({ extra: opts.extra, within: opts.within }),
    opts.display ?? HIDDEN,
  )
}

/**
 * Open the tool list: a call with no tool chosen yet. Anchored, it was aimed at
 * something and its context fills the tool's arguments; unanchored — ⌘P, or the
 * Actions button — it was not, so the context is only offered. Otherwise every
 * tool the launcher offers would already be pointed at whatever happens to be
 * selected, which is precisely when you want to name something else.
 */
export function openToolList(opts: { anchor?: { x: number; y: number }; extra?: Record<string, unknown> } = {}): void {
  const callId = uuid()
  displacePending(callId)
  pendingAtom.set({
    callId,
    toolId: null,
    args: {},
    activeArg: null,
    display: { kind: 'palette', anchor: opts.anchor ?? null },
    context: liveContext({ extra: opts.extra, autofill: opts.anchor != null }),
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

// --- Calls from code --------------------------------------------------------

/**
 * The context a call would be born in if it were aimed at `within` — the path of
 * a row other than the selected one. What a code entity is run with, so that a
 * script's `context` describes the entity it lives on rather than wherever the
 * keyboard happens to be.
 */
export const contextWithin = (within: string[]): CallContext =>
  liveContext({ within })

/**
 * Run a tool the way a script names it: `tool.sendSlackMessage(channel, text)`.
 * Everything else about it is an ordinary call — it is recorded, it refreshes the
 * frames if it wrote anything, and its result is what the tool handed back. An
 * error is thrown rather than returned, so a script can let it fall out to the
 * run's output or catch it.
 *
 * No argument is ever prompted for: a script has no one to ask, so a missing
 * required argument is an error like any other. Nor is it kept in the log, which
 * records what the user did — this is the one path into the machine that isn't a
 * gesture, so it is the one place `origin: 'code'` is set.
 *
 * Unless it passed `$callId`, which names the call: that one is kept, because
 * naming it is how a script points at it later. See {@link takeCallId}.
 */
export async function callToolByName(
  name: string,
  passed: readonly unknown[],
  context: CallContext,
): Promise<unknown> {
  const tool = findToolByName(name)
  if (!tool) {
    const nearest = nearestToolNames(name)
    throw new Error(
      `No tool called "${name}"${nearest.length ? `. Did you mean ${nearest.join(', ')}?` : ''}`,
    )
  }
  const { args, callId } = takeCallId(passed)
  const outcome = await execute({
    callId: callId ?? uuid(),
    toolId: tool.id,
    args: argsFromCall(tool, args),
    context,
    origin: 'code',
    named: callId != null,
  })
  if (outcome.kind === 'error') throw new Error(outcome.message)
  return outcome.kind === 'success' ? outcome.data : undefined
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

/**
 * Pick a tool from the list, keeping the call's id and context. `run: false` is
 * Tab — it steps into the tool's arguments and stops there, however little is
 * left to fill in.
 */
export function chooseTool(toolId: string, opts: { run?: boolean } = {}): void {
  const pending = pendingAtom.get()
  if (!pending) {
    runTool(toolId, { display: CENTRED })
    return
  }
  begin(toolId, pending.context, pending.display, {
    callId: pending.callId,
    fromCallId: pending.fromCallId,
    autorun: opts.run ?? true,
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
    origin: 'user',
  })
  return null
}

/**
 * Fill the argument being entered from somewhere other than the keyboard — the
 * context it was started in, or what this tool was given last time — and move on
 * to whatever is still empty. Never runs, even on the last argument: these are
 * shortcuts for typing, and typing doesn't run anything either. Returns the value
 * it wrote, so the field showing it can be brought up to date.
 */
export function fillActiveArg(value: unknown): ArgValue | null {
  const p = pendingAtom.get()
  const tool = p?.toolId ? findTool(p.toolId) : null
  if (!p || !tool || !p.activeArg || value === undefined) return null
  const applied: ArgValue = { kind: 'value', value }
  const args = { ...p.args, [p.activeArg]: applied }
  pendingAtom.set({ ...p, args, activeArg: nextEmptyAfter(tool, args, p.activeArg) ?? p.activeArg })
  return applied
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
    {
      callId: p.callId,
      toolId: p.toolId,
      args: p.args,
      context: p.context,
      fromCallId: p.fromCallId,
      // Only a pending call can be abandoned, and only the user has one.
      origin: 'user',
    },
    tool,
    { kind: 'cancelled' },
  )
}

// --- Replaying recorded calls ----------------------------------------------

const recorded = (callId: string): RecordedCall | undefined =>
  callsAtom.get().find((r) => r.callId === callId)

/**
 * What this tool was last given for one of its arguments, or null when the log
 * has nothing to say. The log is newest-first, so the first match is the most
 * recent; `except` skips the call being built, since a rerun would otherwise
 * offer back the value already in the field.
 *
 * Bounded by what the log keeps, which is calls that reached outside the app and
 * ones abandoned part-way. Nothing is remembered on top of that: this is the
 * history you can already see in the activity panel, read from the other end.
 */
export function lastArgValue(
  calls: RecordedCall[],
  toolId: string,
  name: string,
  except?: string,
): unknown {
  for (const call of calls) {
    if (call.toolId !== toolId || call.callId === except) continue
    const value = call.args[name]
    if (value?.kind === 'value') return value.value
  }
  return undefined
}

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
    // Pressed in the activity panel, so it is the user's however the original
    // came about — and a call that isn't in the log cannot be rerun from it.
    origin: 'user',
  })
}

export const clearCalls = (): void => callsAtom.set([])
