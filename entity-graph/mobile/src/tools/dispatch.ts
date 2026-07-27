import { refreshQueries } from '../state/query'
import { toast } from '../state/toast'
import { clearUndo } from '../state/undo'
import { openSheet } from '../state/ui'
import { currentContext } from './context'
import { isEnabled, toolById } from './registry'
import { argsOf, type ArgSpec, type ToolOutcome } from './types'

// The one door every command goes through.
//
// A button in the bottom bar, a line in the action sheet, a long-press on a row:
// all of them call `runTool` with an id and whatever they already know. What
// happens to the arguments they *don't* know is the interesting part, and it is
// the same in every case — which is the whole reason for the indirection.

const message = (e: unknown): string => (e instanceof Error ? e.message : String(e))

/** Whether every argument has been resolved, and what is missing if not. */
function resolve(
  specs: ArgSpec[],
  given: Record<string, unknown>,
  context: Record<string, unknown>,
): { args: Record<string, unknown>; missing: ArgSpec[] } {
  const args: Record<string, unknown> = {}
  const missing: ArgSpec[] = []
  for (const spec of specs) {
    if (spec.name in given && given[spec.name] !== undefined && given[spec.name] !== '') {
      args[spec.name] = given[spec.name]
      continue
    }
    // Only where the tool opted in: an argument filled by name alone would take
    // `text` from the row the user is standing on and then never show it.
    if (spec.fromContext != null && context[spec.fromContext] != null) {
      args[spec.name] = context[spec.fromContext]
      continue
    }
    // An empty optional argument never reaches `run` at all, which is how the
    // source's tools tell "not supplied" from "explicitly null".
    if (spec.optional) continue
    missing.push(spec)
  }
  return { args, missing }
}

/**
 * Run a tool.
 *
 * `given` is what the caller already knows — the row that was long-pressed, or the
 * values typed into the argument sheet. Anything still outstanding is asked for:
 * an argument that is *pointed at* puts the app into picking mode, where the
 * outline stays on screen and the next row tapped supplies it; anything else opens
 * a small form. Either way the call comes back through here with more filled in.
 */
export async function runTool(
  toolId: string,
  given: Record<string, unknown> = {},
): Promise<void> {
  const tool = toolById(toolId)
  if (!tool) {
    toast(`No tool called "${toolId}"`, 'error')
    return
  }

  const ctx = currentContext(given)
  if (!isEnabled(tool, ctx)) {
    // Reached by a stale button rather than by the sheet, which filters. Say which
    // tool, since a phone has no status bar to look at.
    toast(`${tool.label} doesn’t apply here`, 'error')
    return
  }

  const { args, missing } = resolve(argsOf(tool), given, ctx.values)

  const picked = missing.find((a) => a.pick)
  if (picked) {
    openSheet({
      kind: 'pick',
      toolId,
      args,
      argName: picked.name,
      prompt: `Tap ${picked.label}`,
    })
    return
  }
  if (missing.length) {
    openSheet({ kind: 'args', toolId, args })
    return
  }

  try {
    const outcome = ((await tool.run(args, ctx)) ?? {}) as ToolOutcome
    // A run can override the declaration when it turns out there was nothing to
    // write — committing an editor that was already empty, say — so a blur doesn't
    // cost a round of queries.
    if (outcome.mutated ?? tool.mutates ?? false) {
      refreshQueries()
      // Any write that didn't come off the undo stack invalidates it: those events
      // are no longer the store's most recent, so replaying them would land them
      // after the newer edit.
      if (!tool.preservesUndo) clearUndo()
    }
    if (outcome.message) toast(outcome.message)
  } catch (e) {
    toast(message(e), 'error')
  }
}

/** Fire a tool from an event handler, where a promise has nowhere to go. */
export const dispatch = (toolId: string, given?: Record<string, unknown>): void => {
  void runTool(toolId, given)
}
