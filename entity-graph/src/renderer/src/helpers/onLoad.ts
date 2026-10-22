import type { CallContext } from '../state/types'
import { runScript } from './codeRunner'

/**
 * Run a type's `onLoad` for one instance: the source is a function expression,
 * called with the instance's values (and its `entityId`) in the script sandbox.
 * What it returns goes back to the cache; what it logs goes to the console,
 * since a function the app ran on its own behalf has nowhere else to say it.
 */
export async function runOnLoad(
  entityId: string,
  code: string,
  values: Record<string, unknown>,
): Promise<unknown> {
  const entity = { ...values, entityId }
  // Born nowhere: no frame and no selection asked for this.
  const context: CallContext = {
    values: entity,
    path: [entityId],
    groupId: null,
    tabId: null,
    frameId: null,
    startedAt: Date.now(),
  }
  try {
    const { result, logs } = await runScript(`onLoad:${entityId}`, `(${code})(context)`, context)
    for (const line of logs) console.log(`[onLoad ${entityId}]`, line)
    return result
  } catch (e) {
    console.error(`[onLoad ${entityId}]`, e)
    throw e
  }
}
